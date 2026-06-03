import os, sys, time
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
import pandas as pd
pd.options.mode.chained_assignment = None
import h5py
import sqlalchemy
from shapely import wkt
import geopandas as gpd
import seaborn as sns
from itertools import cycle, islice
import pyodbc
import warnings
import matplotlib.pyplot as plt
import psrcelmerpy
import numpy as np
from scipy.spatial import cKDTree
from pathlib import Path
import tomllib

def load_config(config_path: Path) -> dict:
    with config_path.open("rb") as f:
        return tomllib.load(f)
    
CONFIG_PATH = Path(__file__).with_name("config.toml")
CONFIG = load_config(CONFIG_PATH)
HH_PERSONS_H5 = Path(CONFIG["paths"]["hh_persons_h5"])


def load_parcels_with_geography(parcels_txt: Path, db_url: str, base_year: str) -> pd.DataFrame:
    parcels = pd.read_csv(parcels_txt, delim_whitespace=True)
    gdf_lu = gpd.GeoDataFrame(parcels, geometry=gpd.points_from_xy(parcels.xcoord_p, parcels.ycoord_p))
    gdf_lu.crs = {'init' : 'EPSG:2285'}
    parcel_geog = pd.read_sql_table(f"parcel_{base_year}_geography", db_url)
    parcel_geog = parcel_geog.drop("geometry", axis=1)
    return gdf_lu.merge(parcel_geog, left_on='parcelid', right_on='ParcelID', how='left')

def get_elmer_layer(layer_name: str) -> gpd.GeoDataFrame:
    eg_conn = psrcelmerpy.ElmerGeoConn()
    layer = eg_conn.read_geolayer(layer_name)
    crs = {'init' : 'EPSG:2285'}
    return layer.to_crs(crs)

def load_population_by_parcel(h5_path: Path) -> pd.DataFrame:
    h5_file = h5py.File(h5_path, 'r')
    hh_df = pd.DataFrame()
    h5_file['Household']['hhtaz'][0]
    for col in h5_file['Household'].keys():
        hh_df[col] = h5_file['Household'][col][:]
    h5_file.close()

    pop = hh_df[['hhparcel','hhsize']].groupby('hhparcel').sum()
    pop.rename(columns={'hhsize':'population'}, inplace=True)
    return pop

def merge_population_with_parcels(parcels_df: pd.DataFrame, population_df: pd.DataFrame) -> pd.DataFrame:
    parcels_df = parcels_df.merge(population_df, left_on="parcelid", right_on="hhparcel", how="inner")
    return parcels_df

def join_school_district_with_population(new_df_lu: pd.DataFrame, school_districts: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    parcel_school_district = gpd.sjoin(
        new_df_lu[["Census2020Tract", "parcelid", "geometry", "population"]],  # keep only what you need
        school_districts[['lea_name', 'geometry']],  # keep only what you need
        how='left',     
        predicate='intersects'   # or 'intersects'
    )
    return parcel_school_district

def join_school_district_with_schools(public_schools: gpd.GeoDataFrame, school_districts: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    school_districts['lea_code'] = school_districts['lea_code'].astype(str)
    public_schools['lea_code'] = public_schools['lea_code'].astype(str)
    school_merge = school_districts.merge(public_schools, left_on='lea_code', right_on='lea_code', how='left')

    school_merge = school_merge.rename(columns={
        "geometry_x": "district_geometry", 
        "geometry_y": "school_geometry"
    })
    return school_merge

def create_school_district_dictionary(gpd: gpd.GeoDataFrame, key_col: str):
    district_dict = {
        district: group
        for district, group in gpd.groupby(key_col)
    }
    return district_dict

def find_nearest(gdA, gdB):
    """ Find nearest value between two geodataframes.
        Returns "dist" for distance between nearest points.
    """
    nA = np.array(list(gdA.geometry.apply(lambda x: (x.x, x.y))))
    nB = np.array(list(gdB.geometry.apply(lambda x: (x.x, x.y))))
    
    btree = cKDTree(nB)
    dist, idx = btree.query(nA, k=1)
    gdB_nearest = gdB.iloc[idx].drop(columns="geometry").reset_index(drop=True)
    gdf = pd.concat(
        [
            gdA.reset_index(drop=True),
            gdB_nearest,
            pd.Series(dist, name='dist')
        ], 
        axis=1)

    return gdf

def calculate_nearest_school(parcel_dict: dict, school_dict: dict):
    results = []
    for district_name in parcel_dict.keys():
        parcels = parcel_dict[district_name].copy()
        schools = school_dict[district_name].copy()
        
        parcels = parcels.rename(columns={'geometry_x': 'geometry'})
        schools = schools.rename(columns={'school_geometry': 'geometry'})

        # Make sure geometry column is active
        parcels = parcels.set_geometry("geometry")
        schools = schools.set_geometry("geometry")
        
        # Compute nearest
        nearest_df = find_nearest(parcels, schools)
        
        # Collect
        results.append(nearest_df)

    all_nearest_df = pd.concat(results, ignore_index=True)
    return all_nearest_df

def trim_columns(df: pd.DataFrame):
    all_nearest_df_small = df[["Census2020Tract", "parcelid", "population", "lea_name", "school", "dist"]]
    return all_nearest_df_small

def weighted_avg(df, val_col, wt_col, agg_col):
    """ Returns weighted average for specified aggregation. 
        
        Parameters
    ----------
    df : Pandas DataFrame 
    val_col: column name of the value being averaged
    wt_col: weight column name
    agg_col: column to be used for aggregation
    ----------
    """
    df = df.copy()
    df['wt_tot'] = df[val_col] * df[wt_col]
    
    # Aggregate sums for each group
    df_agg = df.groupby(agg_col)[[wt_col, 'wt_tot']].sum()
    
    # Compute weighted average
    df_agg['wt_avg'] = df_agg['wt_tot'] / df_agg[wt_col]
    
    return df_agg

def convert_to_miles(df, dist_col='dist'):
    df = df.copy()
    df['miles'] = df[dist_col] / 5280.0
    return df

def rename_output_column(df) -> pd.DataFrame:
    renamed_df = df.rename(columns={'Census2020Tract': 'GEOID20', 'wt_avg': 'school'})
    return renamed_df

def read_to_csv(df, output_path):
    df.to_csv(output_path, columns=['GEOID20', 'school'], index=False)

def main() -> None:
    
    print("in script")
    print("Loading parcels with geography...")
    new_df_lu = load_parcels_with_geography(CONFIG["paths"]["parcels_txt"], CONFIG["paths"]["db_url"], CONFIG["app"]["base_year"])
    population_df = load_population_by_parcel(HH_PERSONS_H5)
    new_df_lu = merge_population_with_parcels(new_df_lu, population_df)
    print(f"Number of parcels after merging with population: {len(new_df_lu)}")
    print("Loading public schools geodataframe...")
    public_schools = get_elmer_layer('public_schools')
    print("Loading school districts geodataframe...")
    school_districts = get_elmer_layer('school_districts')
    school_districts = school_districts[["lea_code", "short_name", "lea_name", "Shape", "geometry"]]

    parcel_school_district = join_school_district_with_population(new_df_lu, school_districts)
    school_merge = join_school_district_with_schools(public_schools, school_districts)

    parcel_dict = create_school_district_dictionary(parcel_school_district, "lea_name")
    school_dict = create_school_district_dictionary(school_merge, "lea_name_x")

    all_nearest_df = calculate_nearest_school(parcel_dict, school_dict)
    all_nearest_df_small = trim_columns(all_nearest_df)

    nearest_df = convert_to_miles(all_nearest_df_small)


    tract_output_df = weighted_avg(nearest_df, val_col='miles', wt_col='population', agg_col='Census2020Tract').reset_index()
    tract_output_df = rename_output_column(tract_output_df)
    read_to_csv(tract_output_df, CONFIG["paths"]["output_path"])
    print("end")

if __name__ == "__main__":
    main()
