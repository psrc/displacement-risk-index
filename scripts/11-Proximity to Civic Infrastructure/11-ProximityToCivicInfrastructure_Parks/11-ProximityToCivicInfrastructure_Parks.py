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
    return gdf_lu.merge(parcel_geog, left_on='parcelid', right_on='ParcelID', how='left')

def load_population_by_parcel(h5_path: Path) -> pd.DataFrame:
    h5_file = h5py.File(h5_path, 'r')
    hh_df = pd.DataFrame()
    h5_file['Household']['hhtaz'][0]
    for col in h5_file['Household'].keys():
        hh_df[col] = h5_file['Household'][col][:]
    h5_file.close()

    pop = hh_df[['hhparcel','hhsize']].groupby('hhparcel').sum()
    pop.rename(columns={'hhsize':'population'}, inplace=True)
    # with h5py.File(h5_path, "r") as h5_file:
    #     hh_df = pd.DataFrame({col: h5_file["Household"][col][:] for col in h5_file["Household"].keys()})

    # pop = (
    #     hh_df[["hhparcel", "hhsize"]]
    #     .groupby("hhparcel", as_index=False)["hhsize"]
    #     .sum()
    #     .rename(columns={"hhsize": "population"})
    # )
    return pop

def merge_population_with_parcels(parcels_df: pd.DataFrame, population_df: pd.DataFrame) -> pd.DataFrame:
    parcels_df = parcels_df.merge(population_df, left_on="parcelid", right_on="hhparcel", how="inner")
    parcels_df = parcels_df[["parcelid", "Census2020Tract", "geometry", "population"]]
    return parcels_df

def rename_geometry_column(df) -> pd.DataFrame:
    df = df.copy()
    new_df = df.rename(columns={'geometry_x': 'geometry'})
    return new_df

def get_parks_gdf() -> gpd.GeoDataFrame:
    eg_conn = psrcelmerpy.ElmerGeoConn()
    parks_gdf = eg_conn.read_geolayer('open_space_parks').query("site_type == 'Park'")
    park_centroids = parks_gdf.copy()
    crs = {'init' : 'EPSG:2285'}
    park_centroids = park_centroids.to_crs(crs)
    park_centroids['geometry'] = park_centroids.geometry.centroid    
    return park_centroids

""" Find nearest value between two geodataframes.
    Returns "dist" for distance between nearest points.
"""
def find_nearest(gdA, gdB):
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

def convert_to_miles(df, dist_col='dist'):
    df = df.copy()
    df['miles'] = df[dist_col] / 5280.0
    return df

""" Returns weighted average for specified aggregation. 
    
    Parameters
----------
df : Pandas DataFrame 
val_col: column name of the value being averaged
wt_col: weight column name
agg_col: column to be used for aggregation
----------
"""
def weighted_avg(df, val_col, wt_col, agg_col):
    df = df.copy()
    df['wt_tot'] = df[val_col] * df[wt_col]
    
    # Aggregate sums for each group
    df_agg = df.groupby(agg_col)[[wt_col, 'wt_tot']].sum()
    
    # Compute weighted average
    df_agg['parks'] = df_agg['wt_tot'] / df_agg[wt_col]
    return df_agg

def rename_output_column(df) -> pd.DataFrame:
    renamed_df = df.rename(columns={'Census2020Tract': 'GEOID20'})
    return renamed_df

def read_to_csv(df, output_path):
    df.to_csv(output_path, columns=['GEOID20', 'parks'], index=False)

def main() -> None:
    print("Loading parcels with geography...")
    new_df_lu = load_parcels_with_geography(CONFIG["paths"]["parcels_txt"], CONFIG["paths"]["db_url"], CONFIG["app"]["base_year"])
    population_df = load_population_by_parcel(HH_PERSONS_H5)
    new_df_lu = rename_geometry_column(new_df_lu)
    new_df_lu = merge_population_with_parcels(new_df_lu, population_df)
    print(f"Number of parcels after merging with population: {len(new_df_lu)}")
    print("Loading parks geodataframe...")
    parks_df = get_parks_gdf()  
    nearest_df = find_nearest(new_df_lu, parks_df)
    nearest_df = convert_to_miles(nearest_df)
    tract_output_df = weighted_avg(nearest_df, val_col='miles', wt_col='population',
                                 agg_col='Census2020Tract').reset_index()
    tract_output_df = rename_output_column(tract_output_df)
    read_to_csv(tract_output_df, CONFIG["paths"]["output_path"])


if __name__ == "__main__":
    main()