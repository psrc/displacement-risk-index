import geopandas as gpd
import pandas as pd
from sqlalchemy import column
import getElmerGeo as geg

working_dir = r'Y:\VISION 2050\Data\Displacement\Displacement Index 2021\data\11-Proximity to Civic Infrastructure\Schools'
elmer_geo_conn_string = (
    r'Driver=SQL Server;'
    r'Server=AWS-Prod-SQL\Sockeye;'
    r'Database=ElmerGeo;'
    r'Trusted_Connection=yes;'
    )
version= "'sde.DEFAULT'"
crs = {'init' : 'EPSG:2285'}

def main():
    ## Get parcels by TAZ from UrbanSim output
    ## If access to R drive
    # parcel_dir = r'R:\e2projects_two\SoundCast\Inputs\dev\landuse\2018\new_emp'
    # parcel = pd.read_csv(parcel_dir + '\\' + 'parcels_urbansim.txt', sep = ' ')
    parcel = pd.read_csv(working_dir + '\\' + 'parcels_urbansim.txt', sep = ' ')
    parcel = parcel[['hh_p', 'parcelid']]
    parcel.rename(columns={'hh_p':'hh_p', 'parcelid':'ParcelID'}, inplace=True)

    ### Calculate weights by proportion of households (default)
    ## Get lookup table to associate parcels with census tracts
    lookup = pd.read_csv(working_dir + '\\' + 'parcel_2018_geography.csv')
    lookup = lookup[['ParcelID', 'Census2010Tract', 'taz_p']]

    joined = pd.merge(parcel, lookup, on='ParcelID')

    # Get total households by tract
    hh_tract = joined.groupby('Census2010Tract', as_index=False)['hh_p'].sum()
    # Get total households by intersection between tract and TAZ
    hh_intersect = joined.groupby(['Census2010Tract', 'taz_p'], as_index=False)['hh_p'].sum()

    # Join summarized datasets, then calculate proportion of households
    taz_hh_weights = pd.merge(hh_intersect, hh_tract, how='left', on='Census2010Tract')
    taz_hh_weights['hh_weights'] = taz_hh_weights['hh_p_x']/taz_hh_weights['hh_p_y']
    taz_hh_weights.rename(columns={'Census2010Tract':'GEOID', 'taz_p':'TAZ', 'hh_p_x':'hh_intersect', 'hh_p_y':'hh_tract', 'hh_weights':'hh_weights'}, inplace=True)

    ### Calculate weights by proportion of area (for tracts with no households)
    # Connect to Elmer
    taz = geg.read_from_sde(elmer_geo_conn_string,
                                        'taz2010_evw',
                                        version, crs=crs, is_table=False)
    taz_sub = taz[['OBJECTID', 'geometry']]
    taz_sub.rename(columns={"OBJECTID":"TAZ"}, inplace=True)

    tracts = geg.read_from_sde(elmer_geo_conn_string,
                                        'tract2020_evw',
                                        version, crs=crs, is_table=False)
    tracts_sub = tracts[['geoid20', 'geometry']]
    tracts_sub.rename(columns={"geoid20":"GEOID"}, inplace=True)
    tracts_sub['TRACTarea'] = tracts_sub['geometry'].area

    intersect = gpd.overlay(taz_sub, tracts_sub, how='intersection')
    intersect['INTarea'] = intersect['geometry'].area
    intersect['weight'] = intersect['INTarea']/intersect['TRACTarea']

    taz_area_weights = intersect[['GEOID', 'TAZ', 'weight']]
    taz_area_weights.rename(columns={'GEOID':'GEOID', 'TAZ':'TAZ', 'weight':'area_weights'}, inplace=True)
    taz_area_weights['GEOID'] = pd.to_numeric(taz_area_weights['GEOID'])

    ### Combine weighting schemes
    # 1025 missing area_weights for tract/TAZ pairs. Many parcels in a TAZ and tract that geopandas does not recognize as overlapping
    taz_tract_weights = pd.merge(taz_hh_weights, taz_area_weights, how='left', on=['TAZ', 'GEOID'])

    ### Create tract_zone_weights.csv
    taz_tract_weights.to_csv(working_dir + '\\' + 'tract_zone_weights.csv', index=False)

if __name__ == "__main__":
    main()