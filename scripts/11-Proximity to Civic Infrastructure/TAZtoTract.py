import pandas as pd
import os
import numpy as np

working_dir = r'Y:\VISION 2050\Data\Displacement\Displacement Index 2021\data\11-Proximity to Civic Infrastructure\Schools'
tract_zone_file = 'tract_zone_weights.csv'
zone_distances = 'TAZ_Near_Analysis_2021_Update.xlsx'
out_file_school = '11-DistanceToSchools.csv'
# out_file_parks = '11-DistanceToParks.csv'

def get_tract_distances(zones_distances,tract,amenity_types):
    zone_dist_tract = pd.merge(zones_distances, tract, on='TAZ')
    g = zone_dist_tract.groupby('GEOID', as_index=False)
    # have to also take care of if there is weird missing data (hh_p)
    tract_distances = g.apply(lambda x: pd.Series(np.average(x[amenity_types], weights=x['hh_weights'], axis=0), amenity_types)
                                  if x['hh_weights'].sum()>0 
                                  else
                                  pd.Series(np.average(x[amenity_types], weights=x['area_weights'], axis=0), amenity_types)
                                  )

    return tract_distances

def main():
    
    tracts = pd.read_csv(working_dir + '\\DistanceScore\\' + tract_zone_file)

    amenity_types = ['school']
    zones_distance_school = pd.read_excel(working_dir + '\\' + zone_distances)
    zones_distance_school_df = zones_distance_school[['TAZ','Nearest School (Miles)']]
    zones_distance_school_df.rename(columns={'TAZ':'TAZ', 'Nearest School (Miles)':'school'}, inplace=True)
    tract_distances = get_tract_distances(zones_distance_school_df, tracts, amenity_types)
    tract_distances.to_csv(working_dir + '\\' + out_file_school, index=False)

    ## The parks dataset was not updated for the 2018BY from the 2014BY. Thus, these lines were not run for the update.
    # amenity_types = ['parks']
    # zones_distance_park = pd.read_excel(working_dir +'\\'+ zone_distances, sheetname= 'Parks')
    # zones_distance_park_df = zones_distance_park[['TAZ','parks']]
    # tract_distances = get_tract_distances(zones_distance_park_df,tracts, amenity_types)
    # tract_distances.to_csv(working_dir+'\\'+out_file_parks)

if __name__ == "__main__":
    main()