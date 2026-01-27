import warnings
# Suppress SyntaxWarnings from the googleplaces library (is vs ==)
warnings.filterwarnings("ignore", category=SyntaxWarning)

from googleplaces import GooglePlaces
import pandas as pd
import os
import numpy as np
import time

# where to find googleplaces: https://github.com/slimkrazy/python-google-places

# Set working directory to the parent directory (get_distances/)
# script is in get_distances/get_distances/get_distances.py
script_dir = os.path.dirname(os.path.abspath(__file__))
# Data directory is .../11-Proximity to Civic Infrastructure/get_distances/
working_dir = os.path.dirname(script_dir)

# ================= CONFIGURATION =================
TEST_MODE = False  # Set to True to test with minimal API calls (1 zone, 3 amenities)

# If True, fetch tract/TAZ household weights directly from Elmer via psrcelmerpy
# instead of reading a CSV from disk.
USE_ELMER_FOR_TRACT_ZONE_HH = True
# =================================================

zone_file = 'zone_lat_long.csv'
tract_zone_hh_file = 'tract_zone.csv'
zone_distances_file = 'zone_dist_amenity.csv'
out_file = 'tract_dist_amenity.csv'

amenity_types = ['supermarket', 'pharmacy', 'restaurant']

api_key_path = os.path.join(working_dir, 'google_api_key.txt')
try:
    with open(api_key_path, 'r') as f:
        API_KEY = f.read().strip()
    google_places = GooglePlaces(API_KEY)
except IOError:
    print(f"Warning: google_api_key.txt not found at {api_key_path}")
    google_places = None

# Search radius (approx 6.2 miles)
max_search = 10000


def load_tract_zone_hh_from_elmer():
    """Load tract/TAZ household weights from Elmer.

    Returns a DataFrame with columns:
    - taz_p (numeric)
    - GEOID (string)
    - hh_p (numeric)
    """
    try:
        import psrcelmerpy
    except ImportError as e:
        raise ImportError(
            "psrcelmerpy is required when USE_ELMER_FOR_TRACT_ZONE_HH=True. "
            "Install it in your environment, or set USE_ELMER_FOR_TRACT_ZONE_HH=False."
        ) from e

    tract_zone_hh_sql = """
        WITH cte AS (
            SELECT
                MAX(ofm_vintage) AS max_vintage,
                MAX(estimate_year) AS max_year
            FROM Elmer.ofm.parcelized_saep_facts
        )
        SELECT
            CAST(taz_2010 AS numeric) AS taz_p,
            tract_geoid20 AS GEOID,
            SUM(s.household_pop) AS hh_p
        FROM Elmer.small_areas.parcel_dim p
        JOIN Elmer.ofm.parcelized_saep_facts s
            ON p.parcel_dim_id = s.parcel_dim_id
        JOIN cte ON 1 = 1
        WHERE s.estimate_year = cte.max_year
          AND s.ofm_vintage = cte.max_vintage
        GROUP BY CAST(taz_2010 AS numeric), tract_geoid20
        HAVING SUM(s.household_pop) > 0
        ORDER BY CAST(taz_2010 AS numeric), tract_geoid20;
    """.strip()

    e_conn = psrcelmerpy.ElmerConn()
    tract_zone_hh = e_conn.get_query(tract_zone_hh_sql)

    # Defensive normalization (types + expected column names)
    required_cols = {"taz_p", "GEOID", "hh_p"}
    missing = required_cols - set(tract_zone_hh.columns)
    if missing:
        raise ValueError(f"Elmer query result missing columns: {sorted(missing)}")

    tract_zone_hh["taz_p"] = pd.to_numeric(tract_zone_hh["taz_p"], errors="coerce")
    tract_zone_hh["hh_p"] = pd.to_numeric(tract_zone_hh["hh_p"], errors="coerce")
    tract_zone_hh["GEOID"] = tract_zone_hh["GEOID"].astype(str)

    return tract_zone_hh

def distance(s_lat, s_lng, e_lat, e_lng):
    # approximate radius of earth in km
    R = 3959.0
    
    s_lat = s_lat*np.pi/180.0                      
    s_lng = np.deg2rad(s_lng)     
    e_lat = np.deg2rad(e_lat)                       
    e_lng = np.deg2rad(e_lng)  
    
    d = np.sin((e_lat - s_lat)/2)**2 + np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2
    
    return 2 * R * np.arcsin(np.sqrt(d))

def find_distance(zone_id, zone_lat, zone_long, amenity):
    if google_places is None:
        return pd.Series([zone_id, -1])
    
    # Rate limiting: sleep 100ms between calls to avoid hitting rate limits
    time.sleep(0.1)

    try:
        query_result = google_places.nearby_search(keyword=amenity,
            lat_lng={'lat': zone_lat, 'lng': zone_long}, rankby = 'distance', 
            radius=max_search)

        if query_result.places:
            nearest_lat = float(query_result.places[0].geo_location['lat'])
            nearest_long = float(query_result.places[0].geo_location['lng'])
            dist_between = distance(zone_lat, zone_long, nearest_lat, nearest_long)
        else:
            print(f"No {amenity} found within {max_search}m for zone {zone_id}")
            dist_between = -1
    except Exception as e:
        print(f"Error calling API for zone {zone_id} amenity {amenity}: {e}")
        # Return -1 to indicate error/no result found
        dist_between = -1

    return pd.Series([zone_id, dist_between])

def get_distances(zones):
    amenity_count = 0
    zones_out = None
    for amenity in amenity_types:
        print(amenity)
        if amenity_count == 0:
            zones_out = zones.apply(lambda row: find_distance(row['ZoneID'], row['LAT'], row['LONG'], amenity), axis=1)
            zones_out.columns = ['ZoneID', amenity]
        else:
            zones_next = zones.apply(lambda row: find_distance(row['ZoneID'], row['LAT'], row['LONG'], amenity), axis=1)
            zones_next.columns = ['ZoneID', amenity]
            zones_out = pd.merge(zones_next, zones_out, on ='ZoneID')
        
        amenity_count = amenity_count + 1
        output_path = os.path.join(working_dir, f"{amenity} {zone_distances_file}")
        zones_out.to_csv(output_path)

    return zones_out

def get_tract_distances(zones_distances, tract_zones_hh):
    zone_dist_tract = pd.merge(zones_distances, tract_zones_hh, left_on = 'ZoneID', right_on = 'taz_p')
    g = zone_dist_tract.groupby('GEOID')

    def weighted_avg(x):
        try:
            if x['hh_p'].sum() > 0:
                return pd.Series(np.average(x[amenity_types], weights=x['hh_p'], axis=0), index=amenity_types)
            else:
                return pd.Series(np.average(x[amenity_types], axis=0), index=amenity_types)
        except ZeroDivisionError:
             return pd.Series([np.nan]*len(amenity_types), index=amenity_types)
        except Exception:
             return pd.Series([np.nan]*len(amenity_types), index=amenity_types)

    tract_distances = g.apply(weighted_avg, include_groups=False)
    return tract_distances

def main():
    zone_path = os.path.join(working_dir, zone_file)
    tract_zone_path = os.path.join(working_dir, tract_zone_hh_file)

    if not os.path.exists(zone_path):
        print(f"Error: {zone_path} not found.")
        # Proceeding might crash, but let standard errors happen or return
        # return

    if (not USE_ELMER_FOR_TRACT_ZONE_HH) and (not os.path.exists(tract_zone_path)):
        print(f"Error: {tract_zone_path} not found.")
        # return
    
    # Load zone centroids
    if not os.path.exists(zone_path):
        print("Required input files missing. check paths.")
        return

    zones = pd.read_csv(zone_path)
    if TEST_MODE:
        print("Running in TEST MODE: Processing only the first zone.")
        zones = zones.head(1)

    # Load tract/TAZ weights
    if USE_ELMER_FOR_TRACT_ZONE_HH:
        tract_zones_hh = load_tract_zone_hh_from_elmer()
    else:
        if not os.path.exists(tract_zone_path):
            print("Required input files missing. check paths.")
            return
        tract_zones_hh = pd.read_csv(tract_zone_path)

    zones_distances = get_distances(zones)
    zones_distances.to_csv(os.path.join(working_dir, zone_distances_file))

    tract_distances = get_tract_distances(zones_distances, tract_zones_hh)
    tract_distances.to_csv(os.path.join(working_dir, out_file))

if __name__ == "__main__":
    main()

