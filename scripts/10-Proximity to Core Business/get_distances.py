import warnings
# Suppress SyntaxWarnings from the googleplaces library (is vs ==)
warnings.filterwarnings("ignore", category=SyntaxWarning)

import pandas as pd
import os
import numpy as np
import time

try:
    import requests
except ImportError:
    requests = None

# NOTE:
# This script previously used the third-party `googleplaces` library.
# To avoid being billed for extra Place Details payload categories (Contact/Atmosphere/etc),
# it now uses Places API (New) with a strict field mask that requests ONLY coordinates.

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
except IOError:
    print(f"Warning: google_api_key.txt not found at {api_key_path}")
    API_KEY = None

# Search radius (approx 6.2 miles)
max_search = 10000


def _places_v1_search_nearby_location(api_key, zone_lat, zone_long, included_type, radius_meters, timeout_seconds=15):
    """Return (lat, lng) for the nearest matching place, or None.

    Uses Places API (New) Nearby Search endpoint and requests ONLY places.location.
    """
    if not api_key:
        return None
    if requests is None:
        raise ImportError(
            "The 'requests' package is required to call Places API (New). "
            "Install it (pip install requests) or add it to your environment."
        )

    url = "https://places.googleapis.com/v1/places:searchNearby"
    headers = {
        "Content-Type": "application/json",
        "X-Goog-Api-Key": api_key,
        # Request ONLY coordinates to minimize returned data.
        "X-Goog-FieldMask": "places.location",
    }
    payload = {
        "includedTypes": [str(included_type)],
        "maxResultCount": 1,
        "rankPreference": "DISTANCE",
        "locationRestriction": {
            "circle": {
                "center": {"latitude": float(zone_lat), "longitude": float(zone_long)},
                "radius": float(radius_meters),
            }
        },
    }

    response = requests.post(url, headers=headers, json=payload, timeout=timeout_seconds)

    # If an includedTypes value is invalid for the API, the service returns a 400.
    # In that case, fall back to a text search using the same field mask.
    if response.status_code == 400:
        return _places_v1_search_text_location(
            api_key=api_key,
            zone_lat=zone_lat,
            zone_long=zone_long,
            text_query=str(included_type),
            radius_meters=radius_meters,
            timeout_seconds=timeout_seconds,
        )

    response.raise_for_status()
    data = response.json() if response.content else {}
    places = data.get("places") or []
    if not places:
        return None

    location = places[0].get("location") or {}
    lat = location.get("latitude")
    lng = location.get("longitude")
    if lat is None or lng is None:
        return None

    return float(lat), float(lng)


def _places_v1_search_text_location(api_key, zone_lat, zone_long, text_query, radius_meters, timeout_seconds=15):
    """Fallback: Text Search (New) with strict field mask, returning nearest (lat, lng) or None."""
    if not api_key:
        return None
    if requests is None:
        raise ImportError(
            "The 'requests' package is required to call Places API (New). "
            "Install it (pip install requests) or add it to your environment."
        )

    url = "https://places.googleapis.com/v1/places:searchText"
    headers = {
        "Content-Type": "application/json",
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": "places.location",
    }
    payload = {
        "textQuery": str(text_query),
        "maxResultCount": 1,
        "rankPreference": "DISTANCE",
        "locationBias": {
            "circle": {
                "center": {"latitude": float(zone_lat), "longitude": float(zone_long)},
                "radius": float(radius_meters),
            }
        },
    }

    response = requests.post(url, headers=headers, json=payload, timeout=timeout_seconds)
    response.raise_for_status()
    data = response.json() if response.content else {}
    places = data.get("places") or []
    if not places:
        return None

    location = places[0].get("location") or {}
    lat = location.get("latitude")
    lng = location.get("longitude")
    if lat is None or lng is None:
        return None

    return float(lat), float(lng)


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
    if API_KEY is None:
        return pd.Series([zone_id, -1])
    
    # Rate limiting: sleep 100ms between calls to avoid hitting rate limits
    time.sleep(0.1)

    try:
        nearest = _places_v1_search_nearby_location(
            api_key=API_KEY,
            zone_lat=zone_lat,
            zone_long=zone_long,
            included_type=amenity,
            radius_meters=max_search,
        )

        if nearest is not None:
            nearest_lat, nearest_long = nearest
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

