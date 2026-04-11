
import pandas as pd
import os
import numpy as np

# Optional override: set this to the folder containing the input CSVs.
working_dir = r''
script_dir = os.path.dirname(os.path.abspath(__file__))
if not working_dir:
    working_dir = script_dir
tract_zone_file = 'tract_zone_hh_file.csv'
zone_distances = 'zone_dist_amenity.csv'
out_file = 'tract_dist_amenity.csv'

amenity_types = ['supermarket','pharmacy', 'restaurant']




def get_tract_distances(zones_distances,tract):
    zone_dist_tract = pd.merge(zones_distances, tract, left_on = 'ZoneID', right_on = 'TAZ')
    g= zone_dist_tract.groupby('GEOID10')
    # have to also take care of if there is weird missing data (hh_p)
    def weighted_avg(x):
        weights = pd.to_numeric(x.get('hh_p'), errors='coerce').fillna(0)
        out = {}
        for amenity in amenity_types:
            values = pd.to_numeric(x.get(amenity), errors='coerce')
            # Historical zone-distance files used -1 to indicate missing/error.
            # Treat any negative value as missing so it is excluded from averages.
            values = values.where(values >= 0)
            valid = values.notna()
            # Distances equal to 10000 are treated as a capped max-search sentinel:
            # they contribute to the average distance, but are excluded from the
            # "contributing" count.
            contributing = valid & (values != 10000)
            out[f"{amenity}_n"] = int(contributing.sum())

            if not valid.any():
                out[amenity] = np.nan
                continue
            if weights.loc[valid].sum() > 0:
                out[amenity] = float(np.average(values.loc[valid], weights=weights.loc[valid]))
            else:
                out[amenity] = float(np.average(values.loc[valid]))

        ordered_cols = []
        for amenity in amenity_types:
            ordered_cols.append(amenity)
            ordered_cols.append(f"{amenity}_n")
        return pd.Series(out, index=ordered_cols)

    tract_distances = g.apply(weighted_avg)

    return tract_distances


def _read_zone_distances_csv(amenity: str) -> pd.DataFrame:
    """Read the per-amenity zone distance CSV produced by get_distances.py.

    Supports a common historical filename typo: 'pharmarcy ...' vs 'pharmacy ...'.
    """
    candidate_names = [f"{amenity} {zone_distances}"]
    if amenity == 'pharmacy':
        candidate_names.append(f"pharmarcy {zone_distances}")

    for name in candidate_names:
        path = os.path.join(working_dir, name)
        if os.path.exists(path):
            df = pd.read_csv(path)

            if 'ZoneID' not in df.columns:
                raise ValueError(f"{name} is missing required column 'ZoneID'")

            # Some historical outputs include multiple amenity columns because the
            # zone-level script writes a cumulative table after each amenity.
            # Keep ONLY the requested amenity column to avoid merge suffixes.
            distance_col = None
            if amenity in df.columns:
                distance_col = amenity
            elif amenity == 'pharmacy' and 'pharmarcy' in df.columns:
                distance_col = 'pharmarcy'
            else:
                # Fallback: pick the first non-ZoneID, non-index-like column.
                candidates = [
                    c for c in df.columns
                    if c not in {'ZoneID'} and not str(c).lower().startswith('unnamed')
                ]
                if len(candidates) == 1:
                    distance_col = candidates[0]

            if distance_col is None:
                raise ValueError(
                    f"{name} does not contain a usable distance column for amenity '{amenity}'. "
                    f"Columns: {list(df.columns)}"
                )

            out = df[['ZoneID', distance_col]].copy()
            out.rename(columns={distance_col: amenity}, inplace=True)
            return out

    raise FileNotFoundError(
        f"Could not find any zone distance CSV for amenity '{amenity}'. Tried: {candidate_names}"
    )

def main():
    
    tracts = pd.read_csv(os.path.join(working_dir, tract_zone_file))
    first_amenity = True
    zones_distances_df = None

    for amenity in amenity_types:
        if first_amenity:
            zones_distances_df = _read_zone_distances_csv(amenity)
            first_amenity = False
        else:
            assert zones_distances_df is not None
            zones_distances_df = pd.merge(
                zones_distances_df,
                _read_zone_distances_csv(amenity),
                on='ZoneID',
            )

    if zones_distances_df is None:
        raise ValueError("No amenity types configured; nothing to aggregate.")
    
    # Ensure -1/negative sentinel values are treated as missing before aggregation.
    for amenity in amenity_types:
        zones_distances_df[amenity] = pd.to_numeric(zones_distances_df[amenity], errors='coerce')
        zones_distances_df.loc[zones_distances_df[amenity] < 0, amenity] = np.nan

    tract_distances = get_tract_distances(zones_distances_df,tracts)
    tract_distances.to_csv(os.path.join(working_dir, out_file))

if __name__ == "__main__":
    main()

