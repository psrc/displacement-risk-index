from pathlib import Path
import warnings

import pandas as pd
import geopandas as gpd
import psrcelmerpy

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:
    import tomli as tomllib  # pip install tomli

warnings.filterwarnings("ignore")
pd.options.mode.chained_assignment = None


def load_config(config_path: Path) -> dict:
    with config_path.open("rb") as f:
        return tomllib.load(f)


CONFIG_PATH = Path(__file__).with_name("config.toml")
CONFIG = load_config(CONFIG_PATH)

CRS_PROJECTED = CONFIG["crs"]["projected"]
BUFFER_QUARTER_MILE_FEET = float(CONFIG["buffer"]["quarter_mile_feet"])
BUFFER_HALF_MILE_FEET = float(CONFIG["buffer"]["half_mile_feet"])

TRANSIT_STOPS_CSV = Path(CONFIG["paths"]["transit_stops_csv"])
OUTPUT_DIR = Path(CONFIG["paths"]["output_dir"])
TRACT_NOWATER_EXPORT_CSV = OUTPUT_DIR / CONFIG["output_files"]["tract_nowater_csv"]
AREA_COVERAGE_EXPORT_CSV = OUTPUT_DIR / CONFIG["output_files"]["area_coverage_csv"]


# =================
# Helper Functions
# =================
def load_transit_stops(file_path: Path) -> pd.DataFrame:
    return pd.read_csv(file_path)


def classify_hct_stops(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    df = df.copy()

    # 0.5-mile set: commuter rail, light rail, ferry, express
    df["hct_0_5"] = (df[["commuter_rail", "light_rail", "ferry", "express"]].sum(axis=1) > 0).astype(int)
    df_hct_05 = df[df["hct_0_5"] == 1].copy()

    # 0.25-mile set: brt
    df["hct_0_25"] = (df[["brt"]].sum(axis=1) > 0).astype(int)
    df_hct_025 = df[df["hct_0_25"] == 1].copy()

    return df_hct_05, df_hct_025


def build_station_geodataframes(
    df_hct_05: pd.DataFrame, df_hct_025: pd.DataFrame
) -> tuple[gpd.GeoDataFrame, gpd.GeoDataFrame]:
    gdf_hct_05 = gpd.GeoDataFrame(
        df_hct_05,
        geometry=gpd.points_from_xy(df_hct_05["x"], df_hct_05["y"]),
        crs=CRS_PROJECTED,
    )
    gdf_hct_025 = gpd.GeoDataFrame(
        df_hct_025,
        geometry=gpd.points_from_xy(df_hct_025["x"], df_hct_025["y"]),
        crs=CRS_PROJECTED,
    )
    return gdf_hct_05, gdf_hct_025


def load_tracts_nowater() -> gpd.GeoDataFrame:
    eg_conn = psrcelmerpy.ElmerGeoConn()
    tracts = eg_conn.read_geolayer("tract2020_nowater")
    return tracts.to_crs(CRS_PROJECTED)


def build_combined_hct_buffer(
    gdf_hct_05: gpd.GeoDataFrame, gdf_hct_025: gpd.GeoDataFrame
) -> gpd.GeoDataFrame:
    gdf_05 = gdf_hct_05.copy()
    gdf_025 = gdf_hct_025.copy()

    gdf_05["geometry"] = gdf_05.buffer(BUFFER_HALF_MILE_FEET)
    gdf_025["geometry"] = gdf_025.buffer(BUFFER_QUARTER_MILE_FEET)

    combined = pd.concat([gdf_05, gdf_025], ignore_index=True)
    combined = gpd.GeoDataFrame(combined, geometry="geometry", crs=CRS_PROJECTED)

    # Dissolve to a single merged buffer geometry
    return combined.dissolve()


def calculate_area_coverage(
    tracts_nowater: gpd.GeoDataFrame, combined_hct_buffer: gpd.GeoDataFrame
) -> pd.DataFrame:
    area_gdf = gpd.overlay(
        tracts_nowater,
        combined_hct_buffer,
        how="intersection",
    ).reset_index(drop=True)

    # Keeping original output column name for compatibility
    area_gdf["quarter_mile_hct_sq_ft"] = area_gdf.geometry.area

    tracts = tracts_nowater.copy()
    tracts["total_area"] = tracts.geometry.area

    result = tracts[["geoid20", "total_area"]].merge(
        area_gdf[["geoid20", "quarter_mile_hct_sq_ft"]],
        on="geoid20",
        how="left",
    )

    # Fill NA values with 0
    result["quarter_mile_hct_sq_ft"] = result["quarter_mile_hct_sq_ft"].fillna(0)
    result["total_area"] = result["total_area"].fillna(0)

    # Avoid divide-by-zero
    result["percent_hct"] = 0.0
    mask = result["total_area"] > 0
    result.loc[mask, "percent_hct"] = (
        result.loc[mask, "quarter_mile_hct_sq_ft"] / result.loc[mask, "total_area"]
    )
    return result
    # result["percent_hct"] = result["quarter_mile_hct_sq_ft"] / result["total_area"].fillna(0)
    # return result


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    stops_df = load_transit_stops(TRANSIT_STOPS_CSV)
    df_hct_05, df_hct_025 = classify_hct_stops(stops_df)

    gdf_hct_05, gdf_hct_025 = build_station_geodataframes(df_hct_05, df_hct_025)

    tracts_nowater = load_tracts_nowater()
    tracts_nowater.to_csv(TRACT_NOWATER_EXPORT_CSV, index=False)

    combined_hct_buffer = build_combined_hct_buffer(gdf_hct_05, gdf_hct_025)

    result = calculate_area_coverage(tracts_nowater, combined_hct_buffer)
    result.to_csv(AREA_COVERAGE_EXPORT_CSV, index=False)

    print(f"Saved: {TRACT_NOWATER_EXPORT_CSV}")
    print(f"Saved: {AREA_COVERAGE_EXPORT_CSV}")


if __name__ == "__main__":
    main()