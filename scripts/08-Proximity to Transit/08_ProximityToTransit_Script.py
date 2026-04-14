import warnings
from pathlib import Path

import h5py
import pandas as pd
import geopandas as gpd
import plotly.express as px
import plotly.graph_objects as go
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

BASE_YEAR = CONFIG["app"]["base_year"]
CRS_PROJECTED = CONFIG["crs"]["projected"]
CRS_GEOGRAPHIC = CONFIG["crs"]["geographic"]
QUARTER_MILE_FEET = float(CONFIG["distance"]["quarter_mile_feet"])

TRANSIT_STOPS_CSV = Path(CONFIG["paths"]["transit_stops_csv"])
PARCELS_TXT = Path(CONFIG["paths"]["parcels_txt"])
HH_PERSONS_H5 = Path(CONFIG["paths"]["hh_persons_h5"])
SOUNCAST_DB_URL = CONFIG["paths"]["soundcast_db_url"]

OUTPUT_DIR = Path(CONFIG["paths"]["output_dir"])
POP_COVERAGE_CSV = OUTPUT_DIR / CONFIG["output_files"]["pop_coverage_csv"]
MAP_HTML = OUTPUT_DIR / CONFIG["output_files"]["map_html"]

# ============
# I/O Functions
# ============
def load_hct_stations(stops_csv: Path) -> gpd.GeoDataFrame:
    df = pd.read_csv(stops_csv)
    hct_cols = ["commuter_rail", "light_rail", "ferry", "brt", "street_car"]
    df["hct"] = (df[hct_cols].sum(axis=1) > 0).astype(int)
    df_hct = df[df["hct"] == 1].copy()

    return gpd.GeoDataFrame(
        df_hct,
        geometry=gpd.points_from_xy(df_hct["x"], df_hct["y"]),
        crs=CRS_PROJECTED,
    )


def load_parcels_with_geography(parcels_txt: Path, db_url: str, base_year: str) -> pd.DataFrame:
    parcels = pd.read_csv(parcels_txt, delim_whitespace=True)
    parcel_geog = pd.read_sql_table(f"parcel_{base_year}_geography", db_url)

    return parcels.merge(parcel_geog, left_on="parcelid", right_on="ParcelID", how="left")


def load_population_by_parcel(h5_path: Path) -> pd.DataFrame:
    with h5py.File(h5_path, "r") as h5_file:
        hh_df = pd.DataFrame({col: h5_file["Household"][col][:] for col in h5_file["Household"].keys()})

    pop = (
        hh_df[["hhparcel", "hhsize"]]
        .groupby("hhparcel", as_index=False)["hhsize"]
        .sum()
        .rename(columns={"hhsize": "population"})
    )
    return pop


# ===================
# Processing Functions
# ===================
def calculate_population_near_hct(
    parcels_df: pd.DataFrame,
    hct_gdf: gpd.GeoDataFrame,
    distance_feet: float = QUARTER_MILE_FEET,
) -> pd.DataFrame:
    gdf_lu = gpd.GeoDataFrame(
        parcels_df.copy(),
        geometry=gpd.points_from_xy(parcels_df["xcoord_p"], parcels_df["ycoord_p"]),
        crs=CRS_PROJECTED,
    )

    hct_buffer = hct_gdf.copy()
    hct_buffer["geometry"] = hct_buffer.buffer(distance_feet)

    gdf_intersect = gpd.overlay(hct_buffer, gdf_lu, how="intersection", keep_geom_type=False)
    parcels_near = gdf_lu[gdf_lu["parcelid"].isin(gdf_intersect["parcelid"].unique())].copy()

    # population within 1/4 mile by tract
    pop_near = (
        parcels_near.groupby("Census2020Tract", as_index=False)["population"]
        .sum()
        .rename(columns={"population": "population_quarter_mile"})
    )

    # total population by tract
    pop_total = parcels_df.groupby("Census2020Tract", as_index=False)["population"].sum()

    result = pop_near.merge(pop_total, on="Census2020Tract", how="right")
    result["population_quarter_mile"] = result["population_quarter_mile"].fillna(0)
    result["percent_pop_quarter_mile"] = (
        result["population_quarter_mile"] / result["population"]
    ).fillna(0)

    result = result[
        ["Census2020Tract", "population", "population_quarter_mile", "percent_pop_quarter_mile"]
    ]
    result.rename(columns={"Census2020Tract": "geoid20"}, inplace=True)
    return result


def attach_tract_geometry(pop_df: pd.DataFrame) -> gpd.GeoDataFrame:
    eg_conn = psrcelmerpy.ElmerGeoConn()
    tract2020_gdf = eg_conn.read_geolayer("tract2020")

    pop_df = pop_df.copy()
    pop_df["geoid20"] = pop_df["geoid20"].astype(str).str.split(".").str[0]

    merged = tract2020_gdf.merge(
        pop_df,
        left_on="geoid20",
        right_on="geoid20",
        how="left",
    )
    merged["population_quarter_mile"] = merged["population_quarter_mile"].fillna(0)
    merged["percent_pop_quarter_mile"] = merged["percent_pop_quarter_mile"].fillna(0)

    return merged


def build_population_map(
    tract_gdf: gpd.GeoDataFrame,
    hct_gdf: gpd.GeoDataFrame,
) -> go.Figure:
    tract_geo = tract_gdf.to_crs(CRS_GEOGRAPHIC)
    hct_geo = hct_gdf.to_crs(CRS_GEOGRAPHIC)

    fig = px.choropleth_mapbox(
        tract_geo,
        geojson=tract_geo.geometry.__geo_interface__,
        locations=tract_geo.index,
        color="percent_pop_quarter_mile",
        hover_name="geoid20",
        hover_data={
            "population": ":,.0f",
            "population_quarter_mile": ":,.0f",
            "percent_pop_quarter_mile": ":.1%",
        },
        color_continuous_scale="OrRd",
        mapbox_style="open-street-map",
        center={"lat": 47.6, "lon": -122.3},
        zoom=8,
        opacity=0.7,
        title="Population within 1/4 Mile of HCT Stations by Census Tract",
    )

    fig.add_trace(
        go.Scattermapbox(
            lat=hct_geo.geometry.y,
            lon=hct_geo.geometry.x,
            mode="markers",
            marker={"size": 8, "color": "blue", "opacity": 0.8},
            text=hct_geo.apply(
                lambda r: (
                    f"Station ID: {r.get('stop_id', 'N/A')}<br>"
                    f"Light Rail: {'Yes' if r.get('light_rail', 0) else 'No'}<br>"
                    f"BRT: {'Yes' if r.get('brt', 0) else 'No'}<br>"
                    f"Commuter Rail: {'Yes' if r.get('commuter_rail', 0) else 'No'}<br>"
                    f"Ferry: {'Yes' if r.get('ferry', 0) else 'No'}<br>"
                    f"Street Car: {'Yes' if r.get('street_car', 0) else 'No'}"
                ),
                axis=1,
            ),
            hovertemplate="<b>HCT Station</b><br>%{text}<extra></extra>",
            name="HCT Stations",
        )
    )

    fig.update_layout(
        title_x=0.5,
        height=800,
        coloraxis_colorbar={
            "title": "% of population within 1/4 mile",
            "tickformat": ".0%",
        },
    )
    return fig


# ==========
# Entry Point
# ==========
def main() -> None:
    print("[1/8] Creating output directory...")
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print("[2/8] Loading HCT stations...")
    hct_gdf = load_hct_stations(TRANSIT_STOPS_CSV)
    print(f"      Loaded {len(hct_gdf):,} HCT stations.")

    print("[3/8] Loading parcels + geography...")
    parcels_df = load_parcels_with_geography(PARCELS_TXT, SOUNCAST_DB_URL, BASE_YEAR)
    print(f"      Loaded {len(parcels_df):,} parcels.")

    print("[4/8] Loading parcel population from hh_and_persons.h5...")
    pop_by_parcel = load_population_by_parcel(HH_PERSONS_H5)
    print(f"      Loaded population for {len(pop_by_parcel):,} parcels.")

    print("[5/8] Merging parcel population...")
    parcels_df = parcels_df.merge(pop_by_parcel, left_on="parcelid", right_on="hhparcel", how="inner")
    print(f"      Parcels after population merge: {len(parcels_df):,}")

    print("[6/8] Calculating 1/4-mile HCT population coverage by tract...")
    pop_coverage_df = calculate_population_near_hct(parcels_df, hct_gdf)
    print(f"      Tracts in result: {len(pop_coverage_df):,}")

    print("[7/8] Writing coverage CSV...")
    pop_coverage_df.to_csv(POP_COVERAGE_CSV, index=False)
    print(f"      Saved: {POP_COVERAGE_CSV}")

    print("[8/8] Building map + saving HTML...")
    tract_gdf = attach_tract_geometry(pop_coverage_df)
    fig = build_population_map(tract_gdf, hct_gdf)
    fig.write_html(MAP_HTML)
    print(f"      Saved: {MAP_HTML}")

    print("[Done] Displaying map...")
    fig.show()

if __name__ == "__main__":
    main()
