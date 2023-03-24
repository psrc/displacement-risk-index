import pyodbc
from pandas import read_sql
from shapely import wkt
import geopandas as gpd
import pandas as pd

def read_from_sde(connection_string, feature_class_name, version,
                  crs={'init': 'epsg:2285'}, is_table=False):
    """
    Returns the specified feature class as a geodataframe from ElmerGeo.

    Parameters
    ----------
    connection_string : SQL connection string that is read by geopandas
                        read_sql function

    feature_class_name: the name of the featureclass in PSRC's ElmerGeo
                        Geodatabase

    cs: cordinate system
    """

    con = pyodbc.connect(connection_string)
    con.execute("sde.set_current_version {0}".format(version))

    if is_table:
        gdf = pd.read_sql('select * from %s' %
                          (feature_class_name), con=con)
        con.close()

    else:
        df = pd.read_sql('select *, Shape.STAsText() as geometry from %s' %
                         (feature_class_name), con=con)
        con.close()

        df['geometry'] = df['geometry'].apply(wkt.loads)
        gdf = gpd.GeoDataFrame(df, geometry='geometry')
        gdf.crs = crs
        cols = [col for col in gdf.columns if col not in
                ['Shape', 'GDB_GEOMATTR_DATA', 'SDE_STATE_ID']]
        gdf = gdf[cols]

    return gdf

