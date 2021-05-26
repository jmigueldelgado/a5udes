import ee
import fiona
from shapely.geometry import shape, polygon
import json

with fiona.open('data/bb.gpkg','r') as bb:
    poly=next(iter(bb))
polystring=json.dumps(poly)

ee.Authenticate()
ee.Initialize()
dataset=ee.Image("WWF/HydroSHEDS/30CONDEM").select('b1')

# https://developers.google.com/earth-engine/apidocs/ee-geometry?hl=en

area_of_interest=ee.Geometry.Polygon(poly['geometry']['coordinates'])

task=ee.batch.Export.image.toDrive(image=dataset,region=area_of_interest,description='export_DEM',fileNamePrefix='export_DEM',scale=1000,crs='EPSG:4326')
task.start()
task.status()
