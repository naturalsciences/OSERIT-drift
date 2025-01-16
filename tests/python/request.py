import copy
import json
import numpy as np
from datetime import datetime

default_lon = 2
default_lat = 51.2

default_cfg = {
   "iopt_wave":0,
   "iopt_meteo":0,
   "max_stockes":10,
   "iopt_grid_interp":{
      "value":2,
      "keys":[
         "Nearest_neighboor",
         "Nearest_neighboor_faster",
         "Trilinear_interpolation"
      ]
   },
   "domains":[
     {
       "hydro":{
         "filename":"",
         "file_duration":{
            "value":"86400",
            "units":"s"
         },
         "start_time":{
            "value":"0",
            "units":"s"
         },
         "iopt_uniform_depth":1,
         "look_up_table":{
           "value":[
             3,1,2,4,3,1,2,4,5,6,7,-1,8,9,10,-1
           ],
           "name":[
              "dim_times",
              "dim_lons",
              "dim_lats",
              "dim_depths",
              "times",
              "lons",
              "lats",
              "depths",
              "uvel",
              "vvel",
              "wvel",
              "zeta",
              "temperature",
              "salinity",
              "density",
              "turbulent diffusivity z"
           ]
         }
       },
       "meteo":{
         "filename":"",
         "file_duration":{
            "value":"86400",
            "units":"s"
         },
         "start_time":{
            "value":"0",
            "units":"s"
         },
         "look_up_table":{
           "value":[
             3,1,2,3,1,2,4,5,6,-1,-1,-1
           ],
           "name":[
              "dim_time",
              "dim_lon",
              "dim_lat",
              "times",
              "lons",
              "lats",
              "uvel wind",
              "vvel wind",
              "temperature",
              "low cloud cover",
              "medium cloud cover",
              "high cloud cover"
           ]
         }
       },
       "waves":{
         "filename":"",
         "file_duration":{
          "value":"86400",
          "units":"s"
          },
          "start_time":{
              "value":"0",
              "units":"s"
          },
         "direction_dirw":{
            "value":0.0,
            "units":"degrees"
         },
         "iopt_clockwise_dirw":{
          "value":0,
          "keys":[
            "counterclockwise",
            "clockwise"
          ]
        },
         "look_up_table":{
           "value":[
             3,1,2,3,1,2,4,5,6,7,8
           ],
           "name":[
              "dim_time",
              "dim_lon",
              "dim_lat",
              "times",
              "lons",
              "lats",
              "hstt",
              "Tm1",
              "dirw",
              "u_stockes",
              "v_stockes"

           ]
         }
       },
       "bathymetry":{
         "filename":"",
         "look_up_table":{
           "value":[
             1,2,1,2,3
           ],
           "name":[
              "dim_lon",
              "dim_lats",
              "lons",
              "lats",
              "depthmean"
           ]
         }
       }

     }
   ]
}





def get_simple_cloud(lon=default_lon, lat=default_lat, depth=0, state = -1, nopart=1, time=datetime(2023, 1, 1, 1, 0, 0), dispersion=0):
   data = {
      "type": "FeatureCollection",
      "features": [
         {
               "type": "Feature",
               "properties": {
                  "time": time.isoformat() + 'Z'
               },
               "geometry": {
                  "type": "MultiPoint",
                  "coordinates": []
               }
         }
      ]
   }
   if dispersion ==0:
      for i in range(nopart):
         data["features"][0]["geometry"]["coordinates"].append([
                              lon,
                              lat,
                              depth
                          ])
   else:
      for i in range(nopart):
         phi = np.random.uniform(0, 2*np.pi)
         r = np.random.uniform(0, dispersion)
         x = r*np.cos(phi) + lon
         y = r*np.sin(phi) + lat
         data["features"][0]["geometry"]["coordinates"].append([
                              x,
                              y,
                              depth
                          ])
   if state > -1:
         data["features"][0]["properties"]["state"] = state

   return data



def get_default_request(name, path_ose):
   with open(f"{path_ose}/simulations/default_files/default_request.json", 'r') as file:
      copy_request = json.load(file)
   return copy_request

def get_default_cfg(path_forcing):
    copy_cfg = copy.deepcopy(default_cfg)
    copy_cfg['domains'][0]['meteo']['filename'] = f"{path_forcing}/wind_"
    copy_cfg['domains'][0]['hydro']['filename'] = f"{path_forcing}/hydro_"
    copy_cfg['domains'][0]['waves']['filename'] = f"{path_forcing}/waves_"
    copy_cfg['domains'][0]['bathymetry']['filename'] = f"{path_forcing}/bathymetry.nc"
    return copy_cfg
