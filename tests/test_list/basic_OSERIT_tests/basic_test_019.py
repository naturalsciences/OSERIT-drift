import os
import sys
import time

# Add the path to the 'python' directory to the sys.path list
current_dir = os.path.dirname(os.path.realpath(__file__))
parent_dir = os.path.dirname(os.path.dirname(current_dir))
sys.path.append(os.path.join(parent_dir, 'python'))

from tests_oserit import *
from request import *
from forcing_generator import *

(id, path_tmp, path_ose, show_ose_res, force_start) = get_args()

init_test(id, path_tmp, force_start)


test_succes = False
test_name = "TO BE RENAME"
test_descritpion = "TO BE DONE"

try:

    """Here start the custom content of the test"""

    test_name = "simple wave"
    test_descritpion = "Test for wave direction"

    forcing_val = forcings()
    forcing_val.depths = [0, 3, 5, 10, 15, 20, 25, 30]
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_wave_drift"] = 1
    cfg =  get_default_cfg(path_tmp+"/"+id)
    cfg["iopt_wave"] = 1

    #to the East
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, save_wave = True)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    error = ""
    if result.variables['lon'][-1][0] <= default_lon:
        error += "particle should go to East"
    
    if result.variables['lat'][-1][0] != default_lat:
        error += "particle should stay at its latitude"

    #to the North anticlockwise
   
    forcings_list[0].dirw = 90.0
    forcings_list[0].u_stockes = 0
    forcings_list[0].v_stockes = 0.1
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, save_wave = True)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if result.variables['lon'][-1][0] != default_lon:
        error += "particle should stay at its longitude"
    
    if result.variables['lat'][-1][0] <= default_lat:
        error += "particle should go to the North"

    #to the south clockwise - 90

    cfg["domains"][0]["waves"]["iopt_clockwise_dirw"]["value"] = 1
    cfg["domains"][0]["waves"]["direction_dirw"]["value"] = 180.0

    forcings_list[0].dirw = -90.0
    forcings_list[0].u_stockes = 0
    forcings_list[0].v_stockes = -0.1
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, save_wave = True)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if result.variables['lon'][-1][0] != default_lon:
        error += "particle should stay at its longitude"
    
    if result.variables['lat'][-1][0] >= default_lat:
        error += "particle should go to the South"


    #particle underwater should go less far

    dist_srfc = result.variables['lat'][-1]
    cloud = get_simple_cloud(depth = -1)
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, save_wave = True, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if result.variables['lon'][-1][0] != default_lon:
        error += "particle should stay at its longitude"
    
    if result.variables['lat'][-1][0] >= default_lat:
        error += "particle should go to the South"

    if result.variables['lat'][-1][0] <= dist_srfc:
        error += "particle underwater should go slower"


    """Here end the custom content of the test"""

    if error != "":
        raise Exception(error)

    test_succes = True
except Exception as e:
    print(e)


if test_succes:
    print(f"success:{test_name}")
    cleanup_test(id, path_tmp)
    exit(0)
else:
    print(f"failed:{test_name} {id}")
    exit(1)
