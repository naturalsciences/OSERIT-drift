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

    test_name = "constant current at different depth with non uniform forcing"
    test_descritpion = "Compare drift at varying depth with a current varying with the depth with non uniform forcing"

    speed = 1 #m/s

    forcing_val = forcings()
    forcing_val.uo = [speed, speed, speed, -speed, -speed, -speed, speed / 2]
    forcing_val.vo = [speed, speed, speed, -speed, -speed, -speed, speed / 2]
    forcing_val.uniform_depth = 0
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    req["drift"]["iopt_3D"] = 1
    cloud = get_simple_cloud(depth = 0)
    cfg =  get_default_cfg(path_tmp+"/"+id)

    cfg["domains"][0]["hydro"]["uniform_depth"] = 0
    cfg["domains"][0]["hydro"]["look_up_table"]["value"][11] = 11


    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    dist_traveled = speed * 22 * 3600 #km

    dist_lat = dist_traveled / compute_RATIO_LATITUDE()
    dist_lon = dist_traveled / compute_RATIO_LONGITUDE(default_lat) #there will be some imprecision here

    delta_dist_lon = abs(result.variables['lon'][-1] - (default_lon + dist_lon))
    delta_dist_lat = abs(result.variables['lat'][-1] - (default_lat + dist_lat))

    error = ""
    if  delta_dist_lat > abs(dist_lat) / 100: #1%error
        error += "surface particle have not the good latitude\n"

    if  delta_dist_lon > abs(dist_lon) / 100: #1%error
        error += "surface particle have not the good longitude\n"

    #the particle underwater with current in the other direction
    cloud = get_simple_cloud(depth = -15)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    dist_traveled = -speed * 22 * 3600 #km

    dist_lat = dist_traveled / compute_RATIO_LATITUDE()
    dist_lon = dist_traveled / compute_RATIO_LONGITUDE(default_lat) #there will be some imprecision here

    delta_dist_lon = abs(result.variables['lon'][-1] - (default_lon + dist_lon))
    delta_dist_lat = abs(result.variables['lat'][-1] - (default_lat + dist_lat))

    error = ""
    if  delta_dist_lat > abs(dist_lat) / 100: #1%error
        error += "underwater particle -15 have not the good latitude\n"

    if  delta_dist_lon > abs(dist_lon) / 100: #1%error
        error += "underwater particle -15 have not the good longitude\n"


    #the particle underwater with current in the other direction at 0.5 time speed
    cloud = get_simple_cloud(depth = -100)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    dist_traveled = 0.5 * speed * 22 * 3600 #km

    dist_lat = dist_traveled / compute_RATIO_LATITUDE()
    dist_lon = dist_traveled / compute_RATIO_LONGITUDE(default_lat) #there will be some imprecision here

    delta_dist_lon = abs(result.variables['lon'][-1] - (default_lon + dist_lon))
    delta_dist_lat = abs(result.variables['lat'][-1] - (default_lat + dist_lat))


    error = ""
    if  delta_dist_lat > abs(dist_lat) / 100: #1%error
        error += "underwater particle -100 have not the good latitude\n"

    if  delta_dist_lon > abs(dist_lon) / 100: #1%error
        error += "underwater particle -100 have not the good longitude\n"

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
