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

    test_name = "test iopt_part_cur"
    test_descritpion = "Test if a 1m current to the NORTHEAST push a particle to the correct location and then deactivate the current"

    speed = 1 #m/s

    forcing_val = forcings()
    forcing_val.uo = speed
    forcing_val.vo = speed
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    cfg =  get_default_cfg(path_tmp+"/"+id)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    dist_traveled = speed * 22 * 3600 #km

    dist_lat = dist_traveled / compute_RATIO_LATITUDE()
    dist_lon = dist_traveled / compute_RATIO_LONGITUDE(default_lat) #there will be some imprecision here

    delta_dist_lon = abs(result.variables['lon'][-1] - (default_lon + dist_lon))
    delta_dist_lat = abs(result.variables['lat'][-1] - (default_lat + dist_lat))

    error = ""
    if  delta_dist_lat > abs(dist_lat) / 100: #1%error
        error += "particle have not the good latitude when current\n"

    if  delta_dist_lon > abs(dist_lon) / 100: #1%error
        error += "particle have not the good longitude when current\n"

    req["drift"]["iopt_cur_drift"] = 0
    save_request(id, path_tmp, req)
    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if result.variables['lon'][-1] != result.variables['lon'][0] or result.variables['lat'][-1] != result.variables['lat'][0]:
        error += "particle has moved when it should not have"

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
