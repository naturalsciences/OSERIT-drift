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

    test_name = "variable current"
    test_descritpion = "Test if a varialbe current in time is working"

    speed = 1 #m/s

    forcing_val = forcings()
    forcing_val.uo = [[speed],[speed],[speed],[speed],[speed],[0],
                        [-0],[-speed],[-speed],[-speed],[-speed],[-speed]]*forcing_val.NBR_DAY
    forcing_val.vo = [[speed],[speed],[speed],[speed],[speed],[0],
                        [-0],[-speed],[-speed],[-speed],[-speed],[-speed]]*forcing_val.NBR_DAY
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    req["time_info"]["end_time"] = "2023/01/01;21:00:00"
    cfg =  get_default_cfg(path_tmp+"/"+id)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    dx = np.diff(np.array(result.variables['lon'][:]).flatten())
    dy = np.diff(np.array(result.variables['lat'][:]).flatten())
    distances = np.sqrt(dx**2 + dy**2)
    sum_of_distances = np.sum(distances)

    dx_fin = result.variables['lon'][0]-result.variables['lon'][-1]
    dy_fin = result.variables['lat'][0]-result.variables['lat'][-1]

    lon_fl = np.array(result.variables['lon'][:]).flatten()
    lat_fl = np.array(result.variables['lat'][:]).flatten()

    error = ""
    if math.sqrt(dx_fin**2 + dy_fin**2) > sum_of_distances / 10000: #0.01%error
        error += "particle is too far from initial location\n"


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
