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

    test_name = "swtich to water column state due to w_imposed"
    test_descritpion = "Test if a particle have the good state when it switch to water column and the correct depth"

    speed = -0.01 #m/s

    forcing_val = forcings()
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["w_imposed"]['value'] = -0.01
    req["drift"]["iopt_3D"] = 1

    cfg =  get_default_cfg(path_tmp+"/"+id)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    depth_final = speed * 22 * 3600

    error = ""
    if  result.variables['lon'][-1] !=  np.float32(default_lon):
        error += "particle have not the good latitude\n"

    if  result.variables['lat'][-1] !=  np.float32(default_lat):
        error += "particle have not the good latitude\n"

    if abs(result.variables['depth'][-1] -depth_final) > abs(depth_final/100):#more than 1% error
        error += "particle not at the good depth"

    for i in range(len(result.variables['depth'][:])):
        if ((result.variables['depth'][i][0] == 0 and result.variables['part_state'][i][0] != 0)
            or (result.variables['depth'][i][0] < 0 and result.variables['part_state'][i][0] != 1)):
            error += f"wrong state, depth of {result.variables['depth'][i][0]} and state of {result.variables['part_state'][i][0]}"
            break

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
