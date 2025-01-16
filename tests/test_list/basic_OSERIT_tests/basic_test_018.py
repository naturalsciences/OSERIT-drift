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

    test_name = "tidal cycle not putting particle at seabed"
    test_descritpion = "The tidal cycle should not put particle at the seabed by itself"


    forcing_val = forcings()
    forcing_val.uniform_depth = 0

    forcing_val.tidal_cycle = [1 + 0.5 * math.cos(i / forcing_val.TMSTPDAY * 2 * math.pi) for i in range(forcing_val.NBR_DAY * forcing_val.TMSTPDAY)] #generate the tidal cycle

    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_3D"] = 1
    cloud = get_simple_cloud(depth = -2900)
    cfg =  get_default_cfg(path_tmp+"/"+id)

    cfg["domains"][0]["hydro"]["iopt_uniform_depth"] = 0
    cfg["domains"][0]["hydro"]["look_up_table"]["value"][11] = 11


    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)


    error = ""
    if  result.variables["part_state"][-1] != 1: #must still be in the water column
        error += "Particle is not in the water column anymore\n"

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
