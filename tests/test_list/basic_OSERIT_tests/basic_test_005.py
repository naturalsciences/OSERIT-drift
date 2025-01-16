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

    test_name = "out of domain"
    test_descritpion = "Test if particle go out of domain"

    speed = 1 #m/s

    forcing_val = forcings()
    forcing_val.uo = speed
    forcing_val.DOM_L_LON_M = 10000
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    cfg =  get_default_cfg(path_tmp+"/"+id)
    cfg["iopt_meteo"] = 1
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, save_wind = True)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    error = ""

    if result.variables['part_state'][0] != 0:
        error += "Particle is not starting at the surface "
    if result.variables['part_state'][-1] != 5:
        error += "Particle is not out of domain at the end of the simulation "

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
