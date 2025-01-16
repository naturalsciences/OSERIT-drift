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

    test_name = "Not outputting every timestep"
    test_descritpion = "Test if only exporting part of the total timestep is working"

    forcings_list = [forcings()]
    req = get_default_request(id,path_ose)
    cfg =  get_default_cfg(path_tmp+"/"+id)
    req['release']['nopart'] = 1
    req['time_info']['end_time']="2023/01/01;22:50:00"
    req['drift']['tmstp_per_outtmstp']['value'] = 1
    req['drift']['iopt_force_last_output'] = 1
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    error = ""

    if len(result.dimensions['time']) != 132:
            error += f"not the correct number of timestep for 1 timestep per output timestep\n"

    req['drift']['tmstp_per_outtmstp']['value'] = 2
    req['drift']['iopt_force_last_output'] = 0
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if len(result.dimensions['time']) != 66:
            error += f"{len(result.dimensions['time'])} is not the correct number of timestep for 2 timestep per output timestep and not forcing last\n"

    req['drift']['tmstp_per_outtmstp']['value'] = 2
    req['drift']['iopt_force_last_output'] = 1
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if len(result.dimensions['time']) != 67:
            error += f"{len(result.dimensions['time'])} is not the correct number of timestep for 2 timestep per output timestep and forcing last\n"

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
