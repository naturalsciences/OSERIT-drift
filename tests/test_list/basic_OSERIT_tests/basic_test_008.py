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

    test_name = "vertical w imposed subtimestepping"
    test_descritpion = "Test of a w imposed for 3D at different subtimestepping"

    speed = -0.01 #m/s

    forcing_val = forcings()
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["w_imposed"]["value"] = speed
    req["drift"]["iopt_3D"] = 1
    req["drift"]["nbr_subtmstp_vertical"]["value"] = 10
    cfg =  get_default_cfg(path_tmp+"/"+id)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    dist_traveled = speed * 22 * 3600

    delta_dist = abs(result.variables['depth'][-1] - dist_traveled)

    error = ""
    if  delta_dist > abs(dist_traveled) / 100: #1%error
        error += "particle have not the good depth at 10 vertical subtmstp\n"

    req["drift"]["nbr_subtmstp_vertical"]["value"] = 1
    save_request(id, path_tmp, req)
    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    delta_dist = abs(result.variables['depth'][-1] - dist_traveled)

    if  delta_dist > abs(dist_traveled) / 100: #1%error
        error += "particle have not the good depth at 1 vertical subtmstp\n"

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
