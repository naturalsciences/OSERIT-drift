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

    test_name = "vertical diffusion not putting particle at surface"
    test_descritpion = "Test iopt_prevent_dif_surface"

    forcing_val = forcings()
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["drift"]["iopt_3D"] = 1
    req['release']['nopart'] = 1
    req["drift"]["iopt_dif_coef_v"]["value"] = 1
    req["drift"]["iopt_prevent_dif_seabed"] = 1
    req["drift"]["iopt_resurfacing"] = 1
    req["drift"]["diffusivity_coefficients"]["K_dif_z"]["value"] = 10000
    cfg =  get_default_cfg(path_tmp+"/"+id)

    cloud = get_simple_cloud(depth = -5, state=1)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    error = ""

    if result.variables['part_state'][-1] != 0:
        error += f"particle state should be at the surface when iopt_prevent_dif_surface=0"
    
    #prevent the diffusion to put particle at the seabed
    req["drift"]["iopt_prevent_dif_surface"] = 1

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if result.variables['part_state'][-1] != 1:
        error += f"particle state should be in water column when iopt_prevent_dif_surface=1"

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
