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

    test_name = "resuspension"
    test_descritpion = "Test if a particle can go and get stuck at the bottom, then resuspensded with uv current"

    speed = 1 #m/s

    forcing_val = forcings()
    forcing_val.wo = [[-speed],[-speed],[-speed],[-speed],[-speed],[-speed]
                        ,[speed],[speed],[speed],[speed],[speed],[speed]]*forcing_val.NBR_DAY
    forcing_val.uo = [[0],[0],[0],[0],[0],[0]
                        ,[speed],[speed],[speed],[speed],[speed],[speed]]*forcing_val.NBR_DAY
    forcing_val.vo = [[0],[0],[0],[0],[0],[0]
                    ,[speed],[speed],[speed],[speed],[speed],[speed]]*forcing_val.NBR_DAY
    forcings_list = [forcing_val]
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_3D"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    req["drift"]["resuspension"]["iopt_resuspension"] = 1
    req["drift"]["resuspension"]["min_speed"]["value"]=1.1
    req["drift"]["resuspension"]["height"]["value"]=1.1
    req["drift"]["iopt_resurfacing"] = 1

    cfg =  get_default_cfg(path_tmp+"/"+id)
    cloud = get_simple_cloud(depth = -15)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)
    
    error = ""

    if result.variables['part_state'][0] != 1:
        error += "Particle should start in the water column"

    if result.variables['part_state'][18] != 4:
        error += "Particle should be at the seabed after 3h"
    
    if result.variables['part_state'][-1] != 0:
        error += "Particle should be at the surface at the end"

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
