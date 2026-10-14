import os
import sys
import time
import matplotlib.pyplot as plt
import numpy as np

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

    test_name = "Profile as a function of Kz"
    test_descritpion = "Test if the vertical diffusion let particle stays in an uniform profile"

    forcing_val = forcings()
    forcing_val.Kz = 0.05
    forcing_val.depths = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 250
    req["drift"]["iopt_3D"] = 1
    req["drift"]["iopt_dif_coef_v"]["value"] = 2
    cfg =  get_default_cfg(path_tmp+"/"+id)
    #particle are dissolved
    cloud = get_simple_cloud(nopart=req["release"]["nopart"], state=7)
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    error = ""


    # Profile must be LINEAR

    depth = np.sort(result.variables['depth'][-1])
    n = len(depth)
    
    q = (np.arange(1, n + 1) - 0.5) / n  # fraction of particles deeper than depth
    depth_pred = -(1-q) * forcing_val.depths[-1]

    # look if OSERIT prediction follow roughtly the expected profile using Rsq
    ss_res = np.sum((depth - depth_pred) ** 2)
    ss_tot = np.sum((depth - depth.mean()) ** 2)
    r2 = 1 - ss_res / ss_tot

    if r2 < 0.99:
            error += f"R² = {r2}, it should be above 0.99, the particle are supposed to follow a linear profile when Kz is constant\n"
    
    
    # colors = plt.cm.viridis(np.linspace(0.1, 0.9, len(result.dimensions['time'])))

    # for i in range(len(result.dimensions['time'])):
    #     depths = result.variables['depth'][i]
    #     depth_profile = np.sort(depths)
    #     plt.plot(depth_profile, color=colors[i])

    # plt.show()

    #Kz non uniform
    forcing_val.Kz = [0.075, 0.075, 0.075, 0.075, 0.075, 0.025, 0.025, 0.025, 0.025, 0.025, 0.025]
    forcings_list = [forcing_val]
    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list, cloud = cloud)

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    depth = np.sort(result.variables['depth'][-1])
    n = len(depth)
    
    q = (np.arange(1, n + 1) - 0.5) / n  # fraction of particles deeper than depth
    depth_pred = -(1-q) * forcing_val.depths[-1]

    # look if OSERIT prediction follow roughtly the expected profile using Rsq
    ss_res = np.sum((depth - depth_pred) ** 2)
    ss_tot = np.sum((depth - depth.mean()) ** 2)
    r2 = 1 - ss_res / ss_tot

    if r2 < 0.99:
            error += f"R² = {r2}, it should be above 0.99, the particle are supposed to follow a linear profile even when Kz is not constant\n"
    
    
    # colors = plt.cm.viridis(np.linspace(0.1, 0.9, len(result.dimensions['time'])))

    # for i in range(len(result.dimensions['time'])):
    #     depths = result.variables['depth'][i]
    #     depth_profile = np.sort(depths)
    #     plt.plot(depth_profile, color=colors[i])

    # plt.show()

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
