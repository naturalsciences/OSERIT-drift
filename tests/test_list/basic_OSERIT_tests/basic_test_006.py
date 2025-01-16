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
import netcdf_cf_helper as nc_h

(id, path_tmp, path_ose, show_ose_res, force_start) = get_args()

init_test(id, path_tmp, force_start)


test_succes = False
test_name = "TO BE RENAME"
test_descritpion = "TO BE DONE"

try:

    """Here start the custom content of the test"""

    test_name = "stranding"
    test_descritpion = "Test of iopt_stranding"

    speed = 1 #m/s

    forcing_val = forcings()
    forcing_val.uo = speed
    forcing_val.DOM_L_LON_M = 10000
    forcings_list = [forcing_val]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    cfg =  get_default_cfg(path_tmp+"/"+id)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    #OVERWRITE THe bathymetry with this one
    nc_file_h = nc_h.netcdf_file( f'{path_tmp}/{id}/bathymetry.nc',
                                    "bathymetry",
                                    forcing_val.longitudes,
                                    forcing_val.latitudes)
    depth_ar = np.full((len(forcing_val.longitudes), len(forcing_val.latitudes)), forcing_val.bath_depth)
    depth_ar[-5,:] = 1e10 #not in the water
    nc_file_h.add_variable_lon_lat('deptho',depth_ar)
    nc_file_h.add_info_var('deptho',
                         "Bathymetry",
                         "sea_floor_depth_below_geoid",
                         "m")
    nc_file_h.close()

    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    error = ""

    if result.variables['part_state'][-1] != 0:
        error += "Particle is not at the surface at the end when iopt_part_stranding is deactivated "


    #redo simulation but with stranding
    req["drift"]["iopt_stranding"] = 1
    save_request(id, path_tmp, req)
    result = run_oserit(id, path_ose, path_tmp, show_ose_res)

    if result.variables['part_state'][-1] != 3:
        error += "Particle is not stranded at the end when iopt_part_stranding is deactivated "


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
