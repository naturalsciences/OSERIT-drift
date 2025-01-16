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

    test_name = "switch forcing file"
    test_descritpion = "Test if the model can switch files for reading forcings"

    speed = 1 #m/s

    forcing_val1 = forcings()
    forcing_val1.uo = speed
    forcing_val1.vo = speed
    forcing_val1.NBR_DAY = 1
    forcing_val2 = forcings()
    forcing_val2.uo = speed
    forcing_val2.vo = speed
    forcing_val2.NBR_DAY = 1
    forcing_val2.TMSTPDAY = 24
    forcing_val2.init_date = 20230102
    forcings_list = [forcing_val1, forcing_val2]
    req = get_default_request(id,path_ose)
    req["release"]["nopart"] = 1
    req["drift"]["iopt_cur_drift"] = 1
    cfg =  get_default_cfg(path_tmp+"/"+id)

    add_OSERIT_inputs(id, path_tmp, req, cfg, forcings_list)

    dist_traveled = speed * 4 * 3600 #km

    dist_lat = dist_traveled / compute_RATIO_LATITUDE()
    dist_lon = dist_traveled / compute_RATIO_LONGITUDE(default_lat) #there will be some imprecision here

    error = ""

    date_start = ["2023/01/01;20:00:00",
                  "2023/01/01;21:00:00",
                  "2023/01/01;22:00:00",
                  "2023/01/01;23:00:00",
                  "2023/01/02;00:00:00"]

    date_end = ["2023/01/02;00:00:00",
                "2023/01/02;01:00:00",
                "2023/01/02;02:00:00",
                "2023/01/02;03:00:00",
                "2023/01/02;04:00:00"]

    for i in range(len(date_start)):

        req["release"]["release_start"] = date_start[i]
        req["release"]["release_end"] = date_start[i]
        req["time_info"]["start_time"] =date_start[i]
        req["time_info"]["end_time"] = date_end[i]
        save_request(id, path_tmp, req)
        result = run_oserit(id, path_ose, path_tmp, show_ose_res)

        delta_dist_lon = abs(result.variables['lon'][-1] - (default_lon + dist_lon))
        delta_dist_lat = abs(result.variables['lat'][-1] - (default_lat + dist_lat))

        if  delta_dist_lat > abs(dist_lat) / 100: #1%error
            error += "particle have not the good latitude\n"

        if  delta_dist_lon > abs(dist_lon) / 100: #1%error
            error += "particle have not the good longitude\n"

        if error != "":
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
