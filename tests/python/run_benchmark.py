import os
import shutil
import time
import numpy as np

from tests_oserit import *
from request import *
from forcing_generator import *

main_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
tmp_path = main_path+"/tmp"
oserit_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..','..'))

print("path:")
print(f"     main_path:{main_path}")
print(f"     tmp_path:{tmp_path}")
print(f"     oserit_path:{oserit_path}")

nbr_run = 10

print(f"{nbr_run} iteration will be done as benchmark")

#cleaning tmp
if os.path.isdir(tmp_path):
    shutil.rmtree(tmp_path, ignore_errors=True)

os.mkdir(tmp_path)

#preparing the test


runtitle = "benchmark"

path_tmp = tmp_path+"/"+runtitle
os.mkdir(path_tmp)



speed = 1 #m/s

forcing_val = forcings()
forcing_val.uo = speed
forcing_val.vo = speed
forcing_val.u_wind = -speed
forcing_val.v_wind = -speed
forcings_list = [forcing_val]
req = get_default_request(runtitle,oserit_path)
req['drifter_parameters']['drift_coefficient']['dwl']['min'] = 0.0315
req['drifter_parameters']['drift_coefficient']['dwl']['max'] = 0.0315
req['drifter_parameters']['drift_coefficient']['cwl']['min'] = 0.001
req['drifter_parameters']['drift_coefficient']['cwl']['max'] = 0.005
req['release']['nopart'] = 1000
req['drift']['iopt_3D'] = 1
req['drift']['iopt_wind_drift'] = 1
req['drift']['iopt_cur_drift'] = 1
req['drift']['iopt_dif'] = 1
req['drift']['iopt_leeway'] = 1
req['drift']['diffusivity_coefficients']['K_dif_x']["value"] = 1.5
req['drift']['diffusivity_coefficients']['K_dif_y']["value"] = 1.5
req['drift']['diffusivity_coefficients']['K_dif_z']["value"] = 1.5
req['drift']['iopt_dif_coef_v']["value"] = 1

cfg =  get_default_cfg(path_tmp)
cfg["iopt_meteo"] = 1
cfg["iopt_grid_interp"]["value"] = 2
cloud = get_simple_cloud(nopart=req['release']['nopart'])
add_OSERIT_inputs(runtitle, tmp_path, req, cfg, forcings_list, save_wind = True, cloud = cloud)


bashCommand = f"{oserit_path}/oserit {runtitle} {path_tmp} {path_tmp}"

times = np.zeros((nbr_run,))

for i in range(nbr_run):
    start = time.time()
    oserit = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #oserit.wait()
    stdout, stderr = oserit.communicate()
    times[i] = (time.time()-start)
    if not stderr.decode().endswith("STOP Main program terminated\n"):
        exit(1)

print(f"mean: {np.mean(times)} s")
print(f"std: {np.std(times)} s")
print(f"min: {np.min(times)} s")
print(f"max: {np.max(times)} s")
