import os
import shutil
import request as req
import subprocess
import json
import time
import sys
import math
import netCDF4
import numpy as np

def init_test(id, path, force_start):
    if os.path.exists(f"{path}/{id}"):
        if force_start:
            return #will not raise the exception and the file should not be created
        raise Exception(f"test has already been created")
    os.mkdir(f"{path}/{id}")


def add_OSERIT_inputs(id, path, request, cfg, forcings, cloud=None, save_wind = False, save_wave = False):
    """
    This will create the json and netcdf files needed to launch OSERIT
    request and cfg are dictionnary that will be converted to json
    forcing is a list of forcing object to will be converted to netcdf
    If cloud is not at "None", it will be put to be read as the initial cloud. Otherwise generate a default simple cloud
    """
    save_request(id, path, request)
    with open(f"{path}/{id}/{id}_cfg.json",'w') as json_file:
            json.dump(cfg, json_file, indent=4)
    for forcing in forcings:
        forcing.save_forcing(f"{path}/{id}", save_wind, save_wave)
    if cloud is None:
        cloud = req.get_simple_cloud()
    with open(f"{path}/{id}/{id}_initial_location.json",'w') as json_file:
            json.dump(cloud, json_file, indent=4)


def save_request(id, path, request):
    """
    save the request
    """
    with open(f"{path}/{id}/{id}_request.json",'w') as json_file:
            json.dump(request, json_file, indent=4)

def cleanup_test(id, path):
    """
    This will remove the files after the test
    """
    shutil.rmtree(f"{path}/{id}", ignore_errors=True)#there are .nfs files blocking the deletion


def get_args():
    """
    get the arguments for a test
    """
    id = sys.argv[1]
    path_tmp = sys.argv[2]
    path_ose = sys.argv[3]
    show_ose_res = (sys.argv[4] == "True" or sys.argv[4] == "1")
    force_start = (sys.argv[5] == "True" or sys.argv[5] == "1")
    return (id, path_tmp, path_ose, show_ose_res, force_start)


def run_oserit(id, path_ose, path_work, need_output):
    """
    run OSERIT in the directory path_work from path_ose
    """
    bashCommand = f"{path_ose}/oserit {id} {path_work}/{id} {path_work}/{id}"
    if need_output:
        print(bashCommand)
    oserit = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #oserit.wait()
    stdout, stderr = oserit.communicate()
    if need_output:
        print(stdout)
        print(stderr)
    return netCDF4.Dataset(f"{path_work}/{id}/{id}_PAout.nc")

def compute_RATIO_LATITUDE():
    """
    Compute the conversion ratio from latitude in degrees to meter
    """
    return 6371000 * 2 * math.pi / 360

def compute_RATIO_LONGITUDE(latitude):
    """
    Compute the conversion ratio from longitude in degrees to meter, use the latitude
    of the interest longitude in order to work
    """
    lat_radius = math.cos(latitude*math.pi /180) * 6371000
    return lat_radius * 2 * math.pi / 360
