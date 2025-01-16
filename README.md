# OSERIT-drift

OSERIT-drift (Oil Spill Evaluation and Response Integrated Tool) is a Lagrangian tracking model to simulate drift at sea. This version is able to simulate it with current, wind and waves.

## Installation

Some prerequisite are needed to install OSERIT:

- gfortran
- cmake
- NetCDF for fortran  with shared libraries (<https://docs.unidata.ucar.edu/netcdf-c/current/building_netcdf_fortran.html>)
- Some dependencies in python are needed in the tests (numpy, netCDF4, json, datetime, pytz, datetime)

### Automatic Installation

To run an automatic installation, run the "install_oserit.py" script: `python3 install_oserit.py`
In case of issue, go to "Manual installation"

If this script is use with the `python3 install_oserit.py --noinput` keyword, everything will be accepted without being prompted, should only be used by advanced users.

### Manual  Installation

#### Creating folders

The following folders must be created:

 1. `bin`
 2. `bin/obj`
 3. `bin/mod`
 4. `src/libs/json-fortran-8.4.0/lib`

The following folder can be created but are not mandatory to make OSERIT works:

 1. `log`
 2. `data/forcing_dynamic`
 3. `data/forcing_static`

#### Compilation of json-fortran

json-fortran is needed in order to make OSERIT able to read and write json files. To do so, cmake must be installed first. then, go to `src/libs/json-fortran-8.4.0/lib` and make the two following command:

1. `cmake ..`
2. `make`

#### Compilation of OSERIT

In `src/comps`, run theses commands:

1. `cp -n ../../procs/compilers_template.cmp compilers.cmp`
2. `make clean`
3. `make oserit_gfortran`

In some system, the file src/comps/compilers.cmp could need to be updated. Update this file accordingly if you encounter issues, it is not in the version control.

#### Verifying installation  

If everything until there has run fine, there should be an executable `oserit` in the root directory. To test it, run `./oserit`, it should return `starting oserit`, and then ask for several other arguments.
There are several tests installed to make sure that all is in order. To run them, in `tests/python` run `python run_test.py 4` (4 is the number of tests run in parallel, it must be at least 1 and can be as large as wanted)
If not all tests are successful, there is an issue somewhere (several python module must be installed to make the tets run succesfully, like `json`, `netCDF4`, `numpy`...).

## Starting a new simulation

### Quick guide

Once the code is compiled, a new simulation can be run using the command `./oserit` on the executable, created a the root of the git folder.
There are 3 arguments needed to start the simulation: the runtitle, the input directory and the output directory.

- runtitle is the name of the simulation
- input directory is the path to the directory where the input files are located
- output directory is the path to the directory where the output files will be generated.

The command will then looks like this: `./oserit name path/input path/output`

The input and output folders can be the same, but they must created in advance and the input folder must contains the 3 following files (if the simulation name is `example`)

- `example_cfg.json`
- `example_request.json`
- `example_initial_location.json` (only if the cloud is not generated, but this method is prefered if possible)

 Example files are presents in `simulations/default_files`

A file `example_PAout.nc` will be created with the simulation results. It will erase the results if another file of the same name was present.

### Where to put the simulations requests and results?

The `simulations` folder has been designed for that, folders can be created in it. They will be ignored by the version management system.

### Where do I put the forcings?

The folders `data/forcing_dynamic` and `data/forcing_static` have been designed for that and will be ignored by the version management system.
