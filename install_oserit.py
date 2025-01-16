import sys
import os
import subprocess
import pkg_resources


def create_folder(path, folder_name):
    """
    Create folder (folder_name) at the path if it does not exist and show them to the user
    """
    full_path = os.path.join(path, folder_name)
    # Check if the directory already exists, and create it if not
    if not os.path.exists(full_path):
        os.makedirs(full_path)
        print(f"Folder '{full_path}' created")
    else:
        print(f"Folder '{full_path}' already exists")


#If --noinput argument is given, the installation will go to the end with no further
#prompt needing the user input
usr_input = True
if len(sys.argv) > 1:
    if sys.argv[1] == "--noinput":
        usr_input = False


print("Starting OSERIT installation...\n")
oserit_dir = os.path.dirname(os.path.realpath(__file__))
print(f"oserit path used:{oserit_dir}\n")
if usr_input:
    answer = input("Proceed to the installation (y/n)?")
    while True:
        if answer == "y":
            break
        elif answer == "n":
            print("")
            exit(1)
        else:
            answer = input("use (y/n)")


library_json_fortran_version = "9.0.2"

print("\nCreation of folders...")
create_folder(oserit_dir, 'bin')
create_folder(oserit_dir, 'src/libs')
create_folder(oserit_dir, 'bin/obj')
create_folder(oserit_dir, 'bin/mod')
create_folder(oserit_dir, 'data/forcing_dynamic')
create_folder(oserit_dir, 'data/forcing_static')


#clone the lib only if needed
path_libs= f"{oserit_dir}/src/libs/"
path_json_fortran = f"{path_libs}/json-fortran/"
if not "json-fortran" in os.listdir():
    print(f"Getting the library json-fortran version {library_json_fortran_version}")
    bashCommand = f"git clone https://github.com/jacobwilliams/json-fortran.git"
    process = subprocess.Popen(bashCommand.split(), stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,cwd=f"{path_libs}")
    process.wait()
    bashCommand = f"git checkout tags/{library_json_fortran_version}"
    process = subprocess.Popen(bashCommand.split(), stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,cwd=f"{path_json_fortran}")
    process.wait()

print('\nCompilation of libraries')
print('Compilation of json-fortran...')


path_compiled = f"{path_json_fortran}/lib"

need_compilation = True

if os.path.exists(path_compiled):
    file_list = [f for f in os.listdir(path_compiled) if os.path.isfile(os.path.join(path_compiled, f))]

    for file in file_list:
        print(file)
        if file == "json_module.mod":
            need_compilation = False
            print(f"json-fortran seems to be already compiled")
            break

if need_compilation:

    create_folder(path_json_fortran, 'lib')

    bashCommand = f"cmake .."
    print(f"Running {bashCommand} in {path_compiled}")
    process = subprocess.Popen(bashCommand.split(), stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,cwd=f"{path_compiled}")
    process.wait()

    bashCommand = f"make"
    process = subprocess.Popen(bashCommand.split(), stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,cwd=f"{path_compiled}")
    print(f"Running {bashCommand} in {path_compiled} (this may take a while...)")
    process.wait()

    compilation_succeed = False

    if os.path.exists(path_compiled):
        file_list = [f for f in os.listdir(path_compiled) if os.path.isfile(os.path.join(path_compiled, f))]

        for file in file_list:
            if file == "json_module.mod":
                compilation_succeed = True
                print(f"json-fortran compilation seems to be succesful, got to src/libs/json-fortran if there is an issue with it")
                break


print("\nCompiling OSERIT")
comps_dir = f"{oserit_dir}/src/comps"
bashCommand = f"make clean"
print(f"Running {bashCommand} in {comps_dir}")
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,cwd=f"{comps_dir}")
process.wait()
bashCommand = f"make oserit_gfortran"
comps_dir = f"{oserit_dir}/src/comps"
print(f"Running {bashCommand} in {comps_dir} (this may take a while...)")
process = subprocess.Popen(bashCommand.split(), stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,cwd=f"{comps_dir}")
process.wait()

if os.path.isfile(f"{oserit_dir}/oserit"):
    print("Oserit executable exist, the installation seems succesful")
else:
    print("Oserit executable is not there, there is maybe an issue in the compilation?")
    exit(1)


if usr_input:
    answer = input("\nDo you want to run the automatic tests to verify installation (y/n)?")
    while True:
        if answer == "y":
            break
        elif answer == "n":
            print("")
            exit(0)
        else:
            answer = input("use (y/n)")


    print("Running tests")
    bashCommand = f"python3 run_test.py 4"
    test_dir = f"{oserit_dir}/tests/python"
    print(f"Running {bashCommand} in {test_dir} (this may take a while...)")
    process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE,stderr=subprocess.PIPE,cwd=f"{test_dir}")
    output, error = process.communicate()

    print("\n------Test results------\n")
    print(output)
