import os
import shutil
import subprocess
import argparse
import tests_oserit


parser = argparse.ArgumentParser()
parser.add_argument("nbr_parallel", type=int, help="Number of test to launch in parallel")
parser.add_argument('-l', '--list_path', nargs='+', default=[], help="Absolute paths to the tests folders. THe basic tests are already integrated")
args = parser.parse_args()

nbr_test_parallel_max = args.nbr_parallel

list_paths = args.list_path

main_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
tmp_path = main_path+"/tmp"
oserit_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..','..'))

print("path:")
print(f"     main_path:{main_path}")
print(f"     tmp_path:{tmp_path}")
print(f"     oserit_path:{oserit_path}")

list_paths.append(main_path+"/test_list/basic_OSERIT_tests")

if os.path.isdir(tmp_path):
    shutil.rmtree(tmp_path, ignore_errors=True)

os.mkdir(tmp_path)
#will contain the command and the name of the file test in a tuple
list_files_to_start = []

id_test = 0

#Making a list with all the test
for path_folder in list_paths:
    print(f"Looking for tests in {path_folder}")
    for id, filename in enumerate(os.listdir(path_folder)):
        f = os.path.join(path_folder, filename)
        bashCommand = f"python3 {f} {id_test} {tmp_path} {oserit_path} False False"
        list_files_to_start.append((bashCommand.split(),filename,[id, tmp_path+"/"+str(id)]))
        id_test+=1

print("")

nbr_failed = 0
nbr_success = 0

#will contain the process and the name of the file test in a tuple
list_of_runnning_test = []
list_of_command = []
list_failed = []

while len(list_files_to_start) > 0 or len(list_of_runnning_test) > 0:
    #looking if a new test must be launched
    if len(list_files_to_start) > 0 and len(list_of_runnning_test) < nbr_test_parallel_max:
        list_of_runnning_test.append((subprocess.Popen(list_files_to_start[0][0], stdout=subprocess.PIPE,stderr=subprocess.PIPE),list_files_to_start[0][1],list_files_to_start[0][2]))
        list_to_add = list_files_to_start[0][0]
        list_to_add[-1] = "True"
        list_to_add[-2] = "True"
        list_of_command.append(list_to_add)
        list_files_to_start.pop(0) #remove the first element

    #looking if a test is finished
    i = 0
    while i <len(list_of_runnning_test):
        error_code = list_of_runnning_test[i][0].poll()
        if error_code is not None: #test finished
            stdout, stderr = list_of_runnning_test[i][0].communicate()
            file_name = list_of_runnning_test[i][1]
            params_cleanup = list_of_runnning_test[i][2]
            del list_of_runnning_test[i]
            if error_code == 0: #no error
                tests_oserit.cleanup_test(params_cleanup[0], params_cleanup[1])
                print(f"Success: {file_name} {stdout} {stderr}")
                nbr_success += 1
            else:
                print(f"Failed: {file_name} {stdout} {stderr}")
                list_failed.append(f"Failed: {file_name} {stdout} {stderr} command: {' '.join(list_of_command[i])}")
                nbr_failed +=1
            del list_of_command[i]

            i -= 1#because one element has been removed
        i += 1
print("")
print("List of failed tests:")
for tests_f in list_failed:
    print(tests_f)
print("")
print(f"Success: {nbr_success}, Failed: {nbr_failed}")
