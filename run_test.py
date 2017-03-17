import os
import sys
import os.path as path
import shutil
import pathlib

if sys.version < '3.5':

    print('run_kernel requires python version >= 3.5')

    sys.exit(1)

class kind_run(object):

    def __init__(self):

        """
        Main driver for run the code
        """

    def __input__run__(self):

        print("Choose what kind of run you want...")

        print("0 - Train Neural Network")

        print("1 - Score data basis")

        print("2 - Clean test folder")

        print("3 - Debug")

        gkind = input("What is your election?")

        print("Your election was: "+ gkind)

        return gkind

    def __choose_run__(self):

        self.gkind = kind_run().__input__run__()

        if (self.gkind == "0"):

            print(" Training Neural Network... ")

            set_enviroment().__clean_out__()

            compile_code()

            self.test_path = os.getcwd() + "/test/run_test"

            os.mkdir(self.test_path)

            run_test().__test_kind__(self.gkind)

        elif (self.gkind == "1"):

            print(" Scoring data basis...")

            run_test().__test_kind__(self.gkind)


        elif (self.gkind == "2"):

            print(" Cleaning folder...")

            set_enviroment().__clean_out__()

        else:

            print(" Debugging: Training Neural Network...")

            set_enviroment().__clean_out__()

            compile_code()

            self.test_path = os.getcwd() + "/test/run_test"

            os.mkdir(self.test_path)

            run_test().__test_kind__(self.gkind)

class compile_code(object):

    """
    Compile code with make
    """

    def __init__(self):

        os.system("make")
        os.system("make")
        os.system("make")
        os.system("make")

        filelist = [ f for f in os.listdir(".") if f.endswith(".mod") ]

        for f in filelist:

            os.remove(f)

class run_test(object):

    def __init__(self):

        """
        Set the kernel code
        """

    def __test_kind__(self,kind):

        self.kind = kind
   
        self.build_path = os.getcwd()+ "/test/run_test"

        print("Run Forest...")

        set_enviroment().__get_exe__()

        set_enviroment().__get_example__(self.kind)

        os.chdir(self.build_path)

        os.system("NN_Kernel.exe")

class set_enviroment(object):
    """
    class for handling the NN_Kernel
    """
    def __init__(self):
        """
        Initialize the global set of variables
        """

    def __get_exe__(self):

        test_path = os.getcwd() + "/test/run_test"

        shutil.copy(os.getcwd()+"/bin/NN_Kernel.exe", test_path) 
  
    def __get_example__(self, kind):

        self.kind = kind

        test_path = os.getcwd() + "/test/run_test"

        example_path = os.getcwd() + "/test/example/"

        if (self.kind == "0"):

            shutil.copy(example_path + "Kernel_Train.inp", test_path+"/Kernel.inp") 

            shutil.copy(example_path + "iris.csv", test_path)

        elif (self.kind == "1"):

            if os.path.exists(test_path+"\iris_scored.csv"):

                os.remove(test_path+"\iris_scored.csv")	

            shutil.copy(example_path + "Kernel_Basis.inp", test_path+"/Kernel.inp") 

            shutil.copy(example_path + "iris.csv", test_path)

        elif (self.kind == "3"):
        
            shutil.copy(example_path + "Kernel_BOT.inp", test_path+"/Kernel.inp") 

            shutil.copy(example_path + "registro_1.csv", test_path)

            shutil.copy(example_path + "registro_2.csv", test_path)

            shutil.copy(example_path + "registro_3.csv", test_path)

            shutil.copy(example_path + "registro_4.csv", test_path)

            shutil.copy(example_path + "registro_5.csv", test_path)

            print("BOT")

        else:

            shutil.copy(example_path + "Kernel_Train_debug.inp", test_path+"/Kernel.inp") 

            shutil.copy(example_path + "iris.csv", test_path)

            print("Debugging")

    def __clean_out__(self):

        print("Cleaning bin...")

        self.bin_path = os.getcwd() + "/bin/NN_Kernel.exe"

        if path.isfile(self.bin_path):

            os.remove(self.bin_path)

        print("Cleaning test...")

        self.test_path = os.getcwd() + "/test/run_test"

        print(self.test_path)

        if os.path.exists(self.test_path):

            print("Cleaning Folder /test/run_test ")

            shutil.rmtree(self.test_path)

        print("done!")

kind_run().__choose_run__()