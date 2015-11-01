#!/usr/bin/env python
__all__=['easy_train']
import sys
import os
from subprocess import *


# svm, grid, and gnuplot executable files
filePath = str(os.path.dirname(os.path.abspath(__file__))) + r"\\";
is_win32 = (sys.platform == 'win32')
if not is_win32:
	svmscale_exe = "../svm-scale"
	svmtrain_exe = "../svm-train"
	svmpredict_exe = "../svm-predict"
	grid_py = "./gridregression.py"
	gnuplot_exe = "/usr/bin/gnuplot"
else:
        # example for windows
	svmscale_exe = filePath+r"..\windows\svm-scale.exe"
	svmtrain_exe = filePath+r"..\windows\svm-train.exe"
	svmpredict_exe = filePath+r"..\windows\svm-predict.exe"
	gnuplot_exe = r"C:\Program Files\gnuplot\bin\gnuplot.exe"
	grid_py = filePath+r".\gridregression.py"

assert os.path.exists(svmscale_exe),"svm-scale executable not found"
assert os.path.exists(svmtrain_exe),"svm-train executable not found"
assert os.path.exists(svmpredict_exe),"svm-predict executable not found"
#assert os.path.exists(gnuplot_exe),"gnuplot executable not found"
assert os.path.exists(grid_py),"grid.py not found"


#Smart train begin
def easy_train(name):
        global svmscale_exe,svmtrain_exe,grid_py,gnuplot_exe
        
        train_pathname = name
        assert os.path.exists(train_pathname),"training file not found"
        file_name = os.path.split(train_pathname)[1]
        scaled_file = file_name + ".scale"
        model_file = file_name + ".model"
        range_file = file_name + ".range"
        #range_file = "LastRange.range"
        
        cmd = '{0} -s "{1}" "{2}" > "{3}"'.format(svmscale_exe, range_file, train_pathname, scaled_file)
        print('Scaling training data...')
        Popen(cmd, shell = True, stdout = PIPE).communicate()	

        cmd = '{0} -svmtrain "{1}" -gnuplot "{2}" "{3}"'.format(grid_py, svmtrain_exe, gnuplot_exe, scaled_file)
        print('Cross validation...')
        f = Popen(cmd, shell = True, stdout = PIPE).stdout

        line = ''
        while True:
                last_line = line
                line = f.readline()
                if not line: break
        c,g,p,mse = map(float,last_line.split())

        print('Best c={0}, g={1}, p={2} CV mse={3}'.format(c,g,p,mse))

        cmd = '{0} -s 3 -c {1} -g {2} -p {3} -b 1 "{4}" "{5}"'.format(svmtrain_exe,c,g,p,scaled_file,model_file)
        print('Training...')
        Popen(cmd, shell = True, stdout = PIPE).communicate()

        print('Output model: {0}'.format(model_file))

        return c,g,p,mse




if __name__ == '__main__':

	def exit_with_help():
		print('Usage: {0} training_file '.format(sys.argv[0]))
		sys.exit(1)
	
	if len(sys.argv) < 2:
		exit_with_help()
		
	train_pathname = sys.argv[1]
	try:
		easy_train(train_pathname)
	except (IOError,ValueError) as e:
		sys.stderr.write(str(e) + '\n')
		sys.exit(1)
