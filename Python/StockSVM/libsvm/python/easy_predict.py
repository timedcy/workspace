__all__=['easy_predict']

import sys
import os
from subprocess import *
import svmutil


filePath = str(os.path.dirname(os.path.abspath(__file__))) + r"\\";
is_win32 = (sys.platform == 'win32')
if not is_win32:
    svmscale_exe = "../svm-scale"

else:
    # example for windows
    
    svmscale_exe = filePath+r"..\windows\svm-scale.exe"


assert os.path.exists(svmscale_exe),"svm-scale executable not found"



def easy_predict(train_name, test_name):
    range_file = train_name + ".range"
    model_file = train_name + ".model"
    assert os.path.exists(test_name),"testing file not found"
    assert os.path.exists(model_file),"model file not found"
    assert os.path.exists(range_file),"range file not found"
    
    file_name = os.path.split(test_name)[1]
    scaled_test_file = file_name + ".scale"
    predict_test_file = file_name + ".predict"

    cmd = '{0} -r "{1}" "{2}" > "{3}"'.format(svmscale_exe, range_file, test_name, scaled_test_file)
    print('Scaling testing data...')
    Popen(cmd, shell = True, stdout = PIPE).communicate()
    
    (prob_y, prob_x) = svmutil.svm_read_problem(scaled_test_file)
    model = svmutil.svm_load_model(model_file)
    pred_labels, (ACC, MSE, SCC), pred_values = svmutil.svm_predict(prob_y, prob_x, model, "-b 1")
    
    return pred_values,MSE,SCC

if __name__ == '__main__':

    def exit_with_help():
        print('Usage: {0} train_file test_file '.format(sys.argv[0]))
        sys.exit(1)
            
    print(len(sys.argv))
    if len(sys.argv) < 2:
        exit_with_help()
            
    train_pathname = sys.argv[1]
    test_pathname = sys.argv[2]
            
    try:
        easy_predict(train_pathname, test_pathname)
    except (IOError,ValueError) as e:
        sys.stderr.write(str(e) + '\n')
        sys.exit(1)
	
