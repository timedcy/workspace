# -*- coding:utf-8 -*-  
'''
 Maintainer:
       Realize Linear Regression with Gradient Descent

 Version:
       1.0 - 23/05/14

 Author:
       Created by Jason Liu

 Section:
       -> Import Training dataset
       -> Gradient Descent
'''
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 


#Import Training dataset
def file2matrix(filename,column):
    text = open('dataset.txt')
    dataset = text.readlines()
    numberOflines = len(dataset)
    x = np.ones((numberOflines,column))
    y = np.ones((numberOflines,1))
    a = 0
    for line in dataset:
        line = line.strip()     #消除首尾的空格
        listFromlines = line.split()    #分割数据，生成 list
        x[a,1:] = listFromlines[0:column-1]
        y[a] = listFromlines[-1]
        a += 1
    return x,y


#Gradient Descent
def gradientDescent(x):
    #init Parameters
    epsilon = 0.00001;alpha = 0.00001;theta0 = 0;theta1 = 0;theta2 = 0;oldtheta0 = 0;oldtheta1 = 0;oldtheta2 = 0;j0 = 0;j1 = 0;j2 = 0
   
    while True:
        #calculate the parameters
        for i in range(len(x)):
            j0 +=  (oldtheta0 + oldtheta1 * x[i][1] + oldtheta2 * x[i][2]  - x[i][0])
            j1 +=  (oldtheta0 + oldtheta1 * x[i][1] + oldtheta2 * x[i][2]  - x[i][0])*x[i][1]
            j2 +=  (oldtheta0 + oldtheta1 * x[i][1] + oldtheta2 * x[i][2]  - x[i][0])*x[i][2]
        theta0 = oldtheta0 - alpha * j0
        theta1 = oldtheta1 - alpha * j1
        theta2 = oldtheta2 - alpha * j2
        error = (theta0 - oldtheta0)**2 + (theta1 - oldtheta1)**2 +(theta2 - oldtheta2)**2
        print error
        if error < epsilon:
            break
        else:
            oldtheta0 = theta0
            oldtheta1 = theta1
            oldtheta2 = theta2

    return theta0,theta1,theta2  
    
def gradientDescentRand(x,y):
    #init Parameters
    alpha = 0.0001; n = np.shape(x)[1]; theta = np.zeros((n,1)); maxCycles = 500
    x = np.mat(x); y = np.mat(y); theta = np.mat(theta)
    #calculate the parameters  
    for i in range(maxCycles):
        j = ( x * theta ) - y
        theta = theta - alpha * x.transpose() * j
    return theta  

if __name__ == '__main__':
    filename = 'dataset.txt';column = 3
    x,y = file2matrix(filename,column)
    theta = gradientDescentRand(x,y)
    print theta
    
