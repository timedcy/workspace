from numpy import *
import operator

def creatDataSet():
    group = array([[1.0,1.1],[1.0,1.0],[0,0],[0,0.1]])
    labels =['A','A','B','B']
    return group,labels

def classify(inx,dataSet,labels,k):     #inxÓÃÓÚ·ÖÀàµÄÊý¾Ý£¬dataSetÑµÁ·Ñù±¾¼¯£¬labels±êÇ©ÏòÁ¿£¬KÁÚ½üµÄÊýÄ¿
    dataSetSize = dataSet.shape[0]      #shape[0]ÓÃÓÚ»ñÈ¡µÚÒ»Î¬µÄ³¤¶È
    diffMat = tile(inx,(dataSetSize,1)) - dataSet      #ÓëÊý¾Ý¼¯Ïà¼õ
    sqDiffMat = diffMat**2
    sqDistances = sqDiffMat.sum(axis=1)     #¶ÔÏòÁ¿ÇóºÍ
    distances = sqDistances**0.5
    sortedDistIndicies = distances.argsort()    #¶Ô¾àÀë½øÐÐÅÅÐò
    classcount={}
    for i in range(k):
        voteIlabel = labels[sortedDistIndicies[i]]
        classCount[voteIlabel] = classCount.get(voteIlabel,0) + 1
    sortedClassCount = sorted(classCount.iteritems(),
    key = operator.itemgetter(1),reverse=Ture)
    return sortedClassCount(0)[0]

def file2matrix(filename):
    fr = open(filename)
    arrayOlines = fr.readlines()
    numberOflines = len(arrayOlines)
    returnMat = zeros((numerOflines,3))
    classLabelvector = []
    index = 0
    for line in arrayOlines
        line = line.strip()
        listFromLine = line.split('\t')
        returnMat[index,:] = listFromLine[0:3]
        classlabelVector.append(int(listFromLine[-1]))
        index += 1
    return returnMat,classLabelVector
