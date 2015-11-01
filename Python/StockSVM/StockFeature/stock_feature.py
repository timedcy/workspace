import sys
import tushare as ts

def getFeatureSample(idx, colum_name, feature_id):
    feature_val = StockDf.ix[idx, colum_name]
    sample = str(feature_id)+':'+str(feature_val)+' '
    return sample


Window_Moving_Size = 3
Window_Moving_Step = 1


#print(sys.path)
for i in range(1,len(sys.argv)):
    print ("Argument",i,sys.argv[i])


StockCode = ""+sys.argv[1]
StockDf = ts.get_h_data(StockCode)
StockDf = StockDf.sort_index(axis=0, ascending=True)

#adding EMA feature
StockDf['ema'] = StockDf['close']
DfLen = len(StockDf.index)
EMA = 0;
for n in range(0,DfLen):
    idx = n
    Close = StockDf.ix[idx, 'close']
    if(n==0):
        EMA = Close
    else:
        EMA = StockDf.ix[idx-11, 'ema']
        EMA = ((n-1)*EMA + 2*Close)/(n+1)
    
    StockDf.ix[idx,'ema'] = EMA

StockDf.to_excel(StockCode +'.xlsx')

#Generating moving window features
problem_file =  open(StockCode, 'w+')
for n in range(0,DfLen-Window_Moving_Size):
    predic_idx = n+Window_Moving_Size
    predict = '0'
    if(StockDf.ix[predic_idx, 'close'] > StockDf.ix[predic_idx-1, 'close']):   #going up
        predict = '1'
    else:
        predict = '-1'
    Sample = predict + ' '
    
    feature_id = 1
    feature_val = 0
    for j in range(n,n+Window_Moving_Size):
        Sample += getFeatureSample(j, 'open', feature_id)
        feature_id+=1
        Sample += getFeatureSample(j, 'high', feature_id)
        feature_id+=1
        Sample += getFeatureSample(j, 'close', feature_id)
        feature_id+=1
        Sample += getFeatureSample(j, 'low', feature_id)
        feature_id+=1
        Sample += getFeatureSample(j, 'volume', feature_id)
        feature_id+=1
        Sample += getFeatureSample(j, 'ema', feature_id)
        feature_id+=1


    Sample += '\n'
    problem_file.write(Sample)
    

problem_file.close()
print('\n sample number: '+ str(n+1) +'\n feature number: '+str(feature_id-1))

del problem_file    
del StockCode
del StockDf



