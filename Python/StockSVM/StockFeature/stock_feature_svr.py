import sys
import tushare as ts


def getFeatureSample(StockDf, idx, colum_name, feature_id):
    feature_val = StockDf.ix[idx, colum_name]
    sample = str(feature_id)+':'+str(feature_val)+' '
    return sample


def fetchStockData(code, output_csv=None):
    StockDf = ts.get_h_data(code)
    StockDf = StockDf.sort_index(axis=0, ascending=True)
    #adding EMA feature
    StockDf['ema'] = StockDf['close']
    StockDf['rise'] = StockDf['close']
    DfLen = len(StockDf.index)
    EMA = 0;
    RISE = 0;
    for n in range(0,DfLen):
        idx = n
        Close = StockDf.ix[idx, 'close']
        if(n==0):
            EMA = Close
            RISE = 0
        else:
            EMA = StockDf.ix[idx-11, 'ema']
            EMA = ((n-1)*EMA + 2*Close)/(n+1)
            CloseP = StockDf.ix[idx-1, 'close']
            RISE = (Close - CloseP)/CloseP
        
        StockDf.ix[idx,'ema'] = EMA
        StockDf.ix[idx,'rise'] = RISE

    if(output_csv != None):
        StockDf.to_csv(output_csv)
        
    return StockDf


def genFeature(StockDf, file_name, win_size=3):
    #Generating moving window features
    problem_file =  open(file_name, 'w+')
    DfLen = len(StockDf.index)
    for n in range(0,DfLen-win_size):
        predic_idx = n+win_size
        predict = 0
        predict = StockDf.ix[predic_idx, 'rise']
        predict = predict*10    #  1= rise 10%
        Sample = str(predict) + ' '
        
        feature_id = 1
        feature_val = 0
        for j in range(n,n+win_size):
            Sample += getFeatureSample(StockDf, j, 'open', feature_id)
            feature_id+=1
            Sample += getFeatureSample(StockDf, j, 'high', feature_id)
            feature_id+=1
            Sample += getFeatureSample(StockDf, j, 'close', feature_id)
            feature_id+=1
            Sample += getFeatureSample(StockDf, j, 'low', feature_id)
            feature_id+=1
            Sample += getFeatureSample(StockDf, j, 'volume', feature_id)
            feature_id+=1
            Sample += getFeatureSample(StockDf, j, 'ema', feature_id)
            feature_id+=1


        Sample += '\n'
        problem_file.write(Sample)
        

    problem_file.close()
    print('\n sample number: '+ str(n+1) +'\n feature number: '+str(feature_id-1))

    del problem_file    
    del StockDf


if __name__ == '__main__':
    #print(sys.path)
    for i in range(1,len(sys.argv)):
        print ("Argument",i,sys.argv[i])


    StockCode = sys.argv[1]
    Df = fetchStockData(StockCode, StockCode+'.csv')
    genFeature(Df,3,StockCode)
    


