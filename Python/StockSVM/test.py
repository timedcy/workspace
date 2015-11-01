import libsvm.python.easy_predict as predict
import libsvm.python.easy_train_svr_b1 as train
import StockFeature.stock_feature_svr as feature
import sys


if __name__ == '__main__':
    #print(sys.path)
    for i in range(1,len(sys.argv)):
        print ("Argument",i,sys.argv[i])

    
    StockCode = sys.argv[1]
    Df = feature.fetchStockData(StockCode, StockCode+'.csv')
    feature.genFeature(Df,StockCode,3)   # moving window size=3

    train.easy_train(StockCode)
    predict.easy_predict(StockCode, StockCode)
    

