from WindPy import *
w.start();

#open a file to write.
pf = open('c:\\pywsqdataif.data', 'w')

#define the callback function
def myCallback(indata):
    if indata.ErrorCode!=0:
        print('error code:'+str(indata.ErrorCode)+'\n');
        return();

    global begintime
    lastvalue ="";
    for k in range(0,len(indata.Fields)):
         if(indata.Fields[k] == "RT_TIME"):
            begintime = indata.Data[k][0];
         if(indata.Fields[k] == "RT_LAST"):
            lastvalue = str(indata.Data[k][0]);

    string = str(begintime) + " " + lastvalue +"\n";
    pf.writelines(string)
    print(string);

    #应该在w.cancelRequest后调用pf.close()
    #pf.close();



#to subscribe if14.CFE
w.wsq("IF.CFE","rt_time,rt_last",func=myCallback)

