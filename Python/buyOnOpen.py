# -*- coding: cp936 -*-

#1) 准备股票池,去除停牌，去除ST
#2) 登陆交易妥当，得到总资金
#3）获得做收价
#4) 等待时间，9.25开始，调用wsq获取股票池股票价格
#5) 跌幅大于8%的，计算购买量，计算购买价，统一使用下单函数下单。
#6）结束
#7）可以通过tquery查询




from WindPy import *
import time as st;
from datetime import *;

def getStocks():
    w.start();
    data=w.wset('SectorConstituent','field=wind_code;sector=全部A股(非ST)')
    
    return (data.ErrorCode,data.Data[0]);

def logonTo():
    data=w.start(60);
    if(data.ErrorCode!=0):
        return (-1,None);
    data=w.tlogon('0000','0','w081263801','0','SHSZ');
    if(data.ErrorCode!=0):
        return (-2,None);
    return(0,data.Data[0][0]);

def ifelse(c,v1,v2):
    if(c):
        return (v1);
    else:
        return (v2);
def buyOnOpen(codes,amount=10000000,chggate=0.08,addprice=0.01):

    (err,logonid)=logonTo()
    if(err):
        print("logon error:",err);
        return(err);
        
    data=w.wsq(codes,'rt_pre_close');
    if(data.ErrorCode!=0):
        return (-1);
    preprice = data.Data[0];
    gateprice = [l*(1-chggate) for l in preprice];
    
    nextquerytime=datetime.now()+timedelta(0,120)
    while(1):
        now = datetime.now();
        endtime = datetime(now.year,now.month,now.day,9,31,00);
        begintime=datetime(now.year,now.month,now.day,9,25,00);

        if(now>endtime):
            print("now >9:30:00")
            return (1)

        if(now<begintime):
            delta = begintime - now;
            delta = delta.total_seconds()
            if(delta<0.1):
                delta = 0.1;
            if(delta>11):
                if(now>nextquerytime):
                    data = w.tquery(1);
                    if(data.ErrorCode!=0):
                        print("tquery(1) error!");
                        return(-1);
                    nextquerytime=datetime.now()+timedelta(0,120);
                    data=w.wsq(codes,'rt_pre_close');
                    if(data.ErrorCode!=0):
                        return (-1);
                    preprice = data.Data[0];
                    gateprice = [l*(1-chggate) for l in preprice];

                delta=10;
                print("wait to open,sleep ten seconds!");
            st.sleep(delta);
            continue;

        data = w.wsq(codes,'rt_last');
        if(data.ErrorCode!=0):
            continue;

        index =list();
        for i in range(len(data.Data[0])):
            if( (data.Data[0][i]<gateprice[i]) & (data.Data[0][i]>0.0001)):
                index.append(i);
        if(len(index)<1):
            continue;
        buycodes = [codes[i] for i in index];
        buyprice = [data.Data[0][i]+addprice for i in index];
        everyamount = amount/len(buyprice);
        buyvol = [int(everyamount/v/100)*100 for v in buyprice]
        buyvol = [v>500000 for v in buyvol]
        logonids = [logonid for v in range(len(buycodes))];
        data=w.torder(buycodes,'buy',buyprice,buyvol,logonid=logonids);
        print(data)
        return (0)
    

(err,codes)=getStocks();
if(err==0):
    buyOnOpen(codes);
else:
    print("getStocks error:",err);
