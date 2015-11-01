library(RODBC)
ch=odbcConnect('jydb',uid='jydb',pwd='jydb')
data1=sqlQuery(ch,"SELECT * from CALDB.dbo.C_CT_SystemConst ")
odbcClose(ch)
ch=odbcConnect('jydb',uid='jydb',pwd='jydb')

head(data1)

data2=sqlQuery(ch,"SELECT * from JYDB.dbo.QH_FuturesNews ")
head(data2)

sqlTables(ch)
data3=odbcDataSources()  ### query the data source which you can use;
summary(data3)
data3[5]

RShowDoc("RODBC", package="RODBC")

sqlTables(ch)
sqlTables(ch,tableType="TABLE")

####2015-03-24
library(RODBC)
ch2=odbcConnect('jydb',uid='jydb',pwd='jydb')
data1=sqlQuery(ch2,"select * from JYDB.dbo.SecuMain")

data2=sqlQuery(ch,"select * from JYDB.dbo.LC_CashFlowStatement")
head(data2)
dim(data2)
data3=sqlQuery(ch,"select * from JYDB.dbo.MF_FundArchives")
head(data3)
dim(data3)

MF_TopTenHolder

data4=sqlQuery(ch,"select * from JYDB.dbo.MF_TopTenHolder")
head(data4)
dim(data4)

data4=sqlQuery(ch,"select * from JYDB.dbo.Fut_MemberInfo")
head(data4)
dim(data4)

mydata=sqlQuery(ch,"select b.SecuAbbr,b.SecuCode, * from JYDB.dbo.LC_IndexComponentsWeight a join JYDB.dbo.SecuMain b on a.InnerCode=b.InnerCode where a.IndexCode='3145' ORDER BY EndDate desc")

