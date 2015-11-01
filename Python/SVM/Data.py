#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pymysql
# 打开数据库连接
con = pymysql.connect(host='localhost', user='root', passwd='', db='winddata', port=3306, charset='utf8')
# 使用cursor()方法获取操作游标

cur = con.cursor()

sql = "SELECT DISTINCT SecuCode FROM new_liquidity"

cur.execute(sql)
result = cur.fetchall()
replace = []

for i in range(len(result)):
    if len(result[i][0])==1:
        replace.append(('00000'+result[i][0], result[i][0]))
    elif len(result[i][0])==2:
        replace.append(('0000'+result[i][0], result[i][0]))
    elif len(result[i][0])==3:
        replace.append(('000'+result[i][0], result[i][0]))
    elif len(result[i][0])==4:
        replace.append(('00'+result[i][0], result[i][0]))
    elif len(result[i][0])==5:
        replace.append(('0'+result[i][0],result[i][0]))
    else:
        pass

sql1 = "UPDATE new_liquidity SET SecuCode = %s WHERE SecuCode = %s"

try:
    # 执行sql语句
    cur.executemany(sql1, replace)
    # 提交到数据库执行
    con.commit()
except:
    # Rollback in case there is any error
    con.rollback()

# 关闭数据库连接,释放资源
cur.close()
print("new_liquidity!")

cur = con.cursor()

sql = "SELECT DISTINCT SecuCode FROM new_beta"

cur.execute(sql)
result = cur.fetchall()
replace = []

for i in range(len(result)):
    if len(result[i][0])==1:
        replace.append(('00000'+result[i][0], result[i][0]))
    elif len(result[i][0])==2:
        replace.append(('0000'+result[i][0], result[i][0]))
    elif len(result[i][0])==3:
        replace.append(('000'+result[i][0], result[i][0]))
    elif len(result[i][0])==4:
        replace.append(('00'+result[i][0], result[i][0]))
    elif len(result[i][0])==5:
        replace.append(('0'+result[i][0],result[i][0]))
    else:
        pass

sql1 = "UPDATE new_beta SET SecuCode = %s WHERE SecuCode = %s"

try:
    # 执行sql语句
    cur.executemany(sql1, replace)
    # 提交到数据库执行
    con.commit()
except:
    # Rollback in case there is any error
    con.rollback()

# 关闭数据库连接,释放资源
cur.close()
print("new_beta!")

cur = con.cursor()

sql = "SELECT DISTINCT SecuCode FROM new_size"

cur.execute(sql)
result = cur.fetchall()
replace = []

for i in range(len(result)):
    if len(result[i][0])==1:
        replace.append(('00000'+result[i][0], result[i][0]))
    elif len(result[i][0])==2:
        replace.append(('0000'+result[i][0], result[i][0]))
    elif len(result[i][0])==3:
        replace.append(('000'+result[i][0], result[i][0]))
    elif len(result[i][0])==4:
        replace.append(('00'+result[i][0], result[i][0]))
    elif len(result[i][0])==5:
        replace.append(('0'+result[i][0],result[i][0]))
    else:
        pass

sql1 = "UPDATE new_size SET SecuCode = %s WHERE SecuCode = %s"

try:
    # 执行sql语句
    cur.executemany(sql1, replace)
    # 提交到数据库执行
    con.commit()
except:
    # Rollback in case there is any error
    con.rollback()

# 关闭数据库连接,释放资源
cur.close()
print("new_size!")

con.close()