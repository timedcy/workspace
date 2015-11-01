library(RODBC)

con.team<-odbcConnect('team',uid='kuanao',pwd='Wxyc2015@')
con.juyuan <- odbcConnect('JYDB',uid='jysj1',pwd='xing2015@')
con.wind <- odbcConnect('Wind',uid='wdsj',pwd='wxyc2015')

