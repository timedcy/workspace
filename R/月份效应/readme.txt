Part1:R文件说明
1，Part1 DrawData.R  （为一段代码，找时间将其函数化）此文件为从数据库中提取所有A股历史上的月内平均成交额和月内平均流通市值，数据存放在mylist里面
ps:由于此数据需要逐月更新，因此后面的日期表示更新的日期；
2，Part2 portfolio_construct.R （为一个函数） 实现的功能是基于output12.csv（月份效应）和mylist（长期成交额效应）来进行选股，返回的结构是2005年1月至今每月的股票组合；
3，Part3 Backtesthedge3.R (为一个函数)此为回测函数，本应该将其改为有成交额阈值的，但由于报错了，因此暂时先用WL的版本；（找时间修改）
4，Part4 plot.R (为一个函数)自己编写的画图模板，与WL画图模板的差别就在于在净值曲线上添加了hs300指数的曲线；
5，Part5 IntoWind.R(为一个函数) 将选取出的股票组合，转换为导入wind的格式；（非必要）
6，MainFunction.R 主程序

Part2:数据说明
1，allAindustry.csv 存放的是所有A股所属的申万二级行业；
2，hs300index.csv 存放的是沪深300成分股权重的历史数据；
3，output12.csv 存放的是2000年到2011年12年的申万二级行业跑赢大盘的频数统计；（这个统计也是基于一个函数的，
只是此表格短时间内不需要更新，因此，短时间内不需要运行此程序）

Part3:文件夹说明
1，tradeT0 存放的是建仓日的前一天的数据，包括所选择的股票和委托单；
2，tradeT1 存放的是建仓日的数据，包括所选择的股票和委托单；
3，wind 存放的是需要导入wind的数据
4，backtest 存放的是利用自己的回测函数回测出的数据，包括RDate,csv,plot等。（每天用于仓位管理的净值数据应当从这里提取）
5，实盘记录 存放的是交易后的反馈数据



该策略的说明：月份效应+长期成交额效应
