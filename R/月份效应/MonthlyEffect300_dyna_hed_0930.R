###MonthlyEffect300_dyna_hed_0930

strategy<-"MonthlyEffect300_dyna_hed_0930"
strategy_p1<-"MonthlyEffect300_dyna_hed"
strategy_p2<-"30"
equity_curve<-read.csv(file=paste("team4share/实盘策略/仓位管理/",strategy,".csv",sep=""),stringsAsFactors=F)
equity_curve[,"datetime"]<-as.character(as.Date(equity_curve[,"datetime"]))

source("//128.1.223.221/team4share/仓位管理/working codes/仓位管理模块1.R") #加载仓位管理模块

back_test_point<-which(as.Date(equity_curve[,"datetime"])==as.Date("2015/3/25"))
back_test<-strategy_report(equity_curve=equity_curve[1:back_test_point,],back_test_point=back_test_point,main=paste(strategy,"(back_test)",sep=""))
back_test_drawdown<-back_test$historical_drawdown
back_test_maxdrawdown<-back_test$indicators$historical_maxdrawdown

trade_dependency_analysis_s<-trade_dependency_analysis(equity_curve=equity_curve,most_recent_days=30,strategy=strategy) #交易依赖性分析

original<-strategy_report(equity_curve=equity_curve,back_test_point=back_test_point,main=paste(strategy,"(original)",sep=""))

basic_dependency_rule_positive_s<-basic_dependency_rule(equity_curve=equity_curve,back_test_point=back_test_point,trade_dependency="positive",strategy=strategy)

basic_dependency_rule_negative_s<-basic_dependency_rule(equity_curve=equity_curve,back_test_point=back_test_point,trade_dependency="negative",strategy=strategy)

consecutive_rule_s<-consecutive_rule(equity_curve=equity_curve,back_test_point=back_test_point,cash=0.2,wins=2,wins_skips=0,decrease_position=0.2,losses=1,losses_skips=0,increase_position=0,strategy=strategy)

crossovers_positive_s<-crossovers(equity_curve=equity_curve,back_test_point=back_test_point,above=T,above_position=1,below=T,below_position=0,N=20,type="simple",strategy=strategy)

crossovers_negative_s<-crossovers(equity_curve=equity_curve,back_test_point=back_test_point,above=T,above_position=0,below=T,below_position=1,N=20,type="simple",strategy=strategy)

dd_1_0.5_0.25_0_s<-dd_1_0.5_0.25_0(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

monthly_level_s<-monthly_level(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_1_0.5_0.25_0_0.25_0.5_0.75_1_s<-dd_1_0.5_0.25_0_0.25_0.5_0.75_1(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_1_0.333_0.667_0_s<-dd_1_0.333_0.667_0(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

pyramid_s<-pyramid(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_1_0.5_0_1_s<-dd_1_0.5_0_1(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_0_1_0_s<-dd_0_1_0(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_0_0.5_1_0_s<-dd_0_0.5_1_0(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,treshold1=quantile(back_test_drawdown,0.3),treshold2=quantile(back_test_drawdown,0.8),strategy=strategy)

rolling_basic_dependency_rule_s<-rolling_basic_dependency_rule(equity_curve=equity_curve,back_test_point=back_test_point,rolling_days=250,strategy=strategy)

rolling_crossovers_s<-rolling_crossovers(equity_curve=equity_curve,back_test_point=back_test_point,rolling_days=250,N=20,type="simple",strategy=strategy)

monthly_level_quantile_s<-monthly_level_quantile(equity_curve=equity_curve,back_test_point=back_test_point,treshold=quantile(back_test_drawdown,0.7),strategy=strategy)

batches_position_s<-batches_position(euqity_curve=equity_curve,back_test_point=back_test_point,strategy=strategy)

batches_position_2_s<-batches_position_2(euqity_curve=equity_curve,back_test_point=back_test_point,strategy=strategy)

batches_position_3_s<-batches_position_3(euqity_curve=equity_curve,back_test_point=back_test_point,strategy=strategy)

dd_0.67_1_hold_s<-dd_0.67_1_hold(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_0.67_1_sell_s<-dd_0.67_1_sell(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_0_0.33_hold_s<-dd_0_0.33_hold(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

dd_0_0.33_sell_s<-dd_0_0.33_sell(equity_curve=equity_curve,back_test_point=back_test_point,maxdrawdown=back_test_maxdrawdown,strategy=strategy)

source("team4share/仓位管理/working codes/数据汇总.R")