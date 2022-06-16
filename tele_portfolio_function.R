# this file was ex=ncoding by UTF-8
setwd("C:/Users/Neil/Documents/git-repos/TW-telecompany-portfolio") # 設定工作目錄
stock_price_data = read.csv2("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20130101_20220607.csv", encoding = "mbcs" , header = T,sep = ",") %>% data.table()
table_data = stock_price_data
table_data$調整開盤價 = as.numeric(table_data$調整開盤價)
table_data$調整收盤價 = as.numeric(table_data$調整收盤價)

# package_list = c("data.table","dplyr","plyr","readr","ggplot2","lubridate","tseries","magrittr","foreach")
# install.packages(package_list)
library(data.table)
library(dplyr)
library(plyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
library(magrittr) # %>% 水管工人
# install.packages("foreach")
# library(foreach)

# table_data = read.table("teleportfolio.txt", encoding = "mbcs" , header = T) %>% data.table()
# colnames(table_data) =  c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")
# table_data = read.csv2("C:/Users/Neil/Documents/git-repos/backtest_in_R/stock_data/tidy_stock_price_data20130101_20220607.csv", encoding = "mbcs" , header = T,sep = ",") %>% data.table()
# table_data$調整開盤價 = as.numeric(table_data$調整開盤價)
# table_data$調整收盤價 = as.numeric(table_data$調整收盤價)

# function
portfolio_function = function(table_data, start_day , end_day = 20220101, stock_list , A = 100 ,global_market_index = 0050){ 
  #table_data = 整理好的dataframe 
  #A = 投入的金額 
  #start_day = 開始的日期 範例: 20200101
  #end_day = 結束的日期 範例: 20220501
  #A = 起始投資金額，預設為100
  #global_market_index = 全域市場變數 
  
  #先篩選資料的時間
  table_data$年月日 = ymd(table_data$年月日)
  start_day = ymd(start_day)
  end_day = ymd(end_day)
  table_data = table_data %>% filter(年月日>= start_day) %>% filter(年月日<= end_day)
  
  ##### 這邊是為了計算投資報酬指數，所做的資料整理
  #設計一個函數，可以分組後往下移n個單位
  group_daily_change_function = function( table_data , n = 1){ #設計一個函數，可以分組後往下移n個單位
    shift_data = ddply( table_data , c("證券代碼","公司名稱") , 
                        .fun= function(x){ #這裡的x是指根據上面分完組切出來的table_data
                          transform(x, 前一日價格 = with(x, shift(調整收盤價 , n )))
                        } )
    shift_data = na.omit(shift_data)
    shift_data$daily_change = (shift_data$調整收盤價 - shift_data$前一日價格) / shift_data$前一日價格
    return (shift_data)
  }
  table_data = group_daily_change_function(table_data)
  #設計一個函數，可以計算累積乘積(每天的複利)
  group_cumprod_func = function(table_data){
    table_data$tmp_index = table_data$daily_change + 1 #tmp_index是每日變動+1，不是複利
    cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
                           .fun= function(x){
                             transform(x, cumprod_return_rate = with(x, cumprod(tmp_index)))
                           } )
    #cumprod_index$cumprod_index = cumprod_index$cumprod_return_rate #施工用看一下還沒-1前的變化
    cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1 #這樣是變成那個時點的複利，所以到時候只要用投入資金乘上1+這個值就是這個時間點的複利效果
    cumprod_index$cumprod_return_rate = round(cumprod_index$cumprod_return_rate,digit = 3)
    return(cumprod_index)
  }
  table_data = group_cumprod_func(table_data)
  
  ##### 接下來要用到上面算完的東西來計算指標，要把上面的資料取出我們要的股票來計算投資報酬率
  n = as.numeric(length(stock_list)) #投資股票的數量
  w = 1/n  #分配的比重 #假設平均分配
  portfolio_return_index_func = function(stock_list){
    portfolio = filter(table_data,證券代碼 %in% stock_list ) #多重篩選用filter比較好用
    portfolio$分配後的投資報酬指數 = A*w*(portfolio$cumprod_return_rate+1)
    portfolio_return_index = portfolio[,c("年月日","分配後的投資報酬指數")]
    portfolio_return_index = portfolio_return_index %>% group_by(年月日) %>% summarise_all(sum)
    colnames(portfolio_return_index)[2] = "投資報酬指數"
    return(portfolio_return_index)
  }
  portfolio_return_index = portfolio_return_index_func(stock_list) #到這邊就計算完投資報酬指數了
  
  ##### 這邊是利用算完的投資報酬指數來計算一些風險_報酬指標
  portfolio_risk_return_func = function( portfolio_return_index ){
    x = portfolio_return_index
    #期末報酬率
    total_return = ((x$投資報酬指數[length(x$投資報酬指數)]- x$投資報酬指數[1])/x$投資報酬指數[1]) #期末報酬率
    #年化報酬率
    via_day = x$年月日[length(x$年月日)] - x$年月日[1] #計算過了幾天
    via_day = as.numeric(via_day) #計算完之後再轉換成數字
    investment_year = via_day/365 #要算過了幾年
    annual_return = ((total_return+1)^(1/investment_year))-1 #年化報酬率計算公式
    annual_return = round(annual_return,digits = 4)
    #最大回落 
    mdd = maxdrawdown(x$投資報酬指數)
    mdd_ratio = (x$投資報酬指數[mdd$to] - x$投資報酬指數[mdd$from]) / x$投資報酬指數[mdd$from]
    mdd_ratio = round(mdd_ratio,digits = 4)
    mdd_start_day = x$年月日[mdd$from]
    mdd_end_day = x$年月日[mdd$to]
    
    #顯示與輸出
    cat("#####################","\n")
    cat("投組投資開始日期為:",as.character(x$年月日[1]),"\n")
    cat("結束期間為:",as.character(x$年月日[length(x$年月日)]),"\n")
    cat("投資的股票數量為",length(stock_list),"檔","\n")
    cat("投資期間共",via_day,"天","\n")
    cat("期末總報酬為:",round(total_return*100,digits = 2),"%","\n")
    cat("年化報酬為:",annual_return*100,"%","\n")
    cat("最大回落為:",-mdd_ratio*100,"%","\n")
    cat("最大回落開始日期為:",as.character(x$年月日[mdd$from]),"\n")
    cat("最大回落結束日期為:",as.character(x$年月日[mdd$to]),"\n")
    cat("#####################","\n")
    
  }
  
portfolio_risk_return_func(portfolio_return_index)

  #####這邊要輸出市場標的來做比較
  #cat("我故意cat的",global_market_index,"\n")  
  
  market_return_index_func = function(market_index = global_market_index){
    market_return = filter(table_data,證券代碼 %in% market_index ) #多重篩選用filter比較好用
    market_return$報酬指數 = A*(market_return$cumprod_return_rate+1)
    market_return_index =  market_return[,c("年月日","報酬指數")]
    market_return_index = market_return_index %>% group_by(年月日) %>% summarise_all(sum)
    colnames(market_return_index)[2] = "市場報酬指數"
    return(market_return_index)
  }
  market_return_index = market_return_index_func() #到這邊就計算完投資報酬指數了
  market_return_risk_func = function(market_return_index){
    x = market_return_index
    #期末報酬率
    total_return = ((x$市場報酬指數[length(x$市場報酬指數)]- x$市場報酬指數[1])/x$市場報酬指數[1]) #期末報酬率
    #年化報酬率
    via_day = x$年月日[length(x$年月日)] - x$年月日[1] #計算過了幾天
    via_day = as.numeric(via_day) #計算完之後再轉換成數字
    investment_year = via_day/365 #要算過了幾年 #如果是用交易天數計算的話就要用252
    annual_return = ((total_return+1)^(1/investment_year))-1 #年化報酬率計算公式
    annual_return = round(annual_return,digits = 4)
    #最大回落
    mdd = maxdrawdown(x$市場報酬指數)
    mdd_ratio = (x$市場報酬指數[mdd$to] - x$市場報酬指數[mdd$from]) / x$市場報酬指數[mdd$from]
    mdd_ratio = round(mdd_ratio,digits = 4)
    mdd_start_day = x$年月日[mdd$from]
    mdd_end_day = x$年月日[mdd$to]
    #顯示與輸出
  #cat("投資開始日期為:",as.character(x$年月日[1]),"\n")
  #cat("結束期間為:",as.character(x$年月日[length(x$年月日)]),"\n")
  #cat("投資期間共",via_day,"天","\-n")
    cat("市場標的為:",global_market_index,"\n")
    cat("同期市場期末總報酬為:",round(total_return*100,digits = 2),"%","\n")
    cat("同期市場年化報酬為:",annual_return*100,"%","\n")
    cat("同期市場最大回落為:",-mdd_ratio*100,"%","\n")
    cat("最大回落開始日期為:",as.character(x$年月日[mdd$from]),"\n")
    cat("最大回落結束日期為:",as.character(x$年月日[mdd$to]),"\n")
    cat("#####################","\n")
  }
market_return_risk_func(market_return_index)  

#合併表格 
#sum_data = cbind(pf_value_df , market_value_df) 
  
 ##### 畫圖的部分
  graphics_data = merge(portfolio_return_index, market_return_index, by = "年月日")
  return_index_image = ggplot(graphics_data , aes(x = 年月日)) +
    geom_line(aes(y = 投資報酬指數), colour = "blue"  ) +
    geom_line(aes(y = 市場報酬指數),colour = "red"  ) +
    # geom_stream(aes(y = 市場報酬指數)) +
    ggtitle("投資組合報酬與市場比較") +
    xlab("投資期間") +
    ylab("報酬指數")
  return_index_image

}

#設定投資參數
 # all_stock_list = unique(table_data$證券代碼)
 # stock_list = all_stock_list
 stock_list = c(3008,0050,0056,1101,1102,1103,1104,1108,1109,1110,1201,1203,1210,1213,1215,1216,1217,1218,1219,1220,1225,1227,1229,1231,1232,1233,1234)
 portfolio_function(table_data,start_day = 20210101, end_day = 20220511,stock_list = stock_list ,global_market_index = 0050)

 


























