setwd("C:/Users/Neil/Documents/git-repos/TW-telecompany-portfolio") # 設定工作目錄
library(data.table)
library(dplyr)
library(plyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
# library(stringr) #字串串接使用
library(magrittr) # %>% 水管工人

#整理資料
tele_table = read.table("teleportfolio.txt", encoding = "mbcs" , header = T)
colnames(tele_table) =  c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")

#設定投資參數
stock_list = c(2412,3045,4904)

# table_data = tele_table #施工使用

my_function = function(table_data, start_day, end_day, stock_list = 0050 , A = 100 ){ #A = 投入的金額
  #先篩選資料的時間
  table_data$年月日 = ymd(table_data$年月日)
  start_day = ymd(start_day)
  end_day = ymd(end_day)
  table_data = table_data %>% filter(年月日> start_day) %>% filter(年月日< end_day)
  
  ##### 這邊是為了計算投資報酬指數，所做的資料整理
  #設計一個函數，可以分組後往下移n個單位
  group_daily_change_function = function( table_data , n = 1){ #設計一個函數，可以分組後往下移n個單位
    shift_data = ddply( table_data , c("證券代碼","公司名稱") , 
                        .fun= function(x){
                          transform(x, 前一天價格 = with(x, shift(調整收盤價 , n )))
                        } )
    shift_data = na.omit(shift_data)
    shift_data$daily_change = (shift_data$調整收盤價 - shift_data$前一天價格) / shift_data$前一天價格
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
    
    #顯示與輸出
    cat("投資開始日期為:",as.character(x$年月日[1]),"\n")
    cat("結束期間為:",as.character(x$年月日[length(x$年月日)]),"\n")
    cat("投資期間共",via_day,"天","\n")
    cat("期末總報酬為:",total_return*100,"%","\n")
    cat("年化報酬為:",annual_return*100,"%","\n")
    cat("最大回落為:",mdd$maxdrawdown,"%","\n")
    cat("#####################","\n")
  }
  
  portfolio_risk_return_func(portfolio_return_index)
  
}

stock_list = c(2412,3045,4904,0050)
my_function(tele_table,start_day = 20140101, end_day = 20220101,stock_list = stock_list)




