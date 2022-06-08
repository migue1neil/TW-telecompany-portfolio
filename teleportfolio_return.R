setwd("C:/Users/Neil/Documents/git-repos/TW-telecompany-portfolio") # 設定工作目錄
library(data.table)
library(plyr) #會用到ddply
library(dplyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
#install.packages('ggstream')
#library(ggstream)
library(lubridate) #轉換日期使用 
library(tseries) #會用到最大回落
library(magrittr) # %>% 水管工人
# library(stringr) #字串串接使用


# 讀取電信三雄的資料，整理資料
tele_data = read.table("teleportfolio.txt", encoding = "mbcs" , header = T) %>% data.table()
table_data = tele_data
colnames(table_data) =  c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")
table_data$年月日 = ymd(table_data$年月日)
# 篩選時間
start_day = 20130101
end_day = 20220101
#防呆設計晚點再加
# if(start_day > end_day){
#   print("錯誤 : 開始日期>結束日期")
# }
# if(start_day < min(table_data$年月日)){start_day = min(table_data$年月日)}
# if(end_day > max(table_data$年月日)){end_day = max(table_data$年月日)}
# cat(as.character(start_day),as.character(end_day))
start_day = ymd(start_day) 
end_day = ymd(end_day) 


table_data = table_data %>% filter(年月日> start_day) %>% filter(年月日< end_day)

# 設計一個函數，可以分組後往下移n個單位
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

# 設計一個函數，可以計算累積乘積
group_cumprod_func = function(table_data){
  table_data$tmp_index = table_data$daily_change + 1 #tmp_index是每日變動+1，不是複利
  cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
                      .fun= function(x){
                        transform(x, cumprod_return_rate = with(x, cumprod(tmp_index)))
                      } )
  cumprod_index$cumprod_index = cumprod_index$cumprod_return_rate #施工用看一下還沒-1前的變化
  cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1 #這樣是變成那個時點的複利，所以到時候只要用投入資金乘上1+這個值就是這個時間點的複利效果
  cumprod_index$cumprod_return_rate = round(cumprod_index$cumprod_return_rate,digit = 3)
  return(cumprod_index)
}
table_data = group_cumprod_func(table_data)

#接下來需要把某些重要的數據打包，可能要再函是裡面先計算好，再拿出來，計算一下
#要計算平均怎麼設計，解:先把資料抓出來 分配好 ，再放到原本上面的裡面計算
stock_list = c(2412,3045,4904)
#stock_list = c(0050)
A = 100 #投入的金額
n = as.numeric(length(stock_list)) #投資股票的數量
w = 1/n  #分配的比重 #假設平均分配

#先篩出要的股票，在計算指數成長
portfolio_return_index_func = function(stock_list){
  portfolio = filter(table_data,證券代碼 %in% stock_list ) #多重篩選用filter比較好用
  portfolio$分配後的投資報酬指數 = A*w*(portfolio$cumprod_return_rate+1)
  portfolio_return_index = portfolio[,c("年月日","分配後的投資報酬指數")]
  portfolio_return_index = portfolio_return_index %>% group_by(年月日) %>% summarise_all(sum)
  colnames(portfolio_return_index)[2] = "投資報酬指數"
  return(portfolio_return_index)
}

portfolio_return_index = portfolio_return_index_func(stock_list)


# 計算投資組合風險指標
portfolio_risk_return_func = function(portfolio_return_index){
  x = portfolio_return_index
  #期末報酬率
  total_return = ((x$投資報酬指數[length(x$投資報酬指數)]- x$投資報酬指數[1])/x$投資報酬指數[1]) #期末報酬率
  #年化報酬率
  via_day = x$年月日[length(x$年月日)] - x$年月日[1] #計算過了幾天
  via_day = as.numeric(via_day) #計算完之後再轉換成數字
  investment_year = via_day/365 #要算過了幾年
  annual_return = ((total_return+1)^(1/investment_year))-1 #年化報酬率計算公式
  annual_return = round(annual_return,digits = 4)
  #標準差
  #sd = sd(x$daily_change)
  # sd(x$調整收盤價)
  # sd(x$cumprod_return_rate)
  #最大回落
  mdd = maxdrawdown(x$投資報酬指數)
  mdd_ratio = (x$投資報酬指數[mdd$to] - x$投資報酬指數[mdd$from]) / x$投資報酬指數[mdd$from]
  mdd_ratio = round(mdd_ratio,digits = 4)
  # print(x$投資報酬指數[mdd$to])  #最大回落低點指數
  # print(x$投資報酬指數[mdd$from]) #最大回落高點指數
  # print(x$年月日[mdd$to]) #最大回落低點天數
  # print(x$年月日[mdd$from]) #最大回落高點天數
  
#顯示與輸出
  #cat("計算股票為:",stock_number,x$公司名稱[1],"\n")
  cat("投資開始日期為:",as.character(x$年月日[1]),"\n")
  cat("結束期間為:",as.character(x$年月日[length(x$年月日)]),"\n")
  cat("投資期間共",via_day,"天","\n")
  cat("期末總報酬為:",round(total_return*100,digits = 4),"%","\n")
  cat("年化報酬為:",annual_return*100,"%","\n")
  cat("最大回落為:",-mdd_ratio*100,"%","\n")
  cat("最大回落開始日期為:",as.character(x$年月日[mdd$from]),"\n")
  cat("最大回落結束日期為:",as.character(x$年月日[mdd$to]),"\n")
  cat("#####################","\n")
  # print(total_return)
  
}

portfolio_mdd = portfolio_risk_return_func(portfolio_return_index)

## 市場標的比較
global_market_index = 0050
market_index = global_market_index
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
  investment_year = via_day/365 #要算過了幾年
  annual_return = ((total_return+1)^(1/investment_year))-1 #年化報酬率計算公式
  annual_return = round(annual_return,digits = 4)
  #最大回落 
  mdd = maxdrawdown(x$市場報酬指數)
  mdd_ratio = (x$市場報酬指數[mdd$to] - x$市場報酬指數[mdd$from]) / x$市場報酬指數[mdd$from]
  mdd_ratio = round(mdd_ratio,digits = 4)
  # print(x$市場報酬指數[mdd$from]) #最大回落高點指數
  # print(x$市場報酬指數[mdd$to])  #最大回落低點指數
  # print(x$年月日[mdd$from]) #最大回落高點天數
  # print(x$年月日[mdd$to]) #最大回落低點天數
  market_mdd_df = data.table(市場最大回落指數 = c(x$市場報酬指數[mdd$from],x$市場報酬指數[mdd$to]),年月日 = c(x$年月日[mdd$from],x$年月日[mdd$to]))
  
  #顯示與輸出
  cat("#####################","\n")
  cat("市場標的為:",global_market_index,"\n")
  cat("同期市場期末總報酬為:",round(total_return*100,digits = 2),"%","\n")
  cat("同期市場年化報酬為:",annual_return*100,"%","\n")
  cat("同期市場最大回落為:",-mdd_ratio*100,"%","\n")
  cat("最大回落開始日期為:",as.character(x$年月日[mdd$from]),"\n")
  cat("最大回落結束日期為:",as.character(x$年月日[mdd$to]),"\n")
  cat("#####################","\n")
  
}
market_return_risk_func(market_return_index)


##### 畫圖的部分
graphics_data = merge(portfolio_return_index, market_return_index, by = "年月日")
#graphics_data = merge(graphics_data , market_mdd , by = "年月日", all = T)
return_index_image = ggplot(graphics_data , aes(x = 年月日) , guide = colour) +
        geom_line(aes(y = 投資報酬指數), colour = "blue") +
        geom_line(aes(y = 市場報酬指數),colour = "red"  ) +
       # geom_stream(aes(y = 市場報酬指數)) +
        ggtitle("投資組合報酬與市場比較") +
        xlab("投資期間") +
        ylab("報酬指數") 
  #把mdd疊上去
      #施工中
  
return_index_image


#####
#目前已知，end_day日期設在最後一天最大回落會錯，不知道為什麼


