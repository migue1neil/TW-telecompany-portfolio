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



# 讀取電信三雄的資料，整理資料
tele_table = read.table("teleportfolio.txt", encoding = "mbcs" , header = T)
colnames(tele_table) =  c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")
tele_table$年月日 = ymd(tele_table$年月日)
#篩選時間
start_day = ymd(20210101)
end_day = ymd(20220101)
tele_table = tele_table %>% filter(年月日> start_day) %>% filter(年月日< end_day)

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
tele_table = group_daily_change_function(tele_table)

#設計一個函數，可以計算累積乘積
group_comprod_func = function(table_data){
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
tele_table = group_comprod_func(tele_table)

#接下來需要把某些重要的數據打包，可能要再函是裡面先計算好，再拿出來，計算一下
#要計算平均怎麼設計，解:先把資料抓出來 分配好 ，再放到原本上面的裡面計算
stock_list = c(2412,3045,4904)
stock_list = c(0050)
A = 100 #投入的金額
n = as.numeric(length(stock_list)) #投資股票的數量
w = 1/n  #分配的比重 #假設平均分配

#先篩出要的股票，在計算指數成長
portfolio_return_index_func = function(stock_list){
portfolio = filter(tele_table,證券代碼 %in% stock_list ) #多重篩選用filter比較好用
portfolio$分配後的投資報酬指數 = A*w*(portfolio$cumprod_return_rate+1)
portfolio_return_index = portfolio[,c("年月日","分配後的投資報酬指數")]
portfolio_return_index = portfolio_return_index %>% group_by(年月日) %>% summarise_all(sum)
colnames(portfolio_return_index)[2] = "投資報酬指數"
plot(x = portfolio_return_index$年月日 ,y = portfolio_return_index$投資報酬指數)
}

ff = portfolio_return_index_func(stock_list)


# 計算投資組合風險指標
portfolio_risk_return_func = function( portfolio_data ){
  x = portfolio_data
  #期末報酬率
  total_return = ((x$投資報酬指數[length(x$投資報酬指數)]- x$投資報酬指數[1])/x$投資報酬指數[1]) #期末報酬率
  #年化報酬率
  #x$年月日 = ymd(x$年月日) #先轉換成日期格式
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
  
#顯示與輸出
  #cat("計算股票為:",stock_number,x$公司名稱[1],"\n")
  cat("投資開始日期為:",as.character(x$年月日[1]),"\n")
  cat("結束期間為:",as.character(x$年月日[length(x$年月日)]),"\n")
  cat("投資期間共",via_day,"天","\n")
  cat("期末總報酬為:",total_return*100,"%","\n")
  cat("年化報酬為:",annual_return*100,"%","\n")
  cat("最大回落為:",mdd$maxdrawdown,"%","\n")
  cat("#####################","\n")
  print(total_return)
}

portfolio_risk_return_func(portfolio_return_index)

## 市場標的比較
#TW50_return_index = 
stock_func(tele_table,stock_number = 0050)

TW50_return_index_func = function(market_index = 0050){
  market_return = filter(table_data,證券代碼 %in% market_index ) #多重篩選用filter比較好用
  market_return$報酬指數 = A*(market_return$cumprod_return_rate+1)
  market_return_index = portfolio[,c("年月日","報酬指數")]
  market_return_index = portfolio_return_index %>% group_by(年月日) %>% summarise_all(sum)
  colnames(portfolio_return_index)[2] = "市場報酬指數"
  return(portfolio_return_index)
}
TW50_return_index = TW50_return_index_func() #到這邊就計算完投資報酬指數了



A =100



###施工區/垃圾堆積區
#####
# 計算個股報酬率，個股報酬使用
stock_func = function( table_data , stock_number ){
  x = table_data[table_data$證券代碼 == stock_number ,] #從已經整理好的table中選出標的x
  #期末報酬率
  total_return = x$cumprod_return_rate[length(x$cumprod_return_rate)] #期末報酬率
  #年化報酬率
  x$年月日 = ymd(x$年月日) #先轉換成日期格式
  via_day = x$年月日[length(x$年月日)] - x$年月日[1] #計算過了幾天
  via_day = as.numeric(via_day) #計算完之後再轉換成數字
  investment_year = via_day/365 #要算過了幾年
  annual_return = (x$cumprod_return_rate[length(x$cumprod_return_rate)]+1)^(1/investment_year)-1 #年化報酬率計算公式
  annual_return = round(annual_return,digits = 4)
  #幫這支股票做一個投資報酬指數
  #標準差
  sd = sd(x$daily_change)
  # sd(x$調整收盤價)
  # sd(x$cumprod_return_rate)
  #最大回落
  mdd = maxdrawdown(x$調整收盤價)
  #未創新高天數
  
  
  #顯示與輸出
  cat("計算股票為:",stock_number,x$公司名稱[1],"\n")
  cat("投資開始日期為:",as.character(x$年月日[1]),"\n")
  cat("結束期間為:",as.character(x$年月日[length(x$年月日)]),"\n")
  cat("投資期間共",via_day,"天","\n")
  cat("期末總報酬為:",total_return*100,"%","\n")
  cat("年化報酬為:",annual_return*100,"%","\n")
  cat("最大回落為:",mdd$maxdrawdown,"%","\n")
  cat("#####################","\n")
}
stock_func(tele_table,stock_number = 0050)

#####

# 想一下要怎麼用迴圈把他丟進去跑，這樣之後如果篩選出一籃子股票可以通通丟進去
#後來發現不用迴圈迴圈太慢
# stock_list = c(2412,3045,4904)
# for (i in stock_list){
#   stock_func(tele_table,stock_number = i)
#   Sys.sleep(3) #讓她暫停一下
# }
# 已打包
##### 
#####取得個股報酬率，計算出標的的，投資報酬率，年化報酬率，標準差，最大回落，#其他的之後在加 
##重要設計未創新高天數
#沒辦法設計起始日期因為，報酬率有先算好了，要設定起始日期的話要先設再重算，須重新設計
x = tele_table[tele_table$證券代碼 == 2412 ,] #選出標的
#期末報酬率
total_return = x$cumprod_return_rate[length(x$cumprod_return_rate)] #期末報酬率
#年化報酬率
x$年月日 = ymd(x$年月日) #先轉換成日期格式
via_day = x$年月日[length(x$年月日)] - x$年月日[1] #計算過了幾天
via_day = as.numeric(via_day) #計算完之後再轉換成數字
investment_year = via_day/365 #要算過了幾年
annual_return = (x$cumprod_return_rate[length(x$cumprod_return_rate)]+1)^(1/investment_year)-1 #年化報酬率計算公式
annual_return = round(annual_return,digits = 4)
#標準差
sd = sd(x$daily_change)
# sd(x$調整收盤價)
# sd(x$cumprod_return_rate)
#最大回落
mdd = maxdrawdown(x$調整收盤價)
mdd
#顯示與輸出
cat("投資開始日期為:",as.character(x$年月日[1]))
cat("結束期間為:",as.character(x$年月日[length(x$年月日)]))
cat("投資期間共",via_day,"天")
cat("期末總報酬為:",total_return*100,"%")
cat("年化報酬為:",annual_return*100,"%")
cat("最大回落為:",mdd$maxdrawdown,"%")

table = data.table()
#####
a = tele_table[tele_table$證券代碼 == 2412,]
v = tele_table[tele_table$證券代碼 == 3045,]
# 需求 ， 分別計算每個股票的報酬指數值，再把它加總起來
# 方法一，把每個股票都用一個df 再把這些df相加
# 方法二，把全部股票用成一個df 再根據年月日東西把df相加
# 完工，想太複雜
#####
## 方法一
for (i in stock_list){
  df = tele_table[tele_table$證券代碼 == i,]
  df$分配後的投資報酬指數 = (df$cumprod_return_rate+1)*w*A
  str_c("s",as.character(i)) = data.table()
}
## 方法二 

group_return_index = function(table_data){  #設計一個函數，新生成一個函數將每檔股票根據分配好的比重算出一個指數
  data = ddply( table_data , c("證券代碼") , 
                .fun= function(x){
                  transform(x, 分配後的投資報酬指數 = with(x, (table_data$cumprod_return_rate+1)*w*A ))
                } )
  return (data)
}

aa =  group_return_index(tele_table)

group_sum_index = function(table_data){  #設計一個函數，根據年月份加總分配好的指數
  # portfolio_index = table_data$
  data = ddply( table_data , c("年月日") , 
                .fun= function(x){
                  transform(x, 投資報酬指數 = with(x, sum(table_data$分配後的投資報酬指數) ))
                } )
  return (data)
}

bb = group_sum_index(aa)
a1 =aa[colnames(aa)==("年月日")]
a2 =aa[colnames(aa)==("分配後的投資報酬指數")]
a3 = cbind(a1,a2)
a4 = a3 %>% group_by(年月日) %>% summarise_all(sum) #這樣是算得出來但好像哪裡怪怪的，數值怪怪的

?summarise_all

a4 = order(a3$年月日)
#####
#####
#接下來要計算個股報酬率
stock_index_func = function(table_data, stock_number = 2412){
  attach(table_data)
  table_data[table_data$證券代碼 == stock_number ,]
  cumprod_return_rate[length(cumprod_return_rate),]
}

stock_index_func(tele_table , stock_number = 2412 )

stock_2412 = tele_table[tele_table$證券代碼 == 2412,]
min(stock_2412$cumprod_return_rate)
sd(stock_2412$daily_change)
stock_2412[length(stock_2412$調整收盤價),]

stock_3045 = tele_table[tele_table$證券代碼 == 3045,]
tail(stock_3045)

mdd = maxdrawdown(stock_2412$調整收盤價)
mdd


stock_TW50 = tele_table[tele_table$證券代碼 == 50 ,]
mdd_TW50 = maxdrawdown(stock_TW50$調整收盤價)
mdd_TW50
min(stock_TW50$cumprod_return_rate) #取幾何報酬率的最小值，是看實質虧損，以0050來講的話會發生在前幾年

?maxdrawdown



stock_4904 = tele_table[tele_table$證券代碼 == 4904,]
tail(stock_4904)

table_last = tele_table[年月日 == 20220512 & 證券代碼 != 50,]
(1+mean(table_last$cumprod_return_rate))**(1/(2022-2013+1))-1 #綜合年化報酬率
1.98**(1/(2022-2013+1))-1


#mdd 設計

x = for (i in c(10:1)){
  n = i - max(i)
  print(n)
}
c(10:1)

view(maxdrawdown)

     