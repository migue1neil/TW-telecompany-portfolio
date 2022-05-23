setwd("C:/Users/Neil/Documents/git-repos/TW-telecompany-profolio") # 設定工作目錄
library(data.table)
library(dplyr)
library(plyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 

# 讀取電信三雄的資料
tele_table = read.table("teleprofolio.txt", encoding = "mbcs" , header = T)
colnames(tele_table) =  c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")

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
  table_data$tmp_index = table_data$daily_change + 1
  cumprod_index = ddply( table_data , c("證券代碼","公司名稱") , 
                      .fun= function(x){
                        transform(x, cumprod_return_rate = with(x, cumprod(tmp_index)))
                      } )
  cumprod_index$cumprod_return_rate = cumprod_index$cumprod_return_rate - 1
  cumprod_index$cumprod_return_rate = round(cumprod_index$cumprod_return_rate,digit = 3)
  return(cumprod_index)
}
tele_table = group_comprod_func(tele_table)

#接下來要計算個股報酬率
stock_index_func = function(table_data, stock_number = 2412){
  attach(table_data)
  table_data[table_data$證券代碼 == stock_number ,]
  print(comprod_return_rate[length(comprod_return_rate),])
}


stock_index_func(tele_table , stock_number = 2412 )

stock_2412 = tele_table[tele_table$證券代碼 == 2412,]
min(stock_2412$cumprod_return_rate)
sd(stock_2412$daily_change)
stock_2412[length(stock_2412$調整收盤價),]

stock_3045 = tele_table[tele_table$證券代碼 == 3045,]
tail(stock_3045)

stock_4904 = tele_table[tele_table$證券代碼 == 4904,]
tail(stock_4904)

table_last = tele_table[年月日 == 20220512 & 證券代碼 != 50,]
(1+mean(table_last$cumprod_return_rate))**(1/(2022-2013+1))-1 #綜合年化報酬率
1.98**(1/(2022-2013+1))-1


