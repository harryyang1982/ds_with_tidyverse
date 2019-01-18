#######################################################################
## 연습문제: 변수이름 재설정 
# 데이터 불러오기 
library('tidyverse')
library('readxl')
setwd("D:/TidyData/data")

# 변수이름재설정_문제_1:
data_pop = read_xls("data_population.xls")
names(data_pop)[1:4]=c('year','district','resident','total')
names(data_pop)[25]=c("age100inf")
names(data_pop)[5:24]=str_replace(names(data_pop)[5:24],"~","")
names(data_pop)[5:24]=str_replace(names(data_pop)[5:24],"세","")
names(data_pop)[5:24]=str_c("age",names(data_pop)[5:24])
data_pop


