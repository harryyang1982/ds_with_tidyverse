#######################################################################
## 연습문제: 날짜 시간변수
# 데이터 불러오기 
library('tidyverse')
library('readxl')
library('lubridate')
setwd("D:/TidyData/data")
mydata = read_csv("data_1000songs.csv")

# 날짜시간변수_문제_1: 프로그램이 가장 많이 방송된 달은 몇월 달? 
mydata %>% 
  mutate(
    month=month(tm_start)
  ) %>% count(month)

# 날짜시간변수_문제_2: 프로그램의 러닝 타임을 구하고, 가장 짧은 방송시간과 긴 방송시간을 보고
mydata %>% 
  mutate(
    time_run=difftime(tm_end,tm_start)
  ) %>% summarize(min(time_run),max(time_run))




