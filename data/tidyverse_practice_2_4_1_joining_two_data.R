# 데이터 불러오기 
library('tidyverse')
library('haven')
library('readxl')
setwd("D:/TidyData/data")

# 데이터합치기_문제_1: 
# 두 데이터를 불러온 후 donor 변수와 COUNTRY 변수를 동일이름을 갖도록 
foreign_aid = read_xlsx("data_foreign_aid.xlsx") %>% 
  rename(ID=donor)
data_country = read_xlsx("data_country.xlsx") %>% 
  rename(ID=COUNTRY)

# 합집합 개념으로 데이터 합치기
mydata_full = full_join(foreign_aid,data_country,by='ID')
# 교집합 개념으로 데이터 합치기
mydata_inner = inner_join(foreign_aid,data_country,by='ID')

# 데이터합치기_문제_2: 
# foreign_aid 데이터에만 등장하는 ID는? 
mydata_full %>% filter(is.na(POPULATION))
data_country %>% 
  mutate(
    myfilter=str_detect(ID,"Slova")
  ) %>% 
  filter(myfilter) %>% select(ID)

# 데이터합치기_문제_3: 
# 이름을 변환한 후 다시 합치기 
foreign_aid = foreign_aid %>% 
  mutate(
    ID=ifelse(ID=="Slovak Republic","Slovakia",ID)
  )
mydata_full = full_join(foreign_aid,data_country,by='ID')
mydata_full %>% filter(is.na(POPULATION))
