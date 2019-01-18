################################################
# 결측값처리_문제_1:
library('tidyverse')
library('readxl')
data_pop = read_xls("data_population.xls",skip=0)
# 어떤 연령대의 통계수치가 문자형으로 입력되었는지 확인
data_pop
# 문자형으로 입력된 변수의 경우 결측값 처리 
data_pop = data_pop %>% 
  mutate_at(
    vars(`85~89세`,`90~94세`,`95~99세`,`100세 이상+`),
    funs(as.double(ifelse(.=="-",NA,.)))
  )
# 결측값 처리 결과 확인 
data_pop

# 결측값처리_문제_2
data_world = read_xlsx("data_country.xlsx")
# 인구가 없는 국가의 경우 0이 아니라 NA를 부여 
data_world = data_world %>% 
  mutate(
    POPULATION = ifelse(POPULATION==0,NA,POPULATION)
  )
data_world

