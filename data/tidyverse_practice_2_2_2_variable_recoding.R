#######################################################################
## 연습문제: 변수리코딩 
# 데이터 불러오기 
library('tidyverse')
library('haven')
library('readxl')
setwd("D:/TidyData/data")
gss_panel = read_dta("data_gss_panel06.dta")
# 변수변환_문제_1: 아이가 있는지 아니면 아무도 없는지?
gss_panel %>% 
  mutate(
    child_atleast2=ifelse(childs_1 < 2,0,1)
  ) %>% count(child_atleast2)

# 변수변환_문제_2: 인종구분(백인 vs. 비백인)
gss_panel %>% 
  mutate(
    nowhite_1=ifelse(race_1==1,0,1)
  ) %>% count(nowhite_1)

# 변수변환_문제_3: cut() 함수 
count(gss_panel,as_factor(relactiv_1))
gss_panel %>% 
  mutate(
    religiosity4=cut(relactiv_1,c(0,1,4,6,10),
                     c('none','year','month','week'))
  ) %>% count(religiosity4)

# 변수변환_문제_4: 
gss_panel %>% 
  mutate(
    nowhite_1=ifelse(race_1==1,0,1),
    child_4group=cut_interval(childs_1,n=4)
  ) %>% 
  count(nowhite_1,child_4group) %>% 
  drop_na()

# 변수변환_문제_5 
count(gss_panel, as_factor(caremost_1))
gss_panel %>% 
  mutate(
    global_warming_concern=fct_collapse(as.character(caremost_1),
                                        "climate"=c("2","5"),
                                        "animals"=c("1","3"),
                                        "Inuit"="4")
  ) %>% count(global_warming_concern)

# 변수변환_문제_6
# 데이터 불러오기 
data_foreign_aid = read_xlsx("data_foreign_aid.xlsx")
# 조건에 맞도록 총 개발지원액 변수들 리코딩 
data_foreign_aid2 = data_foreign_aid %>% 
  mutate(
    total_development_aid=str_replace(total_development_aid,"\\$",""),
    development_aid_per_capita=as.double(str_replace(development_aid_per_capita,"\\$","")),
    GDP_percent=as.double(GDP_percent)
  ) %>%
  separate(total_development_aid,c("total_development_aid","char"),sep=" ")
# 어떤 금액단위가 쓰였는지 확인 
count(data_foreign_aid2,char)
# 최종 마무리 
data_foreign_aid2 = data_foreign_aid2 %>% 
  mutate(
    total_development_aid=as.double(total_development_aid) * 10^6
  ) %>% select(-char)
data_foreign_aid2

# 변수변환_문제_7: 개인함수 이용 리코딩
# 결측값 처리후 역코딩(예를 들어 1-> 7, 7->1 과 같이)
data_131 = read_spss("data_TESS3_131.sav")
reverse_coding=function(myvariable){
  myvariable=ifelse(myvariable >=1 & myvariable <= 7,
                    myvariable,NA)
  myvariable=(8-myvariable)
}
data_131 = data_131 %>% 
  mutate(
    Q1r=Q1,Q2r=Q2,Q3r=Q3
  ) %>% 
  mutate_at(
    vars(Q1r,Q2r,Q3r),
    funs(reverse_coding(.))
  ) 
count(data_131,Q1,Q1r)
count(data_131,Q2,Q2r)
count(data_131,Q3,Q3r)

# 변수변환_문제_8
# 결측값 처리후 강도(strength) 변수로 리코딩
make_strength_variable=function(myvariable){
  myvariable=ifelse(myvariable >=1 & myvariable <= 7,
                    myvariable,NA)
  myvariable=abs(myvariable-4)
}
data_131 = data_131 %>% 
  mutate(
    Q1s=Q1,Q2s=Q2,Q3s=Q3
  ) %>% 
  mutate_at(
    vars(Q1s,Q2s,Q3s),
    funs(make_strength_variable(.))
  ) 
count(data_131,Q1,Q1s)
count(data_131,Q2,Q2s)
count(data_131,Q3,Q3s)