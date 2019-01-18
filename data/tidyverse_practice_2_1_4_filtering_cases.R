#######################################################################
## 사례선별
# 사례선별_문제_1
library('readxl')
case_Q1 = read_xls("data_library.xls") %>% 
  select(기간, 자치구, ends_with("도서관")) %>% 
  filter(기간==2016 & 자치구 != "합계")

# 사례선별_문제_2
read_xls("data_library.xls") %>% 
  filter(자치구 == "합계") %>% 
  count(기간, 공공도서관)
# 혹은 
read_xls("data_library.xls") %>% 
  filter(자치구 == "합계") %>% 
  select(기간, 공공도서관)

# 그래프를 그리면 매우 효과적(그래프 작업은 나중에 다시 설명하겠습니다)
read_xls("data_library.xls") %>% 
  filter(자치구 == "합계") %>% 
  ggplot(aes(x=as.integer(기간),y=공공도서관))+
  geom_point(size=4,color='blue')+
  geom_line(size=2,color='red',alpha=.5)+
  coord_cartesian(ylim=c(80,160))+
  labs(x="년도",y="공공도서관 수(서울시 25개 구 전체)")

# 사례선별_문제_3
case_Q3 = read_dta("data_gss_panel06.dta") %>% 
  select(starts_with("letin1_"))
# 조건A:
case_Q3 %>% 
  filter(!is.na(letin1_1) & (is.na(letin1_2)|is.na(letin1_3))) 
# 조건B:
case_Q3 %>% 
  filter(letin1_1 == letin1_2 & letin1_1 == letin1_3)
# 조건C:
case_Q3 %>% 
  filter(letin1_1 != letin1_2 & letin1_1 != letin1_3 & letin1_2 != letin1_3)