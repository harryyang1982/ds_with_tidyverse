#######################################################################
## 연습문제: 티테스트 
# 데이터 불러오기 
library('tidyverse')
library('haven')
library('readxl')
library('magrittr')
library('broom')
setwd("D:/TidyData/data")

# 티테스트_문제_1
gss_panel = read_dta("data_gss_panel06.dta")
mydata = gss_panel %>% 
  select(ends_with("_1")) %>% 
  select(polviews_1,sex_1,age_1,abany_1:absingle_1) %>% 
  drop_na() %>% 
  mutate_at(
    vars(abany_1:absingle_1),
    funs(ifelse(.==2,1,0))
  ) 
names(mydata)=str_replace(names(mydata),"_1","")
mydata = mydata %>% 
  mutate(
    anti_abortion=rowSums(mydata[,4:10]),
    gen=as.double(cut(age,c(10,29,49,69,Inf),1:4)),
    gen=labelled(gen,c(`10-20s`=1,`30-40s`=2,`50-60s`=3,`70s+`=4))
  )

t.test(anti_abortion~sex,data=mydata)

# 티테스트_문제_2
mydata %>% 
  split(.$gen) %>% 
  map(~ t.test(anti_abortion~sex,data=.)) %>% 
  map_dfr(~ tidy(.))

Confidence_Interval_calculation=function(myvariable,myproportion){
  tmp=summary(lm(myvariable~1))
  my_se=tmp$coef[2] #표준오차: 표준편차를 자유도로 나누어 준 것
  my_df=tmp$df[2]  #자유도
  myP=1-(0.5*(1-myproportion))  #t값 계산을 위한 확률계산 
  my_ci=qt(myP,my_df)*my_se  # ci 계산
  my_ci
}
# 시각화를 위한 데이터 생성
myresult = mydata %>% 
  group_by(gen,sex) %>% 
  summarize(
    y_mn=mean(anti_abortion),
    y_ci=Confidence_Interval_calculation(anti_abortion,0.95)
  ) %>% 
  ungroup() %>% 
  mutate(
    mysig=ifelse(gen==1,"significant","insignificant"),
    sex=labelled(sex,c(Men=1,Women=2))
  )
# 95% CI 포함된 막대그래프
ggplot(myresult,aes(x=as_factor(gen),y=y_mn,fill=as_factor(sex)))+
  geom_bar(stat="identity",position=position_dodge(width=0.9))+
  geom_errorbar(aes(ymin=y_mn-y_ci,ymax=y_mn+y_ci,linetype=mysig),
                width=0.3,position=position_dodge(width=0.9))+
  scale_linetype_manual(values=c("dotted","solid")) +
  labs(x="Generation",
       y="Averaged score of anti-abortion attitude",
       fill="Gender",linetype="Difference between sexes?")+
  theme(legend.position = "top")

# 티테스트_문제_3
data_school = read_xls("data_student_class.xls",skip=2)
mydata = data_school %>% 
  select(기간, 지역, 학급당원아수) %>% 
  rename(
    year=기간, district=지역, y=학급당원아수
  ) %>% 
  filter(year>2014 & district != "합계")

# 대응표본 티테스트가 적절합니다. 
mydata %>% 
  spread(key=year,value=y) %$% 
  t.test(`2015`,`2016`,paired=TRUE)

# 주의한다면 다음과 같이 하는 것도 가능합니다. 단 주의! 
mydata %>% arrange(year, district) %>% 
  t.test(y~year,paired=TRUE,data=.)

# 아래를 보시면 상당히 다른 결과인 것을 알 수 있습니다. 왜 그럴까요? 
t.test(y~year,data=mydata)

