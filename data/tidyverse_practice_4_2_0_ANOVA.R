#######################################################################
## 연습문제: 분산분석 
# 데이터 불러오기 
library('tidyverse')
library('haven')
library('broom')
setwd("D:/TidyData/data")

# 분산분석_문제_1
data_131 = read_spss("data_TESS3_131.sav")
count(data_131, as_factor(STUDY3_ASSIGN))

mydata = data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0, NA, .))
  ) %>% mutate(
  treat3=ifelse(STUDY3_ASSIGN==1|STUDY3_ASSIGN==3,1,0),
  local=ifelse(STUDY3_ASSIGN==1|STUDY3_ASSIGN==2,1,0),
  treat3=labelled(treat3,c(Control=0,Treatment=1)),
  local=labelled(local,c(Distant=0,Local=1))
  )
aov(Q10 ~ treat3*local, mydata) %>% summary()

# 분산분석_문제_2
count(data_131, as_factor(REL1))
count(data_131, as_factor(REL2))
mydata = mydata %>% 
  mutate(
    Relig2=as.double(cut(REL2,c(1,4,6),1:2)),
    Relig2=labelled(Relig2,c(`Religious`=1,`Not religious`=2))
  ) %>% 
  filter(REL1<4|REL1>10) %>% 
  select(Q10,treat3,local,Relig2) %>% drop_na()

# 3원 분산분석 실시 
mydata %>% 
  aov(Q10 ~ treat3*local*Relig2, data=.) %>% 
  summary(.)
# 결과 시각화 
Confidence_Interval_calculation=function(myvariable,myproportion){
  tmp=summary(lm(myvariable~1))
  my_se=tmp$coef[2] #표준오차: 표준편차를 자유도로 나누어 준 것
  my_df=tmp$df[2]  #자유도
  myP=1-(0.5*(1-myproportion))  #t값 계산을 위한 확률계산 
  my_ci=qt(myP,my_df)*my_se  # ci 계산
  my_ci
}
myfig = mydata %>% 
  filter(REL1<4|REL1>10) %>% 
  group_by(treat3,local,Relig2) %>% 
  summarize(mn=mean(Q10),
            ci=Confidence_Interval_calculation(Q10,0.95))
ggplot(myfig,aes(x=as_factor(treat3),y=mn,fill=as_factor(local)))+
  geom_bar(stat='identity',position=position_dodge(width=0.9))+
  geom_errorbar(aes(ymin=mn-ci, ymax=mn+ci),width=0.3,
                position=position_dodge(width=0.9))+
  labs(x="Treatment",y='7 indicates "Rally should be allowed"',
       fill="Whether respondents are ...")+
  coord_cartesian(ylim=c(2,5.5))+
  facet_wrap(~as_factor(Relig2))+
  theme(legend.position="top")

