#######################################################################
## 연습문제: 기술통계분석
# 데이터 불러오기 
library('tidyverse')
library('haven')
setwd("D:/TidyData/data")
mydata = read_csv("data_survey_comma.csv")

# 기술통계분석_문제_1: 
mydata = mydata %>% 
  mutate(
    female=ifelse(gender==1,0,1),
    female=labelled(female,c(남성=0,여성=1)),
    generation=as.double(cut(ageyear,c(10,19,29,39,49,59),1:5)),
    generation=labelled(generation,
                        c(`10s`=1,`20s`=2,`30s`=3,`40s`=4,`50s`=5))
  )
mydata %>% count(female,generation)
# 시각화 
mydata %>% count(female,generation) %>%
ggplot()+
  geom_bar(aes(x=as_factor(generation),y=n,fill=as_factor(female)),
           stat="identity",position="fill")+
  labs(x="세대",y="빈도수",fill="성별")

# 기술통계분석_문제_2: 
mylong = mydata %>% 
  select(starts_with("SWL"),female,generation) %>% 
  gather(key=Qs,value=value,-female,-generation) %>% 
  mutate(
    Question=as.double(as.factor(Qs)),
    Question=labelled(Question,c(
      `전반적으로 볼 때 나의 삶은 이상향에 가깝다.`=1,
      `내 삶의 상황들은 아주 좋다.`=2,
      `나는 내 삶에 만족한다.`=3,
      `지금까지 내 삶에서 내가 성취하길 원했던 중요한 일들을 이루어냈다.`=4,
      `만약 삶을 다시 살 수 있다면 나는 지금의 삶에서 거의 아무것도 바꾸지 않을 것이다.`=5
    ))
  )

# 시각화
Confidence_Interval_calculation=function(myvariable,myproportion){
  tmp=summary(lm(myvariable~1))
  my_se=tmp$coef[2] #표준오차: 표준편차를 자유도로 나누어 준 것
  my_df=tmp$df[2]  #자유도
  myP=1-(0.5*(1-myproportion))  #t값 계산을 위한 확률계산 
  my_ci=qt(myP,my_df)*my_se  # ci 계산
  my_ci
}

myfig1 = mylong %>% 
  group_by(female,generation,Question) %>% 
  summarize(mn=mean(value))
myfig2 = mylong %>% 
  group_by(female,generation,Question) %>% 
  summarize(ci=Confidence_Interval_calculation(value,0.95))
myfig=full_join(myfig1,myfig2,by=c("female","generation","Question"))

# 95% CI가 포함된 막대그래프의 경우 
myfig %>% 
ggplot(aes(x=as_factor(generation),y=mn,fill=as_factor(female)))+
  geom_bar(stat='identity',position='dodge')+
  geom_errorbar(aes(ymin=mn-ci,ymax=mn+ci),width=.3,
                position=position_dodge(width=1))+
  coord_cartesian(ylim=c(1,4))+
  labs(x="세대",y="응답평균(5점일수록 강한 긍정)",fill="성별")+
  facet_wrap(~as_factor(Question),ncol=1)+
  theme(legend.position="bottom")

# 95% CI가 포함된 점그래프의 경우 
myfig %>% 
  ggplot(aes(x=as_factor(generation),y=mn,color=as_factor(female)))+
  geom_point(stat='identity',position=position_dodge(width=1))+
  geom_errorbar(aes(ymin=mn-ci,ymax=mn+ci),width=.3,
                position=position_dodge(width=1))+
  coord_cartesian(ylim=c(1,4))+
  labs(x="세대",y="응답평균(5점일수록 강한 긍정)",color="성별")+
  facet_wrap(~as_factor(Question),ncol=1)+
  theme(legend.position="bottom")


