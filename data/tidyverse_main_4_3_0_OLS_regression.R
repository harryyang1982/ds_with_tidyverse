##############################################################################
# 분포가 정규분포인 종속변수인 경우 GLM: OLS 회귀모형
library('tidyverse')
library('readxl')
library('haven')
library('broom')  # tidy(), glance() 함수 
setwd("D:/TidyData/data")

data_survey = read_csv("data_survey_comma.csv")
data_survey %>% print(n=2)

# 각 개념들을 단일변수로
mydata = data_survey %>% 
  mutate(
    swl=rowMeans(data_survey %>% select(starts_with("SWL"))),
    sca=rowMeans(data_survey %>% select(starts_with("SCA"))),
    scb=rowMeans(data_survey %>% select(starts_with("SCB"))),
    fem=ifelse(gender==1,0,1)
  ) 

# 모형투입변수들 사이의 상관계수 점
mydata %$% 
  cor(tibble(fem,ageyear,educ,income,sca,scb,swl)) %>% 
  round(.,2)

mydata %>% 
  select(fem,ageyear,educ,income,sca,scb,swl) %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(.) %>% 
  tidy(.) %>% as_tibble()

# 연속형 독립변수에 대한 평균중심화 변환 
mydata2 = mydata %>% 
  select(swl,fem,ageyear,educ,income,sca,scb) %>% 
  mutate_at(
    vars(ageyear:scb),
    funs(. - mean(.,na.rm=T)) # 결측값이 없기 때문에 na.rm 옵션은 붙이지 않아도 무방함
  )

# 우선 OLS 모형 선정 
my_interaction_model = lm(swl ~ (fem+ageyear+educ+income)*sca+scb,mydata2)
OLS_summary_function(my_interaction_model,2)
# OLS 모형에서 2가지 상호작용 효과를 발견하였습니다. 
# 1) fem:sca => sca가 swl에 미치는 효과는 남성보다 여성에게 더 부정적으로 변함
# 2) educ:sca => sca가 swl에 미치는 효과는 교육수준이 더 높은 응답자에게서 더 부정적으로 나타남

# 우선 성별과 SCA 상호작용 효과 시각화 
# 모형 예측치를 선정 
mygrid = mydata2 %>% 
  data_grid(sca,fem,ageyear=0,educ=0,income=0,scb=0) %>% 
  add_predictions(my_interaction_model)
# 중심화 변환을 한 경우 원래의 변수로 되돌리는 과정
mygrid = mygrid %>% 
  mutate(
    sca=sca+mean(mydata$sca)
  )
# 범례를 사용하고, 모형 예측결과만 사용하는 경우 
ggplot(mygrid,aes(x=sca,y=pred,color=factor(fem)))+
  geom_line(size=2)+
  scale_color_discrete(breaks=c(0,1),label=c("남성","여성"))+
  labs(x="사회비교성향(능력비교)",y="삶에 대한 만족도",
       color="성별")+
  coord_cartesian(ylim=c(1.8,3.7),xlim=c(0.5,5.5))
# 패시팅을 사용하고, 원 데이터 산점도에 모형 예측결과 추가사용
ggplot(mydata,aes(x=sca,y=swl))+
  geom_point(aes(color=factor(fem)),alpha=0.3)+
  geom_line(data=mygrid,aes(y=pred,color=factor(fem)),size=1)+
  scale_color_discrete(breaks=c(0,1),label=c("남성","여성"))+
  labs(x="사회비교성향(능력비교)",y="삶에 대한 만족도",
       color="성별")+
  coord_cartesian(ylim=c(0.8,5.2),xlim=c(0.5,5.5))+
  facet_wrap(~factor(fem),
             labeller=as_labeller(c("0"="남성","1"="여성")))

# 다음으로 교육수준과 SCA 상호작용 효과 시각화 
# 모형 예측치를 선정(성별의 경우 0.5는 남성/여성이 1:1의 비율인 경우)
# 연속형 변수의 경우 M-SD/M/M+SD의 세 수준으로 나누는 것이 보통
mydata2 %>% summarize(sd(educ))
mygrid_group3 = mydata2 %>% 
  data_grid(sca,educ=c(-1.26,0.00,1.26),
            fem=0.5,ageyear=0,income=0,scb=0) %>% 
  add_predictions(my_interaction_model)

# 각주: 만약에 10th, 25th, 50th, 75th, 90th 위치를 지정하려고 한다면
mylocation=quantile(mydata2$educ,c(.1,.25,.5,.75,.90))
mygroup_group5 = mydata2 %>% 
  data_grid(sca,educ=mylocation,
            fem=0.5,ageyear=0,income=0,scb=0) %>% 
  add_predictions(my_interaction_model)

# 중심화 변환을 한 경우 원래의 변수로 되돌리는 과정
mygrid = mygrid_group3 %>% 
  mutate(
    sca=sca+mean(mydata$sca),
    educ=factor(as.double(factor(educ)))
  )

# 범례를 사용하고, 모형 예측결과만 사용하는 경우 
ggplot(mygrid,aes(x=sca,y=pred,color=factor(educ)))+
  geom_line(size=2)+
  scale_color_discrete(breaks=1:3,
                       label=c("낮은 교육수준","평균 교육수준","높은 교육수준"))+
  labs(x="사회비교성향(능력비교)",y="삶에 대한 만족도",
       color="교육수준")+
  coord_cartesian(ylim=c(2.2,3.6),xlim=c(0.5,5.5))+
  theme(legend.position="top")

# 원 데이터 산점도에 모형 예측결과 추가사용
ggplot(mydata,aes(x=sca,y=swl))+
  geom_point(alpha=0.2)+
  geom_line(data=mygrid,aes(y=pred,color=educ),size=2)+
  scale_color_discrete(breaks=1:3,
                       label=c("낮은 교육수준","평균 교육수준","높은 교육수준"))+
  labs(x="사회비교성향(능력비교)",y="삶에 대한 만족도",
       color="교육수준")+
  coord_cartesian(ylim=c(0.8,5.2),xlim=c(0.5,5.5))+
  theme(legend.position="top")
