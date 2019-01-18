#####################################################################
# 로지스틱 회귀분석
setwd("D:/TidyData/data")
# 로지스틱회귀_문제_1
library('tidyverse')
library('haven')
library('modelr')
library('broom')
gss_panel = read_dta("data_gss_panel06.dta")
# 조건에 맞는 변수들과 사례들만 선별 
mydata = gss_panel %>% 
  select(starts_with("affrmact_"),
         "sex_1","age_1","race_1","educ_1","income06_1","polviews_1") %>% 
  drop_na()
# 변수이름 정리
names(mydata)[4:9]=str_replace(names(mydata)[4:9],"_1","")
# 적극적 평등정책에 대한 찬성의견을 1, 반대의견을 0
# 성별과 인종을 이분변수로 
mydata = mydata %>% 
  mutate_at(
    1:3,
    funs(ifelse(.==1|.==2,1,0))
  ) %>% 
  mutate(
    female=ifelse(sex==1,0,1),
    nonwhite=ifelse(race==1,0,1)
  ) %>% 
  gather(key=time,value=y,-(sex:nonwhite)) %>% 
  mutate(
    time=str_extract(time,"[[:digit:]]{1}")
  )
# 연속형 변수의 경우 평균중심화 변환
mydata_center = mydata %>% 
  mutate_at(
    vars(age,educ,income06,polviews),
    funs(. - mean(.))
  )

# 로지스틱회귀_문제_2
# 상호작용효과 모형 추정 
my_interactE = mydata_center %>% 
  split(.$time) %>% 
  map(~ glm(y~(female+nonwhite+age+I(age^2)+educ+income06)*polviews,,
            data=.x,family=binomial(link='logit')))
# Interaction effects 해석
my_interactE %>% 
  map_dfr(~ tidy(.)) %>% 
  filter(term=='polviews') %>% 
  mutate(OR = exp(estimate))

# 로지스틱회귀_문제_3
# 교육수준별로 세 집단을 나누기 위해 SD 계산 
mydata_center %>% filter(time==3) %>% summarize(sd(educ))
# 시점별 상호작용효과 그래프를 그리기 위한 잠정적 개인함수 생성 
temporary_function=function(i){
  # 보다 매끈한 선을 그리기 위해 다음과 같이 정치성향을 촘촘하게 배치하였습니다. 
  new_range_polviews=0.10*(10:70)-mean(mydata$polviews)
  myfig = mydata_center %>% 
    filter(time==i) %>% 
    data_grid(educ=c(-2.96,0,2.96),polviews=new_range_polviews,
              female=0,age=0,nonwhite=0,income06=0) %>% 
    mutate(
      predy=predict(my_interactE[[i]],newdata=.,type='response')
    )
  # 정치적 성향의 평균중심화 변환값을 원값으로 전환 
  myfig %>% 
    mutate(
      educ=as.factor(as.double(as.factor(educ))),
      polviews=polviews+4.15
    )
}
# 각 집단별로 그래프 작성용 데이터 생성 
myfig1 = temporary_function(i=1) %>% mutate(time=2006)
myfig2 = temporary_function(i=2) %>% mutate(time=2008)
myfig3 = temporary_function(i=3) %>% mutate(time=2010)
# 데이터 합치기 
myfig = bind_rows(myfig1,myfig2,myfig3)
# 시각화
ggplot(data=myfig,aes(x=polviews,y=predy,color=educ))+
  geom_line(size=1)+
  labs(x="Political ideology",
       y="Probability to show positive view on affirmative action",
       color="Education")+
  coord_cartesian(ylim=c(0,1))+
  scale_color_discrete(breaks=1:3,
                       labels=c("Low (M-SD)",
                                "Moderate (M)",
                                "High (M+SD)"))+
  scale_x_continuous(breaks=c(1,4,7),
                     labels=c("Strongly\nliberal",
                              "Ideologically\nmoderate",
                              "Strongly\nconservative"))+
  theme(legend.position = "top")+
  facet_wrap(~time,ncol=1)