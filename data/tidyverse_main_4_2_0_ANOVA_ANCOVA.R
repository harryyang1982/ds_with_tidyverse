##############################################################################
## 일반선형모형: 모형추정 및 추정결과 시각화
library('tidyverse')
library('readxl')
library('haven')
library('broom')  # tidy(), glance() 함수 
setwd("D:/TidyData/data")

## ANOVA, ANCOVA

# 데이터 불러오기 
data_131 = read_spss("data_TESS3_131.sav")
# 음수로 입력된 변수값은 결측값
mydata = data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0,NA,.))
  )
# 2번째 실험의 경우 5개 실험집단입니다. 
mydata %>% count(STUDY2_ASSIGN)
# 변수의 라벨 작업 
mydata = mydata %>% 
  mutate(
    treat2=labelled(STUDY2_ASSIGN,
                    c(pro_cue_y_pol=1,
                      pro_cue_n_pol=2,
                      con_cue_y_pol=3,
                      con_cue_n_pol=4,
                      control=5)),
    treat2=fct_relevel(as_factor(treat2),"control")
  )
# 기술통계분석 및 시각화 
myresult = mydata %>% 
  group_by(treat2) %>% 
  summarize(mn=mean(Q6, na.rm=T),
            ci=Confidence_Interval_calculation(Q6, .95))
myresult
myresult %>% 
  ggplot(aes(x=treat2,y=mn))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mn-ci,ymax=mn+ci),width=0.05)+
  labs(x="Groups",
       y="Support for the DREAM act (7, stronger support)")+
  scale_x_discrete(labels=c("Control","Pro-cue with\nparty polarization",
                            "Pro-cue without\nparty polarization",
                            "Con-cue with\nparty polarization",
                            "Con-cue without\nparty polarization"))+
  coord_cartesian(ylim=c(2.5,5.5))

# ANOVA 
mydata %>% 
  aov(Q6~treat2,.) %>% 
  summary()

# ANOVA 
mydata %>% 
  aov(Q6~treat2,.) %>% 
  tidy()

# 사후 비교의 경우도 타이디데이터로 정리할 수 있습니다
mydata %>% 
  aov(Q6~treat2,.) %>% 
  TukeyHSD(.,which="treat2") %>% 
  tidy() %>% 
  as_tibble() %>% 
  filter(adj.p.value < .05)

# 사후비교 결과를 이용 시각화를 조금 더 개선해 봅시다. 
# 5개 집단을 평균차이가 명확하게 나타나는 집단에 따라 구분함
# A/B 집단을 구분하여 시각화 
myresult = myresult %>% 
  mutate(
    posthoc=as.factor(c(rep('A',3),rep('B',2)))
  )
myresult %>% 
  ggplot(aes(x=treat2,y=mn,color=posthoc))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mn-ci,ymax=mn+ci),width=0.05)+
  labs(x="Groups",
       y="Support for the DREAM act (7, stronger support)",
       color="Grouping based on Tukey's HSD")+
  scale_x_discrete(labels=c("Control","Pro-cue with\nparty polarization",
                            "Pro-cue without\nparty polarization",
                            "Con-cue with\nparty polarization",
                            "Con-cue without\nparty polarization"))+
  coord_cartesian(ylim=c(2.5,5.5))+
  theme(legend.position="top")

# 직교 계획비교(orthogonal planned contrast)
# 계획비교를 위한 비교코딩을 마련합니다. 
mycontrast = cbind(c(0,1,1,-1,-1)) # 2 pro-cues vs. 2 con-cues 
mydata %>% 
  aov(Q6~treat2,data=.,contrasts=list(treat2=mycontrast)) %>% 
  tidy()

# ANCOVA도 유사합니다[물론 ANCOVA의 가정이 타당한가는 이론적 문제입니다]
mydata %>% 
  aov(Q6~treat2+IDEO,.) %>% 
  tidy(.)

# ANCOVA 모형기반 종속변수 예측값 추정 및 시각화 
library("modelr")  # data_grid(), add_predictions() 함수 
# 1단계
my_ANCOVA = mydata %>% lm(Q6~treat2+IDEO,.)
mygrid = mydata %>% 
  drop_na(Q6,treat2,IDEO) %>% 
  data_grid(treat2,IDEO=mean(mydata$IDEO,na.rm=T)) %>% # 2단계
  add_predictions(my_ANCOVA)  # 3단계
mygrid 

# 시각화
mygrid %>% 
  ggplot(aes(x=treat2,y=pred))+
  geom_bar(stat="identity")+
  labs(x="Groups",
       y="Support for the DREAM act
       (7, stronger support, covariate is assumed at its mean)")+
  scale_x_discrete(labels=c("Control","Pro-cue with\nparty polarization",
                            "Pro-cue without\nparty polarization",
                            "Con-cue with\nparty polarization",
                            "Con-cue without\nparty polarization"))+
  coord_cartesian(ylim=c(2.5,5.5))

# 95% CI를 같이 그리는 경우는 조금 복잡합니다. 
# 아마 broom 라이브러리 버전이 업그레이드되면 해소될 것으로 생각합니다
mygrid = mydata %>% 
  drop_na(Q6,treat2,IDEO) %>% 
  data_grid(treat2,IDEO=mean(mydata$IDEO,na.rm=T)) %>% # 2단계
  mutate(
    pred=predict(my_ANCOVA,newdata=.),
    my_df=predict(my_ANCOVA,newdata=.,se=T)$df,
    # 95% 신뢰구간 기준, 다른 조건인 경우 .975의 값 조정필요 
    ci=qt(.975,my_df)*predict(my_ANCOVA,newdata=.,se=T)$se.fit 
  ) %>% # 3단계 
  select(-my_df)  
mygrid 
mygrid %>% 
  ggplot(aes(x=treat2,y=pred))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=pred-ci,ymax=pred+ci),width=0.05)+
  labs(x="Groups",y="Support for the DREAM act
       (7, stronger support, covariate is assumed at its mean)")+
  scale_x_discrete(labels=c("Control","Pro-cue with\nparty polarization",
                            "Pro-cue without\nparty polarization",
                            "Con-cue with\nparty polarization",
                            "Con-cue without\nparty polarization"))+
  coord_cartesian(ylim=c(2.5,5.5))+
  theme(legend.position="top")

# 만약 IDEO = 7(강한 보수적 성향)인 경우, 예측값은? 
mygrid2 = mydata %>% 
  drop_na(Q6,treat2,IDEO) %>% 
  data_grid(treat2,IDEO=7) %>% # 2단계
  add_predictions(my_ANCOVA)  # 3단계
mygrid2 

## OLS: 종속변수가 정규분포일 때 사용하는 OLS  
# 사실 ANOVA/ANCOVA = OLS 입니다
mydata %>% 
  aov(Q6~treat2,.) %>% 
  lm() %>% 
  summary()

# OLS 추정결과 정리 
lm(Q6~treat2,mydata) %>% tidy(.)

lm(Q6~treat2,mydata) %>% glance(.)

# ANCOVA 역시도 마찬가지. 
my_ancova = mydata %>% 
  aov(Q6~treat2+IDEO,.) %>% 
  lm()
tidy(my_ancova)

glance(my_ancova)

# 회귀모형 추정결과의 해석을 돕기 위해 중심화 변환을 실시하자. 
my_ols_MC = mydata %>% 
  mutate(IDEO2=IDEO-mean(mydata$IDEO,na.rm=T)) %>% 
  lm(Q6~treat2+IDEO2,.) 
tidy(my_ols_MC);glance(my_ols_MC)

# OLS 회귀계수의 결과요약 
OLS_summary_function = function(my_model_estimation, mydigit){
  mycoefs = tidy(my_model_estimation) %>% 
    mutate(
      est2=format(round(estimate, mydigit),mydigit),
      se2=format(round(std.error, mydigit),mydigit),
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      report=str_c(" ",est2,mystars,"\n(",se2,")",sep="")
    ) %>% select(term,report)
  myGOF=glance(my_model_estimation) %>% 
    mutate_all(
      funs(round(.,3))
    ) %>% 
    mutate(
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      model_dfs = str_c("(",(df-1),", ",df.residual,")"),
      model_F = str_c(statistic,mystars),
      R2=format(round(r.squared,mydigit),mydigit),
      adjR2=format(round(adj.r.squared,mydigit),mydigit)
    ) %>% select(model_F, model_dfs, R2, adjR2) %>% 
    gather(key=term,value=report)
  # sortid를 만든 이유는 여러 모형들의 결과를 하나의 표에 합칠 경우
  mytable=bind_rows(mycoefs,myGOF) %>%
    mutate(sortid=row_number()) %>% 
    select(sortid,term,report) %>%
    mutate_at(vars(2:3),funs(as.character(.)))
  mytable
}
OLS_summary_function(my_ols_MC,2)

# 상호작용 효과 테스트 및 시각화
mydata = mydata %>% 
  mutate(Q7_mc=Q7-mean(Q7,na.rm=T)) 
model_mainE = lm(Q6~treat2+Q7_mc,mydata)
model_interactE = lm(Q6~treat2*Q7_mc,mydata)

myresult = full_join(OLS_summary_function(model_mainE,2)[,-1],
                     OLS_summary_function(model_interactE,2),
                     by="term") %>% arrange(sortid) %>% select(-sortid)
write_excel_csv(myresult,"model_comparison_table.csv")

# 상호작용효과의 시각화 
# 예측값 산출
mygrid = mydata %>% 
  drop_na(Q6,treat2,Q7_mc) %>% 
  data_grid(treat2,Q7_mc) %>%
  add_predictions(model_interactE) %>% 
  mutate(
    Q7=Q7_mc+mean(mydata$Q7,na.rm=T)
  )

# 범례를 사용 
mygrid %>% 
  filter(Q7==2|Q7==4|Q7==6) %>% 
  ggplot(aes(x=factor(as.double(treat2)),y=pred,fill=factor(Q7)))+
  geom_bar(stat="identity",position=position_dodge(width=0.8))+
  labs(x="Experimental conditions",
       y="Support for the DREAT act (7, stronger support)",
       fill="Certainty about own opinion")+
  scale_x_discrete(breaks=1:5,
                   labels=c("Control\n","Pro-cue with\nparty polarization",
                            "Pro-cue without\nparty polarization",
                            "Con-cue with\nparty polarization",
                            "Con-cue without\nparty polarization"))+
  scale_fill_discrete(breaks=c(2,4,6),
                      labels=c("Weak (2)","Moderate (4)","Strong (6)"))+
  coord_cartesian(ylim=c(1.5,5.5))+
  theme(legend.position="top")

# 패시팅 라벨 생성
treat2_names <- c(
  "control" = "Control\n",
  "pro_cue_y_pol" = "Pro-cue with\nparty polarization",
  "pro_cue_n_pol" = "Pro-cue without\nparty polarization",
  "con_cue_y_pol" = "Con-cue with\nparty polarization",
  "con_cue_n_pol" = "Con-cue without\nparty polarization"
)
mygrid %>% 
  ggplot(aes(x=Q7,y=pred))+
  geom_line()+
  labs(x="Certainty about own opinion",
       y="Support for the DREAT act (7, stronger support)",
       color="Experimental\nconditions")+
  coord_cartesian(ylim=c(1.5,6.0),xlim=c(0.5,7.5))+
  facet_wrap(~treat2,ncol=3,labeller=as_labeller(treat2_names))

# 7점 척도 측정변수를 등간변수로 하는 경우 히트맵이 보기 좋습니다
crosstab_Q6_Q7 = mydata %>% 
  drop_na(Q6,Q7,treat2) %>% 
  count(Q6,Q7,treat2)
ggplot(crosstab_Q6_Q7,aes(x=Q7,y=Q6))+
  geom_tile(aes(fill = n))+ 
  scale_fill_gradient(low = "grey80", high = "black")+
  geom_line(data=mygrid,aes(y=pred),color='blue')+
  labs(x="Certainty about own opinion",
       y="Support for the DREAT act (7, stronger support)",
       fill="Respondents\nfor each cell")+
  facet_wrap(~treat2,ncol=3,labeller=as_labeller(treat2_names))+
  theme_bw()  # 그래프 배경을 회색으로 하면 패턴이 잘 안드러나 남. 

mydata %>% 
  mutate(
    strong_Q6=abs(Q6-4)
  ) %>% lm(Q7~treat2*strong_Q6,.) %>% 
  OLS_summary_function(.,2)