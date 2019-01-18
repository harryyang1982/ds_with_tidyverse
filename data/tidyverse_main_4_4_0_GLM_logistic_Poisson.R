##############################################################################
# 분포가 정규분포가 아닌 종속변수인 경우 GLM
library('tidyverse')
library('readxl')
library('haven')
library('broom')  # tidy(), glance() 함수 
setwd("D:/TidyData/data")


gss_panel = read_dta("data_gss_panel06.dta")
# 세 조건에 맞는 변수들과 사례들만 선별 
mydata = gss_panel %>% 
  select(starts_with("suicide"),
         "sex_1","age_1","race_1","educ_1","income06_1","polviews_1") %>% 
  select(ends_with("_1")) %>% 
  drop_na()
# 변수이름에서 _1 이라는 표현을 제거
names(mydata)=str_replace(names(mydata),"_1","")
# 변수들을 사전처리 합시다. 
mydata = mydata %>% 
  mutate(
    fem=ifelse(sex==1,0,2),
    nonwhite=ifelse(race==1,0,1)
  ) %>% 
  mutate_at(
    vars(suicide1:suicide4),
    funs(ifelse(.==1,1,0))
  ) %>% 
  rename(libcon7=polviews)

# 결과의 이해를 돕기 위해 연속형 변수의 경우 중심화 변환을 실시하였습니다. 
mydata_center = mydata %>% 
  mutate_at(
    vars(age,educ,income06,libcon7),
    funs(. - mean(.))
  )

# 로지스틱 회귀분석 모형
# 주효과만 고려한 모형
Logistic_mainE = glm(suicide1~fem+age+I(age^2)+nonwhite+educ+
                       income06+libcon7,mydata_center,
                     family=binomial(link='logit'))
tidy(Logistic_mainE)
glance(Logistic_mainE)

# 모형의 예측값을 저장: 아깝지만 add_predictions() 함수는 사용불가
myfigure = mydata_center %>% 
  data_grid(age,fem=0,nonwhite=0,educ=0,income06=0,libcon7=0) %>% 
  mutate(pred=predict(Logistic_mainE,.,type="response"))
# 중심화 변환된 연령변수를 원래대로 바꿈
myfigure = myfigure %>% 
  mutate(age=age+mean(mydata$age,na.rm=T))
# 예측확률을 시각화 
ggplot(myfigure,aes(x=age,y=pred)) +
  geom_line(size=1)+
  labs(x="age",y="Probability of 'yes'
       (Suicide, if incurable disease?)")+
  scale_x_continuous(breaks=10*(2:8))+
  coord_cartesian(ylim=c(0.4,.8),xlim=c(15,90))

# 예측확률과 실측된 데이터를 같이 시각화 
ggplot(data=mydata,aes(x=age,y=suicide1)) +
  geom_point(alpha=.05)+
  geom_line(data=myfigure,aes(y=pred),size=1)+
  labs(x="age",y="Probability of 'yes'
       (Suicide, if incurable disease?)")+
  scale_x_continuous(breaks=10*(2:8))+
  coord_cartesian(ylim=c(0,1),xlim=c(15,90))

# 로지스틱 회귀계수의 결과요약 
Logistic_summary_function = function(my_model_estimation, mydigit){
  #my_model_estimation=Logistic_mainE; mydigit=2
  mycoefs = tidy(my_model_estimation) %>% 
    mutate(
      est2=format(round(estimate, mydigit),mydigit),
      se2=format(round(std.error, mydigit),mydigit),
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      report=str_c(" ",est2,mystars,"\n(",se2,")",sep=""),
      my_or=format(round(exp(estimate), mydigit),mydigit)
    ) %>% select(term,report,my_or)
  myGOF=glance(my_model_estimation) %>% 
    mutate(
      LL_CHI2=null.deviance-deviance,
      DF_CHI2=df.null-df.residual,
      p.value=1 - pchisq(LL_CHI2,DF_CHI2),
      McFaddenR2=format(round((null.deviance-deviance)/null.deviance,
                              mydigit),mydigit),
      LL_CHI2=format(round(LL_CHI2, mydigit),mydigit),
      mystars=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      my_CHI2 = str_c(LL_CHI2,mystars),
      LL_CHI2_df = str_c("(",DF_CHI2,")"),
      AIC=format(round(AIC,mydigit),mydigit),
      BIC=format(round(BIC,mydigit),mydigit)
    ) %>% select(McFaddenR2, my_CHI2, LL_CHI2_df, AIC, BIC) %>% 
    gather(key=term,value=report) %>% mutate(my_or="")
  # sortid를 만든 이유는 여러 모형들의 결과를 하나의 표에 합칠 경우
  mytable=bind_rows(mycoefs,myGOF) %>%
    mutate(sortid=row_number()) %>% 
    select(sortid,term,report,my_or) %>%
    mutate_at(vars(2:4),funs(as.character(.)))
  mytable
}

# 상호작용효과를 추가로 고려한 모형
Logistic_interactE = glm(suicide1~fem+age+I(age^2)+nonwhite+educ+
                           income06*libcon7,mydata_center,
                         family=binomial(link='logit'))
# 정리한 후 CSV 화일로 저장 후 약간의 편집을 적용하여 사용하면 됩니다
myresult = full_join(Logistic_summary_function(Logistic_mainE,3)[,-1],
                     Logistic_summary_function(Logistic_interactE,3),by="term") %>% 
  arrange(sortid) %>% select(-sortid)
write_excel_csv(myresult,"Logistic_regression_table.csv")  

# 소득수준과 정치적 성향의 상호작용 효과 시각화 
mydata %>% summarize(sd(libcon7))
mygrid = mydata_center %>%
  data_grid(income06,libcon7=c(-1.43,0,1.43),fem=0.5,nonwhite=0.5,age=0,educ=0) %>% 
  mutate(pred=predict(Logistic_interactE,.,type="response"))
mygrid = mygrid %>% 
  mutate(
    libcon7=libcon7+mean(mydata$libcon7),
    income06=income06+mean(mydata$income06),
    libcon3=cut(libcon7,c(0,3,4.5,Inf),
                c("Liberal (M-SD)","Moderate (M)","Conservative (M+SD)"))
  )
mygrid %>% 
  ggplot(aes(x=income06,y=pred))+
  geom_line(aes(color=libcon3),size=1)+
  labs(x="Household income",y="probability to choose 'yes'",
       color="Political ideology")+
  coord_cartesian(ylim=c(0.3,0.9),xlim=c(0,26))+
  scale_x_continuous(breaks=c(1+5*(0:4)),
                     label=c("under\n$1000","$6,000-\n$6999",
                             "$15000-\n17499","$30000-\n$34999",
                             "$75000-\n$89999"))+
  ggtitle("Suicide, if incurable disease?")+
  theme(legend.position="bottom")

# 포아송 회귀분석(Poisson regression)
mydata = mydata %>% 
  mutate(
    sui4=rowSums(mydata[,1:4])
  )

# 포아송 분포가 아닐 수도 있지만 맞다고 가정해 봅시다
mydata %>% 
  ggplot(aes(x=sui4)) +
  geom_histogram(bins=5,fill="lightblue")+
  labs(x="Summed number of 'yes' for 4 conditions",
       y="Frequency")

# 각주: 데이터의 분포와 lambda=1인 경우 포아송 분포
P_Poisson=dpois(x=0:4, lambda=1) # density function for Poisson distribution
tmp=tibble(P_Poisson)
ggplot(mydata,aes(x=sui4,y=..density..)) +
  geom_histogram(bins=5,fill="lightblue")+
  geom_line(data=tmp,aes(x=0:4,y=P_Poisson),
            size=1,color='red')+
  labs(x="Summed number of 'yes' for 4 conditions",
       y="Probability")

# 중심화 변환 처리하였습니다. 
mydata_center = mydata %>% 
  mutate_at(
    vars(age,educ,income06,libcon7),
    funs(. - mean(.))
  )
# 포아송 회귀모형 추정 
Pois_mainE = glm(sui4~fem+age+I(age^2)+nonwhite+educ+income06+libcon7,
                 mydata_center,family=poisson(link='log'))
tidy(Pois_mainE)
glance(Pois_mainE)

# 상호작용효과를 추가로 고려한 모형
Pois_interactE = glm(sui4~fem+age+I(age^2)+nonwhite+educ+income06*libcon7,
                     mydata_center,family=poisson(link='log'))
# Logistic_summary_function() 을 그대로 사용해도 무방하겠죠. 
Poisson_summary_function = Logistic_summary_function
# 정리한 후 CSV 화일로 저장 후 약간의 편집을 적용하여 사용하면 됩니다
myresult = full_join(Poisson_summary_function(Pois_mainE,3)[,-1],
                     Poisson_summary_function(Pois_interactE,3),by="term") %>% 
  arrange(sortid) %>% select(-sortid)
write_excel_csv(myresult,"Poisson_regression_table.csv")  

# 소득수준과 정치적 성향의 상호작용 효과 시각화 
mygrid = mydata_center %>%
  data_grid(income06,libcon7=c(-1.43,0,1.43),
            fem=0.5,nonwhite=0.5,age=0,educ=0) %>% 
  mutate(pred=predict(Pois_interactE,.,type="response"))
mygrid = mygrid %>% 
  mutate(
    libcon7=libcon7+mean(mydata$libcon7),
    income06=income06+mean(mydata$income06),
    libcon3=cut(libcon7,c(0,3,4.5,Inf),
                c("Liberal (M-SD)","Moderate (M)","Conservative (M+SD)"))
  )
mygrid %>% 
  ggplot(aes(x=income06,y=pred))+
  geom_line(aes(color=libcon3),size=1)+
  labs(x="Household income",y="Summed score of 'yes'",
       color="Political ideology")+
  coord_cartesian(ylim=c(0.5,1.5),xlim=c(0,26))+
  scale_x_continuous(breaks=c(1+5*(0:4)),
                     label=c("under\n$1000","$6,000-\n$6999",
                             "$15000-\n17499","$30000-\n$34999",
                             "$75000-\n$89999"))+
  ggtitle("Suicide, if 1) incurable disease, 2) bankrupt?, 
          3) dishonored family, 4) tired of living")+
  theme(legend.position="bottom")
