# 데이터 불러오기 
library('tidyverse')
library('haven')
library('broom')
library('modelr')
setwd("D:/TidyData/data")

# 카이제곱_문제_1
gss_panel = read_dta("data_gss_panel06.dta")
mydata = gss_panel %>% 
  select(hrs1_1,sex_1,age_1,educ_1,income06_1,race_1)
names(mydata)=str_replace(names(mydata),"_1","")
mydata = mydata %>% 
  mutate(
    fem=ifelse(sex==1,0,1),
    fem=labelled(fem,c(Men=0,Women=1)),
    nonwhite=ifelse(race==1,0,1),
    nonwhite=labelled(nonwhite,c(White=0,Nonwhite=1))
  ) %>% drop_na()

mycentering = mydata %>% 
  mutate_at(
    vars(age,educ,income06),
    funs(. - mean(.))
  )

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
      funs(round(.,mydigit))
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
  mytable=bind_rows(mycoefs,myGOF) %>%
    mutate(sortid=row_number()) %>% 
    select(sortid,term,report) %>%
    mutate_at(vars(2:3),funs(as.character(.)))
  mytable
}

# OLS 회귀모형 추정 
myOLS1=lm(hrs1 ~ fem+nonwhite+age+educ+income06,mycentering)
myOLS2=lm(hrs1 ~ fem+nonwhite+age+I(age^2)+educ+I(educ^2)+income06+I(income06^2),mycentering)
myOLS=lm(hrs1 ~ fem+nonwhite+age+I(age^2)+nonwhite*(educ+I(educ^2))+income06+I(income06^2),mycentering)

myresult = full_join(OLS_summary_function(myOLS,2),
                     OLS_summary_function(myOLS2,2)[,-1],
                     by="term") %>% 
  full_join(OLS_summary_function(myOLS1,2)[,-1],
            by="term") %>% 
  arrange(sortid) %>% select(-sortid) %>% 
  select(term,report,report.y,report.x)
write_excel_csv(myresult,"practice_OLS_reg.csv")

# 연령과 노동시간의 곡선관계 시각화 
mypred1 = mycentering %>% 
  data_grid(age,nonwhite=0,fem=0,educ=0,income06=0) %>%
  add_predictions(myOLS) %>% 
  mutate(
    age=age+mean(mydata$age)
  )
ggplot(data=mypred1,aes(x=age,y=pred))+
  geom_line()+
  geom_point(data=mydata,aes(x=age,y=hrs1),alpha=0.1)+
  labs(x="age, years",
       y="Working hours last week")+
  scale_x_continuous(breaks=10*(2:8))+
  coord_cartesian(ylim=c(0,90))

# 교육수준과 노동시간의 관계는 인종에 따라 어떻게 바뀌는지 시각화
mypred2 = mycentering %>% 
  data_grid(educ,nonwhite,fem=0,age=0,income06=0) %>%
  add_predictions(myOLS) %>% 
  mutate(
    educ=educ+mean(mydata$educ)
  )

ggplot(data=mypred2,aes(x=educ,y=pred))+
  geom_line()+
  geom_point(data=mydata,aes(x=educ,y=hrs1),alpha=0.1)+
  labs(x="Education, years",
       y="Working hours last week")+
  coord_cartesian(ylim=c(0,90))+
  facet_wrap(~nonwhite,
             labeller=as_labeller(c("0"="White","1"="Non-white")))
