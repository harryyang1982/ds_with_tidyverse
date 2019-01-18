##############################################################################
## 일반선형모형: 모형추정 및 추정결과 시각화
library('tidyverse')
library('readxl')
library('haven')
library('broom')  # tidy(), glance() 함수 
setwd("D:/TidyData/data")

### 상관계수, 티테스트, 카이제곱분석
## 상관계수
# 데이터 불러오기 
seoul_library = read_xls("data_library.xls")
seoul_pop = read_xls("data_population.xls")

# 데이터 사전처리: '합계'표시 사례 제거, - 표시를 0으로 전환
mydata1 = seoul_library %>% 
  filter(자치구!="합계") %>% 
  mutate_at(
    vars(국립도서관,대학도서관,전문도서관),
    funs(as.double(str_replace(.,"-","0")))
  )
# 변수이름 교체 
names(mydata1)=c("year","district",
                 str_c("lib_",c("tot","nat","pub","uni","spe"),sep=""))
mydata1 %>% print(n=2)
# 데이터 사전처리: '합계'표시 사례 제거, 한국인만 포함, - 표시를 0으로 전환
mydata2 = seoul_pop %>% 
  filter(구분!="합계"&구분__1=="한국인") %>% 
  mutate(
    계=as.double(str_replace_all(계,",",""))
  ) %>% 
  mutate_at(
    vars(4:25),
    funs(as.double(str_replace(.,"-","0")))
  ) %>% 
  select(-구분__1)
# 변수이름 정리 
names(mydata2)=c("year","district","total",
                 str_c("gen_",names(mydata2)[4:24],sep=""))  
names(mydata2)=str_replace(names(mydata2)," 이상\\+","_")
names(mydata2)=str_replace(names(mydata2),"세","")
names(mydata2)=str_replace(names(mydata2),"~","_")
mydata2 %>% print(n=2)
# 두 데이터를 합치고 2016년 자료만 남기자 
mydata = full_join(mydata1,mydata2,by=c("year","district")) %>% 
  filter(year==2016)
mydata %>% print(n=2)

# 전체도서관 수와 거주민 총수의 관계는?
mydata %>% 
ggplot(aes(x=total,y=lib_tot))+
  geom_point()+
  labs(x="거주민 총수",y="도서관 총수")

# 시각적 설득력 향상시킨 결과 
mydata %>% 
ggplot(aes(x=(total/100000),y=lib_tot))+
  geom_point()+
  scale_x_continuous(breaks=0:7,labels=str_c(10*(0:7),"만명",sep=""))+
  coord_cartesian(ylim=c(0,60),xlim=c(0.5,6.5))+
  labs(x="거주민 총수",y="도서관 총수")

# 점이 아니라 지역구의 이름을 표기
mydata = mydata %>% 
  mutate(
    district2=str_sub(district,1,2)
  ) 
mydata %>%  
ggplot(aes(x=(total/100000),y=lib_tot,label=district2))+
  geom_text()+
  scale_x_continuous(breaks=0:7,labels=str_c(10*(0:7),"만명",sep=""))+
  coord_cartesian(ylim=c(0,60),xlim=c(0.5,6.5))+
  labs(x="거주민 총수",y="도서관 총수")

# 도서관 유형별로 어떻게 다를까? 
mydata_long1 = mydata %>% 
  gather(key=lib_type,value=number,-(total:gen_100_),
         -year,-district,-district2)

mydata_long1 %>% filter(lib_type!="lib_tot") %>% 
ggplot(aes(x=(total/100000),y=number,label=district2))+
  geom_text()+
  scale_x_continuous(breaks=0:7,labels=str_c(10*(0:7),"만명",sep=""))+
  coord_cartesian(xlim=c(0.5,6.5))+
  labs(x="거주민 총수",y="도서관 총수")+
  facet_wrap(~lib_type,ncol=2,labeller = c(""=""))

# 도서관 유형별 도서관수를 0-1로 표준화 
mydata_long1 %>% filter(lib_type!="lib_tot") %>% 
  group_by(lib_type) %>% 
  mutate(
    number2=(number-min(number))/(max(number)-min(number))
  ) %>% 
ggplot(aes(x=(total/100000),y=number2,label=district2))+
  geom_text()+
  scale_x_continuous(breaks=0:7,labels=str_c(10*(0:7),"만명",sep=""))+
  coord_cartesian(xlim=c(0.5,6.5))+
  labs(x="거주민 총수",y="도서관 총수(0-1의 값으로 재조정)")+
  facet_wrap(~lib_type,
             ncol=2,
             labeller=as_labeller(c("lib_nat" = "국립도서관",
                                    "lib_pub" = "공공도서관",
                                    "lib_spe" = "특별도서관",
                                    "lib_uni" = "대학도서관")))

# 공공도서관수와 거주민수
mydata_long1 %>% 
  filter(lib_type=="lib_pub") %>% 
  cor.test(~ total+number,data=.)
# 특별도서관수와 거주민수
mydata_long1 %>% 
  filter(lib_type=="lib_spe") %>% 
  cor.test(~ total+number,data=.)
# 대학도서관수와 거주민수
mydata_long1 %>% 
  filter(lib_type=="lib_uni") %>% 
  cor.test(~ total+number,data=.)

# %$% 오퍼레이터 사용
mydata_long1 %>% 
  filter(lib_type=="lib_pub") %$% 
  cor.test(total,number)

# 네 유형의 도서관에 대해 모두 적용할 경우
mydata_long1 %>% 
  filter(lib_type!="lib_tot") %>% 
  split(.$lib_type) %>% 
  map(~ cor.test(~ total+number,data=.x))

# 위의 결과보다 좀 더 깔끔하게 정리된 데이터를 원한다면...
myresult = mydata_long1 %>% 
  filter(lib_type!="lib_tot") %>% 
  split(.$lib_type) %>% 
  map(~ cor.test(~ total+number,data=.x)) %>% 
  map_dfr(~ tidy(.))
myresult 

# 다음과 같이 정리하면 훨씬 더 간결해 집니다. 
myresult %>% 
  mutate(
    Pearson_r=format(round(estimate,3),3),
    p_value=format(round(p.value,3),3),
    what_i_want=str_c("r(",parameter,") = ",Pearson_r,", p = ",p_value)
  ) %>% select(what_i_want)

# 통계적 유의도 표시 기호를 붙이는 개인함수를 만들어 볼까요? 
Correlation_statistics_summary=function(my_cor_result,mydigit){
  my_cor_result %>% mutate(
    Pearson_r=format(round(estimate,mydigit),mydigit),
    mystar=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),right=FALSE,
               c("***","** ","*  ","+  ","   ")),
    r_star=str_c(Pearson_r,mystar,sep="")
  ) %>% select(r_star)
}
Correlation_statistics_summary(myresult,3)

# 상관계수의 95% 신뢰구간을 그래프로 표시할 수도 있습니다. 
myresult %>% 
ggplot(aes(x=1:4))+ 
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),
                width=0.2,color='blue')+
  geom_hline(yintercept=0,linetype=2,color='red')+
  scale_x_continuous(breaks=1:4,label=c("국립도서관","공공도서관",
                            "특별도서관","대학도서관"))+
  coord_cartesian(ylim=c(-1,1),xlim=c(0.5,4.5))+
  labs(x="도서관 유형",y="거주민수와 도서관수의 피어슨 상관계수")

# 기간이 공존하는 경우만 살펴봅시다. 
mydata = inner_join(mydata1,mydata2,by=c("year","district"))
# 긴형태로 전환 후, 전체도서관수는 삭제, 
# 그리고 년도와 도서관유형 변수는 합침
mylong = mydata %>% 
  gather(key=lib_type,value=number,-(total:gen_100_),
         -year,-district) %>% 
  filter(lib_type != "lib_tot") %>% 
  unite(year_lib_type,c("year","lib_type"),sep="-")
# 년도와 도서관 유형 변수의 수준별로 상관계수 구함
myresult = mylong %>% 
  split(.$year_lib_type) %>% 
  map(~ cor.test(~ total+number,data=.x)) %>% 
  map_dfr(~ tidy(.))
myresult
# 그림용 데이터(95% CI, 상한과 하한) 
myfigure=myresult %>% 
  mutate(
    year=rep(2014:2016,each=4),
    lib_type=rep(c("nat","pub","spe","uni"),3)
  ) %>% 
  select(year,lib_type,starts_with("conf.")) 
# 시각화
myfigure %>% 
ggplot(aes(x=lib_type))+ 
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),
                width=0.2,color='blue')+
  geom_hline(yintercept=0,linetype=2,color='red')+
  scale_x_discrete(breaks=c("nat","pub","spe","uni"),
                   label=c("국립도서관","공공도서관",
                           "특별도서관","대학도서관"))+
  coord_cartesian(ylim=c(-1,1),xlim=c(0.5,4.5))+
  labs(x="도서관 유형",y="거주민수와 도서관수의 피어슨 상관계수")+
  facet_wrap(~year)

## 티테스트 
data_131 = read_spss("data_TESS3_131.sav")
# 음수로 입력된 변수값은 결측값
mydata = data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0,NA,.))
  )
# 실험집단 변수에 라벨 작업 
mydata = mydata %>% 
  mutate(
    treat1=labelled(STUDY1_ASSIGN,
                    c(treatment=1,control=2))
  )
# 평균과 표준편차는?
mydata %>% 
  group_by(as_factor(treat1)) %>% 
  summarize(mean(Q2,na.rm=T),
            sd(Q2,na.rm=T))

# 신뢰구간(CI) 계산을 위한 개인함수
Confidence_Interval_calculation=function(myvariable,myproportion){
  tmp=summary(lm(myvariable~1))
  my_se=tmp$coef[2] #표준오차: 표준편차를 자유도로 나누어 준 것
  my_df=tmp$df[2]  #자유도
  myP=1-(0.5*(1-myproportion))  #t값 계산을 위한 확률계산 
  my_ci=qt(myP,my_df)*my_se  # ci 계산
  my_ci
}

# 평균과 95% CI를 표시해 봅시다. 
myresult = mydata %>% 
  group_by(as_factor(treat1)) %>% 
  summarize(q2_m=mean(Q2,na.rm=T),
            q2_ci=Confidence_Interval_calculation(Q2,0.95))
myresult %>% 
  ggplot(aes(x=`as_factor(treat1)`,y=q2_m))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=q2_m-q2_ci,ymax=q2_m+q2_ci),width=0.05)+
  labs(x="Treatment, Study 1",
       y="Support for student loan forgivness program\n(7, stronger support)")+
  coord_cartesian(ylim=c(2.5,4.5))

# 독립표본(independent sample)[혹은 이표본(two sample)] t-test 
# treat1 변수 수준별 Q2 변수 평균비교
# R 베이스의 경우: t.test(Q2~treat1,data=mydata)
mydata %>% t.test(Q2~treat1,data=.)

# 출력결과 정리 
myresult = mydata %>% t.test(Q2~treat1,data=.) %>% 
  tidy(.) 
myresult

# 원하는 형태로
ttest_Interval_calculation=function(my_ttest_result,mydigit){    
  my_ttest_result %>% mutate(
    t_stat=format(round(statistic,mydigit),mydigit),
    mystar=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),right=FALSE,
               c("***","** ","*  ","+  ","   ")),
    report=str_c("t(",format(round(parameter,0),0),") = ",
          t_stat,mystar)
  ) %>% select(report)
} 
ttest_Interval_calculation(myresult,3)

# 성별 변수를 생성한 후, 성별 변수와 Q14-Q18 변수들을 선별하였습니다
mysubdata = mydata %>% 
  mutate(
    female=labelled(PPGENDER,c(male=1,female=2)),
    female=as_factor(female)
  ) %>% 
  select(female,Q14:Q18) %>% 
  gather(key=Qs,value=y,-female)

# 문항별로 각각 티테스트를 실시후 결과를 저장합니다. 
myresult = mysubdata %>% 
  split(.$Qs) %>%  # 문항별로 분리 
  map(~ t.test(y~female,data=.x)) %>%  # 티테스트 결과 저장 
  map_dfr(~ tidy(.))  
myresult 

# 앞서 설정했던 ttest_Interval_calculation 함수를 이용해 결과를 정리합니다. 
ttest_Interval_calculation(myresult,3) %>% 
  mutate(Qs=str_c("Q",14:18)) # 문항을 붙여 어떤 결과인지 알 수 있도록

# 평균과 95% CI를 그린 후, 성별에 따라 통계적으로 유의미한 차이가 나타나는 문항과
# 그렇지 않은 문항을 구분하여 시각화 
myfigure = mysubdata %>% 
  group_by(female,Qs) %>% 
  summarize(y_mn=mean(y,na.rm=T),
            y_ci=Confidence_Interval_calculation(y,0.95)) %>% 
  mutate(
    significance=ifelse(Qs=="Q14"|Qs=="Q17",
                        "significant","insignificant")
  )
myfigure %>% 
  ggplot(aes(x=Qs,y=y_mn,shape=female))+
  geom_point(stat='identity',size=3,
             position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=y_mn-y_ci,ymax=y_mn+y_ci,color=significance),
                width=0.2,size=1,position=position_dodge(width=0.5))+
  labs(x="Statements",y="Average (5 = more important)",
       shape="Gender",color="Significantly different?")+
  scale_x_discrete(breaks=str_c("Q",14:18),
                   labels=c("Freedom of speech",
                            "Right to listen",
                            "Risk of violence",
                            "Freedom to speak/hear",
                            "Safety & security"))+
  coord_cartesian(ylim=c(2.5,5))+
  theme(legend.position="top")

## 대응표본(paired sample)[혹은 종속표본(dependent sample)] t-test
# R 베이스의 경우 
t.test(mydata$Q14,mydata$Q15,paired=T)
# %$% 오퍼레이터 사용(%$% 다음에는 데이터가 아닌 변수만 지정)
mydata %$%  # 오퍼레이터 바뀐 것 주의
  t.test(Q14,Q15,paired=TRUE) %>% 
  tidy(.)
# 그러나 타이디데이터 접근에서는 긴형태 데이터를 선호하기 때문에,
# 대응표본 티테스트의 경우 사용이 까다로운 편입니다. 
mydata_complete = mydata %>%
  mutate(id=row_number()) %>% 
  select(id,Q14:Q15) %>% 
  drop_na() 
mydata_complete %>% 
  gather(key=Qs,value=y,-id) %>% 
  t.test(y~Qs,.,paired=T) %>% 
  tidy(.) 

mydata_complete %>% 
  gather(key=Qs,value=y,-id) %>% 
  arrange(Qs,y) %>% # 이부분이 추가되어 결과가 달라집니다.
  t.test(y~Qs,.,paired=T) %>% 
  tidy(.) 

mydata_complete %>% 
  gather(key=Qs,value=y,-id) %>% 
  arrange(Qs,y) %>% 
  arrange(Qs,id) %>% # 아이디 변수를 이용하면 좋음
  t.test(y~Qs,.,paired=T) %>% 
  tidy(.) 

# 본문에는 등장하지 않는 부분 
# 여러 변수들에 대해 대응표본 티테스트를 수행하려면 
# 조금 복잡한 프로그래밍이 필요합니다. 
# 대상 변수들을 지정한 후 리스트제거 방식으로 결측값을 없앱니다. 
mydata_complete = mydata %>%
  select(Q14:Q18) %>% 
  drop_na() 
# 결과표를 다음과 같이 먼저 만들어 두고 
myresult = as_tibble(matrix(NA,nrow=25,ncol=10))
myresult[,1] = rep(1:5,5)
myresult[,2] = rep(1:5,each=5)
names(myresult)[1:2] = c("I","J")
# for 구문을 이용해 반복계산을 실시합니다
for (i in 1:5) {
  for (j in 1:5) {
    myPAIREDresult = t.test(unlist(mydata_complete[,i]),
           unlist(mydata_complete[,j]),
           paired=T) %>% 
      tidy(.) 
    myresult[myresult$I==i&myresult$J==j,3:10]=myPAIREDresult
    names(myresult)[3:10] = names(myPAIREDresult)
  }
}
# 다음과 같이 정리된 데이터를 얻을 수 있습니다.
myresult %>% 
  select(estimate:alternative) %>% 
  ttest_Interval_calculation(.,3) %>% 
  mutate(I=myresult$I,J=myresult$J) %>% 
  spread(key=I,value=report)


## 카이제곱 분석
data_131 = read_spss("data_TESS3_131.sav")
# 음수로 입력된 변수값은 결측값
mydata = data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0,NA,.))
  ) %>% 
  mutate(
    libcon3=cut(IDEO,c(0,3,4,7),
                c("liberal","moderate","conservative")),
    female=as_factor(labelled(PPGENDER,
                              c(male=1,female=2))),
    white=ifelse(PPETHM==1,1,2),
    white=as_factor(labelled(white,c(white=1,nonwhite=2)))
  ) %>% select(libcon3,female,white) %>% 
  drop_na() 

# 빈도표 계산
mydata %>% 
  count(white,female,libcon3) %>% 
  spread(key=libcon3,value=n)

# R 베이스를 이용한 카이제곱분석 
mytable=table(mydata$female,mydata$libcon3)
mytable
chisq.test(mytable)

chisq.test(mytable) %>% tidy(.)

# 2개의 범주형 변수들인 경우 
mydata %>% 
  count(female,libcon3) %>% 
  xtabs(n~ female+libcon3, data=.) %>% 
  chisq.test() %>% 
  tidy(.)

# 특정 범주형 변수의 수준에 따라 다른 범주형 변수들 사이의 카이제곱분석
# 이를테면 만약 성별로 인종과 정치적 성향집단의 상관관계를 확인한다면?
myresult = my_long %>% 
  split(.$female) %>% 
  map(~ xtabs(n ~ libcon3+white, data=.x)) %>% 
  map(~ chisq.test(.)) %>% 
  map_dfr(~ tidy(.))
myresult

# 추가로 편집하면 
myresult %>% 
  mutate(
    chi2=format(round(statistic,3),3),
    mystar=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),right=FALSE,
               c("***","** ","*  ","+  ","   ")),
    report=str_c("CHI2(",parameter,") = ",
                 chi2,mystar)
  ) %>% select(report)



  
