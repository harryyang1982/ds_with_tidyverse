##############################################################################
## 기술통계분석 및 분석결과 시각화
# 대상변수가 명목변수인 경우
library('tidyverse')
library('readxl')
library('haven')
library('magrittr')
setwd("D:/TidyData/data")

# 데이터 불러오기 
data_131 = read_spss("data_TESS3_131.sav")
# 음수로 입력된 변수값은 결측값
mydata = data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0,NA,.))
  )

# 간단한 명목변수 분석: 성별 응답자 빈도
mydata %>% 
  count(PPGENDER)
# 변수에 라벨을 붙이면 시각화에 좋다. 
mydata = mydata %>% 
  mutate(
    female=labelled(PPGENDER,c(남성=1,여성=2))
  )
mydata %>% 
  count(as_factor(female))

# 시각화: 아랫줄은 "mydata %>% ggplot(aes(x=as_factor(female)))+"로 표현해도 동일
ggplot(mydata,aes(x=as_factor(female)))+
  geom_bar()
# 그래프를 보다 보기 좋게
mydata %>% ggplot(aes(x=as_factor(female)))+
  geom_bar()+
  labs(x="응답자의 성별",y="빈도수")+ #추가작업1
  coord_cartesian(ylim=c(280,320)) #추가작업2

# 명목변수의 수준이 많은 경우 빈도분석
mydata = mydata %>% 
  mutate(
    religion=as.character(as_factor(data_131$REL1)) #
  )
mydata %>% count(religion)
# 시각화: 복잡한 경우 빈도표를 저장하면 편리합니다
myresult = mydata %>% count(religion) 
ggplot(myresult,aes(x=religion,y=n))+
  geom_bar(stat="identity")+
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents")
# 일단 그래프를 뒤집어 보죠
ggplot(myresult,aes(x=religion,y=n))+
  geom_bar(stat="identity")+
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents")+
  coord_flip()
# 그래도 이상하니, Protestant~ 이름 다음의 괄호표현은 삭제하겠습니다. 
myresult = myresult %>% 
  mutate(
    religion=ifelse(str_detect(religion,"Protestant"),"Protestants",religion)
  ) 
ggplot(myresult,aes(x=religion,y=n))+
  geom_bar(stat="identity")+
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents")+
  coord_flip()
# 라벨은 정리가 되었지만 명목변수 수준을 빈도수에 따라 정렬하면 더 좋을 듯 합니다. 
myresult = myresult %>% 
  mutate(
    religion=fct_reorder(religion,n,"mean")
  ) 
ggplot(myresult,aes(x=religion,y=n))+
  geom_bar(stat="identity")+
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents")+
  coord_flip()

# 정치적 성향(IDEO)변수를 3집단으로 리코딩 
mydata = mydata %>% 
  mutate(
    libcon3=as.double(cut(IDEO,c(0,3,4,Inf),1:3)),
    libcon3=labelled(libcon3,c(진보=1,중도=2,보수=3))
  )
# libcon3과 female 변수의 교차표 
mydata %>% 
  count(as_factor(female),as_factor(libcon3))
# 만약 NA를 제외하고 성별X정치성향 형식의 교차표를 그린다면
myresult = mydata %>% 
  count(as_factor(female),as_factor(libcon3)) %>% 
  drop_na()
myresult %>% 
  spread(key=`as_factor(libcon3)`,value=n)
myresult
# 막대그래프로 시각화를 해보자. 
ggplot(myresult,aes(x=`as_factor(female)`,y=n,
                    fill=`as_factor(libcon3)`))+
  geom_bar(stat="identity")+
  labs(x="응답자의 성별",y="빈도수",fill="정치적 성향")

# 정치적 성향별 응답자를 쌓아두는 방식이 아니라 병렬시키는 방식
# 보다 보기 좋은 방식으로 시각화
ggplot(myresult,aes(x=`as_factor(female)`,y=n,
                    fill=`as_factor(libcon3)`))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="응답자의 성별",y="빈도수",fill="정치적 성향")

# 정치적 성향을 X축에, 성별을 범례로
ggplot(myresult,aes(x=`as_factor(libcon3)`,y=n,
                    fill=`as_factor(female)`))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="정치적 성향",y="빈도수",fill="응답자의 성별")

# 정치적 성향별 남녀 퍼센트 계산
myresult %>% 
  spread(key=`as_factor(libcon3)`,value=n) %>% 
  mutate_if(
    is.integer,
    funs(100*(./sum(.)))
  )

# 만약 성별 내부의 정치적 성향 응답자 비율을 표시하고자 한다면?
ggplot(myresult,aes(x=`as_factor(libcon3)`,y=n,
                    fill=`as_factor(female)`))+
  geom_bar(stat="identity",position="fill")+
  labs(x="정치적 성향",y="응답자 비율",fill="응답자의 성별")
# 그래프에 대한 추가작업
ggplot(myresult,aes(x=`as_factor(libcon3)`,y=n,
                    fill=`as_factor(female)`))+
  geom_bar(stat="identity",position="fill")+
  labs(x="정치적 성향",y="응답자 퍼센트",fill="응답자의 성별")+ 
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5),"%",sep=""))+ #추가작업1
  theme(legend.position="top")  #추가작업2

# 명목변수가 3개인 경우도 어렵지 않게 알 수 있습니다(그러나 해석은 까다로울 수도). 
mydata = mydata %>% 
  mutate(
    white=ifelse(PPETHM==1,1,0),
    white=labelled(white,c(다수인종=1,소수인종들=0))
  )
myresult = mydata %>% 
  count(as_factor(white),as_factor(female),as_factor(libcon3))
myresult
# 결측값 제거 후 넓은 형태 데이터 교차표로 정리
myresult %>% 
  drop_na() %>% 
  spread(key=`as_factor(libcon3)`,value=n)
# 인종변수 수준에 따라 정치적 성향집단별 남녀 응답자 퍼센트 
myresult %>% 
  drop_na() %>% 
  group_by(`as_factor(white)`) %>% 
  spread(key=`as_factor(libcon3)`,value=n) %>% 
  mutate_if(
    is.integer,
    funs(100*(./sum(.)))
  )
  
# 패시팅을 이용한 데이터 시각화 
myresult %>% drop_na() %>% 
ggplot(aes(x=`as_factor(female)`,y=n,fill=`as_factor(libcon3)`))+
  geom_bar(stat='identity',position="fill")+
  labs(x="응답자의 성별",y="응답자 퍼센트",fill="정치적 성향")+
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5),"%",sep=""))+
  theme(legend.position="top")+
  facet_grid(.~`as_factor(white)`)

# 패시팅을 하면 4개 이상의 명목변수들도 고려할 수 있겠지만, 
# 분석결과 해석이 복잡하기 때문에 개인적으로 권하지는 않습니다.
mydata = mydata %>% 
  mutate(
    gen2=as.double(cut(PPAGE,c(0,50,99),1:2)),
    gen2=labelled(gen2,
                  c(`저연령(50세 이하)`=1,`고연령(51세 이상`=2))
  )
myresult = mydata %>% 
  count(as_factor(gen2),as_factor(white),
        as_factor(female),as_factor(libcon3))
myresult %>% drop_na() %>% 
  ggplot(aes(x=`as_factor(female)`,y=n,fill=`as_factor(libcon3)`))+
  geom_bar(stat='identity',position="fill")+
  labs(x="응답자의 성별",y="응답자 퍼센트",fill="정치적 성향")+
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5),"%",sep=""))+
  theme(legend.position="top")+
  facet_grid(`as_factor(gen2)`~`as_factor(white)`)
ggsave("freq_gender_ideo_race_generation.jpeg",
       width=16,height=16,units="cm")

# summarize_*()/summarise_*() 함수
# 데이터 불러오기 및 사전처리 작업
data_131 = read_spss("data_TESS3_131.sav")
mydata = data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0,NA,.))
  )

# 한 개의 등간변수에 대한 기술통계치 구하기 
mydata %>% 
  summarize(mean(Q14,na.rm=T))
mydata %>% 
  summarize(mean_Q14=mean(Q14,na.rm=T))

# 막대그래프로 시각화
mydata %>% 
  summarize(mean_Q14=mean(Q14,na.rm=T)) %>% 
ggplot(aes(x="",y=mean_Q14))+
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(1,5))+ #5점척도이기 때문
  labs(x="Q14",y="Arithmatic mean")

# 등간변수의 분포를 시각화하는 경우: 박스플롯, 히스토그램, 빈도폴리곤
# 박스플롯으로 시각화 
g1=mydata %>% filter(!is.na(Q14)) %>% 
  ggplot(aes(x="",y=Q14))+
  geom_boxplot()+
  ggtitle("Box-Whisker plot")
# 히스토그램으로 시각화 
g2=mydata %>% filter(!is.na(Q14)) %>% 
ggplot(aes(x=Q14))+
  geom_histogram(bins=5)+
  ggtitle("Histogram")
# 빈도 폴리곤
g3=mydata %>% filter(!is.na(Q14)) %>% 
ggplot(aes(x=Q14))+
  geom_freqpoly(bins=5)+
  ggtitle("Frequency polygon")
gridExtra::grid.arrange(g1,g2,g3,nrow=1)

# 연령변수처럼 측정치가 치밀하면 훨씬 더 효과적
g1=mydata %>% 
  ggplot(aes(x="",y=PPAGE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(15,90))+
  labs(x="",y="Age")+
  ggtitle("Box-Whisker plot")
g2=mydata %>% 
  ggplot(aes(x=PPAGE))+
  geom_histogram(bins=30)+
  coord_cartesian(xlim=c(15,90))+
  scale_x_continuous(breaks=c(15,10*(2:9)))+
  labs(x="Age",y="Frequency")+
  ggtitle("Histogram")
g3=mydata %>% 
  ggplot(aes(x=PPAGE))+
  geom_freqpoly(bins=30)+
  coord_cartesian(xlim=c(15,90))+
  scale_x_continuous(breaks=c(15,10*(2:9)))+
  labs(x="Age",y="Frequency")+
  ggtitle("Frequency polygon")
gridExtra::grid.arrange(g1,g2,g3,nrow=1)

# summarize() 함수: 간단사
mydata %>% 
  summarize(mean_Q14=mean(Q14,na.rm=T),
            sd_Q14=sd(Q14,na.rm=T),
            min_Q14=min(Q14,na.rm=T),
            max_Q14=max(Q14,na.rm=T))

# 타이디데이터 접근법의 위력은 대용량의 데이터죠. 
# 여러 변수들(Q14:Q18)의 평균값들을 구해봅시다. 
myresult = mydata %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(mean(.,na.rm=T))
  )
myresult

# 시각화
myresult = myresult %>% 
  gather(key=Qs,value=Ms) 
myresult %>% 
ggplot(aes(x=Qs,y=Ms))+
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(2.5,5))+
  labs(x="Statements",y="Average (5 = more important)")

# 보다 효과적인 시각화
myresult = myresult %>% 
  mutate(
    mylbls=as.double(as.factor(Qs)),
    mylbls=labelled(mylbls,c(`Freedom of speech`=1,
                         `Right to listen`=2,
                         `Risk of violence`=3,
                         `Freedom to speak/hear`=4,
                         `Safety & security`=5)),
    mylbls=fct_reorder(as_factor(mylbls),Ms,"mean")
    )
  
myresult %>% 
ggplot(aes(x=mylbls,y=Ms))+
  geom_bar(stat="identity")+
  coord_flip(ylim=c(2.5,5))+
  labs(x="Statements",y="Average (5 = more important)")

# 성별에 따라 Q14:Q18의 평균값들은 어떤 차이?
mydata = mydata %>% 
  mutate(
    female=labelled(PPGENDER,c(male=1,female=2)),
    female=as_factor(female)
  ) 
myresult = mydata %>% 
  group_by(female) %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(mean(.,na.rm=T))
  )
myresult

# 시각화 
myresult = myresult %>% 
  gather(key=Qs,value=Ms,-female) %>% 
  mutate(
    mylbls=as.double(as.factor(Qs)),
    mylbls=labelled(mylbls,c(`Freedom of speech`=1,
                             `Right to listen`=2,
                             `Risk of violence`=3,
                             `Freedom to speak/hear`=4,
                             `Safety & security`=5)),
    mylbls=fct_reorder(as_factor(mylbls),Ms,"mean")
  )
myresult %>% 
  ggplot(aes(x=mylbls,y=Ms,fill=female))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip(ylim=c(2.5,5))+
  labs(x="Statements",
       y="Average (5 = more important)",
       fill="Gender")+
  theme(legend.position="top")

# 정치적 성향 집단변수를 추가로 투입
mydata = mydata %>% 
  mutate(
    libcon3=as.double(cut(IDEO,c(0,3,4,Inf),1:3)),
    libcon3=labelled(libcon3,c(liberal=1,moderate=2,conservative=3)),
    libcon3=as_factor(libcon3)
  )
myresult = mydata %>% 
  group_by(libcon3,female) %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(mean(.,na.rm=T))
  )
myresult
# 시각화 
myresult = myresult %>% 
  drop_na() %>% 
  ungroup() %>% 
  gather(key=Qs,value=Ms,-libcon3,-female) %>% 
  mutate(
    mylbls=as.double(as.factor(Qs)),
    mylbls=labelled(mylbls,c(`Freedom of speech`=1,
                             `Right to listen`=2,
                             `Risk of violence`=3,
                             `Freedom to speak/hear`=4,
                             `Safety & security`=5)),
    mylbls=fct_reorder(as_factor(mylbls),Ms,"mean")
  )
myresult %>% 
  ggplot(aes(x=mylbls,y=Ms,fill=libcon3))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip(ylim=c(2.5,5))+
  labs(x="Statements",
       y="Average (5 = more important)",
       fill="Political ideology")+
  facet_wrap(~female,nrow=2)
ggsave("average_Qtype_female_ideo.jpeg",width=16,height=20,unit='cm')


# 개인함수를 이용하여 효율적으로 기술통계분석하기
MySummarize_mean_and_SD=function(myvariable){
  myMEAN=round(mean(myvariable,na.rm=T),2) #1단계
  mySD=round(sd(myvariable,na.rm=T),2) #2단계
  mySD2=str_c(" (",mySD,")",sep="") #3단계
  STAT_I_WANT=str_c(myMEAN,mySD2,sep="") #4단계
  STAT_I_WANT
}
myresult = mydata %>% 
  group_by(libcon3) %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(MySummarize_mean_and_SD)
  )
myresult

# 결과가 좀더 좋게 표현되도록 함수를 수정해 보죠
MySummarize_mean_and_SD=function(myvariable,mydigit){
  str_c(
    format(round(mean(myvariable,na.rm=T),mydigit),nsmall=mydigit),
    "\n(",
    format(round(sd(myvariable,na.rm=T),mydigit),nsmall=mydigit),
    ")",sep=""
  )
}
myresult = mydata %>% 
  group_by(libcon3) %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(MySummarize_mean_and_SD(.,3))
  )
myresult
# 이 결과는 CSV 버전으로 보낸 후 다소의 편집을 거친 후 사용하세요
write_excel_csv(myresult,"temporary_table.csv")

# 신뢰구간(CI) 계산을 위한 개인함수
Confidence_Interval_calculation=function(myvariable,myproportion){
  tmp=summary(lm(myvariable~1))
  my_se=tmp$coef[2] #표준오차: 표준편차를 자유도로 나누어 준 것
  my_df=tmp$df[2]  #자유도
  myP=1-(0.5*(1-myproportion))  #t값 계산을 위한 확률계산 
  my_ci=qt(myP,my_df)*my_se  # ci 계산
  my_ci
}
# 평균
myresult_M = mydata %>% 
  group_by(female) %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(mean(.,na.rm=T))
  ) %>%
  gather(key=Qs,value=mn,-female)
# 신뢰구간
myresult_ci = mydata %>% 
  group_by(female) %>% 
  summarize_at(
    vars(Q14:Q18),
    funs(Confidence_Interval_calculation(.,0.90))
  ) %>%
  gather(key=Qs,value=ci,-female)
# 평균과 신뢰구간 데이터 합침
myresult=full_join(myresult_M,myresult_ci,by=c("female","Qs"))
myresult

# 시각화 
myresult = myresult %>% 
  mutate(
    mylbls=as.double(as.factor(Qs)),
    mylbls=labelled(mylbls,c(`Freedom of speech`=1,
                             `Right to listen`=2,
                             `Risk of violence`=3,
                             `Freedom to speak/hear`=4,
                             `Safety & security`=5)),
    mylbls=fct_reorder(as_factor(mylbls),mn,"mean")
  )
# 평균을 점으로, 90% CI를 덧붙임
myresult %>% 
ggplot(aes(x=mylbls,y=mn,color=female))+
  geom_point(position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=mn-ci,ymax=mn+ci),
                width=0.3,position=position_dodge(width=0.5))+
  labs(x="Statements",y="Average (5 = more important)",
       color="Gender")+
  coord_flip()+
  theme(legend.position="top")
# 평균을 막대로, 90% CI를 덧붙임
myresult %>% 
ggplot(aes(x=mylbls,y=mn,fill=female))+
  geom_bar(stat="identity",position=position_dodge(width=1))+
  geom_errorbar(aes(ymin=mn-ci,ymax=mn+ci),
                width=0.3,position=position_dodge(width=1))+
  labs(x="Statements",y="Average (5 = more important)",
       fill="Gender")+
  coord_flip(ylim=c(2.8,4.8))+
  theme(legend.position="top")

# 박스플롯
seoul_educ = read_xls("data_student_class.xls",skip=2)
# 데이터 정리
mydata = seoul_educ %>% 
  filter(지역!="합계") %>% 
  select(기간,지역,starts_with("학급당"))
names(mydata)=c("year","district",str_c("stdt_per_clss",1:4,sep="_"))
mydata

# 긴형태 데이터로 정리
mydata = mydata %>% 
  gather(key=type,value=stdn_per_clss,-year,-district) %>% 
  mutate(
    type=as.double(as.factor(type)),
    type=labelled(type,
                  c(유치원=1,초등학교=2,중학교=3,고등학교=4)),
    type=as_factor(type)
  )
mydata

# 일단 유치원만을 대상으로 살펴보자.     
mydata %>% filter(type=='유치원') %>% 
ggplot(aes(x=year,y=stdn_per_clss))+
  geom_boxplot()+
  labs(x="년도",y="유치원: 학급당 원아수")
# 유치원, 초/중/고교까지 다 살펴보자
mydata %>% 
  ggplot(aes(x=year,y=stdn_per_clss,fill=type))+
  geom_boxplot(position=position_dodge(width=1.2))+
  labs(x="년도",y="학급당 원아수/학생수",fill="교육기관")+
  theme(legend.position="top")
# 유치원, 초/중/고교까지 다 살펴보자(패시팅 이용)
mydata %>% 
  ggplot(aes(x=year,y=stdn_per_clss))+
  geom_boxplot(fill="grey70")+
  labs(x="년도",y="학급당 원아수/학생수")+
  facet_wrap(~type,ncol=1)

