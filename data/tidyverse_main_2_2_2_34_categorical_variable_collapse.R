###############################################################
# 명목변수(여러수준) -> 명목변수(간단한 수준)
# ifelse() 함수를 이용해 3개 집단으로 리코딩하는 경우
data_131 %>% 
  mutate(
    libcon3=ifelse(IDEO==-1,NA,
                   ifelse(IDEO <= 2,1,
                          ifelse(IDEO >= 6, 3, 2)))
  ) %>% 
  count(IDEO,libcon3)

# 각주: 라벨을 붙이는 경우 
mylabels = c(진보=1,중도=2,보수=3) #라벨정의 
data_131 %>% 
  mutate(
    libcon3=ifelse(IDEO==-1,NA,
                   ifelse(IDEO <= 2,1,
                          ifelse(IDEO >= 6, 3, 2))),
    libcon3=labelled(libcon3,mylabels) #라벨을 붙임
  ) %>% 
  count(as_factor(IDEO),as_factor(libcon3))

# 명목변수의 수준들을 묶어 보다 간단한 명목변수로 리코딩 
data_131 %>% 
  mutate(
    libcon3=cut(IDEO,c(-Inf,0,2,5,Inf),c(NA,1:3))
  ) %>% 
  count(IDEO,libcon3)

# 텍스트로 입력된 IDEO 변수를 임의로 생성 
data_131 = data_131 %>% 
  mutate(
    IDEO2=as.character(as_factor(IDEO))
  ) 
data_131 %>% 
  count(IDEO2)

# fct_collapse() 함수를 이용하여 명목변수 리코딩 
data_131 %>% 
  mutate(
    # 결측값 처리 
    libcon3=ifelse(IDEO2=='Refused',NA,IDEO2),
    # 3개 집단으로 구분
    libcon3=fct_collapse(libcon3,
                         '진보' = c('Liberal','Extremely liberal'),
                         '중도' = c('Moderate, middle of the road',
                                  'Slightly liberal','Slightly conservative'),
                         '보수' = c('Conservative','Extremely conservative'))
  ) %>% 
  count(libcon3)

# 각주: 수치형으로 입력된 경우에도 fct_collapse()를 이용할 수는 있지만, 
# 수치를 텍스트로 처리해야 한다. 
data_131 %>% 
  mutate(
    libcon3=ifelse(IDEO==-1,NA,IDEO),
    libcon3=fct_collapse(as.character(libcon3),
                         '진보' = as.character(1:2),
                         '중도' = as.character(3:5),
                         '보수' = as.character(6:7))
  ) %>% 
  count(libcon3)

# 종교 변수
data_131 %>% 
  count(as_factor(REL1)) %>% 
  arrange(desc(n))

# 종교 변수와 같이 복잡하게 구성된 변수는 리코딩이 조금 고될 수 있다
data_131 %>% 
  mutate(
    # 연습을 위해 텍스트 형태로 변환 
    religion5=as_factor(REL1),
    religion5=fct_collapse(religion5,
                           가톨릭="Catholic",
                           프로테스탄트="Protestant (e.g., Methodist, Lutheran, Presbyterian, Episcopal)",
                           무종교="None",
                           침례교="Baptist-any denomination",
                           기타종교=c("Other Christian",
                                  "Pentecostal",
                                  "Jewish",
                                  "Mormon",
                                  "Other non-Christian",
                                  "Hindu",
                                  "Muslim",
                                  "Buddhist",
                                  "Eastern Orthodox")
    ),
    # 결측값 처리 
    religion5=ifelse(religion5=="Refused",NA,as.character(religion5))
  ) %>% 
  count(religion5)

# 기타종교를 기준으로 나머지를 변환시키면 편함
data_131 %>% 
  mutate(
    religion5="기타종교",
    religion5=ifelse(as_factor(REL1)=="Refused",NA,religion5),
    religion5=ifelse(as_factor(REL1)=="Baptist-any denomination","침례교",religion5),
    religion5=ifelse(
      as_factor(REL1)=="Protestant (e.g., Methodist, Lutheran, Presbyterian, Episcopal)",
      "프로테스탄트",religion5),
    religion5=ifelse(as_factor(REL1)=="Catholic","가톨릭",religion5),
    religion5=ifelse(as_factor(REL1)=="None","무종교",religion5)
  ) %>% 
  count(religion5)

# 번호에 대해서 분석자가 완전히 알고 있다면, 다음이 편할 수도 있다.
data_131 %>% 
  mutate(
    religion5=ifelse(REL1==4|REL1==5|REL1==6|REL1==7|REL1==8|
                       REL1==9|REL1==10|REL1==11|REL1==12,14,REL1),
    religion5=ifelse(REL1==-1,NA,religion5),
    # 아래는 라벨을 붙이는 작업(나중에 보다 자세히 설명)
    religion5=labelled(religion5,
                       c(침례교=1,프로테스탄트=2,가톨릭=3,무종교=13,기타종교=14))
  ) %>% 
  count(as_factor(religion5))

# fct_lump() 함수를 쓰면 매우 쉽다: 상위 n개와 나머지 집단(즉 총 n+1개 집단)
data_131 %>% 
  mutate(
    # 문자형으로 변환 
    religion5=as_factor(REL1),
    # 상위 4개와 나머지 1개 집단으로 구분
    religion5=fct_lump(religion5,n=4),
    # 결측값 전환
    religion5=ifelse(REL1==-1,NA,as.character(religion5)),
    # 첫단어만 선택(텍스트 변환에 대해서는 나중에 보다 자세하게 설명)
    religion5=str_extract(religion5,"[[:alpha:]]{1,}")
  ) %>% 
  count(religion5)

# 우선 데이터 내부에 religion5 변수 생성 
data_131 = data_131 %>% 
  mutate(
    religion5=as_factor(REL1),
    religion5=fct_lump(religion5,n=4),
    religion5=ifelse(REL1==-1,NA,as.character(religion5)),
    religion5=str_extract(religion5,"[[:alpha:]]{1,}")
  ) 
data_131 %>% 
  count(religion5)

# None 집단만 맨 앞으로 옮기고 싶다면?
data_131 %>% 
  mutate(
    religion5=fct_relevel(religion5,"None")
  ) %>% 
  count(religion5)
# None 집단만 맨 뒤로 옮기고 싶다면?
data_131 %>% 
  mutate(
    religion5=fct_relevel(religion5,"None",after=Inf)
  ) %>% 
  count(religion5)
# "None", "Catholic", "Protestant", "Baptist","Other" 순서를 원한다면
data_131 %>% 
  mutate(
    # 마지막 집단은 별도 지정하지 않아도 무방
    religion5=fct_relevel(religion5,
                          "None","Catholic","Protestant","Baptist","Other")
  ) %>% 
  count(religion5)

# 각주: 원하는 순서를 직접 지정하는 방법
order_I_want=c("None","Baptist","Catholic","Other","Protestant")
data_131 %>% 
  mutate(
    religion5=factor(religion5,levels=order_I_want)
  ) %>% 
  count(religion5)

# 만약 가장 많은 신자를 갖는 종교 순서대로 리코딩
myresult = data_131 %>% 
  mutate(
    religion5R=fct_infreq(religion5)
  ) 
myresult %>% 
  count(religion5R)

# religion5와 비교해 보면 위의 과정을 거치면 분석결과를 더 쉽게 알 수 있음
g1 = myresult %>% drop_na(religion5) %>%
  ggplot(aes(x=religion5))+
  geom_bar()+
  labs(x='Religions, five groups',y='Number of respondents')
g2 = myresult %>% drop_na(religion5R) %>%
  ggplot(aes(x=religion5R))+
  geom_bar()+
  labs(x='Religions, five groups',y='Number of respondents')
gridExtra::grid.arrange(g1,g2,nrow=1,ncol=2)

# 평균연령이 가장 높은 집단순서대로 religion5 변수를 리코딩
by_data_131 = data_131 %>% 
  mutate(
    religion5R=fct_reorder(religion5,PPAGE,fun=mean,.desc=TRUE)
  )
myresult = by_data_131 %>% 
  group_by(religion5R) %>% 
  summarize(mean(PPAGE)) 
myresult
# 위의 과정은 시각화에 매우 유리
g1 = myresult %>% drop_na() %>% 
  ggplot(aes(x=religion5R))+
  geom_bar()+
  stat_summary_bin(aes(y=`mean(PPAGE)`),fun.y='mean',geom='bar')+
  labs(x='Religons',y='Averaged age')
g2 = myresult %>% drop_na() %>% 
  ggplot(aes(x=fct_rev(religion5R)))+
  geom_bar()+
  stat_summary_bin(aes(y=`mean(PPAGE)`),fun.y='mean',geom='bar')+
  labs(x='Religons',y='Averaged age')
gridExtra::grid.arrange(g1,g2,nrow=1,ncol=2)

# 등간변수인 연령을 순위변수인 세대로
data_131 %>% 
  mutate(
    generation=cut(PPAGE,
                   c(10,19,29,39,49,59,69,79,Inf),
                   c("10s","20s","30s","40s","50s","60s","70s","80s"))
  ) %>% 
  count(generation)

# 연령변수를 10살 단위로 구분하여 리코딩
data_131 %>% 
  mutate(
    gen_width10=cut_width(PPAGE,width=10)
  ) %>% 
  count(gen_width10)

# 연령변수를 10살 단위로 구분하여 리코딩
data_131 %>% 
  mutate(
    gen_width10=cut_width(PPAGE,width=10,boundary=0,closed='left')
    # 다음과 같이 하면 (39, 49]가 됩니다. 
    # gen_width10=cut_width(PPAGE,width=10,boundary=9,closed='right')
  ) %>% 
  count(gen_width10)

# 연령의 범위를 4등분 한 후 4집단으로 리코딩 
data_131 %>% 
  mutate(
    gen_interval4=cut_interval(PPAGE,n=4)
  ) %>% 
  count(gen_interval4)

# 연령변수의 빈도수를 기준으로 4집단 리코딩
data_131 %>% 
  mutate(
    gen_number4=cut_number(PPAGE,n=4)
  ) %>% 
  count(gen_number4)
