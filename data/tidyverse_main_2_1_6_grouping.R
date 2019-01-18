###############################################################
## 변수 수준에 따라 집단 구분 
# 성별에 따라 응답자를 집단 구분 
data_131 = read_spss("data_TESS3_131.sav") %>%   
  select(starts_with("PP"))  #PP로 시작하는 이름의 변수만 
by_data_131 = data_131 %>% 
  group_by(PPGENDER) 
# PPGENDER 수준에 따라 2개 집단으로 구분된 것을 확인할 수 있다. 
by_data_131 %>% 
  print(n=2)

# 각 집단의 평균소득의 평균과 표준편차를 구해보죠
# (기술통계에 대해서는 조금 후에 보다 자세히 살펴보겠습니다)
by_data_131 %>% 
  summarize(mean(PPINCIMP),sd(PPINCIMP),n()) 
# 본문에는 없음: 아래와 같이 하면 소수점 2자리에서 반올림된 결과를 얻을 수 있다.
by_data_131 %>% 
  summarize(mean(PPINCIMP),sd(PPINCIMP),n()) %>% 
  round(., 2)

# 두 개 이상의 변수를 이용해 집단 구분 
by_data_131 = data_131 %>% 
  group_by(PPGENDER, PPREG4) 
by_data_131 %>% 
  print(n=2)

# 집단구분 삭제
by_data_131 %>% 
  ungroup() %>% 
  print(n=2)

# 성별, 지역별로 구분한 8개 집단의 평균연령 비교 
data_131 %>% 
  group_by(as_factor(PPGENDER), as_factor(PPREG4)) %>% 
  summarize(y=mean(PPAGE)) %>% 
  ggplot(aes(x=`as_factor(PPREG4)`)) + 
  geom_bar()+
  stat_summary_bin(aes(y = y), fun.y='mean', geom='bar')+
  labs(x='Regions in USA', y='Age, averaged')+
  coord_cartesian(ylim=c(45,55))+
  facet_grid(.~`as_factor(PPGENDER)`)

# 성별에 따라 데이터를 2개로 나누기 
by_data_131 = data_131 %>% 
  split(.$PPGENDER)
by_data_131 

# summary() 함수를 이용해 보면 더 명확합니다. 
summary(by_data_131)

# 집단별 상관계수 
# 먼저 magrittr 라이브러리 구동
library('magrittr')
data_131 %>%
  split(.$PPGENDER) %>% 
  map(~ cor.test(~PPAGE+PPINCIMP, data=.x)) #상관계수 추정결과 저장 

# 만약 상관계수만 확인하고자 한다면?
data_131 %>%
  split(.$PPGENDER) %>% 
  map(~ cor.test(~PPAGE+PPINCIMP, data=.x)) %>% 
  map("estimate") %>% # 상관계수만 추출하여 저장함
  as_tibble() %>%  #결과를 티블 데이터로 저장 
  round(3) # 소수점 3자리에서 반올림된 값

# PPGENDER, PPREG4 변수를 교차시킨 후 집단구분 
data_131 %>% 
  group_by(gender_reg4=10*PPGENDER+PPREG4) %>%
  split(.$gender_reg4) %>% 
  map(~ cor.test(~ PPAGE+PPINCIMP,data=.x)) %>% 
  map("estimate") %>% 
  as_tibble() %>% 
  round(3) 

# 통계적 유의도 테스트 결과만 추출하면?
data_131 %>% 
  group_by(gender_reg4=10*PPGENDER+PPREG4) %>%
  split(.$gender_reg4) %>% 
  map(~ cor.test(~ PPAGE+PPINCIMP,data=.x)) %>% 
  map("p.value") %>% # 통계적 유의도의 경우 
  as_tibble() %>% 
  round(3) 

# 각주: 타이디데이터 접근에 익숙해지면 다음과 같이 할 수도 있습니다. 
data_131 %>% 
  mutate(
    GENDER_REG4=str_c(as_factor(PPGENDER),as_factor(PPREG4),sep="_")
  ) %>% 
  split(.$GENDER_REG4) %>% 
  map(~ cor.test(~PPAGE+PPINCIMP, data=.x)) %>% 
  map("estimate") %>% 
  as_tibble() %>% 
  round(3) %>%  
  gather(key=myvars,value=mycor) %>% # 긴 형태 티블 데이터로 변경함 
  mutate(
    gender=str_replace(str_extract(myvars,"[[:alpha:]]{1,}_"),"_",""),
    reg4=str_replace(str_extract(myvars,"_[[:alpha:]]{1,}"),"_","")
  ) %>% # 성별로 표시된 부분과 지역으로 표시된 구분을 구분함 
  select(-myvars) %>% 
  spread(key=reg4,value=mycor) # 지역을 세로줄에 성별을 가로줄에 배치 
