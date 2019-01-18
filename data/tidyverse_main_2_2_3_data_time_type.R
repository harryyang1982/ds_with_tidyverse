###############################################################
## 시간변수 관리 
data_131 = read_spss("data_TESS3_131.sav")
mydata = data_131 %>% 
  select(starts_with("tm_"),duration,PPAGE,PPEDUC)
mydata

# <dttm>, <date>, <time> 데이터 형태의 경우 lubridate 라이브러리 필요 
library("lubridate")
# 년, 월, 일, 시, 분, 초의 정보 추출 
mydata = mydata %>% 
  mutate(
    start_yr=year(tm_start),
    start_mt=month(tm_start),
    start_dy=day(tm_start),
    start_hr=hour(tm_start),
    start_mn=minute(tm_start),
    start_sc=second(tm_start),
    end_yr=year(tm_finish),
    end_mt=month(tm_finish),
    end_dy=day(tm_finish),
    end_hr=hour(tm_finish),
    end_mn=minute(tm_finish),
    end_sc=second(tm_finish)
  ) 
mydata %>% select(starts_with("start_"),starts_with("end_"))

# 초단위로 설문조사 소요시간 계산
mydata = mydata %>% 
  mutate(
    survey_second=as.double(tm_finish-tm_start)
  )
g1 = mydata %>% 
  ggplot(aes(x=survey_second))+
  geom_histogram(bins=50)+
  labs(x="설문소요시간(단위: 초)")
g2 = mydata %>% 
  ggplot(aes(x=log10(survey_second)))+
  geom_histogram(bins=50)+
  labs(x="상용로그 전환 설문소요시간(단위: 초)")
gridExtra::grid.arrange(g1,g2,nrow=1,ncol=2)

# 성실/불성실 응답자의 연령 및 교육년수 평균비
mydata %>% 
  mutate(
    good_surveyer=ifelse(survey_second>(10^4.5)|survey_second<(10^2),
                         0,1)
  ) %>% 
  group_by(good_surveyer) %>% 
  summarize(mean(PPAGE),mean(PPEDUC),n()) 

# 초단위 없이 새로운 <dttm> 형태 변수생성 
mydata = mydata %>% 
  mutate(
    start_time=make_datetime(start_yr,start_mt,start_dy,start_hr,start_mn),
    end_time=make_datetime(end_yr,end_mt,end_dy,end_hr,end_mn)
  )

# 새로 생성된 변수와 duration 변수는 같은가? 
mydata %>% 
  mutate(
    survey_minute=as.double(end_time-start_time)
  ) %>% 
  filter(duration != survey_minute) %>% 
  select(starts_with("tm"),duration,survey_minute)
# 아래처럼 하면 동일한 것을 알 수 있다. 
mydata %>% 
  mutate(
    survey_minute=floor(as.double(tm_finish-tm_start)/60)
  ) %>% 
  filter(duration != survey_minute) %>% 
  select(starts_with("tm"),duration,survey_minute)
