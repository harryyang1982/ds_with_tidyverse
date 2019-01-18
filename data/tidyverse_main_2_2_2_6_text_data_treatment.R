################################################################
## 텍스트 형태 변수 처리 
world_country = read_xlsx("data_country.xlsx")
world_country

# 앞서 배운 리코딩 과정과 연계하면 GDP를 더블형 숫자로 바꿀 수 있습니다.
mydata = world_country %>% 
  separate(`GDP $USD`,c("gdp_dbl","gdp_chr"),sep=" ",remove=F) %>% #1단계
  mutate(
    #2단계
    gdp_dbl=as.double(gdp_dbl),
    gdp_unit=gdp_chr,
    gdp_unit=ifelse(gdp_chr=="Million",10^6,gdp_unit),
    gdp_unit=ifelse(gdp_chr=="Billion",10^9,gdp_unit),
    gdp_unit=ifelse(gdp_chr=="Trillion",10^12,gdp_unit),
    gdp_unit=as.double(gdp_unit),
    #3단계
    gdp_total=gdp_dbl*gdp_unit
  ) 
mydata %>% 
  select(`GDP $USD`,gdp_total)

# 기술통계분석이나 데이터 시각화도 가능합니다. 
mydata %>% summarize(mean(gdp_total,na.rm=TRUE))
mydata %>% 
  ggplot(aes(x=log10(gdp_total)))+
  geom_histogram(na.rm=T)+
  labs(x="국내 총생산(GDP, 미국달러로 환산된 값을 상용로그로 전환)")

# 각주: 원래의 `GDP $USD`를 다시 만들 수도 있다(결측값의 경우 주의할 필요)
mydata = mydata %>% 
  unite(GDP_USD,gdp_dbl,gdp_chr,sep=" ") 
mydata %>% 
  select(GDP_USD,`GDP $USD`)

# 텍스트 데이터에 사용된 문자수를 세어보자. 
mydata = mydata %>% 
  mutate(
    country_name=str_count(COUNTRY, "")
  ) 
mydata %>% 
  summarize(min(country_name,na.rm=T),max(country_name,na.rm=T))
mydata %>% filter(country_name==4|country_name==32) %>% select(COUNTRY) 

# 위의 방식에서는 공란,쉼표 등도 1개의 문자로 세었다는 문제가 있다.
# 조금 복잡하지만 정규표현을 쓰면 알파벳만 셀 수 있다. 
mydata = mydata %>% 
  mutate(
    country_name=str_count(COUNTRY, "[[:alpha:]]")
  )
mydata %>% count(country_name)

# 각주: 아래처럼 [a-zA-Z]를 이용해도 동일한 결과
mydata %>% 
  mutate(country_name=str_count(COUNTRY,"[a-zA-Z]")) %>% 
  count(country_name) %>% 
  print(n=3)

# 단어수를 세는 것도 가능하다. 공란을 단어 구분의 지표로 간주하자. 
mydata = mydata %>% 
  mutate(
    country_word=1+str_count(COUNTRY, " ")
  ) 
mydata %>% count(country_word)

# 특수한 표현이 들어간 사례들만 선별할 수도 있다
# stan으로 끝나는 국가는?
mydata = mydata %>% 
  mutate(
    include_stan=str_detect(COUNTRY,"stan$")
  ) 
mydata %>% 
  filter(include_stan) %>% 
  select(include_stan,COUNTRY)

# South로 시작되는 국가는?
mydata %>% 
  mutate(
    include_south=str_detect(COUNTRY,"^South")
  ) %>% 
  filter(include_south) %>% 
  select(COUNTRY)
