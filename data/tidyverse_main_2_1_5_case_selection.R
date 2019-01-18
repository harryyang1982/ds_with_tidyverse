####################################################################
## 사례선별 
# data_TESS3_131.sav 불러온 후 PP로 시작하는 변수들만 추려냄 
library('haven')
data_131 = read_spss("data_TESS3_131.sav")
data_131 = data_131 %>% 
  select(starts_with("PP")) %>%
  print(n=2)
# 여기서 PPGENDER 빈도표는?
data_131 %>% 
  count(PPGENDER)

# 각주: PPGENDER 변수에서 1과 2에 붙은 라벨의 의미는? 
print_labels(data_131$PPGENDER)

# 여성 응답자만 추려내면?
data_131 %>% 
  filter(PPGENDER==2) %>% 
  count(PPGENDER)

# 여성 응답자만 배제해 보자. 
data_131 %>% 
  filter(PPGENDER!=2) %>% 
  count(as_factor(PPGENDER)) # 1이 아니라 Male 이라는 라벨이 나타남

# South 혹은 West 거주 응답자는?
data_131 %>% 
  filter(PPREG4==3|PPREG4==4) %>% 
  count(as_factor(PPREG4))
# South 혹은 West 거주 응답자: 부등호 사용 
data_131 %>% 
  filter(PPREG4 >= 3) %>% # filter(PPREG4 > 2) 라고 해도 결과 동일
  count(as_factor(PPREG4))
# 남부가 아닌 다른 지역에 거주하는 남성 응답자를 선별
data_131 %>% 
  filter(PPREG4 != 3 & PPGENDER==1) %>%  
  # filter(PPREG4 != 3, PPGENDER==1) 동일하지만 개인적으로 권장하지는 않
  count(as_factor(PPGENDER),as_factor(PPREG4))

# 1. 데이터 불러오기 
# 2. PPGENDER, PPAGE, PARTY7 로 시작하는 변수들을 선별. 
# 3. 40-59세의 남성 응답자를 선별
# 4. 이렇게 얻은 데이터에서 PARTY7 변수의 분포를 살펴보자. 
# 위와 같은 순서로 data_TESS3_131.sav 데이터를 사전처리 해보자.
mydata_131 = read_spss("data_TESS3_131.sav") %>%   #1단계
  select(PPGENDER, PPAGE, PARTY7) %>%   #2단계
  filter(PPGENDER==1 & (PPAGE >= 40 & PPAGE <= 59))   #3단계
mydata_131 %>% count(as_factor(PARTY7))   #4단계

# 결측값 제거 
myGSS = read_dta("data_gss_panel06.dta") %>% 
  select(starts_with("astrolgy_"))
# astrolgy_3 변수의 빈도표는? 
myGSS %>% 
  count(as_factor(astrolgy_3))
# astrolgy_3 변수에서 결측값이 아닌 응답자만 선별
myGSS %>% 
  filter(!is.na(astrolgy_3)) %>% 
  print(n=2)
# listwise deletion 적용
myGSS %>% 
  filter(!is.na(astrolgy_1) & 
           !is.na(astrolgy_2) & 
           !is.na(astrolgy_3)) %>% 
  print(n=2)
# drop_na() 함수:astrolgy_3 변수에서 결측값이 아닌 응답자만 선별
myGSS %>% 
  drop_na(astrolgy_3) %>% 
  print(n=2)
# drop_na() 함수: listwise deletion 적용
myGSS %>% 
  drop_na() %>% 
  print(n=2)