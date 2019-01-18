##################################################################################################
# 데이터 합치기: *_join() 함수들 
# 서울시 구별 도서관과 학생,학급수 관련 데이터 열기 
seoul_library = read_xls("data_library.xls")
seoul_library %>% print(n=2)
seoul_educ = read_xls("data_student_class.xls",skip=2)
seoul_educ %>% print(n=2)

# 설명의 편의를 위해 간단한 데이터로 바꿈
mydata1 = seoul_library %>% 
  filter(기간==2016 & 자치구 != "합계") %>% 
  select(1:3)
mydata1 %>% print(n=2)
mydata2 = seoul_educ %>% 
  filter(기간==2016 & 지역 != "합계") %>% 
  select(1:3)
mydata2 %>% print(n=2)

# 다음과 같이 해도 가능: full_join(mydata1,mydata2,by=c("자치구"="지역"))
# 변수이름 조정 후 데이터 합치기
names(mydata1)=c("year","district","lib_total")
names(mydata2)=c("year","district","stdt_kinder")
mydata1 %>% 
  full_join(mydata2,by="district")

# 식별변수가 2개 이상인 경우
mydata1 %>% 
  full_join(mydata2,by=c("year","district"))

# 데이터를 다음과 같이 선별하여 저장 
mydata1 = seoul_library %>% 
  filter(자치구 != "합계") %>% 
  select(1:3)
names(mydata1)=c("year","district","lib_total")
mydata1 %>% print(n=2)
mydata2 = seoul_educ %>% 
  filter(기간 <= 2013) %>% 
  select(1:3)
names(mydata2)=c("year","district","stdt_kinder")
mydata2 %>% print(n=2)

# 사례가 상이한 경우 full_join() 함수 이용 데이터 합치기 
mydata = mydata1 %>% 
  full_join(mydata2,by=c("year","district"))
mydata %>% count(is.na(lib_total)) # 도서관수 변수의 결측값수는?
mydata %>% count(is.na(stdt_kinder)) # 유치원생 변수의 결측값수는?

# 사례가 상이한 경우 inner_join() 함수 이용 데이터 합치기 
mydata = mydata1 %>% 
  inner_join(mydata2,by=c("year","district"))
mydata %>% count(is.na(lib_total)) # 도서관수 변수의 결측값수는?
mydata %>% count(is.na(stdt_kinder)) # 유치원생 변수의 결측값수는?

# 사례가 상이한 경우 left_join() 함수 이용 데이터 합치기 
mydata = mydata1 %>% 
  left_join(mydata2,by=c("year","district"))
mydata %>% count(is.na(lib_total)) # 도서관수 변수의 결측값수는?
mydata %>% count(is.na(stdt_kinder)) # 유치원생 변수의 결측값수는?

# 사례가 상이한 경우 right_join() 함수 이용 데이터 합치기 
mydata = mydata1 %>% 
  right_join(mydata2,by=c("year","district"))
mydata %>% count(is.na(lib_total)) # 도서관수 변수의 결측값수는?
mydata %>% count(is.na(stdt_kinder)) # 유치원생 변수의 결측값수는?

mydata = mydata1 %>% 
  inner_join(mydata2,by=c("year","district"))
# 강남4구로 구성된 티블데이터 구성
filter_data = tibble(
  district=c("강동구", "강남구", "서초구", "송파구")
)

# 강남4구만 포함할 경우 
mydata %>% 
  semi_join(filter_data,by="district") %>% 
  count(district)

# 강남4구만 배제할 경우 
mydata %>% 
  anti_join(filter_data,by="district") %>% 
  count(district)