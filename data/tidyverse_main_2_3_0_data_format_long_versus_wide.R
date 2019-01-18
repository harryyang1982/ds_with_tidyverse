###############################################################
## 긴 데이터 및 넓은 데이터: 형태 변환 
# 간단한 사례 
long_data = read_xls("data_library.xls") %>% 
  filter(자치구 == "서대문구") %>% 
  select(기간, 자치구, 공공도서관) 
long_data 

# 넓은 형태 데이터로 변환 
wide_data = long_data %>% 
  spread(key=기간,value=공공도서관)
wide_data

# 다시 긴 형태 데이터로 재변환 
long_data2 = wide_data %>% 
  gather(key=기간, value=공동도서관, -자치구)
long_data2 

# 조금 더 복잡한 사례 
long_data = read_xls("data_library.xls") %>% 
  select(기간, 자치구, 공공도서관) 
long_data 

# 자치구를 key 로 지정할 경우 
wide_data1 = long_data %>% 
  spread(key=자치구,value=공공도서관)
wide_data1
# 기간을 key 로 지정할 경우 
wide_data2 = long_data %>% 
  spread(key=기간,value=공공도서관)
wide_data2
# 기간과 자치구를 모두 key 로 지정할 경우 
wide_data3 = long_data %>% 
  unite(자치구_기간, 자치구,기간,sep="_") %>% #별개의 두 변수들을 하나로 통합
  spread(key=자치구_기간,value=공공도서관)
wide_data3

# 다시 긴데이터로 
long_data3 = wide_data3 %>% 
  gather(key=자치구_기간,value=공공도서관) %>% 
  separate(자치구_기간, c("자치구","기간"),sep="_") #하나의 변수를 두 변수로 분리 
long_data3

# 데이터 전체를 긴 형태로 바꿀 수 있다
long_data = read_xls("data_library.xls") %>% 
  gather(key=도서관,value=숫자,-자치구,-기간) 
long_data 

# 다시 넓은 형태로 돌리려면 
wide_data = long_data %>% 
  spread(key=도서관,value=숫자)
wide_data


## 복잡한 데이터에 적용해 보자
wide_data = read_dta("data_gss_panel06.dta") %>% 
  mutate(
    rid=row_number() #아이디 변수 생성 
  ) %>% 
  select(rid,starts_with("affrmact_")) 
wide_data %>% 
  print(n=2)

# 긴 형태 데이터로 
long_data1 = wide_data %>% 
  gather(key=time,value=score,-rid)
long_data1 %>% 
  arrange(rid)
# 결측값을 삭제하는 경우 
long_data2 = wide_data %>% 
  gather(key=time, value=score,-rid,na.rm=TRUE)
long_data2 %>% 
  print(n=4)

# 각주: 만약 Warning message를 보기 싫다면 
long_data1 = wide_data %>% 
  mutate_all(funs(as.numeric)) %>% 
  gather(key=time,value=score,-rid)
long_data1 %>% 
  arrange(rid)

# 넓은 형태 데이터로 재전환 
wide_data1 = long_data1 %>% 
  spread(key=time,value=score)
wide_data2 = long_data2 %>% 
  spread(key=time,value=score,fill=99)

## 매우 복잡한 상황이라도 변수이름만 체계적이라면 아무 문제없다. 
gss_panel = read_dta("data_gss_panel06.dta")
# GSS 패널 데이터를 다음의 순서로 완전한 긴 형태 데이터로 만
long_full_gss = gss_panel %>% 
  #고유데이터를 생성(가로줄 번호로 생성)
  mutate(rid=row_number()) %>% 
  #_으로 구분된 변수들의 경우 데이터 변호
  gather(key=question,value=resp,contains("_")) %>%
  #question 변수의 경우 변수명과 측정시점으로 구분
  separate(question,c("var","time"),sep="_") 
# 완전하게 긴 형태의 데이터를 측정시점만 긴 형태의 데이터로 재변환
long_time_gss = long_full_gss %>% 
  #변수명만을 넓은 형태로 생성함 
  spread(key=var,value=resp)  
# 측정시점으로 구분된 것을 발견할 수 있음(데이터 중 일부만 제시하) 
long_time_gss %>% 
  select(rid,time,form,affrmact) %>% 
  arrange(rid, time)