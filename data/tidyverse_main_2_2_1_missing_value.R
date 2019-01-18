###############################################################
## 변수 리코딩 
# 결측값 확인 및 처리
small_gss = read_dta("data_gss_panel06.dta") %>% 
  select(starts_with("affrmact_")) 
small_gss %>%
  print(n=3)

# affrmact_1 결측값 여부를 확인할 수 있는 변수 생성
small_gss2 = small_gss %>% 
  mutate(
    affrmact_NA_1=is.na(affrmact_1)
  ) %>% 
  print(n=3)
small_gss2 %>% 
  count(affrmact_NA_1)

# 결측값의 수를 나타내는 변수를 생성
small_gss2 = small_gss %>% 
  mutate(
    n.NA=is.na(affrmact_1)+is.na(affrmact_2)+is.na(affrmact_3)
  )
small_gss2 %>% 
  count(n.NA)
# .를 이용해 간단하게 결측값 수를 얻을 수 있음(n.NA와 t.NA는 동일함)
small_gss2 %>% 
  mutate(
    t.NA=rowSums(is.na(.))
  ) %>% 
  print(n=3)

# 특정 실측값을 결측값으로 변환해야 하는 경우 
data_131 = read_spss("data_TESS3_131.sav")
data_131 %>% 
  count(Q2)
# ifelse() 함수를 이용하여 결측값 변환 
data_131 %>% 
  mutate(
    Q2r = ifelse(Q2==-1, NA, Q2)
  ) %>% 
  count(Q2r)
# 여러 값들을 결측값으로 변환하는 경우 
data_131 %>% 
  mutate(
    Q2r = ifelse(Q2==-1|Q2==4, NA, Q2)
  ) %>% 
  count(Q2r)
# 결측값이 문자형 데이터로 투입된 경우 
seoul_library = read_xls("data_library.xls")
seoul_library %>% 
  print(n=3)

# -인 경우 NA 부여 
seoul_library2 = seoul_library %>% 
  mutate(
    계=ifelse(계=='-',NA,계),
    국립도서관=ifelse(국립도서관=='-',NA,국립도서관),
    공공도서관=ifelse(공공도서관=='-',NA,공공도서관),
    대학도서관=ifelse(대학도서관=='-',NA,대학도서관),
    전문도서관=ifelse(전문도서관=='-',NA,전문도서관)
  )
seoul_library2 %>% 
  print(n=3)

# mutate_all() 함수를 사용
seoul_library %>% 
  mutate_all(
    funs(ifelse(.=='-',NA,.))
  ) %>% 
  print(n=3)
# mutate_at() 함수를 사용
seoul_library %>% 
  mutate_at(
    3:7,funs(ifelse(.=='-',NA,.))
  ) %>% 
  print(n=3)
# mutate_at() 함수를 사용
seoul_library %>% 
  mutate_at(
    vars(계,ends_with('도서관')),funs(ifelse(.=='-',NA,.))
  ) %>% 
  print(n=3)

# 문자형 데이터를 수치형으로 전환
seoul_library %>% 
  mutate(
    국립도서관=ifelse(국립도서관=="-",NA,국립도서관),
    국립도서관=as.integer(국립도서관)
  ) %>% 
  print(n=3)

# 문자형 데이터를 수치형으로 전환
seoul_library2 = seoul_library %>% 
  mutate_at(
    3:7, funs(as.integer(ifelse(.=='-',NA,.)))
  ) 
seoul_library2 %>% 
  print(n=3)

myresult = read_xls("data_library.xls") %>% 
  filter(자치구 != '합계') %>% #1단계
  mutate_at(
    3:7, funs(as.double(ifelse(.=='-',NA,.)))
  ) %>% #2단계, 3단계
  group_by(기간) %>%  #4단계
  summarize_if(
    is.double,funs(sum(.,na.rm = T))
  ) #4단계, 더블형 데이터에 대해서만 합계를 적용함. 
myresult
