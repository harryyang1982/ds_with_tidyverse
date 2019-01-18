####################################################################
## 변수선별 
# 서울시 25개 구별 도서관 현황 데이터(data.seoul.go.kr)
seoul_library = read_xls("data_library.xls")
seoul_library

# 기간, 자치구, 계 변수들만 선별
seoul_library2 = seoul_library %>% 
  select(기간, 자치구, 계)
seoul_library2

# 국립도서관 공공도서관 대학도서관 전문도서관 변수들만 버리는 경우 
seoul_library2 = seoul_library %>% 
  select(-국립도서관, -공공도서관, -대학도서관, -전문도서관) 
seoul_library2 %>% 
  print(n=2)

# 특정 표현이 규칙적으로 등장하는 경우 
seoul_library2 = seoul_library %>% 
  select(-ends_with("도서관")) 
seoul_library2 %>% 
  print(n=2)

# 데이터에서 기간, 자치구, 그리고 “도서”라는 표현이 등장한 어떠한 이름의 변수를 선별
seoul_library2 = seoul_library %>% 
  select(기간, 자치구, contains("도서")) 
seoul_library2 %>% 
  print(n=2)

# 데이터에서 기간 변수부터 공공도서관 변수까지의 범위에 속하는 변수들을 선별 
seoul_library2 = seoul_library %>% 
  select(기간:공공도서관) 
seoul_library2 %>% 
  print(n=2)

# 2번째 부터 5번째 까지의 변수들을 선별 
seoul_library2 = seoul_library %>% 
  select(2:5) 
seoul_library2 %>% 
  print(n=2)

# 범위지정시 주의사항: 아래는 오류 발생
seoul_library2 = seoul_library %>% 
  select(-2:5) 

# 2번째 부터 5번째 까지의 변수들을 선별 
seoul_library2 = seoul_library %>% 
  select(-(2:5))
seoul_library2 %>% 
  print(n=2)
