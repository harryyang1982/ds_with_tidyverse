###############################################################
## 데이터 정렬하기 
# 데이터의 현재 정렬 상태 확인 
seoul_library = read_xls("data_library.xls")
seoul_library %>% 
  print(n=3)

# 자치구의 가나다 순서로 재정렬 하는 경우 
seoul_library %>% 
  arrange(자치구)

# 자치구의 가나다 역순서로 재정렬
seoul_library %>% 
  arrange(desc(자치구))

# 두 개이상의 변수들에 따라 정렬하는 것도 가능
seoul_library %>% 
  filter(기간==2016) %>% 
  arrange(desc(공공도서관), 자치구)

# 집단 구분된 데이터를 정렬할 경우 .by_group 옵션에 주 
seoul_library %>% 
  group_by(기간) %>% 
  arrange(자치구,.by_group=TRUE)
