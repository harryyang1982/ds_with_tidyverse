###############################################################
## 변수이름 관리 
seoul_library = read_xls("data_library.xls")
seoul_library %>% print(n=2)

# rename() 함수 이용 
seoul_library %>% 
  rename(
    year_id=기간,
    district_id=자치구,
    lib_total=계,
    lib_national=국립도서관,
    lib_public=공공도서관,
    lib_university=대학도서관,
    lib_special=전문도서관
  ) %>% print(n=2)

# 데이터의 변수명을 직접변경 
mylabels=c("year_id","district_id","lib_total","lib_national",  
           "lib_public","lib_university","lib_special")
names(seoul_library) = mylabels
seoul_library %>% print(n=2)

# 문자형 변수 리코딩에서 배웠던 지식을 활용하면 더욱 효율적
seoul_library = read_xls("data_library.xls")
seoul_library %>% print(n=2)
names(seoul_library)=str_replace(names(seoul_library),"도서관","_lib")
seoul_library %>% print(n=2)

# 특정위치의 변수이름만 변경
names(seoul_library)[3] = "계_lib"
seoul_library %>% print(n=2)