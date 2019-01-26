library(tidyverse)
library(haven)
library(readxl)

# 변수 관리
# 2-1 결측치 처리

## 데이터 로딩

small_gss <- read_dta("data/data_gss_panel06.dta") %>% 
  select(starts_with("affrmact_"))

small_gss %>% 
  print(n=3)

small_gss2 <- small_gss %>% 
  mutate(
    affrmact_NA_1=is.na(affrmact_1)
  ) %>% 
  print(n=3)

small_gss2 %>% 
  count(affrmact_NA_1)

small_gss2 <- small_gss %>% 
  mutate(n.NA = is.na(affrmact_1) + is.na(affrmact_2) + is.na(affrmact_3))

small_gss2 %>% 
  count(n.NA)

small_gss2 %>% 
  mutate(
    t.NA = rowSums(is.na(.))
  ) %>% 
  print(n=3)

data_131 <- read_spss("data/data_TESS3_131.sav")
data_131 %>% 
  count(Q2)

data_131 %>% 
  mutate(
    Q2r = ifelse(Q2 == -1, NA, Q2)
  ) %>% 
  count(Q2r)

data_131 %>% 
  mutate(
    Q2r = ifelse(Q2==-1 | Q2==4, NA, Q2)
    ) %>% 
      count(Q2r)

seoul_library <- read_excel("data/data_library.xls")
seoul_library %>% 
  print(n=3)

# mutate 노가다 way

seoul_library2 <- seoul_library %>% 
  mutate(
    계=ifelse(계=="-", NA,계),
    국립도서관=ifelse(국립도서관=="-", NA, 국립도서관),
    공공도서관=ifelse(공공도서관=="-", NA, 공공도서관),
    대학도서관=ifelse(대학도서관=="-", NA, 대학도서관),
    전문도서관=ifelse(전문도서관=="-", NA, 전문도서관)
  )

seoul_library2 %>% 
  print(n=3)

# mutate_all

seoul_library %>% 
  mutate_all(
    funs(ifelse(.=='-', NA, .))
  ) %>% 
  print(n=3)

# mutate_at

seoul_library %>% 
  mutate_at(
    3:7, funs(ifelse(.=='-', NA, .))
  ) %>% 
  print(n=3)


seoul_library %>% 
  mutate_at(
    vars(계, ends_with("도서관")), 
    funs(ifelse(.=='-', NA, .))
  ) %>% 
  print(n=3)

# 문자형 데이터의 수치형 변환
seoul_library %>% 
  mutate(
    국립도서관=ifelse(국립도서관=="-", NA, 국립도서관),
    국립도서관=as.integer(국립도서관)
  ) %>% 
  print(n = 3)

seoul_library2 <- seoul_library %>% 
  mutate_at(
    3:7, funs(as.integer(ifelse(.=='-', NA, .)))
  )
seoul_library2 %>% 
  print(n = 3)

# exercise p.122
myresult <- read_excel("data/data_library.xls") %>% 
  filter(자치구!="합계") %>% 
  mutate_at(vars(계, ends_with("도서관")),
            funs(as.double(ifelse(.=="-", NA, .)))) %>% 
  group_by(기간) %>% 
  summarize_if(
    is.double, funs(sum(., na.rm = T))
  )
myresult  
  
# exercise p.124

##1
population <- read_excel("data/data_population.xls") %>% 
  mutate_at(vars(ends_with("세")),
            funs(as.integer(ifelse(.=="-", NA, .))))

##2
country <- read_excel("data/data_country.xlsx") %>% 
  mutate_all(
    funs(ifelse(.==0, NA, .))
  )


# 2-2 변수 리코딩

# 가) 범주형 변수를 이분변수로 리코딩 binary variable

data_131 %>% 
  mutate(
    female = ifelse(PPGENDER==2, 1, 0)
  ) %>% 
  count(female)

data_131 %>% 
  mutate(
    Q5_A = ifelse(Q5==2, 1, 0),
    Q5_B = ifelse(Q5== -1, NA, 
                  ifelse(Q5==2, 1, 0))
    ) %>% count(Q5_A, Q5_B)

data_131 %>% 
  mutate(
    Q5_A = ifelse(Q5==2, 1, 0),
    Q5_B = ifelse(ifelse(Q5==-1, NA, Q5)==2, 1, 0)
  ) %>% 
  count(Q5_A, Q5_B)

data_131 %>% 
  mutate(
    white=ifelse(PPETHM==1, 1, 0)
  ) %>% 
  count(PPETHM, white)

## 나) 연속형 변수를 이분변수로 리코딩

data_131 %>% 
  mutate(
    senior = ifelse(PPAGE >= 65, 1, 0)
  ) %>% 
  group_by(senior) %>% 
  summarize(min(PPAGE), max(PPAGE))

data_131 %>% 
  mutate(
    pol.middle=ifelse(IDEO >= 3 & IDEO <= 5, 1, 0),
    pol.middle=ifelse(IDEO == -1, NA, pol.middle)
  ) %>% 
  count(IDEO, pol.middle)
