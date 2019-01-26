library(tidyverse)
library(haven)
library(readxl)

# 변수 관리

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
