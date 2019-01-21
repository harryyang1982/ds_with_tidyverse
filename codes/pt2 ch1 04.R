#1-4 변수선별

library(tidyverse) ; library(haven); library(readxl)

seoul_library <- read_excel("data/data_library.xls")
seoul_library

seoul_library2 <- seoul_library %>% 
  select(기간, 자치구, 계)

seoul_library2

seoul_library2 <- seoul_library %>% 
  select(-국립도서관, -공공도서관, -대학도서관, -전문도서관)
seoul_library2 %>% print(n = 2)
seoul_library2 %>% head(2)

seoul_library2 <- seoul_library %>% 
  select(-ends_with("도서관"))
seoul_library2 %>% 
  print(n = 2)

seoul_library2 <- seoul_library %>% 
  select(기간, 자치구, contains("도서")) %>% 
  print(n=2)

seoul_library2 <- seoul_library %>% 
  select(기간:공공도서관)

print(seoul_library2, n=2)

seoul_library2 <- seoul_library %>% 
  select(2:5)
seoul_library2 %>% 
  print(n=2)

seoul_library %>% 
  select(-2:-5)

# as same as above
seoul_library %>% 
  select(-(2:5))

# exercise

gss_panel <- read_dta("data/data_gss_panel06.dta")

#1

gss_panel %>% 
  select(-ends_with("_2"), -ends_with("_3")) -> gss_06

#2
gss_06 %>% 
  select(contains("relig"))

#3
data_131 <- read_spss("data/data_TESS3_131.sav") %>% 
  select(starts_with("PP"))
data_131

#4
data_131 <- read_spss("data/data_TESS3_131.sav") %>% 
  select(starts_with("PP", ignore.case = F))

#5
data_131 <- read_spss("data/data_TESS3_131.sav") %>% 
  select(contains("_"))
