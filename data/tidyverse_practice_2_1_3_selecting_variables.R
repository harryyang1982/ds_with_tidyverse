#######################################################################
## 문제영역: 변수선별
# 변수선별_문제_1 
library('tidyverse')
library('haven')
gss_06 = read_dta("data_gss_panel06.dta") %>% 
  select(-ends_with("_2"),-ends_with("_3"))
gss_06
dim(gss_06) # 이렇게 하면 보다 간단합니다. 

# 변수선별_문제_2 
gss_06 %>% 
  select(contains("relig")) %>% 
  print(n=2)

# 변수선별_문제_3 
data_131 = read_spss("data_TESS3_131.sav") %>% 
  select(starts_with("PP"))
dim(data_131) 

# 변수선별_문제_4 
data_131 = read_spss("data_TESS3_131.sav") %>% 
  select(starts_with("PP", ignore.case = FALSE))
data_131 %>% 
  print(n=2)

# 변수선별_문제_5 
data_131 = read_spss("data_TESS3_131.sav") %>% 
  select(contains("_"))
dim(data_131) 