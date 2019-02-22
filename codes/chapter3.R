library(tidyverse)
library(haven)

wide_data <- read_dta("data/data_gss_panel06.dta") %>% 
  mutate(
    rid=row_number()
  ) %>% 
  select(rid, starts_with("affrmact_"))

wide_data %>% 
  print(n = 2)

wide_data %>% 
  gather(time, score, affrmact_1:affrmact_3)

# as same as the above
long_data1 <- wide_data %>% 
  gather(time, score, -rid)

# long_data1 <- wide_data %>% 
#   mutate_all(funs(as.numeric)) %>% 
#   gather(time, score, -rid)
long_data1

long_data2 <- wide_data %>% 
  gather(time, score, -rid, na.rm=TRUE)
long_data2 %>% 
  print(n=4)

gss_panel <- read_dta("data/data_gss_panel06.dta")

long_full_gss <- gss_panel %>% 
  mutate(rid=row_number()) %>% 
  gather(question, resp, contains("_")) %>% 
  separate(question, c("var", "time"), sep="_")

long_full_gss
long_time_gss <- long_full_gss %>% 
  spread(var, resp)

long_time_gss %>% 
  select(rid, time, form, affrmact) %>% 
  arrange(rid, time)

#p.214 exercise

#1
library(readxl)
student <- read_excel("data/data_student_class.xls", skip=2)
student2 <- read_excel("data/data_student_class.xls") %>% 
  .[-1:-2, ]

mystudent <- student %>% 
  mutate(rid = row_number()) %>% 
  gather(dest, value, 원아수:`학급당학생수..14`) %>% 
  mutate(dest = str_replace_all(dest, "[:digit:]", ""),
         dest = str_replace_all(dest, "\\..", "")) %>% 
  select(rid, everything())
mystudent2 <- student2 %>% 
  mutate(rid = row_number()) %>% 
  gather(rank, value, `유치원..3`:`고등학교..14`) %>% 
  mutate(rank = str_replace_all(rank, "[:digit:]", ""),
         rank = str_replace_all(rank, "\\..", "")) %>% 
  select(rid, everything())

total_student <- bind_cols(mystudent, mystudent2) %>% 
  select(rid, 기간, 지역, dest, value, rank)

total_student <- total_student %>% 
  mutate(dest = str_replace_all(dest, "원아수", "학생수"),
         dest = str_replace_all(dest, "학급당원아수", "학급당학생수")) %>% 
  select(rid, year=기간, district=지역, rank, dest, value)

total_spread <- total_student %>% 
  spread(dest, value) %>% 
  select(rid, year, district, student=학생수, rank, class=학급수, student_per_class=학급당학생수)

# 2

k <- total_spread %>% 
  filter(district != "합계", rank == "고등학교") %>% 
  select(year, district, student_per_class) %>% 
  spread(year, student_per_class)

# 3
tess <- read_sav("data/data_TESS3_131.sav")

tess %>% 
  count(as_factor(PPGENDER))

tess %>% 
  count(as_factor(PPREG4))

tess_table <- tess %>% 
  mutate(PPREG4 = as_factor(PPREG4),
         PPGENDER = as_factor(PPGENDER)) %>% 
  select(PPREG4, PPGENDER) %>% 
  count(PPREG4, PPGENDER) %>% 
  spread(PPREG4, n)
