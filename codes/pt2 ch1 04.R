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

# 1-5 사례선별

data_131 <- read_spss("data/data_TESS3_131.sav")

data_131 %>% 
  select(starts_with("PP", ignore.case = F)) %>% 
  print(n=2)

data_131 %>% 
  count(PPGENDER)

data_131 %>% 
  filter(PPGENDER == 2) %>% 
  count(PPGENDER)

print_labels(data_131$PPGENDER)

data_131 %>% 
  filter(PPGENDER == 2) %>% 
  count(as_factor(PPGENDER))

data_131 %>% 
  filter(PPREG4 == 3 | PPREG4 == 4) %>% 
  count(as_factor(PPREG4))

data_131 %>% 
  filter(PPREG4 >= 3) %>% 
  count(as_factor(PPREG4))

data_131 %>% 
  filter(PPREG4 != 3, PPGENDER == 1) %>% 
  count(as_factor(PPGENDER), as_factor(PPREG4))

data_131 %>% 
  filter(PPREG4 != 3, PPGENDER == 1) %>% 
  count(PPGENDER=as_factor(PPGENDER), PPREG4=as_factor(PPREG4))

## exercises

mydata_131 <- read_sav("data/data_TESS3_131.sav") %>% 
  select(PPGENDER, PPAGE, PARTY7) %>% 
  filter(PPGENDER == 1, (PPAGE >= 40 & PPAGE <= 59))

mydata_131 %>% 
  count(PARTY7=as_factor(PARTY7))

my_GSS <- read_dta("data/data_gss_panel06.dta") %>% 
  select(starts_with("astrolgy_"))

my_GSS %>% 
  count(astrolgy_3=as_factor(astrolgy_3))

# astrolgy_3 변수에서 결측값이 아닌 응답자만 선별
my_GSS %>% 
  filter(!is.na(astrolgy_3)) %>% 
  print(n=2)

my_GSS %>% 
  filter(!is.na(astrolgy_1) &
           !is.na(astrolgy_2) &
           !is.na(astrolgy_3)) %>% 
  print(n=2)

my_GSS %>% 
  drop_na(astrolgy_3) %>% 
  print(n=2)

# different from the above

my_GSS %>% 
  na.omit(astrolgy_3) %>% 
  print(n=2)

# as same as the above

my_GSS %>% 
  drop_na() %>% 
  print(n=2)

# p.91 사례선별 과정 연습문제

#q1
case_Q1 <- read_excel("data/data_library.xls") %>% 
  select(기간, 자치구, ends_with("도서관")) %>% 
  filter(기간 == 2016, 자치구 != "합계")

#q2
case_Q2 <- read_excel("data/data_library.xls") %>% 
  filter(자치구 == "합계")

ggplot(case_Q2) +
  geom_line(aes(x=기간, y=공공도서관, group = 1))

#q3
case_Q3 <- read_dta("data/data_gss_panel06.dta") %>% 
  select(starts_with("letin1_"))

#q3A
case_Q3 %>% 
  filter(!is.na(letin1_1) & (is.na(letin1_2) | is.na(letin1_3)))

#q3B
case_Q3 %>% 
  filter(letin1_1 == letin1_2, 
         letin1_2 == letin1_3)

#q3C
case_Q3 %>% 
  filter(letin1_1 != letin1_2,
         letin1_2 != letin1_3,
         letin1_3 != letin1_1)

#1-6
data_131 <- read_sav("data/data_TESS3_131.sav") %>% 
  select(starts_with("PP", ignore.case = FALSE))

by_data_131 <- data_131 %>% 
  group_by(PPGENDER) %>% 
  summarize(mean(PPINCIMP), sd(PPINCIMP), n())

by_data_131

by_data_131 <- data_131 %>% 
  group_by(PPGENDER, PPREG4)

by_data_131 %>% 
  ungroup() %>% 
  print(n=2)

data_131 %>% 
  group_by(PPGENDER=as_factor(PPGENDER), PPREG4=as_factor(PPREG4)) %>% 
  summarize(y=mean(PPAGE)) %>% 
  ggplot(aes(x=PPREG4)) +
  geom_bar() +
  stat_summary_bin(aes(y = y), fun.y='mean', geom='bar') +
  labs(x="Regions in USA", y="Age, averaged")+
  coord_cartesian(ylim=c(45, 55)) +
  facet_grid(.~PPGENDER)

by_data_131 <- data_131 %>% 
  split(.$PPGENDER)

by_data_131

summary(by_data_131)

data_131 %>% 
  split(.$PPGENDER) %>% 
  map(~cor.test(~PPAGE + PPINCIMP, data=.x))

data_131 %>% 
  split(.$PPGENDER) %>% 
  map(~cor.test(~PPAGE + PPINCIMP, data=.x)) %>% 
  map_dfc("estimate") %>% 
  round(3)

data_131 %>% 
  group_by(gender_reg4 = 10*PPGENDER + PPREG4) %>% 
  split(.$gender_reg4) %>% 
  map(~cor.test(~PPAGE+PPINCIMP, data=.x)) %>% 
  map_dfc("estimate") %>% 
  round(3)

data_131 %>% 
  group_by(gender_reg4=10*PPGENDER + PPREG4) %>% 
  split(.$gender_reg4) %>% 
  map(~cor.test(~PPAGE+PPINCIMP, data=.x)) %>% 
  map_dfc("p.value") %>% 
  round(3)

# p.104 exercise

#q1
data_educ <- read_excel("data/data_student_class.xls", skip=2) %>% 
  filter(지역 != "합계")

data_educ %>% 
  group_by(기간=as_factor(기간)) %>% 
  summarize(원아수=mean(원아수)) %>% 
  ggplot(aes(x=기간, y=원아수)) +
  geom_line(group=1)

#q2
data_educ %>% 
  group_by(지역) %>% 
  summarize(원아수=mean(원아수)) %>% 
  ggplot(aes(x=reorder(지역, 원아수), y=원아수)) +
  geom_col(aes(fill = 지역)) +
  coord_flip()

#q3
data_educ %>% 
  group_by(기간, 지역) %>% 
  summarize(학급당원아수=mean(학급당원아수),
                  학급당학생수=mean(`학급당학생수..8`)) %>% 
  split(.$지역) %>% 
  map(~cor.test(~학급당원아수+`학급당학생수`, data=.x)) %>% 
  map_df("estimate") %>% 
  round(3)

data_educ %>% 
  group_by(기간, 지역) %>% 
  summarize(학급당원아수=mean(학급당원아수),
                  학급당학생수=mean(`학급당학생수..8`)) %>% 
  split(.$지역) %>% 
  map(~cor.test(~학급당원아수+`학급당학생수`, data=.x)) %>% 
  map_df("p.value") %>% 
  round(3) %>% 
  gather(지역, p_value, 강남구:중랑구) %>% 
  filter(p_value <=0.05) %>% 
  ggplot(aes(x=지역, y=p_value)) +
  geom_col(aes(fill=지역))

#q4
data_educ %>% 
  group_by(기간, 지역) %>% 
  summarize(학급당원아수=mean(학급당원아수),
                  학급당학생수=mean(`학급당학생수..8`)) %>% 
  split(.$기간) %>% 
  map(~cor.test(~학급당원아수+학급당학생수, data=.x)) %>% 
  map_df("estimate") %>% 
  gather(기간, estimate, `2004`:`2016`) %>% 
  ggplot(aes(x=기간, y=estimate)) +
  geom_line(group=1)

# 1-7 데이터 정렬하기

seoul_library <- read_excel("data/data_library.xls")
seoul_library %>% 
  print(n=3)

seoul_library %>% 
  arrange(자치구)

seoul_library %>% 
  arrange(desc(자치구))

seoul_library %>% 
  filter(기간==2016) %>% 
  arrange(desc(공공도서관), 자치구) 

## 집단 구분 여부

seoul_library %>% 
  group_by(기간) %>% 
  arrange(자치구, .by_group=TRUE)

seoul_library %>% 
  group_by(기간) %>% 
  arrange(자치구, .by_group=FALSE)

# p.110 quiz

country <- read_excel("data/data_country.xlsx")

country %>% 
  arrange(POPULATION) %>% 
  head(5)

country %>% 
  arrange(POPULATION) %>% 
  tail(5)
