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


## 다) 범주형 변수의 수준 간소화

data_131 %>% 
  mutate(
    libcon3 = ifelse(IDEO==-1, NA,
                     ifelse(IDEO <= 2, 1,
                            ifelse(IDEO >= 6, 3, 2)))
  ) %>% 
  count(IDEO, libcon3)

data_131 %>% 
  mutate(
    libcon3 = cut(IDEO, c(-Inf, 0, 2, 5, Inf), c(NA, 1:3))
  ) %>% 
  count(IDEO, libcon3)

data_131 <- data_131 %>% 
  mutate(
    IDEO2 = as.character(as_factor(IDEO))
  )

data_131 %>% 
  mutate(
    libcon3 = ifelse(IDEO2 == "Refused", NA, IDEO2),
    libcon3 = fct_collapse(libcon3,
                           "진보"=c("Liberal", "Extremely liberal"),
                           "중도"=c("Moderate, middle of the road",
                                  "Slightly liberal", "Slightly conservative"),
                           "보수"=c("Conservative", "Extremely conservative"))
  ) %>% 
  count(libcon3)

data_131 %>% 
  count(as_factor(REL1)) %>% 
  arrange(desc(n))

data_131 %>% 
  mutate(
    religion5 = ifelse(REL1 %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12), 14, REL1),
    religion5 = ifelse(REL1==-1, NA, religion5),
    religion5 = labelled(religion5,
                         c(침례교=1, 프로테스탄트=2, 가톨릭=3, 무종교=13, 기타종교=14))
  ) %>% 
  count(as_factor(religion5))

data_131 %>% 
  mutate(
    religion5 = as_factor(REL1),
    religion5 = fct_lump(religion5, n=4),
    religion5 = ifelse(REL1==-1, NA, as.character(religion5)),
    religion5 = str_extract(religion5, "[[:alpha:]]{1,}")
  ) %>% 
  count(religion5)

## 라) 범주형 변수의 수준을 재배열

data_131 <- data_131 %>% 
  mutate(
    religion5 = as_factor(REL1),
    religion5 = fct_lump(religion5, n=4),
    religion5 = ifelse(REL1 == -1, NA, as.character(religion5)),
    religion5 = str_extract(religion5, "[[:alpha:]]{1,}")
  )

data_131 %>% 
  count(religion5)

data_131 %>% 
  mutate(
    religion5 = fct_relevel(religion5, "None")
  ) %>% 
  count(religion5)

# None 만 맨 뒤로 옮기기 위해서

data_131 %>% 
  mutate(
    religion5 = fct_relevel(religion5, "None", after=Inf)
  ) %>% 
  count(religion5)

data_131 %>% 
  mutate(
    religion5 = fct_relevel(religion5, "None", "Catholic", "Protestant", "Baptist", "Other")
  ) %>% 
  count(religion5)

myresult <- data_131 %>% 
  mutate(
    religion5R = fct_infreq(religion5)
  )

myresult %>% 
  count(religion5R)

g1 <- myresult %>% 
  drop_na(religion5) %>% 
  ggplot(aes(x=religion5))+
  geom_bar()+
  labs(x="Religions, five groups", y="Number of respondents")

g2 <- myresult %>% 
  drop_na(religion5R) %>% 
  ggplot(aes(x=religion5R)) +
  geom_bar()+
  labs(x="Religions, five groups", y="Number of respondents")

gridExtra::grid.arrange(g1, g2, nrow=1, ncol=2)

by_data_131 <- data_131 %>% 
  mutate(
    religion5R=fct_reorder(religion5, PPAGE, fun=mean, .desc=TRUE)
  )

myresult <- by_data_131 %>% 
  group_by(religion5R) %>% 
  summarize(mean(PPAGE))

myresult

g1 <- myresult %>% drop_na() %>% 
  ggplot(aes(x=religion5R)) +
  geom_bar() +
  stat_summary_bin(aes(y=`mean(PPAGE)`), fun.y='mean', geom='bar') +
  labs(x="Religions", y="Averaged Age")

g2 <- myresult %>% drop_na() %>% 
  ggplot(aes(x=fct_rev(religion5R))) +
  geom_bar() +
  stat_summary_bin(aes(y=`mean(PPAGE)`), fun.y='mean', geom='bar') +
  labs(x="Religions", y="Averaged Age")

gridExtra::grid.arrange(g1, g2, nrow=1, ncol=2)

## 마) 연속형 변수를 범주형 변수로 리코딩

data_131 %>% 
  mutate(
    generation=cut(PPAGE,
                   c(10, 19, 29, 39, 49, 59, 69, 79, Inf),
                   c("10s", "20s", "30s", "40s", "50s", "60s", "70s", "80s"))
  ) %>% 
  count(generation)

data_131 %>% 
  mutate(
    gen_width10 = cut_width(PPAGE, width=10)
  ) %>% 
  count(gen_width10)

data_131 %>% 
  mutate(
    gen_width10=cut_width(PPAGE, width=10, boundary=0, closed='left')
  ) %>% 
  count(gen_width10)

data_131 %>% 
  mutate(
    gen_interval4 = cut_interval(PPAGE, n=4)
  ) %>% 
  count(gen_interval4)

data_131 %>% 
  mutate(
    gen_number4=cut_number(PPAGE, n=4)
  ) %>% 
  count(gen_number4)

## 바) 변수의 데이터 타입 변환

temporary <- data_131 %>% 
  mutate(
    sex_fct1 = as.factor(PPGENDER),
    sex_chr1 = as.character(PPGENDER),
    sex_fct2 = as_factor(PPGENDER),
    sex_chr2 = as.character(as_factor(PPGENDER))
  )

temporary %>% 
  count(sex_fct1)

temporary %>% 
  count(sex_fct2)

temporary %>% 
  count(sex_chr1)

temporary %>% 
  count(sex_chr2)

# 연속형 변수: as.integer(), as.double()

temporary <- tibble(
  x1=as.integer(1:3),
  x2=as.double(1:3),
  x3=as.double(1+0.1*(1:3))
)

temporary

temporary %>% 
  mutate(
    x1.dbl = as.double(x1),
    x2.int = as.integer(x2),
    x3.int = as.integer(x3)
  )

temporary %>% 
  mutate(
    x2.chr = as.factor(x2),
    x3.chr = as.character(x3)
  )

temporary <- data_131 %>% 
  filter(IDEO>0) %>% 
  mutate(
    ideo_chr = as.character(as_factor(IDEO)),
    ideo_fct = as_factor(IDEO)
  ) %>% 
  select(ideo_chr, ideo_fct)

temporary

temporary <- temporary %>% 
  mutate(
    ideo_chr_dbl = as.double(ideo_chr),
    ideo_fct_dbl = as.double(ideo_fct)
  )

temporary

temporary %>% count(ideo_fct, ideo_fct_dbl)

# ideo_fct 변수의 수준들이 어떤 순서를 갖는지 체크
# %$%를 사용한 것 주의(행렬 데이터가 아닌 변수 단위인 경우 사용하는 파이프 오퍼레이터)
library(magrittr)
temporary %$% fct_unique(ideo_fct)

temporary %>% 
  mutate(
    ideo_chr_fct = as_factor(ideo_chr),
    ideo_chr_fct_dbl = as.double(ideo_chr_fct)
  ) %>% 
  count(ideo_chr_fct, ideo_chr_fct_dbl)

# 노가다로 수치 부여
temporary %>% 
  mutate(
    ideo_chr_dbl = NA,
    ideo_chr_dbl=ifelse(ideo_chr=="Extremely liberal", 1, ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Liberal", 2, ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Slightly liberal", 3, ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Moderate, middle of the road", 4, ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Slightly conservative", 5, ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Conservative", 6, ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Extremely conservative", 7, ideo_chr_dbl)
  ) %>% 
  count(ideo_chr, ideo_chr_dbl)

# 텍스트 형태의 변수 처리

world_country <- read_excel("data/data_country.xlsx")
world_country

my_data <- world_country %>% 
  separate(`GDP $USD`, into = c("GDP", "USD"), sep = " ", remove = F) %>% 
  mutate(gdp_dbl = as.double(GDP),
         gdp_unit = USD,
         gdp_unit = ifelse(USD == "Million", 10^6,
                           ifelse(USD == "Billion", 10^9,
                                  ifelse(USD == "Trillion", 10^12, gdp_unit))),
         gdp_unit = as.double(gdp_unit),
         gdp_total = gdp_dbl * gdp_unit)

my_data %>% 
  select(`GDP $USD`, gdp_total)

my_data %>% 
  summarize(gdp_total = mean(gdp_total, na.rm = TRUE))

library(extrafont)
theme_update(text=element_text(family="NanumGothic"))

my_data %>% 
  ggplot(aes(x=log10(gdp_total))) +
  geom_histogram(na.rm = T) +
  labs(x = "국내 총생산(GDP, 미국달러로 환산된 값을 상용로그로 전환")

my_data <- my_data %>% 
  mutate(
    country_name = str_count(COUNTRY, "")
  )

my_data %>% 
  summarize(min_name=min(country_name, na.rm=T),
            max_name=max(country_name, na.rm=T))

my_data %>% 
  filter(country_name==4|country_name==32) %>% 
  select(COUNTRY)

my_data <- my_data %>% 
  mutate(
    country_name = str_count(COUNTRY, "[[:alpha:]]")
  )
my_data %>% count(country_name)

my_data <- my_data %>%
  mutate(country_word = 1 + str_count(COUNTRY, " "))

my_data %>% count(country_word)

my_data <- my_data %>% 
  mutate(
    include_stan = str_detect(COUNTRY, "stan$")
  )

my_data %>% 
  filter(include_stan) %>% 
  select(include_stan, COUNTRY)

my_data %>% 
  mutate(
    include_south=str_detect(COUNTRY, "^South")
  ) %>% 
  filter(include_south) %>% 
  select(include_south, COUNTRY)

# 개인 함수를 이용한 리코딩
refused_to_missing <- function(myvariable) {
  ifelse(myvariable == -1, NA, myvariable)
}

data_131 <- read_spss("data/data_TESS3_131.sav")

data_131 %>% 
  mutate(
    ideo2=refused_to_missing(IDEO)
  ) %>% 
  count(IDEO, ideo2)

data_131_2 <- data_131 %>% 
  mutate_if(
    is.double,
    funs(refused_to_missing)
  )
data_131_2 %>% 
  count(IDEO)

data_131_2 %>% 
  count(Q1)

seven_to_collapse <- function(myvariable){
  myvariable = ifelse(myvariable == -1, NA, myvariable)
  myvariable = cut(myvariable, c(0, 3, 4, 7), 1:3)
}

data_131 %>% 
  mutate(
    Q1_3 = seven_to_collapse(Q1)
  ) %>% 
  count(Q1, Q1_3)

data_131_2 <- data_131 %>% 
  mutate_at(
    vars(Q1, Q2, Q3, Q4, IDEO, PARTY7),
    funs(seven_to_collapse)
  )

data_131_2 %>% 
  count(Q3)

# p.178 exercise

#1

mydata <- read_dta("data/data_gss_panel06.dta")

mydata %>% 
  mutate(child_re = ifelse(childs_1 >=0 & childs_1 <= 1, "below 1", "above 2")) %>% 
  select(childs_1, child_re) %>% 
  count(child_re)

#2

mydata %>% 
  mutate(race_re = ifelse(race_1 == 1, "white", "non white")) %>% 
  count(race_re)

#3
mydata %>% 
  count(relactiv_1)

mydata %>% 
  mutate(rel_re = cut(relactiv_1, c(0, 4, 6, 9, Inf), c(1:4))) %>% 
  count(rel_re)

#4
mydata %>% 
  mutate(race_1 = as_factor(race_1),
         race_1 = ifelse(race_1 == "white", "white", "non_white"),
         childs_1 = cut(childs_1, c(0, 2, 4, 6, Inf), c(1:4))) %>% 
  count(race_1, childs_1) %>% 
  spread(race_1, n) %>% 
  na.omit(childs_1) %>% 
  select(childs_1, white, everything())
  
#5
mydata %>% 
  mutate(caremost_1 = as_factor(caremost_1),
         caremost_re = NA,
         caremost_re = ifelse(str_detect(caremost_1, "bears|seals"), "animal", caremost_re),
         caremost_re = ifelse(str_detect(caremost_1, "sea|cap"), "climate", caremost_re),
         caremost_re = ifelse(str_detect(caremost_1, "inuit"), "inuit", caremost_re)) %>% 
  count(caremost_re)
  
#6
foraid <- read_excel("data/data_foreign_aid.xlsx")
foraid %>% 
  separate(total_development_aid, into=c("td", "billion"), sep = " ") %>% 
  mutate_at(vars(2, 4, 5),
            funs(as.double(str_replace_all(., "\\$", "")))) %>% 
  mutate(billion = ifelse(billion == "billion", 10^9,
                          ifelse(billion == "million", 10^6,
                                 ifelse(billion == "triilion", 10^12, billion)))) %>% 
  mutate(total_development_aid = td*billion) %>% 
  select(donor, total_development_aid, development_aid_per_capita, GDP_percent)

#7
tess <- read_sav("data/data_TESS3_131.sav")
tess

make_na <- function(variable){
  variable = ifelse(variable %in% c(1:7), variable, NA)
}

#7-1
tess %>% 
  mutate_at(vars(Q1, Q2, Q3),
            funs(make_na)) %>% 
  count(Q3)

make_rev <- function(variable){
  variable = ifelse(variable == -1, NA, variable)
  variable = 8 - variable
}

tess %>% 
  mutate_at(vars(Q1, Q2, Q3),
            funs(make_rev)) %>% 
  count(Q3)

#8

recode2 <- function(variable){
  variable = ifelse(variable == 4, 0,
                    ifelse(variable == 3 | variable == 5, 1,
                           ifelse(variable == 2 | variable == 6, 2,
                                  ifelse(variable == 1 | variable == 7, 3, variable ))))
}

tess %>% 
  mutate_at(vars(Q1, Q2, Q3),
            funs(make_na)) %>% 
  mutate_at(vars(Q1, Q2, Q3),
            funs(recode2)) %>% 
  count(Q1)

#2-3 날짜 및 시간 변수
library(tidyverse)
library(haven)

data_131 <- read_sav("data/data_TESS3_131.sav")
mydata <- data_131 %>% 
  select(starts_with("tm_"), duration, PPAGE, PPEDUC)

mydata

library(lubridate)

mydata <- mydata %>% 
  mutate(
    start_yr = year(tm_start),
    start_mt = month(tm_start),
    start_dy = day(tm_start),
    start_hr = hour(tm_start),
    start_mn = minute(tm_start),
    start_sc = second(tm_start),
    end_yr = year(tm_finish),
    end_mt = month(tm_finish),
    end_dy = day(tm_finish),
    end_hr = hour(tm_finish),
    end_mn = minute(tm_finish),
    end_sc = second(tm_finish)
  )

mydata %>% 
  select(starts_with("start_"), starts_with("end_"))

mydata <- mydata %>% 
  mutate(survey_second = as.double(tm_finish - tm_start))

g1 <- mydata %>% 
  ggplot(aes(x=survey_second)) +
  geom_histogram(bins=50)+
  labs(x="설문소요시간(단위: 초")
g2 <- mydata %>% 
  ggplot(aes(x=log10(survey_second))) +
  geom_histogram(bins=50)+
  labs(x="상용로그 전환 설문소요시간(단위: 초")

gridExtra::grid.arrange(g1, g2, nrow=1, ncol=2)

mydata %>% 
  mutate(
    good_survey = ifelse(survey_second > 10^4.5 | survey_second < 10^2, F, T)
  ) %>% 
  group_by(good_survey) %>% 
  summarize(mean(PPAGE), 
            mean(PPEDUC),
            n())

mydata <- mydata %>% 
  mutate(
    start_time = make_datetime(start_yr, start_mt, start_dy, start_hr, start_mn),
    end_time = make_datetime(end_yr, end_mt, end_dy, end_hr, end_mn)
  )
mydata %>% 
  select(start_time, end_time)

mydata %>% 
  mutate(
    survey_minute = as.double(end_time - start_time)
  ) %>% 
  filter(duration != survey_minute) %>% 
  select(starts_with("tm"), duration, survey_minute)

mydata %>% 
  mutate(
    survey_minute = floor(as.double(tm_finish - tm_start)/60)
  ) %>% 
  filter(duration != survey_minute) %>% 
  select(starts_with("tm"), duration, survey_minute)

# p.190 exercise

songs <- read_csv("data/data_1000songs.csv")
songs

#1
songs %>% 
  mutate(    start_yr = year(tm_start),
             start_mt = month(tm_start),
             start_dy = day(tm_start),
             start_hr = hour(tm_start),
             start_mn = minute(tm_start),
             start_sc = second(tm_start),
             end_yr = year(tm_end),
             end_mt = month(tm_end),
             end_dy = day(tm_end),
             end_hr = hour(tm_end),
             end_mn = minute(tm_end),
             end_sc = second(tm_end)
  ) %>% 
  count(start_mt)

#2
songs %>% 
  mutate(duration = as.double(tm_end - tm_start)) %>% 
  summarize(max(duration),
            min(duration))

#2-4 변수이름 재설정
library(readxl)
seoul_library <- read_excel("data/data_library.xls")
seoul_library %>% 
  print(n = 2)

seoul_library %>% 
  rename(
    year_id = 기간,
    district_id = 자치구,
    lib_total = 계,
    lib_national = 국립도서관,
    lib_public = 공공도서관,
    lib_university = 대학도서관,
    lib_special = 전문도서관
  ) %>% 
  print(n=2)

mylabels <- c("year_id", "district_id", "lib_total", "lib_national",
           "lib_public", "lib_university", "lib_special")

names(seoul_library) <- mylabels
seoul_library %>% 
  print(n=2)

seoul_library <- read_excel("data/data_library.xls")
seoul_library %>% 
  print(n=2)

names(seoul_library) <- str_replace(names(seoul_library), "도서관", "_lib")
seoul_library %>% print(n=2)

names(seoul_library)[3] <- "계_lib"
seoul_library %>% print(n=2)

# p.195 exercise
popul <- read_excel("data/data_population.xls")

mypopu <- popul %>% 
  rename(year=기간,
         district="구분..2",
         resident=`구분..3`,
         total=계)

names(mypopu) <- str_replace(names(mypopu), "세", "")
names(mypopu) <- str_replace(names(mypopu), "~", "")
names(mypopu) <- paste("age", names(mypopu), sep="")
mypopu
