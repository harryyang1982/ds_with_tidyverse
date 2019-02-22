library(tidyverse)
library(readxl)

seoul_library <- read_excel("data/data_library.xls")
seoul_educ <- read_excel("data/data_student_class.xls", skip=2)

seoul_educ %>% print(n=2)

mydata1 <- seoul_library %>% 
  filter(기간==2016 & 자치구 != "합계") %>% 
  select(1:3)

mydata2 <- seoul_educ %>% 
  filter(기간==2016 & 지역 !="합계") %>% 
  select(1:3)

mydata2 %>% 
  print(n=2)

full_join(mydata1, mydata2, by=c("자치구"="지역"))

names(mydata1) <- c("year", "district", "lib_total")
names(mydata2) <- c("year", "district", "stdt_kinder")

mydata1 %>% 
  full_join(mydata2, by="district")

mydata1 %>% 
  full_join(mydata2, by=c("year", "district"))

mydata1 <- seoul_library %>% 
  filter(자치구 !="합계") %>% 
  select(1:3)
names(mydata1) <- c("year", "district", "lib_total")
mydata1 %>% print(n=2)

mydata2 <- seoul_educ %>% 
  filter(기간 <= 2013) %>% 
  select(1:3)

names(mydata2) <- c("year", "district", "stdt_kinder")
mydata2 %>% print(n=2)

mydata <- mydata1 %>% 
  full_join(mydata2, by=c("year", "district"))

mydata %>% 
  count(is.na(lib_total))

mydata %>% count(is.na(stdt_kinder))

mydata <- mydata1 %>% 
  inner_join(mydata2, by=c("year", "district"))

mydata %>% 
  count(is.na(lib_total))
mydata %>% 
  count(is.na(stdt_kinder))

mydata <- mydata1 %>% 
  left_join(mydata2, by=c("year", "district"))

mydata %>% 
  count(is.na(lib_total))

mydata %>% 
  count(is.na(stdt_kinder))

mydata <- mydata1 %>% 
  right_join(mydata2, by = c("year", "district"))

mydata %>% count(is.na(lib_total))
mydata %>% count(is.na(stdt_kinder))

mydata <- mydata1 %>% 
  inner_join(mydata2, by=c("year", "district"))

filter_data <- tibble(
  district = c("강동구", "강남구", "서초구", "송파구")
)

mydata %>% 
  semi_join(filter_data, by="district") %>% 
  count(district)

mydata %>% 
  anti_join(filter_data, by="district") %>% 
  count(district)

# p.227 exercise
library(tidyverse)
library(readxl)
foreign_aids <- read_excel("data/data_foreign_aid.xlsx")
data_country <- read_excel("data/data_country.xlsx")


#q1
foreign_aids <- foreign_aids %>% 
  rename(ID=donor)

data_country <- data_country %>% 
  rename(ID=COUNTRY)

fd1 <- full_join(foreign_aids, data_country)
fd2 <- inner_join(foreign_aids, data_country)

fd1

#q2
foreign_aids %>% 
  anti_join(data_country)

#q3

filter_data <- tibble(
  ID = "Slovak Republic"
)


foreign_aids <- foreign_aids %>% 
  mutate(ID = str_replace(ID, "Slovakia", "Slovak Republic"))

fd3 <- foreign_aids %>% 
  full_join(data_country)
