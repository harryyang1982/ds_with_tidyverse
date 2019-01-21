library(tidyverse)
library(readxl)

directory <- dir("data", pattern = "*.xls|*.csv|*.dta")
path <- "data"

data_files <- str_c(path, directory, sep = "/")
data_files

file_parser <- function(x) {
  if(str_detect(x, "xls")) read_excel(x)
  else if(str_detect(x, "csv")) read_csv(x)
  else haven::read_dta(x)
}

datas <- data_files %>% 
  map(file_parser)

read_excel("data/data_library.xls") %>% 
  count(기간)

seoul_library <- read_excel("data/data_library.xls")
count(seoul_library, 기간)

# 구조화된 R 코드 vs not

my_recoding_function <- function(myvariable) {
  if (is_character(myvariable)) {
    myvariable <- as.numeric(recode(myvariable, "-" = "0"))
  } else {
    myvariable <- as.numeric(myvariable)
  }
}

seoul_library %>% 
  filter(기간==2010 & 자치구 !="합계") %>% 
  select(ends_with("도서관")) %>% 
  mutate_at(
    vars(ends_with("도서관")),
    my_recoding_function
  ) %>% 
  summarize_all(mean)

