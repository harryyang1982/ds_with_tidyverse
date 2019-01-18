library(tidyverse)

name <- c("연돌이", "세순이")
born <- c("1999-3-2", "1999-3-3")
year <- c(2L, 4L)
grade <- c('A+', 'A-')
height <- c(178, 170)

my_first_tibble <- tibble(name, born, year, grade, height)
my_first_tibble

my_first_tibble[1,]
my_first_tibble[, 2:4]

tibble(
  name = c("Jake", "Jessy", "Jack"),
  `^__^` = c("used", "used", "not used"),
  `ㅠ.ㅠ` = c("used", "not used", "used"),
  `Emotion status?` = c("ambivalence", "happy", "sad")
)

data.frame(
  name = c("Jake", "Jessy", "Jack"),
  `^__^` = c("used", "used", "not used"),
  `ㅠ.ㅠ` = c("used", "not used", "used"),
  `Emotion status?` = c("ambivalence", "happy", "sad")
)

# exercises

tribble(
  ~country, ~code2, ~area_km2,
  "China", "CHN", 9596960,
  "Japan", "JPN", 377835,
  "South Korea", "KOR", 98480
)

library(readxl)

world_country <- read_excel("data/data_country.xlsx")
world_country

seoul_library <- read_excel("data/data_library.xls")
seoul_library

seoul_educ <- read_excel("data/data_student_class.xls", skip = 2)
seoul_educ

seoul_loc <- read_csv("data/data_district_lonlat.csv")
seoul_loc

seoul_loc <- read_delim("data/data_district_lonlat.txt", delim = '\t')
seoul_loc

# as same as above

seoul_loc <- read_delim("data/data_district_lonlat.csv", delim = ',')
seoul_loc

library(haven)
tess131 <- read_spss("data/data_TESS3_131.sav")
tess131

gss_panel <- read_dta("data/data_gss_panel06.dta")
gss_panel
