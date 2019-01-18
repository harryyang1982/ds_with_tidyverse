#######################################################################
## 문제영역: 티블 데이터 구성하기 
# 티블데이터입력_문제_1 
library('tidyverse')
myanswer = tibble(
  country=c("China","Japan","South Korea"),
  code2=c("CHN","JPN","KOR"),
  area_km2=c(9596960,377835,98480)
)
myanswer