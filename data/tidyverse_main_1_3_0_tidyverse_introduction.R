
# 타이디버스 라이브러리 구동 
library('tidyverse')
# 아래와 같이 하위 라이브러리를 각각 살펴보자. 
help(package='ggplot2')
help(package='tibble')
help(package='tidyr')
help(package='readr')
help(package='purrr')
help(package='dplyr')
help(package='stringr')
help(package='forcats')

# 타이디버스가 아닌 기타 라이브러리들(별도의 설치는 필요없지만, 
# library() 함수를 이용해 추가로 구동해야 함)
# 날짜 및 시간 관련 변수처리 
library('lubridate')

# 엑셀 형식 데이터 처리 
library('readxl')

# SPSS, SAS, STATA 형식 데이터 처리 
library('haven')

# 모형 예측 및 진단
library('modelr')

# 모형추정 결과를 타이디데이터로 저장 및 활용
library('broom')

