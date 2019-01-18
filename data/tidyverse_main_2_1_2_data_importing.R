####################################################################
# 현재 사용중인 파일 위치는? 
getwd()
# 파일 위치를 바꾸고자 한다면? 
setwd("D:/TidyData/data")
getwd()  # 제대로 수정된 것을 확인
## 외부에 저장된 데이터 불러오기 
# 엑셀데이터 불러오기: 2007 버전 이후(*.xlsx)
library('tidyverse')
library('readxl')
world_country = read_xlsx("data_country.xlsx")
world_country

# 엑셀데이터 불러오기: 97-2003 버전(*.xls)
seoul_library = read_xls("data_library.xls")
seoul_library

# 데이터가 어디서 시작하는가에 따라 skip 옵션을 조정해야 한다. 
seoul_educ = read_xls("data_student_class.xls")
seoul_educ

seoul_educ = read_xls("data_student_class.xls",skip=2)
seoul_educ

# 쉼표로 분리된 텍스트 형식의 데이터(*.csv)
seoul_loc = read_csv("data_district_lonlat.csv")
seoul_loc

# 탭으로 분리된 텍스트 형식의 데이터(*.txt)
seoul_loc = read_delim("data_district_lonlat.txt",delim='\t')
seoul_loc

# csv 데이터를 read_delim() 함수로 불러오기 
seoul_loc = read_delim("data_district_lonlat.csv",delim=',')
seoul_loc

# SPSS 형태의 데이터를 불러오기 
library('haven')
tess131 = read_spss("data_TESS3_131.sav")
tess131 

# STATA 형태의 데이터를 불러오기 
gss_panel = read_dta("data_gss_panel06.dta")
gss_panel
