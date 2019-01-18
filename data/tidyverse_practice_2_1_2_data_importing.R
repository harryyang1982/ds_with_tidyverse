#######################################################################
## 문제영역: 저장된 데이터 불러오기 
# 데이터불러오기_문제_1
# xls 확장자의 엑셀 파일들
library('readxl')
seoul_educ = read_xls("data_student_class.xls")
seoul_pop = read_xls("data_population.xls")
seoul_library = read_xls("data_library.xls")

# xlsx 확장자의 엑셀 파일들
world_country = read_xlsx("data_country.xlsx")

# csv 데이터 파일들 
survey_csv = read_csv("data_survey_comma.csv")
seoul_loc = read_csv("data_district_lonlat.csv")

# 탭 구분 데이터 파일
survey_tab = read_delim("data_survey_tab.txt",delim='\t')

# SPSS 데이터 파일
library('haven')
data_131 = read_spss("data_TESS3_131.sav")

# STATA 데이터 파일 
gss_panel = read_dta("data_gss_panel06.dta")

# 데이터불러오기_문제_2
myanswer = tibble(
  country=c("China","Japan","South Korea"),
  code2=c("CHN","JPN","KOR"),
  area_km2=c(9596960,377835,98480)
)
write_excel_csv(myanswer, 'my_answer.csv')