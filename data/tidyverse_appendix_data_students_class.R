library(tidyverse)
library(readxl)

####################################################################################
setwd("D:/TidyData/data")
seoul_educ = read_xls("data_student_class.xls",skip=2)

# 각줄별로 변수 이름 저장 
mylbl_1=names(read_xls("data_student_class.xls",skip=0))
mylbl_2=names(read_xls("data_student_class.xls",skip=1))
mylbl_3=names(seoul_educ)
# '_숫자' 표현을 삭제 
mylbl_1=str_replace(mylbl_1,"_{1,}[[:digit:]]{1}","")
mylbl_2=str_replace(mylbl_2,"_{1,}[[:digit:]]{1}","")
mylbl_3=str_replace(mylbl_3,"_{1,}[[:digit:]]{1}","")
# 중복된 경우는 반복하지 않지만, 이름이 다른 경우 _를 이용 덧붙임
mylbl = ifelse(mylbl_1==mylbl_2,mylbl_1,str_c(mylbl_1,mylbl_2,sep="_"))
mylbl = ifelse((mylbl==mylbl_3 | mylbl_2==mylbl_3), mylbl,str_c(mylbl,mylbl_3,sep="_"))
mylbl
# 정리된 변수 이름을 데이터의 변수명으로 
names(seoul_educ)=mylbl
seoul_educ

