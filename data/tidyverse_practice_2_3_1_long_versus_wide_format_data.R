#######################################################################
## 문제영역: 데이터 형태 변환
# 데이터변환_문제_1
data_educ = read_xls("data_student_class.xls",skip=2)
# 변수이름 정리 
names(data_educ)[1:2] = c("year","district")
temp = c("student","class","st.per.cl")
names(data_educ)[3:5] = str_c("kindergarten",temp,sep="_")
names(data_educ)[6:8] = str_c("elementary",temp,sep="_")
names(data_educ)[9:11] = str_c("middle",temp,sep="_")
names(data_educ)[12:14] = str_c("high",temp,sep="_")
# 완전하게 긴 형태의 데이터로 변환
long_educ = data_educ %>% 
  gather(key=vars,value=number,-year,-district) 
# 통계치의 종류를 변수이름으로 변환
wide_educ = long_educ %>% 
  separate(vars,c("rank","type"),sep="_") %>% 
  spread(key=type,value=number) %>% 
  select(year,district,rank,student,class,st.per.cl)
wide_educ 

# 데이터변환_문제_2 
wide_educ2 = wide_educ %>% 
  filter(district != '합계' & rank == 'high') %>% 
  select(year,district,st.per.cl) %>% 
  spread(key=year,value=st.per.cl)
names(wide_educ2)[2:14] = str_c("st.per.cl",
                                names(wide_educ2)[2:14],sep="_")
wide_educ2  

# 데이터변환_문제_3
data_131 = read_spss("data_TESS3_131.sav")
data_131 %>% 
  group_by(as_factor(PPGENDER),as_factor(PPREG4)) %>% 
  count() %>% 
  spread(key=`as_factor(PPREG4)`,value=n)