##############################################################################
## 연습문제: PCA, EFA, Cronbach's alpha 
library('tidyverse')
library('haven')
setwd("D:/TidyData/data")

# PCA_EFA_문제_1 
# 데이터를 불러오기 
mydata = read_dta("data_gss_panel06.dta") %>% 
  select(conarmy_1,conbus_1,conclerg_1,coneduc_1,confed_1,confinan_1,
         conlabor_1,conlegis_1,conmedic_1,conpress_1,consci_1,contv_1) %>%
  mutate_at(
    vars(conarmy_1:contv_1),
    funs(4 - .)
  ) %>% 
  drop_na()
names(mydata)=str_replace(names(mydata),"_1","")
names(mydata)=str_replace(names(mydata),"con","")
mydata %>% 
  cor(.) %>% round(.,2)

# 몇 개가 적당? 
# 카이저기준 
myeigen=prcomp(mydata, scale=TRUE)$sdev^2 
myeigen[myeigen>1]
# 스크리 플롯 
myeigen %>% as_tibble() %>% 
  ggplot(aes(x=1:12,y=value))+
  geom_line(color='red')+
  labs(x="principal component",y="Eigen-value")+
  scale_x_continuous(breaks=1:16)
# 평행분석 
psych::fa.parallel(cor(mydata),n.obs=1159,cor="cor",plot=T)

# EFA with 2 factors 
EFA2 = mydata %>% 
  psych::fa(.,2,rotate="promax")
EFA2
psych::fa.sort(EFA2)

# EFA with 3 factors 
EFA3 = mydata %>% 
  psych::fa(.,3,rotate="promax")
EFA3
psych::fa.sort(EFA3)

# EFA with 4 factors 
EFA4 = mydata %>% 
  psych::fa(.,4,rotate="promax")
EFA4
psych::fa.sort(EFA4)

# 제 선택은.... 
EFA3a = mydata %>% 
  select(fed,legis,army,bus,finan,press,tv) %>%
  psych::fa(.,2,rotate="promax")
psych::fa.sort(EFA3a)

# PCA_EFA_문제_2
# 5개 기관에 대한 크론바흐의 알파
mydata %>% 
  select(fed,legis,army,bus,finan) %>%
  psych::alpha()

# 측정문항이 2개인 경우는 r만 사용 (왜냐하면 평균 r을 구하는 것이 무의미함)
mydata %>% 
  select(press,tv) %>%
  psych::alpha()
