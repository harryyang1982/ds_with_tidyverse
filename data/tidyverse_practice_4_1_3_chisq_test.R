#######################################################################
## 연습문제: 카이제곱 테스트  
# 데이터 불러오기 
library('tidyverse')
library('haven')
library('broom')
setwd("D:/TidyData/data")

# 카이제곱_문제_1
gss_panel = read_dta("data_gss_panel06.dta")
mydata = gss_panel %>% 
  select(sex_1, educ_1, astrosci_1) %>% 
  drop_na() %>% 
  mutate(
    fem=ifelse(sex_1==1,0,1),
    educ3=as.double(cut(educ_1,c(0,12,15,20),1:3)),
    astrology=ifelse(astrosci_1==3,0,1),
    fem=labelled(fem,c(Men=0,Women=1)),
    educ3=labelled(educ3,c(`HS graduate\nor less`=1,
                           `Some college \nor below BA`=2,
                           `BA\norabove`=3)),
    astrology=labelled(astrology,c(`Not scientific`=0,Scientific=1))
  ) %>% select(fem:astrology)
# 카이제곱 테스트 
mydata %>% count(fem,astrology) %>% 
  xtabs(n~fem+astrology,.) %>% 
  chisq.test(.)
mydata %>% count(educ3,astrology) %>% 
  xtabs(n~educ3+astrology,.) %>% 
  chisq.test(.)

# 시각화
g1 = mydata %>% count(fem,astrology) %>%
ggplot(aes(x=as_factor(fem),y=n,fill=as_factor(astrology)))+
  geom_bar(stat="identity",position="fill")+
  labs(x="Gender",y="Proportion of respondents",
       fill="Astrology is scientific?")+
  theme(legend.position = "top")
g2 = mydata %>% count(educ3,astrology) %>%
  ggplot(aes(x=as_factor(educ3),y=n,fill=as_factor(astrology)))+
  geom_bar(stat="identity",position="fill")+
  labs(x="Education level",y="Proportion of respondents",
       fill="Astrology is scientific?")+
  theme(legend.position = "top")
gridExtra::grid.arrange(g1,g2,nrow=1)

# 카이제곱_문제_2
mydata %>% 
  count(fem,educ3,astrology) %>% 
  split(.$fem) %>% 
  map(~ xtabs(n~ educ3+astrology,data=.)) %>% 
  map(~ chisq.test(.)) %>% 
  map_dfr(~ tidy(.))
# 시각화 
mydata %>% count(fem,educ3,astrology) %>%
  ggplot(aes(x=as_factor(educ3),y=n,fill=as_factor(astrology)))+
  geom_bar(stat="identity",position="fill")+
  labs(x="Education level",y="Proportion of respondents",
       fill="Astrology is scientific?")+
  theme(legend.position = "top")+
  facet_wrap(~as_factor(fem))
