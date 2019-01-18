#######################################################################
## 문제영역: 집단구분 
# 집단구분_문제_1: 
library('readxl')
setwd("D:/TidyData/data")
data_educ = read_xls("data_student_class.xls",skip=2) 
data_educ = data_educ %>% 
  filter(지역 != "합계") %>%
  print(n=2)
# 통계치만 알고자 한다면?
data_educ %>% 
  group_by(기간) %>% 
  summarize(mean(원아수)) 
# 통계치 + 패턴변화 시각화 
data_educ %>% 
  group_by(기간) %>% 
  summarize(y=mean(원아수)) %>% 
  print(n=20) %>% 
  ggplot(aes(x=as.integer(기간),y=y)) + 
  geom_point(size=4)+
  geom_line(size=1)+
  labs(x="년도",y="유치원 원아수")

# 집단구분_문제_2: 
# 통계치 + 패턴변화 시각화 
data_educ %>% 
  group_by(지역) %>% 
  summarize(y=mean(원아수)) %>% 
  print(n=30) %>% 
  #원아수에 따라 지역의 순위를 재조정
  #(fct_reorder() 함수에 대해서는 나중에 보다 자세 소개) 
  ggplot(aes(x=fct_reorder(지역,y,fun=mean)))+ 
  geom_bar()+
  stat_summary_bin(aes(y=y),fun.y='mean',geom="bar")+
  labs(x="서울시 25개구",y="유치원 원아수")

# 집단구분_문제_3: 
data_educ %>% 
  split(.$기간) %>% 
  map(~ cor.test(~ 학급당원아수+학급당학생수,data=.x)) %>% 
  map("estimate") %>% 
  as_tibble() %>% 
  gather(key=year,value=cor)

data_educ %>% 
  split(.$기간) %>% 
  map(~ cor.test(~ 학급당원아수+학급당학생수,data=.x)) %>% 
  map("estimate") %>% 
  as_tibble() %>% 
  gather(key=year,value=cor) %>% 
  ggplot(aes(x=as.integer(year),y=cor))+
  geom_point(size=3,color='red')+
  geom_line(size=1,color='lightblue')+
  coord_cartesian(ylim=c(-.3,.6))+
  labs(x="년도",y="유치원 학급당 원아수와 초교 학급당 학생수")

# 집단구분_문제_4 
# 그러나 아래처럼 여러 단계를 거치는 경우 2-3번으로 끊어주는 것이 더 좋습니다. 
# 즉 아래는 학습을 위한 예시에 불과합니다. 
data_educ %>% 
  split(.$지역) %>% 시
map(~ cor.test(~ 학급당원아수+학급당학생수,data=.x)) %>% 
  map("conf.int") %>% 
  as_tibble() %>% 
  gather(key=area,value=cor) %>% #95% 상한과 하한을 구분하기 위해 
  mutate(
    conf2=rep(c('min','max'),25)
  ) %>%  #95% CI 상한과 하한을 표현하였음
  spread(key=conf2,value=cor) %>% 
  mutate(
    point.est=0.5*(max+min), #상관계수(point estimate)
    mycut=ifelse(min>0,"significant","not sig.") #통계적으로 유의미한가 여부 구분
  ) %>%
  ggplot(aes(x=fct_reorder(area,point.est),
             y=point.est,color=mycut))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min,ymax=max),width=0.2,size=1)+
  geom_hline(yintercept=0,color='red')+
  coord_cartesian(ylim=c(-1.0,1.0))+
  labs(x="지역",
       y="유치원 학급당 원아수와 초교 학급당 학생수 상관관계",
       color='statistical significance')+
  theme(legend.position='top')