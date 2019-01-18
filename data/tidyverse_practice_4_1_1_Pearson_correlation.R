#######################################################################
## 연습문제: 피어슨 상관계수
# 데이터 불러오기 
library('tidyverse')
library('haven')
library('broom')
setwd("D:/TidyData/data")

# 상관계수_문제_1
gss_panel = read_dta("data_gss_panel06.dta")
mydata = gss_panel %>% 
  select(ends_with("_1")) %>% 
  select(polviews_1,sex_1,age_1,abany_1:absingle_1) %>% 
  drop_na() %>% 
  mutate_at(
    vars(abany_1:absingle_1),
    funs(ifelse(.==2,1,0))
  ) 
names(mydata)=str_replace(names(mydata),"_1","")
mydata = mydata %>% 
  mutate(
    anti_abortion=rowSums(mydata[,4:10])
  )
count(mydata, anti_abortion)
cor.test(~ polviews+anti_abortion,data=mydata)

# 시각화 
mydata %>% 
  filter(anti_abortion>0) %>% 
  ggplot(aes(x=as_factor(polviews),y=anti_abortion))+
  geom_point(alpha=0.05)+
  labs(x='Political ideology',
       y="Negative attitude towards abortion")+
  theme_bw()

# 시각화는 아래의 방식이 더 낫습니다(히트맵에 대해서는)
# 회귀분석을 소개하면서 자세히 다루겠습니다. 
mydata %>% count(polviews,anti_abortion) %>% 
  filter(anti_abortion>0) %>% 
  ggplot(aes(x=as_factor(polviews),y=anti_abortion,fill=n))+
  geom_tile()+
  scale_fill_gradient(low = "grey80", high = "black")+
  labs(x='Political ideology',
       y="Negative attitude towards abortion",
       fill="Frequency")+
  theme_bw()+
  theme(legend.position="top")

# 상관계수_문제_2
mydata = mydata %>% 
  mutate(
    gen=as.double(cut(age,c(10,29,49,69,Inf),1:4)),
    gen=labelled(gen,c(`10-20s`=1,`30-40s`=2,`50-60s`=3,`70s+`=4))
  )
myresult_sex = mydata %>% 
  split(.$sex) %>% 
  map(~ cor.test(~ anti_abortion+polviews, data=.)) %>% 
  map(~ tidy(.))
myresult_gen = mydata %>% 
  split(.$gen) %>% 
  map(~ cor.test(~ anti_abortion+polviews, data=.)) %>% 
  map(~ tidy(.))
myresult = bind_rows(myresult_sex,myresult_gen)
myresult = myresult %>% 
  mutate(
    group=c("Gender\n(Men)","Gender\n(Women)","Generation\n(10~20s)",
            "Generation\n(30~40s)","Generation\n(50~60s)","Generation\n(70s+)")
  )
myresult %>% 
ggplot(aes(x=group,y=estimate))+ 
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),
                width=0.2,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype=2,color='black')+
  coord_cartesian(ylim=c(-0.6,0.6))+
  labs(x="Groups",
       y="Pearson correlation's 95% CI between
political ideology and anti-abortion attitude")

# 상관계수_문제_3
myresult = mydata %>% 
  unite("sex_generation",c("sex","gen")) %>% 
  split(.$sex_generation) %>% 
  map(~ cor.test(~ anti_abortion+polviews, data=.)) %>% 
  map_dfr(~ tidy(.))

myresult = myresult %>% as_tibble() %>% 
  mutate(
    sex=rep(1:2,each=4),
    gen=rep(1:4,2),
    sex=labelled(sex,c(Male=1,Female=2)),
    gen=labelled(gen,c(`10-20s`=1,`30-40s`=2,`50-60s`=3,`70s+`=4))
  )

myresult %>% 
ggplot(aes(x=as_factor(gen),y=estimate,color=as_factor(sex)))+ 
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),
                width=0.2,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype=2,color='black')+
  coord_cartesian(ylim=c(-0.6,0.6))+
  labs(x="Generations",
       y="Pearson correlation's 95% CI between
political ideology and anti-abortion attitude",
       color="Gender")
