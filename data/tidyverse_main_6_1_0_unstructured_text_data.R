#######################################################
# 비정형 텍스트 데이터를 타이디데이터로: 개방형 응답
library("tidyverse")
library("haven")
library("tidytext")  # 텍스트분석을 위해 추가설치필요 

# 데이터를 불러온 후 티블형태의 데이터로 저장하고 응답자 ID부여
mydata_response = stm::gadarian %>% as_tibble() %>% 
  mutate(rid=row_number())

# 데이터의 전체적 형태는?
mydata_response

# 개방형 응답을 단어단위로 쪼개 보면? 
mydata_word = mydata_response  %>% 
  unnest_tokens(word,open.ended.response,token="words")
mydata_word 

# 일상적으로 등장하는 불용단어는 삭제
mydata_substantive_word = mydata_word %>% 
  anti_join(get_stopwords(source = "smart"),by="word")
mydata_substantive_word

# 응답자별 등장 단어수 계산 
mydata_total_word = mydata_substantive_word %>% 
  group_by(rid) %>%
  summarize(treat=mean(treatment),
            demrep7=mean(pid_rep),
            no_words=n()) %>% 
  # 변수들의 리코딩 작업 
  mutate(
    treat=labelled(treat,c(controlled=0,treated=1)),
    demrep7=as.double(as.factor(demrep7)),
    demrep7=labelled(demrep7,c(`Strong\nDemocrats`=1,
                               `\nDemocrats`=2,
                               `Weak\nDemocrats`=3,
                               `\nModerates`=4,
                               `Weak\nRepublicans`=5,
                               `\nRepublicans`=6,
                               `Strong\nRepublicans`=7))
  )
mydata_total_word

# 실험처치별/정당지지성향별로 개방형 응답에 등장한 평균단어수는?
mydata_total_word %>% 
  group_by(treat,demrep7) %>% 
  summarize(y=mean(no_words)) %>% 
ggplot()+
  geom_bar(aes(x=as_factor(demrep7),
               y=y,fill=as_factor(treat)),
           stat='identity',position="dodge")+
  labs(x="Party identification",
       y="averaged number of words\nin open-ended responses",
       fill="treatment")+
  theme(legend.position = "top")

# 분산분석
aov(no_words~as_factor(treat)*as_factor(demrep7),mydata_total_word) %>%
  summary()

# 감정분석 
mydata_sentiment = mydata_substantive_word %>%
  inner_join(get_sentiments("nrc"),by="word") %>% 
  filter(!is.na(sentiment)) 
mydata_sentiment
# 감정어 범주별 등장횟수 결과 
mydata_sentiment_sum = mydata_sentiment %>% 
  group_by(rid,sentiment) %>% 
  summarize(sent_sum=n()) %>% 
  spread(key=sentiment,value=sent_sum,fill=0) %>%
  ungroup() %>% 
  gather(key=sentiment,value=sent_sum,-rid)
mydata_sentiment_sum

# 감정분석 결과를 합치기 
mydata_SA = full_join(mydata_sentiment_sum,
          mydata_response,by="rid") %>% 
  mutate(
    treat=labelled(treatment,c(controlled=0,treated=1)),
    demrep7=as.double(as.factor(pid_rep)),
    demrep7=labelled(demrep7,c(`Strong\nDemocrats`=1,
                               `\nDemocrats`=2,
                               `Weak\nDemocrats`=3,
                               `\nModerates`=4,
                               `Weak\nRepublicans`=5,
                               `\nRepublicans`=6,
                               `Strong\nRepublicans`=7))
  ) 
mydata_SA 

# 실험처치별/정당지지성향별 감정어 분석 
mydata_SA %>% 
  group_by(treat,demrep7,sentiment) %>% 
  summarize(y=mean(sent_sum)) %>% 
  drop_na(y) %>% # 몇몇 응답자는 어떠한 감정어도 사용않음
ggplot()+
  geom_bar(aes(x=as_factor(demrep7),
               y=y,fill=as_factor(treat)),
           stat='identity',position="dodge")+
  labs(x="Party identification",
       y="averaged number of words\nin open-ended responses",
       fill="treatment")+
  theme(legend.position = "top")+
  facet_wrap(~sentiment,nrow=5)

# 티테스트: 감정유형별
mydata_SA %>% 
  split(.$sentiment) %>% 
  map(~ t.test(sent_sum~as_factor(treat),data=.x)) %>% 
  map_dfr(~ tidy(.)) %>% 
  as_tibble() %>% 
  mutate(
    sentiment=count(mydata_SA,sentiment)$sentiment[1:10],
    mystar=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),right=FALSE,
               c("***","** ","*  ","+  ","   "))
  ) %>% select(sentiment,estimate1,estimate2,mystar)

# 감정분석 결과
mydata_SA %>%
  full_join(mydata_total_word[,c('rid','no_words')],by='rid') %>% 
  split(.$sentiment) %>% 
  map(~ aov(sent_sum ~ as_factor(treat)*as_factor(demrep7),.)) %>% 
  map_dfr(~ tidy(.)) %>% as_tibble() %>% 
  mutate(
    sentiment=rep(count(mydata_SA,sentiment)$sentiment[1:10],each=4),
    mystar=cut(p.value,c(0,0.001,0.01,0.05,0.10,1),right=FALSE,
               c("***","** ","*  ","+  ","   "))
  ) %>% select(sentiment,term,mystar) %>% 
  print(n=Inf)
