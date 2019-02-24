library(tidyverse)
library(haven)
library(readxl)

data_131 <- read_spss("data/data_TESS3_131.sav")
mydata <- data_131 %>% 
  mutate_if(
    is.double,
    funs(ifelse(. < 0, NA, .))
  )
mydata

mydata %>% 
  count(PPGENDER)

mydata <- mydata %>% 
  mutate(
    female=labelled(PPGENDER, c(남성=1, 여성=2))
  )

mydata %>% 
  count(as_factor(female))

ggplot(mydata, aes(x=as_factor(female))) +
  geom_bar(aes(fill = as_factor(female)))

mydata %>% 
  ggplot(aes(x=as_factor(female))) +
  geom_bar() +
  labs(x="응답자 성별", 
       y="빈도수") +
  coord_cartesian(ylim=c(280, 320))

# mydata %>% 
#   ggplot(aes(x=as_factor(female))) +
#   geom_bar() +
#   labs(x="응답자 성별", 
#        y="빈도수") +
#   ylim(280, 320)

# 범주형 변수의 수준이 많을 경우 빈도분석

mydata <- mydata %>% 
  mutate(
    religion = as.character(as_factor(data_131$REL1))
  )


mydata %>% count(religion)

myresult <- mydata %>% count(religion)
ggplot(myresult, aes(x=religion, y=n)) +
  geom_col() +
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents")

ggplot(myresult, aes(x=religion, y=n)) +
  geom_col() +
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents") +
  coord_flip()

myresult <- myresult %>% 
  mutate(
    religion=ifelse(str_detect(religion, "Protestant"), "Protestants", religion)
  )

myresult %>% 
  ggplot(aes(x=religion, y=n)) +
  geom_col() +
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents") +
  coord_flip()

myresult <- myresult %>% 
  mutate(
    religion=fct_reorder(religion, n, "mean")
  )

myresult %>% 
  ggplot(aes(x=religion, y=n)) +
  geom_col() +
  labs(x="Respondents' religions (including 'Refused' & 'None')",
       y="Number of respondents") +
  coord_flip()

mydata <- mydata %>% 
  mutate(
    libcon3 = as.double(cut(IDEO, c(0, 3, 4, Inf), 1:3)),
    libcon3 = labelled(libcon3, c(진보=1, 중도=2, 보수=3))
  )

mydata %>% 
  count(libcon3)

mydata %>% 
  count(as_factor(female), as_factor(libcon3))

myresult <- mydata %>% 
  count(as_factor(female), as_factor(libcon3)) %>% 
  drop_na()

myresult %>% 
  spread(key=`as_factor(libcon3)`, value=n)

ggplot(myresult, aes(x=`as_factor(female)`, y=n,
                     fill=`as_factor(libcon3)`)) +
  geom_col() +
  labs(x="응답자의 성별", 
       y="빈도수",
       fill="정치적 성향")

ggplot(myresult, aes(x=`as_factor(female)`, y=n,
                     fill=`as_factor(libcon3)`)) +
  geom_col(position = "dodge") +
  labs(x="응답자의 성별", 
       y="빈도수",
       fill="정치적 성향")

ggplot(myresult, aes(x=`as_factor(libcon3)`, y=n,
                     fill=`as_factor(female)`)) +
  geom_col(position = "dodge") +
  labs(x="정치적 성향", 
       y="빈도수",
       fill="응답자의 성별")

myresult %>% 
  spread(`as_factor(libcon3)`, n) %>% 
  mutate_if(
    is.integer,
    funs(100*(./sum(.)))
  )

ggplot(myresult, aes(x=`as_factor(libcon3)`, y=n,
                     fill=`as_factor(female)`)) +
  geom_col(position = "fill") +
  labs(x="정치적 성향", y="응답자 비율", fill="응답자의 성별")

ggplot(myresult, aes(x=`as_factor(libcon3)`, y=n,
                     fill=`as_factor(female)`)) +
  geom_col(position = "fill") +
  labs(x="정치적 성향", y="응답자 비율", fill="응답자의 성별") +
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5), "%", sep="")) +
  theme(legend.position="top")

mydata <- mydata %>% 
  mutate(
    white=ifelse(PPETHM==1, 1, 0),
    white=labelled(white, c(다수인종=1, 소수인종들=0))
  )

myresult <- mydata %>% 
  count(as_factor(white), as_factor(female), as_factor(libcon3))

myresult %>% 
  drop_na() %>% 
  spread(`as_factor(libcon3)`, value=n)

myresult %>% 
  drop_na() %>% 
  group_by(`as_factor(white)`) %>% 
  spread(`as_factor(libcon3)`, n) %>% 
  mutate_if(
    is.integer,
    funs(100*(./sum(.)))
  )

myresult %>% 
  drop_na() %>% 
  ggplot(aes(x=`as_factor(female)`, 
             y=n,
             fill=`as_factor(libcon3)`)) +
  geom_col(position="fill")+
  labs(x="응답자의 성별",
       y="응답자 퍼센트",
       fill="정치적 성향") +
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5), "%", sep="")) +
  theme(legend.position="top") +
  facet_grid(~`as_factor(white)`)

myresult %>% 
  drop_na() %>% 
  ggplot(aes(x=`as_factor(female)`, 
             y=n,
             fill=`as_factor(libcon3)`)) +
  geom_col(position="fill")+
  labs(x="응답자의 성별",
       y="응답자 퍼센트",
       fill="정치적 성향") +
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5), "%", sep="")) +
  theme(legend.position="top") +
  facet_wrap(~`as_factor(white)`)

mydata <- mydata %>% 
  mutate(
    gen2=as.double(cut(PPAGE, c(0, 50, 99), 1:2)),
    gen2=labelled(gen2,
                  c("저연령(50세 이하)"=1, 
                    "고연령(51세 이상"=2))
  )

myresult <- mydata %>% 
  count(as_factor(gen2), as_factor(white),
        as_factor(female), as_factor(libcon3))

myresult %>% 
  drop_na() %>% 
  ggplot(aes(x=`as_factor(female)`,
             y=n,
             fill=`as_factor(libcon3)`)) +
  geom_col(position="fill") +
  labs(x="응답자의 성별",
       y="응답자 퍼센트",
       fill="정치적 성향") +
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5),"%", sep="")) +
  theme(legend.position="top") +
  facet_grid(`as_factor(gen2)`~`as_factor(white)`)

myresult %>% 
  drop_na() %>% 
  ggplot(aes(x=`as_factor(female)`,
             y=n,
             fill=`as_factor(libcon3)`)) +
  geom_col(position="fill") +
  labs(x="응답자의 성별",
       y="응답자 퍼센트",
       fill="정치적 성향") +
  scale_y_continuous(breaks=0.2*(0:5),
                     labels=str_c(20*(0:5),"%", sep="")) +
  theme(legend.position="top") +
  facet_wrap(~`as_factor(gen2)`+`as_factor(white)`)

ggsave("freq_gender_ideo_race_generation.jpeg",
       width=16, height=16, units="cm")
