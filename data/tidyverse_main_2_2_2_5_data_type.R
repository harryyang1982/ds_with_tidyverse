################################################################
## 데이터의 타입전환: character, factor, integer, double
# 명목변수: as.factor(), as_factor(), as.character() 함수
temporary = data_131 %>% 
  mutate(
    # dbl+lbl 중에서 더블형 관측값을 요인형 데이터로
    sex_fct1=as.factor(PPGENDER),
    # dbl+lbl 중에서 더블형 관측값을 문자형 데이터로
    sex_chr1=as.character(PPGENDER),
    # dbl+lbl 중에서 라벨값을 요인형 데이터로
    sex_fct2=as_factor(PPGENDER),
    # dbl+lbl 중에서 라벨값을 요인형 데이터로 바꾼 후 문자형 데이터로 
    sex_chr2=as.character(as_factor(PPGENDER)),
    # dbl+lbl 중에서 라벨값을 요인형 데이터로 바꾼 후, 
    # 문자형 데이터로 바꾸고 다시 요인형 데이터로 
    sex_fct3=as.factor(as.character(as_factor(PPGENDER)))
  ) 
temporary %>% count(sex_fct1)
temporary %>% count(sex_fct2)
temporary %>% count(sex_fct3)
temporary %>% count(sex_chr1)
temporary %>% count(sex_chr2)

# 순위변수 혹은 등간변수: as.integer(), as.double() 함수 
temporary = tibble(
  x1=as.integer(1:3),
  x2=as.double(1:3),
  x3=as.double(1+0.1*(1:3))
)
temporary 
temporary %>% 
  mutate(
    x1.dbl=as.double(x1),
    x2.int=as.integer(x2),
    x3.int=as.integer(x3)
  )

# 문자형 데이터로 리코딩
temporary %>% 
  mutate(
    x2.chr=as.factor(x2),
    x3.chr=as.character(x3)
  )

# 7점 리커트 척도로 측정된 정치적 성향 변수가 문자형/요인형으로 입력된 경우
temporary = data_131 %>% 
  filter(IDEO>0) %>% #Refused 응답은 걸러냄
  mutate(
    ideo_chr=as.character(as_factor(IDEO)), # 문자형으로
    ideo_fct=as_factor(IDEO)  # 요인형으로
  ) %>% 
  select(ideo_chr,ideo_fct)
temporary

# 두변수를 각각 더블형으로 변환후 어떻게 변환되었는지 체크 
temporary = temporary %>% 
  mutate(
    ideo_chr_dbl=as.double(ideo_chr),
    ideo_fct_dbl=as.double(ideo_fct)
  ) 
temporary
temporary %>% count(ideo_fct, ideo_fct_dbl)

# ideo_fct 변수의 수준들이 어떤 순서를 갖는지 체크
# %$%를 사용한 것 주의(행렬 데이터가 아닌 변수단위인 경우 사용하는 파이프 오퍼레이터)
temporary %$% fct_unique(ideo_fct) 

# 문자형 -> 요인형 -> 더블형
temporary %>% 
  mutate(
    ideo_chr_fct=as.factor(ideo_chr), #as_factor()를 써도 무방 
    ideo_chr_fct_dbl=as.double(ideo_chr_fct)
  ) %>% 
  count(ideo_chr_fct, ideo_chr_fct_dbl)

# 원하는 텍스트 표현에 원하는 수치를 부여
temporary %>% 
  mutate(
    ideo_chr_dbl=NA,
    ideo_chr_dbl=ifelse(ideo_chr=="Extremely liberal",1,ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Liberal",2,ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Slightly liberal",3,ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Moderate, middle of the road",4,ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Slightly conservative",5,ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Conservative",6,ideo_chr_dbl),
    ideo_chr_dbl=ifelse(ideo_chr=="Extremely conservative",7,ideo_chr_dbl)
  ) %>% 
  count(ideo_chr,ideo_chr_dbl)