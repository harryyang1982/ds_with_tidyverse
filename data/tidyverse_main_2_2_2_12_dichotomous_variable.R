###############################################################
# 이분변수로 리코딩 
data_131 = read_spss("data_TESS3_131.sav")
# female이라는 이름의 이분변수 생성 
data_131 %>% 
  mutate(
    female=ifelse(PPGENDER==2,1,0)
  ) %>% 
  count(female)
# ifelse() 함수인 경우 결측값 존재 여부에 주의
data_131 %>% 
  mutate(
    Q5_A=ifelse(Q5==2,1,0),
    Q5_B=ifelse(ifelse(Q5==-1,NA,Q5)==2,1,0)
  ) %>% 
  count(Q5_A,Q5_B)

# 다수인종 vs. 소수인종들을 구분하는 이분변수
data_131 %>% 
  mutate(
    white=ifelse(PPETHM==1,1,0)
  ) %>% 
  count(PPETHM,white)

# 순위변수, 등간변수를 이분변수로 
data_131 %>% 
  mutate(
    senior=ifelse(PPAGE >= 65,1,0)
  ) %>% 
  group_by(senior) %>% 
  summarize(min(PPAGE),max(PPAGE))

# 범위를 기준으로 이분변수 변환
data_131 %>% 
  mutate(
    # 원하는 범위를 선정
    pol.middle=ifelse(IDEO >= 3 & IDEO <= 5,1,0),
    # 응답거부인 경우 결측값 처리
    pol.middle=ifelse(IDEO==-1,NA,pol.middle)
  ) %>% 
  count(IDEO,pol.middle)
