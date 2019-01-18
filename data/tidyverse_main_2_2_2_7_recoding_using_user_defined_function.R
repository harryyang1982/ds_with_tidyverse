################################################################
## # 개인함수를 이용하여 변수 리코딩
data_131 = read_spss("data_TESS3_131.sav")
refused_to_missing=function(myvariable){
  ifelse(myvariable==-1,NA,myvariable)
}

# 데이터 불러오기 
data_131 = read_spss("data_TESS3_131.sav")
# 개인함수를 이용해 결측값 처리 
data_131 %>% 
  mutate(
    ideo2=refused_to_missing(IDEO)
  ) %>% 
  count(IDEO,ideo2)

# 더블형 변수에 모두 적용한다면 mutate_if() 함수를 이용하면 매우 편리 
data_131_2 = data_131 %>% 
  mutate_if(
    is.double, # 더블형에 대해서만 적용
    funs(refused_to_missing) # 적용되는 함수(funs) 설정
  )
data_131_2  %>% 
  count(IDEO)

data_131_2  %>% 
  count(Q1)

# -1을 결측값으로 처리한 후 
# 7점척도로 측정된 변수를 3개 집단을 나타내는 명목변수로 변환
seven_to_three_collapse=function(myvariable){
  # 1단계
  myvariable=ifelse(myvariable==-1,NA,myvariable)
  # 2단계
  myvariable=cut(myvariable,c(0,3,4,7),1:3)
}

# Q1 변수에 적용 
data_131 %>% 
  mutate(
    Q1_3=seven_to_three_collapse(Q1)
  ) %>% 
  count(Q1,Q1_3)

# 변수들 중 일부만 적용하려면 mutate_at() 함수를 이용
data_131_2 = data_131 %>% 
  mutate_at(
    vars(Q1, Q2, Q3, Q4, IDEO, PARTY7),
    funs(seven_to_three_collapse)
  ) 
data_131_2 %>% count(Q3)

