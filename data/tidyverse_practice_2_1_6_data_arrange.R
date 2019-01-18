#######################################################################
## 문제영역: 데이터 정렬 
# 데이터정렬_문제_1
world_country = read_xlsx("data_country.xlsx")
world_country %>% 
  arrange(POPULATION)
world_country %>% 
  arrange(desc(POPULATION))