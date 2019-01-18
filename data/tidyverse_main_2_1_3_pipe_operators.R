####################################################################
# 파이프 오퍼레이터 이해 
library('readxl')
read_xls("data_library.xls") %>% 
  count(기간)

# 전통적 방식
seoul_library = read_xls("data_library.xls")  #단계1
count(seoul_library, 기간)  #단계2

# 시각적으로 잘 구조화할 것
# 좋은 예
my_recoding_function = function(myvariable){
  if (is_character(myvariable)) {
    myvariable=as.numeric(recode(myvariable,"-"="0"))
  } else {
    myvariable=as.numeric(myvariable)
  }
}

seoul_library %>% 
  filter(기간==2010) %>% 
  select(ends_with("도서관")) %>% 
  mutate_at(
    vars(ends_with("도서관")),
    my_recoding_function
  ) %>% 
  summarize_all(mean)

# 좋지 않은 예
seoul_library %>% 
  filter(기간==2010) %>% 
  select(ends_with("도서관")) %>% 
  mutate_at(vars(ends_with("도서관")),
            function(myvariable){
              if (is_character(myvariable)) 
              {myvariable=as.numeric(recode(myvariable,"-"="0"))} 
              else {myvariable=as.numeric(myvariable)}}) %>% 
  summarize_all(mean)