####################################################################
# 티블 데이터 직접입력 
# 변수지정 
name = c('연돌이','세순이')
born = c("1999-3-2","1999-3-3")
year = c(2L,4L)
grade = c('A+','A-')
height = c(178,170)
# 티블 데이터 설정 
library('tidyverse')
my_first_tibble = tibble(name,born,year,grade,height)
my_first_tibble

# 각주: born 변수를 <date> 형식으로 저장하는 법 
born = lubridate::as_date(born)  # lubridate 라이브러리를 구동하지 않고 내부 함수활용 
tibble(name,born,year,grade,height)

# 변수이름을 붙이는데 tibble 데이터가 더 자유롭다. 
tibble(
  name=c("Jake","Jessy","Jack"),
  `^__^` = c("used","used","not used"),
  `ㅠ.ㅠ` = c("used","not used","used"),
  `Emotion status?` = c("ambivalence","happy","sad")
)

data.frame(
  name=c("Jake","Jessy","Jack"),
  `^__^` = c("used","used","not used"),
  `ㅠ.ㅠ` = c("used","not used","used"),
  `Emotion status?` = c("ambivalence","happy","sad")
)