
# 변수(가로줄) 단위 데이터 입력
country=c("Japan","Switzerland","Singapore","Spain",
          "Australia","Italy","Iceland","Israel",
          "France","Sweden","South Korea")
BR=c(1,2,3,4,4,6,6,8,9,9,11)
BLE=c(83.7,83.4,83.1,82.8,82.8,82.7,82.7,82.5,82.4,82.4,82.3)
FR=c(1,6,2,3,7,7,10,9,5,12,3)
FLE=c(86.8,85.3,86.1,85.5,84.8,84.8,84.1,84.3,85.4,84.0,85.5)
MR=c(6,1,10,9,3,6,2,5,16,4,20)
MLE=c(80.5,81.3,80.0,80.1,80.9,80.5,81.2,80.6,79.4,80.7,78.8)

# 데이터프레임 함수로 저장
mydata=data.frame(country,BR,BLE,FR,FLE,MR,MLE)
mydata 
str(mydata)

# cbind() 함수이용
mymat=cbind(country,BR,BLE,FR,FLE,MR,MLE)
mymat
str(mymat)

# mydata, mymat 비교 
class(mydata)
class(mymat)

# country 변수 제외 후 cbind() 함수
mymat2=cbind(BR,BLE,FR,FLE,MR,MLE)
mymat2
str(mymat2)

# 인덱싱: 1-5번 가로줄
mydata[1:5,]
# 인덱싱: 1,2,4,6번 세로줄
mydata[,c(1,2,4,6)]
# 인덱싱: 가로줄 세로줄 동시에 선정하여 추출
mydata[1:5,c(1,2,4,6)]

## is.*() 함수 
is.data.frame(mydata)  # data.frame?
is.matrix(mydata)  # matrix?
is.data.frame(mymat)  # data.frame?
is.matrix(mymat) # matrix?

is.character(country)  # 문자형 데이터인 범주형 변수? 
is.character(BR)  # 문자형 데이터인 범주형 변수? 
is.character(BLE)  # 문자형 데이터인 범주형 변수? 

is.numeric(country)  # 수치형 데이터인 연속형 변수?
is.numeric(BR)  # 수치형 데이터인 연속형 변수?
is.numeric(BLE)  # 수치형 데이터인 연속형 변수?

is.integer(BR)  # 정수형 데이터인 연속형 변수?
is.double(BR)  # 더블형 데이터인 연속형 변수? 
is.integer(BLE)  # 정수형 데이터인 연속형 변수?
is.double(BLE)  # 더블형 데이터인 연속형 변수? 

is.integer(c(1))  # 정수형이 아닌 더블형으로 인식
is.integer(c(1L))  #1L이라고 해야 정수형으로 인식

# as.*() 함수 
BR2 = as.integer(BR)
is.integer(BR2)

# 더블형을 정수형으로 변환하면 문제가 발생한다. 
BLE2 = as.integer(BLE)  #정수형으로 전환한 더블형 데이터
BLE3 = as.double(BLE2)  #더블형 -> 정수형 -> 더블형
data.frame(BLE,BLE2,BLE3)

# 문자형을 더블형으로?
as.double(country)

# 더블형을 문자형으로?
as.character(BR)

# 문자형 데이터를 요인형 데이터로
as.factor(country)

# 문자형 -> 요인형 -> 더블형
as.double(as.factor(country))

# 정수형 -> 요인형
as.factor(as.integer(BR))

# 정수형 -> 요인형 -> 정수형
as.integer(as.factor(BR))

# 더블형 -> 요인형
as.factor(BLE)
# 더블형 -> 요인형 -> 더블형 
as.double(as.factor(BLE))

# "더블형 -> 요인형" 후 원래 모습을 회복하려면: 문자형 -> 더블형 
as.double(as.character(as.factor(BLE)))

# 변수 길이 계산 
length(country)

# 경우에 따라 결측값을 고려해 주어야 한다. 
country2=c(country,NA)
country2

length(country2)  # NA가 포함된 길이계산 
length(country[is.na(country2)])  # NA의 개수 계산 
length(country[!is.na(country2)])  # NA를 배제한 실측값의 개수 계산

# 평균을 구해봅시다. 
mean(BLE);mean(FLE);mean(MLE)

# 결측값 부여
MLE2=MLE
MLE2[1]=NA
# 평균을 구해봅시다. 
mean(BLE);mean(FLE);mean(MLE2)

# 평균을 구해봅시다. 
mean(BLE,na.rm=TRUE);mean(FLE,na.rm=TRUE)
mean(MLE,na.rm=TRUE);mean(MLE2,na.rm=TRUE)

# 가로줄 기준으로 변수들의 평균 구하기
mydata$FMLE=rowMeans(mydata[,c("FLE","MLE")]) # mydata[,c(5,7)]로 해도 됨
mydata

# 가로줄 기준으로 변수들의 합산 구하기
mydata$FMR=rowMeans(mydata[,c("FR","MR")]) # mydata[,c(4,6)]로 해도 됨
mydata

# 밑이 2인 로그
mynumbers=c(1,2,4,8,16)
mynumbers
log2(mynumbers)
# 다음과 같은 방식으로 원 값을 다시 얻을 수 있음
2^log2(mynumbers)

# 밑이 10인 로그: 10진법일 때 많이 사용
mynumbers=c(1,10,100,1000,10000)
mynumbers
log10(mynumbers)
# 다음과 같은 방식으로 원 값을 다시 얻을 수 있음
10^log10(mynumbers)

# 자연로그: 일반적으로 많이 사용함
mynumbers=11:15
mynumbers
log(mynumbers)
# 다음과 같은 방식으로 원 값을 다시 얻을 수 있음
exp(log(mynumbers))


