##################################################################
## 군집분석
library('tidyverse')
library('readxl')
setwd("D:/TidyData/data")
# 필요한 라이브러리 
library('cluster')      #clusGap()
library('factoextra')   #군집분석 시각 

# 데이터를 불러오기 
seoul_educ = read_xls("data_student_class.xls",skip=2)
# 데이터 정리
mydata = seoul_educ %>% 
  filter(지역!="합계"&기간==2016) %>% 
  select(기간,지역,starts_with("학급당"))
names(mydata)=c("year","district",str_c("stdt_per_clss",1:4,sep="_"))
mydata2 = mydata[,3:6] # 수치형 데이터만 선택하였음

# 최적의 K를 찾는 방법: 어떤 방법인가에 따라 값이 달라질 수도 있습니다
# 군집내 총분산합을 이용 
fviz_nbclust(mydata2, kmeans, method = "wss")
# 격차 통계치 이용 
gap_stat <- clusGap(mydata2,FUN=kmeans,
                    nstart=50,K.max=10,B=1000)
fviz_gap_stat(gap_stat)

# 군집분석 결과의 시각화(K=4)
kclust4 = kmeans(mydata2,4,nstart=50)
# 구의 이름을 붙임: 티블의 경우 가로줄 이름이 나타나지 않기 때문에
# warning 메시지가 표현될 뿐입니다.  
rownames(mydata2)=mydata$district 
fviz_cluster(kclust4,data=mydata2,geom="text")+
  theme(legend.position="none")+
  ggtitle("")

# 위계적 군집분석의 경우 
# Euclidean distance 기반, Ward’s method로 linkage
hclust = mydata2 %>% 
  dist(., method="euclidean") %>% 
  hclust(., method="ward.D2")

# 덴드로그램을 그리면 다음과 같습니다.
fviz_dend(hclust)
fviz_dend(hclust,k=4)
# 각 군집별로 사각형 표시를 하면 다음과 같습니다.
fviz_dend(hclust,k=4,rect=TRUE,rect_fill=TRUE)
# 덴드로그램 형태를 원형으로 바꾸면 다음과 같습니다.
fviz_dend(hclust,k=4,type="circular")
# 덴드로그램 형태를 계통발생도(진화방향성 그림)로 나타내면 다음과 같습니다.
fviz_dend(hclust,k=4,type="phylogenic")

# 위계적 군집분석 결과를 2차원 평면에 시각화 
hclust4=hcut(mydata2,k=4,
             hc_metric="euclidean",hc_method="ward.D2")
fviz_cluster(hclust4,data=mydata2,geom="text")+
  theme(legend.position="none")+
  ggtitle("")



