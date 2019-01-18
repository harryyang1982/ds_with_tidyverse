##############################################################################
## 연습문제: 군집분석  
library('tidyverse')
library('readxl')
library('cluster')    
library('factoextra') 
setwd("D:/TidyData/data")

tmp1 = read_xls("data_library.xls")
tmp2 = read_xls("data_student_class.xls",skip=2)
# 변수정리 
tmp1 = tmp1 %>%
  filter(자치구 != "합계" & 기간==2014) %>%
  mutate_at(
    vars(계:전문도서관),
    funs(as.double(ifelse(.=="-",0,.)))
  ) %>% select(-기간, -계,-국립도서관)
names(tmp1)=c('district',"lib_public","lib_univ","lib_spe")
tmp2 = tmp2 %>% 
  filter(지역 != "합계" & 기간==2014) %>%
  select(지역,starts_with("학급당"))
names(tmp2)=c('district',str_c("stdnt_clss",1:4))
mydata = full_join(tmp1,tmp2,by="district")

# 군집분석_문제_1:
# 격차 통계치 이용 
mydata2 = mydata[,-1]
gap_stat <- clusGap(mydata2,FUN=kmeans,
                    nstart=50,K.max=10,B=1000)
fviz_gap_stat(gap_stat)

# 군집분석 결과의 시각화(K=2)
kclust2 = kmeans(mydata2,2,nstart=50)
rownames(mydata2)=mydata$district 
fviz_cluster(kclust2,data=mydata2,geom="text")+
  theme(legend.position="none")+
  ggtitle("")

# 군집분석_문제_2
# Euclidean distance 기반, Ward’s method로 linkage
hclust = mydata2 %>% 
  dist(., method="euclidean") %>% 
  hclust(., method="ward.D2")
# 덴드로그램
fviz_dend(hclust,k=2,rect=TRUE,rect_fill=TRUE)
# 혹은 
fviz_dend(hclust,k=2,type="circular")
# 혹은 
hclust2=hcut(mydata2,k=2,
             hc_metric="euclidean",hc_method="ward.D2")
fviz_cluster(hclust2,data=mydata2,geom="text")+
  theme(legend.position="none")+
  ggtitle("")

# # 군집분석_문제_3
mycluster = tibble(
  district=hclust2$labels,
  cluster=hclust2$cluster
  )
# 군집번호 부여 
mydata_cluster = full_join(mydata,mycluster,by='district') 
# 군집별 변수들의 평균
myresult = mydata_cluster %>% 
  group_by(cluster) %>% 
  summarize_at(
    vars(lib_public:stdnt_clss4),
    funs(mean(.))
  ) %>% gather(key=type,value=value,-cluster) %>% 
  mutate(
    library=as.double(str_detect(type,"lib_")),
    cluster=str_c("cluster = ",cluster)
  ) 
g1=ggplot(data=myresult%>%filter(library==1))+
  geom_bar(aes(x=cluster,y=value,fill=type),
           stat="identity",position=position_dodge(width=0.8))+
  labs(x="cluster",y="Averaged number of libraries",
       fill="Library type")+
  scale_fill_discrete(labels=c("Public","Special","University"))+
  theme(legend.position='top')
g2=ggplot(data=myresult%>%filter(library==0))+
  geom_bar(aes(x=cluster,y=value,fill=type),
           stat="identity",position=position_dodge(width=0.8))+
  labs(x="cluster",y="Averaged students per class",
       fill="Institution type")+
  coord_cartesian(ylim=c(18,32))+
  scale_fill_discrete(labels=c("Kindergarten","Elementary","Middle","High"))+
  theme(legend.position='top')  
gridExtra::grid.arrange(g1,g2,ncol=2)  



