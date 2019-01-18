##############################################################################
## 측정의 신뢰도와 타당도 
library('tidyverse')
setwd("D:/TidyData/data")

# 측정의 신뢰도와 타당도  
# 데이터를 불러오기 
mydata = read_csv("data_survey_comma.csv") %>% 
  select(starts_with("SWL"),starts_with("SCA"),
         starts_with("SCB")) 

# 각주: 주성분 개수 결정 
# 카이저 규칙 
myeigen=prcomp(mydata, scale=TRUE)$sdev^2 
myeigen[myeigen>1]
# 스크리 플롯 
myeigen %>% as_tibble() %>% 
ggplot(aes(x=1:16,y=value))+
  geom_line(color='red')+
  labs(x="principal component",y="Eigen-value")+
  scale_x_continuous(breaks=1:16)
# 평행분석 
psych::fa.parallel(cor(mydata),n.obs=331,cor="cor",plot=T)
# 이론적 결정: 측정항목을 연구자가 이론적 관점에 따라 설정했기 때문에 

# 주성분분석(PCA)
my_PCA1 = mydata %>% 
  psych::principal(.,3,rotate="promax")  #default = varimax
my_PCA1

# 각주: SCB7,SCB11 두 항목들 제거후 주성분분석 실시 
my_PCA2 = mydata %>% 
  select(-SCB7,-SCB11) %>% 
  psych::principal(.,nfactors=3,rotate="promax") %>% 
my_PCA2

# 주성분분석 정리결과 
PCA_summary_function = function(my_PCA_result, mydigit){
  # 아이겐값과 각 아이겐값이 차지하는 분산비 및 누적분산비 
  eigenvalues = matrix(NA,nrow=3,ncol=my_PCA_result$factors)
  for (i in 1:my_PCA_result$factors){
    eigenvalues[1,i] = sum(my_PCA_result$loadings[,i]^2)
    eigenvalues[2,i] = eigenvalues[1,i]/dim(my_PCA_result$loadings[,])[1]
  } 
  eigenvalues[3,] = cumsum(eigenvalues[2,])
  # 적절하게 이름을 붙이고 
  rownames(eigenvalues)=c("eigenvalue",
                          "var_explained","cum_var_explained")
  # 적재치 결과와 합쳐서 제공함 
  round(rbind(my_PCA_result$loadings[,], eigenvalues),mydigit)
}
# 가로줄 이름을 저장해야 하기 때문에 R 베이스의 write.csv() 사용
write.csv(PCA_summary_function(my_PCA1,2),
          "PCA_result_summary.csv",row.names=T)

# 결과의 시각화: 측정문항 
myfigure = my_PCA1$loadings[,] %>% 
  as_tibble() %>% 
  mutate(
    vars=row.names(my_PCA1$loadings[,]),
    var_type=str_extract(vars,"[[:alpha:]]{3}")
  )
# 시각화(2차원 그래프를 3개 제시하였음)
g12 = ggplot()+
  geom_text(data=myfigure,
            aes(x=RC1,y=RC2,label=vars,color=var_type))+
  coord_cartesian(ylim=c(-1,1),xlim=c(-1,1))+
  labs(x="Component 1",y="Component 2",color="Question type")+
  theme(legend.position="none")
g13 = ggplot()+
  geom_text(data=myfigure,
            aes(x=RC1,y=RC3,label=vars,color=var_type))+
  coord_cartesian(ylim=c(-1,1),xlim=c(-1,1))+
  labs(x="Component 1",y="Component 3",color="Question type")+
  theme(legend.position="none")
g23 = ggplot()+
  geom_text(data=myfigure,
            aes(x=RC2,y=RC3,label=vars,color=var_type))+
  coord_cartesian(ylim=c(-1,1),xlim=c(-1,1))+
  labs(x="Component 2",y="Component 3",color="Question type")+
  theme(legend.position="none")
gridExtra::grid.arrange(g12,g13,g23,ncol=2)

# 각주: 측정사례 
myfigure2 = as_tibble(my_PCA1$scores)
ggplot(data=myfigure,aes(x=RC1,y=RC2,label=vars))+
  geom_text()+
  geom_point(data=myfigure2)

# 문항과 사례를 동시에 제시하는 경우(1번과 2번 주성분만)
ggplot()+
  geom_point(data=myfigure2,aes(x=RC1,y=RC2),alpha=.1)+
  geom_text(data=myfigure, # myfigure의 생성방법은 본문에 설명제시
            aes(x=RC1,y=RC2,label=vars,color=var_type),
            size=3)+
  coord_cartesian(ylim=c(-3,3),xlim=c(-3,3))+
  labs(x="Component 1",y="Component 2",color="Question type")+
  theme(legend.position="top")

# 최소잔차(minres) 탐색적 인자분석(EFA)
my_FA1 = mydata %>% 
  psych::fa(.,nfactors=3,rotate="promax") 
my_FA1 

# 함수생성(구성형태가 동일하기 때문에 그대로 사용해도 됩니다)
EFA_summary_function = PCA_summary_function
write.csv(cbind(PCA_summary_function(my_PCA1,2),EFA_summary_function(my_FA1,2)),
          "PCA_versus_PCA.csv",row.names=T)

# KMO sampling adequacy
mydata %>% 
  cor() %>% 
  psych::KMO()
# Bartlett's test of sphericity
mydata %>% 
  cor() %>% 
  psych::cortest.bartlett(.,n=331)

######################################################
# Cronbach's alpha 
mydata %>% 
  select(starts_with("SWL")) %>% 
  psych::alpha() %>% 
  summary() 

mydata %>% 
  select(starts_with("SCB")) %>% 
  psych::alpha() %>% 
  summary()

mydata %>% 
  select(starts_with("SCB")) %>% 
  select(-SCB7,-SCB11) %>% 
  psych::alpha() %>% 
  summary()

# Loevinger's coefficient H 
library("haven")
gss_panel = read_dta("data_gss_panel06.dta")
mydata = gss_panel %>% 
  select(starts_with("suicide")) %>% 
  select(ends_with("_1")) %>% 
  drop_na() %>% 
  mutate_all(
    funs(ifelse(.==1,1,0))
  )
mokken::coefH(as.data.frame(mydata))$H
psych::alpha(mydata)$total %>% round(.,2)
