install.packages("flexclust")
install.packages("NbClust")
install.packages("tidyverse")
library(flexclust)
library(NbClust)
library(tidyverse)


# pca

# IRIS데이터에서 연속형 변수만 선택
df <- iris[, -5]


#변수간 상관관계를 확인해보자
cor(df)

#주성분분석 함수를 실행하고 summary를 통해 확인해보자
prcomp_df = prcomp(df)

#새로 생성된 주성분은 각 변수의 선형계수와 측정값을 곱한 값입니다
prcomp_df

# standard deviation은 표준편차 proportion of variance는 분산비율(각 변수의 공헌도), cumulative proportion 누적분산비율(누적공헌도)
summary(prcomp_df) 

#데이터프레임을 행렬로 바꾸고 주성분 매트릭스를 곱하면 새로운 테이블이 나옵니다
pca_iris<- as.matrix(df)%*%prcomp_df$rotation
as.data.frame(pca_iris)
head(pca_iris)

# 변수의 갯수만큼 차원이 만들어진다. 그 중 누적공헌도가 높은 차원을 pc n개까지 취향에 따라 선택하세요

screeplot(prcomp_df, type="lines", pch=1, main="scree plot")



#Clustering

#내장 데이터셋 불러오기
data(nutrient,package='flexclust')

#확인
head(nutrient)

# 관측치간의 거리 구하기
d=as.matrix(dist(nutrient))

head(d)[1:4,1:4]

# 클러스터링은 개체간의 거리가 중요
# 그러기에 각 속성간 단위가 다르면 클러스터링이 어려우므로
# 하기전에 무조건 스케일링!

nutrient_scaled=scale(nutrient)

#스케일링 된 거리척도를 통해 각 관측치간에 거리 구하기
d=dist(nutrient_scaled)


# 군집화 방법 1. 계층적 클러스터링

# 1-1) 평균 연결법


fit_average=hclust(d,method='average')

plot(fit_average,hang=1,cex=.8,main='평균 연결법')

# 1-2) 최장 연결법

fit_complete=hclust(d,method='complete')

plot(fit_complete,hang=1,cex=.8,main='최장 연결법')

# 1-3) 최단 연결법

fit_single=hclust(d,method='single')

plot(fit_single,hang=1,cex=.8,main='최단 연결법')


# Nbclust -- 최적의 군집수를 자동으로 정해주는 알고리즘

# 스케일링된 데이터를 토대로 거리척도와 최대 군집, 최소군집수를 주면 알아서 구해줌
nc=NbClust(nutrient_scaled,distance = 'euclidean',
           min.nc=2,max.nc=15,method='average')

par(mfrow=c(1,1))


# 지표에 따르면  2, 3, 5, 15가 최적의 군집수라는 결론에 이름
t1=table(nc$Best.n[1,]);t1

barplot(t1,xlab='Number of Clusters',ylab='Number of Criteria')


# cutree
# 계층적 클러스터링으로 생성된 모델을 통해 군집수 설정해서 클러스터 생성하기

clusters_1=cutree(fit_average,k=5)

#원 데이터 프레임과 결합
nutrient_1=cbind(nutrient,clusters_1)


table(clusters_1)

ggplot(nutrient_1,aes(x=nutrient_1$energy,y=nutrient_1$protein,col=factor(nutrient_1$clusters_1)))+geom_point(size=3)

#클러스터 간의 피쳐별 중앙값
aggregate(nutrient,by=list(cluster=clusters_1),median)

par(mfrow=c(1,1))

plot(fit_average,hang=-1,cex=.8,main='Average Lingkage')


rect.hclust(fit_average,k=4)



wssplot=function(data,nc=15,seed=1234){
  wss=(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]=sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type='b',xlab='number of clusters',
       ylab='within groups sum of squares')
}

# 그래프 보면 군집이 5개 이상으로 가면 군집간의 분산이 그렇게 낮아지지 않음을 알수 있음.
wssplot(nutrient)


# kmeans 알고리즘

nc2=NbClust(nutrient,min.nc=2,max.nc=15,method='kmeans')

fit_km=kmeans(nutrient,4)

aggregate(nutrient,by=list(fit_km$cluster),mean)


#그래프로 시각화
ggplot(nutrient,aes(x=nutrient$energy,y=nutrient$protein,col=factor(fit_km$cluster)))+geom_point(size=3)



