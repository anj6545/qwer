### 군집분석 Ward 방법 ###
attach(data2)
x <- data2[,2:9]
dx<-round(dist(x),digits = 2)
dx
hc = hclust(dist(x)^2, method='ward.D')
hc
summary(hc)
plot(hc,labels=idx, hang = -1)


hc.result = cutree(hc,k=10000)
plot(x,pch=hc.result)
hc.result
length(unique(hc.result))

### 군집 중심 개수 결정 - mclust ###
memory.limit(size=128000)
gc()
library(mclust)
data_mc = Mclust(x)
data_mc
data_mc$classification

### NbClust - 용량 부족 ###
library(NbClust)
y <- x[1:7000,]
nc <- NbClust(x, min.nc=2, max=5, method="kmeans")
nc=NbClust(x,distance="euclidean",min.nc=2,max.nc=15, method="average")
res<-NbClust(y, distance = "euclidean", min.nc=2, max.nc=5, 
             method = "ward.D", index = "all")

### K-means ###

data_k <- kmeans(x, centers=198)
length(unique(data_k$cluster))
ccent = function(y, cl){
  f=function(i)
}
ccent(x, data_k$cluster)

### 집단내 제곱합 그래프 그리는 함수 - 급격한 경사가 있는 부분에서 군집 개수 결정 ###
wssplot <- function(data, nc=100, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")} 
df<-scale(x)
wssplot(df)
