setwd("./data")

# Read data from CSV file
country_3 <- read.csv("UE_3lata.csv", header = TRUE, sep = ";", dec = ",")

# Standardize the data
country_3.stand <- cbind(country_3[, 1], scale(country_3[, 2:15]))

# Calculate Euclidean distances
dist.country.eucl <- dist(country_3.stand[, 2:15], method = "euclidean")

# Display rounded distance matrix and minimum distance
round_dist <- round(dist.country.eucl, 2)
min_dist <- min(dist.country.eucl)
cat("Rounded Distance Matrix:\n")
print(round_dist)
cat("Minimum Distance:", min_dist, "\n")

# Hierarchical clustering using single linkage
clusters.country.eucl.single <- hclust(dist.country.eucl, method = "single")
plot(clusters.country.eucl.single, labels = country_3.stand[, 1], 
     main = "Single Linkage Clustering, Euclidean Distance", hang = -1)
rect.hclust(clusters.country.eucl.single, k = 4, border = "red")

# Hierarchical clustering using Ward's method
clusters.country.eucl.ward <- hclust(dist.country.eucl, method = "ward.D")
plot(clusters.country.eucl.ward, labels = country_3.stand[, 1], 
     main = "Ward's Clustering, Euclidean Distance", hang = -1)
rect.hclust(clusters.country.eucl.ward, k = 4, border = "red")






# 2011


country.data=read.csv("UE_2011.csv", header=T,sep=";", dec=",",row.names=1)
names(country.data)
country.data.stand<- scale(country.data[,1:14])
fix(country.data.stand)

a=max(country.data.stand[,1])
b=min(country.data.stand[,2])
c=max(country.data.stand[,3])
d=max(country.data.stand[,4])
e=min(country.data.stand[,5])
f=max(country.data.stand[,6])
g=max(country.data.stand[,7])
h=max(country.data.stand[,8])
i=max(country.data.stand[,9])
j=max(country.data.stand[,10])
k=max(country.data.stand[,11])
l=max(country.data.stand[,12])
m=max(country.data.stand[,13])
n=max(country.data.stand[,14])

model=cbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
print(model)



o=(country.data.stand[,1]-model[,1])^2
p=(country.data.stand[,2]-model[,2])^2
q=(country.data.stand[,3]-model[,3])^2
r=(country.data.stand[,4]-model[,4])^2
s=(country.data.stand[,5]-model[,5])^2
t=(country.data.stand[,6]-model[,6])^2
u=(country.data.stand[,7]-model[,7])^2
v=(country.data.stand[,8]-model[,8])^2
w=(country.data.stand[,9]-model[,9])^2
x=(country.data.stand[,10]-model[,10])^2
y=(country.data.stand[,11]-model[,11])^2
z=(country.data.stand[,12]-model[,12])^2
aa=(country.data.stand[,13]-model[,13])^2
bb=(country.data.stand[,14]-model[,14])^2

distance=cbind(o,p,q,r,s,t,u,v,w,x,y,z,aa,bb)
fix(distance)
distance.vec<-rowSums(distance)^0.5
print(distance.vec)

d<-as.matrix(distance.vec)
print(d)

dev=sd(distance.vec)
tmr=1-distance.vec/(mean(distance.vec)+2*dev)
print(tmr)

TMR<-as.matrix(tmr)
print(TMR)

sort=TMR[order(TMR,decreasing=TRUE),]
sort<-as.matrix(sort)
print(sort)  #2011

country.data<-read.csv("UE_2011.csv",header=T,sep=";", dec=",")
names(country.data)
summary(country.data[,2:15])
country.stand<-cbind(country.data[,1],scale(country.data[,2:15]))
dist.country.eucl<-dist(country.stand[,2:10],method="eucl")
round(dist.country.eucl,2)
min(dist.country.eucl,2)
clusters.country.eucl.single<-hclust(dist.country.eucl,method="single")
plot(clusters.country.eucl.single,labels=country.stand[,1],main="Single linkage method, Euclidean distance")
plot(clusters.country.eucl.single,labels=country.stand[,1],main="Single linkage method, Euclidean distance",hang=-1)



rect.hclust(clusters.country.eucl.single,k=2,border="blue")
rect.hclust(clusters.country.eucl.single,k=3,border="green") 
rect.hclust(clusters.country.eucl.single,k=4,border="red")

country.clust4<-cutree(clusters.country.eucl.single,k=4)
country.clust4
country.clus<-lapply(1:4,function(which.group)country.data[country.clust4==which.group,])

clusters.country.eucl.ward<-hclust(dist.country.eucl,method="ward.D")
plot(clusters.country.eucl.ward,labels=country.stand[,1],main="Ward's method, Euclidean distance")
plot(clusters.country.eucl.ward,labels=country.stand[,1],main="Ward's method, Euclidean distance",hang=-1)
rect.hclust(clusters.country.eucl.ward,k=2,border="blue")
rect.hclust(clusters.country.eucl.ward,k=3,border="green")
rect.hclust(clusters.country.eucl.ward,k=4,border="red")
country.clust4<-cutree(clusters.country.eucl.ward,k=4)
country.clust4
country.clus<-lapply(1:4,function(which.group)country.data[country.clust4==which.group,])
country.clus

# Dla roku 2015

setwd("C:/Users/weron/Desktop/3lata")
country.data=read.csv("UE_2015.csv", header=T,sep=";", dec=",",row.names=1)
names(country.data)
country.data.stand<- scale(country.data[,1:14])
fix(country.data.stand)
fix(country.data.stand)
a=max(country.data.stand[,1])
b=min(country.data.stand[,2])
c=max(country.data.stand[,3])
d=max(country.data.stand[,4])
e=min(country.data.stand[,5])
f=max(country.data.stand[,6])
g=max(country.data.stand[,7])
h=max(country.data.stand[,8])
i=max(country.data.stand[,9])
j=max(country.data.stand[,10])
k=max(country.data.stand[,11])
l=max(country.data.stand[,12])
m=max(country.data.stand[,13])
n=max(country.data.stand[,14])

model=cbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
print(model)



o=(country.data.stand[,1]-model[,1])^2
p=(country.data.stand[,2]-model[,2])^2
q=(country.data.stand[,3]-model[,3])^2
r=(country.data.stand[,4]-model[,4])^2
s=(country.data.stand[,5]-model[,5])^2
t=(country.data.stand[,6]-model[,6])^2
u=(country.data.stand[,7]-model[,7])^2
v=(country.data.stand[,8]-model[,8])^2
w=(country.data.stand[,9]-model[,9])^2
x=(country.data.stand[,10]-model[,10])^2
y=(country.data.stand[,11]-model[,11])^2
z=(country.data.stand[,12]-model[,12])^2
aa=(country.data.stand[,13]-model[,13])^2
bb=(country.data.stand[,14]-model[,14])^2

distance=cbind(o,p,q,r,s,t,u,v,w,x,y,z,aa,bb)
fix(distance)
distance.vec<-rowSums(distance)^0.5
print(distance.vec)

d<-as.matrix(distance.vec)
print(d)

dev=sd(distance.vec)
tmr=1-distance.vec/(mean(distance.vec)+2*dev)
print(tmr)

TMR<-as.matrix(tmr)
print(TMR)

sort=TMR[order(TMR,decreasing=TRUE),]
sort<-as.matrix(sort)
print(sort)   #2015

country.data<-read.csv("UE_2015.csv",header=T,sep=";", dec=",")
names(country.data)
summary(country.data[,2:15])
country.stand<-cbind(country.data[,1],scale(country.data[,2:15]))
dist.country.eucl<-dist(country.stand[,2:10],method="eucl")
round(dist.country.eucl,2)
min(dist.country.eucl,2)
clusters.country.eucl.single<-hclust(dist.country.eucl,method="single")
plot(clusters.country.eucl.single,labels=country.stand[,1],main="Single linkage method, Euclidean distance")
plot(clusters.country.eucl.single,labels=country.stand[,1],main="Single linkage method, Euclidean distance",hang=-1)



rect.hclust(clusters.country.eucl.single,k=2,border="blue")
rect.hclust(clusters.country.eucl.single,k=3,border="green") 
rect.hclust(clusters.country.eucl.single,k=4,border="red")

country.clust4<-cutree(clusters.country.eucl.single,k=4)
country.clust4
country.clus<-lapply(1:4,function(which.group)country.data[country.clust4==which.group,])

clusters.country.eucl.ward<-hclust(dist.country.eucl,method="ward.D")
plot(clusters.country.eucl.ward,labels=country.stand[,1],main="Ward's method, Euclidean distance")
plot(clusters.country.eucl.ward,labels=country.stand[,1],main="Ward's method, Euclidean distance",hang=-1)
rect.hclust(clusters.country.eucl.ward,k=2,border="blue")
rect.hclust(clusters.country.eucl.ward,k=3,border="green")
rect.hclust(clusters.country.eucl.ward,k=4,border="red")
country.clust4<-cutree(clusters.country.eucl.ward,k=4)
country.clust4
country.clus<-lapply(1:4,function(which.group)country.data[country.clust4==which.group,])
country.clus

#2019
country.data=read.csv("UE_2019.csv", header=T,sep=";", dec=",",row.names=1)
names(country.data)
country.data.stand<- scale(country.data[,1:14])
fix(country.data.stand)
a=max(country.data.stand[,1])
b=min(country.data.stand[,2])
c=max(country.data.stand[,3])
d=max(country.data.stand[,4])
e=min(country.data.stand[,5])
f=max(country.data.stand[,6])
g=max(country.data.stand[,7])
h=max(country.data.stand[,8])
i=max(country.data.stand[,9])
j=max(country.data.stand[,10])
k=max(country.data.stand[,11])
l=max(country.data.stand[,12])
m=max(country.data.stand[,13])
n=max(country.data.stand[,14])

model=cbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
print(model)



o=(country.data.stand[,1]-model[,1])^2
p=(country.data.stand[,2]-model[,2])^2
q=(country.data.stand[,3]-model[,3])^2
r=(country.data.stand[,4]-model[,4])^2
s=(country.data.stand[,5]-model[,5])^2
t=(country.data.stand[,6]-model[,6])^2
u=(country.data.stand[,7]-model[,7])^2
v=(country.data.stand[,8]-model[,8])^2
w=(country.data.stand[,9]-model[,9])^2
x=(country.data.stand[,10]-model[,10])^2
y=(country.data.stand[,11]-model[,11])^2
z=(country.data.stand[,12]-model[,12])^2
aa=(country.data.stand[,13]-model[,13])^2
bb=(country.data.stand[,14]-model[,14])^2

distance=cbind(o,p,q,r,s,t,u,v,w,x,y,z,aa,bb)
fix(distance)
distance.vec<-rowSums(distance)^0.5
print(distance.vec)

d<-as.matrix(distance.vec)
print(d)

dev=sd(distance.vec)
tmr=1-distance.vec/(mean(distance.vec)+2*dev)
print(tmr)

TMR<-as.matrix(tmr)
print(TMR)

sort=TMR[order(TMR,decreasing=TRUE),]
sort<-as.matrix(sort)
print(sort)  #2019

country.data<-read.csv("UE_2019.csv",header=T,sep=";", dec=",")
names(country.data)
summary(country.data[,2:15])
country.stand<-cbind(country.data[,1],scale(country.data[,2:15]))
dist.country.eucl<-dist(country.stand[,2:10],method="eucl")
round(dist.country.eucl,2)
min(dist.country.eucl,2)
clusters.country.eucl.single<-hclust(dist.country.eucl,method="single")
plot(clusters.country.eucl.single,labels=country.stand[,1],main="Single linkage method, Euclidean distance")
plot(clusters.country.eucl.single,labels=country.stand[,1],main="Single linkage method, Euclidean distance",hang=-1)



rect.hclust(clusters.country.eucl.single,k=2,border="blue")
rect.hclust(clusters.country.eucl.single,k=3,border="green") 
rect.hclust(clusters.country.eucl.single,k=4,border="red")

country.clust4<-cutree(clusters.country.eucl.single,k=4)
country.clust4
country.clus<-lapply(1:4,function(which.group)country.data[country.clust4==which.group,])

clusters.country.eucl.ward<-hclust(dist.country.eucl,method="ward.D")
plot(clusters.country.eucl.ward,labels=country.stand[,1],main="Ward's method, Euclidean distance")
plot(clusters.country.eucl.ward,labels=country.stand[,1],main="Ward's method, Euclidean distance",hang=-1)
rect.hclust(clusters.country.eucl.ward,k=2,border="blue")
rect.hclust(clusters.country.eucl.ward,k=3,border="green")
rect.hclust(clusters.country.eucl.ward,k=4,border="red")
country.clust4<-cutree(clusters.country.eucl.ward,k=4)
country.clust4
country.clus<-lapply(1:4,function(which.group)country.data[country.clust4==which.group,])
country.clus

