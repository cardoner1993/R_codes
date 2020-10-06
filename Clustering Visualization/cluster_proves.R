library(readr)
library(tidyr)
library(dplyr)
US_Dexma <- read_delim("C:/Users/David/Desktop/US_energy_Dexma_test.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)

##Separate the data.
USDexma<-spread(US_Dexma,key = year,value = price)

#################################CLUSTER##############################################################
library(ggplot2)
library(cluster)
library(dendextend)
library(caret)
library(gridExtra)
library(grid)
library(ggthemes)
library(RColorBrewer)
library(ggdendro)
library(factoextra)
library(NbClust)

#Without others
USDexma %>% filter(sector!="Other") %>% 
ggplot(.,aes(substr(state,1,4),VarPr,color=as.factor(sector))) + geom_point()

##################################################
##ELBOW method
######################################################
#lets find the number of clusters visually.

numvars<-USDexma[,sapply(USDexma,function(x){is.numeric(x)})]

k.max<-15
wss <- sapply(1:k.max, 
              function(k){kmeans(numvars, k, nstart=50,iter.max = 15 )$tot.withinss})
wss 
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

catvar<-USDexma %>% select_if(is.character) %>% mutate_all(as.factor)

##Dissimilarity Matrix Calculation
####### Dendogram with euclidean distance and agregation with Ward

dd <- dist(scale(numvars), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE) + 
  xlab('') + ylab('') + 
  theme_fivethirtyeight() +
  ggtitle('Dendograma') + 
  theme(axis.text.x = element_text(size=7))

list_plot<-list()
for(i in 2:6){
  grp<-cutree(hc, k=i)
  list_plot[[i-1]]<-fviz_cluster(list(data = numvars, cluster = grp)) + 
    ggtitle('')
}
grid.arrange(list_plot[[2]],list_plot[[3]],list_plot[[4]],list_plot[[5]],ncol=2)
list_plot[[1]] #dos clusters

##GRAFICOS PARA EXPLICAR LOS CLUSTERS
####################################
##lets now explore visually between 3, 4 and 6 clusters.

#k=3
grp<-cutree(hc, k=3)
numvars$group<-grp
#rebind ratings
numvars$state<-USDexma$state
numvars$sector<-USDexma$sector

g1<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(group)),size=3) + 
  scale_color_manual(name="clusters",values = c("#78B7C5", "#9C964A", "#E1AF00", "#F21A00")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

g2<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(sector)),size=3) + geom_jitter(position = "jitter")+ 
  scale_color_manual(name="sector",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

grid.arrange(g1,g2,ncol=2)

#k=4
grp<-cutree(hc, k=4)
numvars$group<-grp

g1<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(group)),size=3) + 
  scale_color_manual(name="clusters",values = c("#78B7C5", "#9C964A", "#E1AF00", "#F21A00")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

g2<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(sector)),size=3) + 
  scale_color_manual(name="sector",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

grid.arrange(g1,g2,ncol=2)
#k=6.
grp<-cutree(hc, k=6)
numvars$group<-grp
#rebind ratings
numvars$state<-USDexma$state
numvars$sector<-USDexma$sector

g1<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(group)),size=3) + 
  scale_color_manual(name="clusters",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

g2<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(sector)),size=3) + geom_jitter()+
  scale_color_manual(name="sector",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

grid.arrange(g1,g2,ncol=2)
