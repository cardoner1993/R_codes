library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(e1071)
library(gridExtra)
library(grid)
library(testthat)
library(readr)
library(ggthemes)
library(RColorBrewer)
library(ggdendro)
library(NbClust)
library(grid)
library(factoextra)
library(dendextend)

train_data <- read_csv("Dataset_Salesforce_Predictive_Modelling_TRAIN.txt")

train_data<-train_data[-which(is.na(train_data$Socio_Demo_01)),]

length(unique(train_data$Socio_Demo_01))

train_data %>% group_by(Socio_Demo_01) %>% summarize(Media=mean(Poder_Adquisitivo),Mediana=median(Poder_Adquisitivo),Cuenta=n()) %>% arrange(desc(Cuenta)) 
##Opcion 1. socio<-train_data %>% group_by(Socio_Demo_01) %>% summarize(Media=mean(Poder_Adquisitivo),Maximo=max(Poder_Adquisitivo),Minimo=min(Poder_Adquisitivo),Mediana=median(Poder_Adquisitivo),q75=quantile(Poder_Adquisitivo,0.75),q25=quantile(Poder_Adquisitivo,0.25)) %>% as.data.frame()

##opcion 2.
socio<-train_data %>% group_by(Socio_Demo_01) %>% summarize(Media=mean(Poder_Adquisitivo),Mediana=median(Poder_Adquisitivo)) %>% as.data.frame()
###cluster para ver el comportamiento de la variable socio demo_01.
k.max<-15
wss <- sapply(1:k.max, 
              function(k){kmeans(socio[,2:3], k, nstart=50,iter.max = 15 )$tot.withinss})
wss 
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#catvar<-USDexma %>% select_if(is.character) %>% mutate_all(as.factor)

##Dissimilarity Matrix Calculation
####### Dendogram with euclidean distance and agregation with Ward

rownames(socio) <- make.names(socio$Socio_Demo_01, unique = TRUE)
socio$name<-rownames(socio)

#default parameters
dd <- dist(scale(socio %>% select_if(is.numeric)), method = "euclidean")
hc<-hclust(dd, method = "ward.D2")
dd.dendro <- as.dendrogram(hc)
dendro_data <- dendro_data(dd.dendro)

#making color labels
kk<-data.frame(tre=cutree(hc,4),socio$name)
labs <- label(dendro_data)
colnames(kk)[2]<-'label'
newlabs<-merge(labs,kk,by='label')
#head(newlabs)
ggplot(segment(dendro_data)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=newlabs,aes(label=label, x=x, y=-3, colour=as.factor(newlabs$tre)),size=2) + 
  scale_color_manual(name="",values=c("red","blue","green","orange")) + 
  coord_flip() + theme_fivethirtyeight() + 
  theme(axis.text=element_blank()) + ggtitle("Dendrogram of estimators Socio vs Poder")

socio<-train_data %>% group_by(Socio_Demo_01) %>% summarize(Media=mean(Poder_Adquisitivo),Mediana=median(Poder_Adquisitivo),Cuenta=n()) %>% as.data.frame()
socio$Cluster<-cutree(hc,4)
View(socio)
#################################################################################
list_plot<-list()
for(i in 2:6){
  grp<-cutree(hc, k=i)
  list_plot[[i-1]]<-fviz_cluster(list(data = socio[,2:3], cluster = grp)) + 
    ggtitle('')
}
grid.arrange(list_plot[[2]],list_plot[[3]],list_plot[[4]],list_plot[[5]],ncol=2)
list_plot[[1]] #dos clusters

###############################################
####Separacio segons classe variables.
cat_vars<-train_data[,grep("Socio_Demo_01|Socio_Demo_02|Ind_Prod_",colnames(train_data))]
num_vars<-train_data[,-grep("Socio_Demo_01|Socio_Demo_02|Ind_Prod_",colnames(train_data))]

#To transform categorical data in numeric is important to use the levels.
# cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in features) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

##Transformamos las variables categoricas en numericas.

##########
##Cogeremos y realizaremos ahora una comparativa entre los muy ricos, los medios y los pobres.

ordtr<-train_data %>% arrange(desc(Poder_Adquisitivo))
ord<-seq(nrow(ordtr)/2-7,nrow(ordtr)/2+7)
comp<-rbind(head(ordtr,15),ordtr[ord,],tail(ordtr,15))
comp$Estat<-rep(c("Ric","Mig","Pobre"),each=15)
View(comp)

###Grafic.

gg<-ggplot(train_data,aes(x=Imp_Sal_09,y=Poder_Adquisitivo)) + geom_point(aes(color=as.factor(Socio_Demo_02),size=Num_Oper_17),alpha=.4,position = position_jitter(w = 1.5, h = 1.5)) + geom_smooth() + facet_wrap(~Ind_Prod_01)
plot(gg)
