library(Rtsne)
library(ggplot2)
iris.x = iris[,-5]
iris.y = iris[,5]

##########################################
## Remove duplicates (needed for t-SNE) ##
##########################################

dup = which(duplicated(iris.x))

# Observation 102 and 143 are duplicates
iris[c(102,143),]

iris.x = iris.x[-dup,]
iris.y = iris.y[-dup]


?Rtsne
?tsne
###############
## Run t-SNE ##
###############
?Rtsne
tsne = Rtsne(iris.x, dims = 2,  perplexity = 15,pca=FALSE,eta=400)

######################
## Visualize result ##
######################
tsne$Y
df.tsne = data.frame(tsne$Y)  
df.tsne = scale(df.tsne) 
df.tsne = as.data.frame(df.tsne)
ggplot(df.tsne, aes(x=X1, y=-X2, color=iris.y)) + geom_point(size=2)


system("ls ../input")
