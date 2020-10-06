install.packages("igraph")
require(igraph)

# MST is some dist function
net=graph.adjacency(MST,mode="undirected",weighted=NULL,diag=FALSE)

plot.igraph(net,mark.shape = 1,mark.border = '000000',
            vertex.color='white')



# s<-matrix(1:25,5)
# s[lower.tri(s)] = t(s)[lower.tri(s)]
# s

library(Matrix)
x<-Matrix(round(runif(n=16,1,100),0), 4)
A <- forceSymmetric(x)
for ( i in 1:nrow(A)) {A[i,i]=0}
A
B <- A
prim_tree(as.data.frame(as.matrix(B)))
