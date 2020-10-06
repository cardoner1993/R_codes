require(ggplot2)
require(dplyr)

x<-rnorm(1e6)
y<-rnorm(1e6,mean = 10,sd=3)
z<-rexp(1e6)
d<-rnorm(1e6,mean=30,sd=7)
factor<-rep(c("A","B"),times=c(7e5,3e5))

data <- data.frame(x=x,y=y,z=z,d=d,factor=factor)

plot_list <- list()

## es bastant util
theme <-  theme(axis.text.y = element_text(color="grey20",size=14,hjust=1,face="plain")) +
  theme(axis.text.x = element_text(color="grey20",size=14,vjust=1,face="plain")) +
  theme(axis.title=element_text(size=14,face="plain"))


## evalua i substitueix el plot a una llista, sino no li agrada al ggplot()
for (i in 1:dim(data)[2]) {
  if (is.numeric(data[,i])==TRUE) {
  p1 <- eval(substitute(
    ggplot(data=data,aes(x=data[ ,i]))+ 
      geom_histogram(fill="blue",bins=30) +
      xlab(colnames(data)[i])+ylab("")+theme
    ,list(i = i)))
   plot_list[[i]] <- p1  # afegim a llista el plot 
  }
  
 else {
   p1 <- eval(substitute(
     ggplot(data=data,aes(x=data[ ,i]))+ 
       geom_bar(fill="blue") +
       xlab(colnames(data)[i])+xlab("")+theme
     ,list(i = i)))
   plot_list[[i]] <- p1 
   
 } 
}

lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

lay_out(list(plot_list[[1]],1,1:3),
        list(plot_list[[2]],2,1:3),
        list(plot_list[[5]],1:2,3)
        )

lay_out(list(plot_list[[1]],1,1),
        list(plot_list[[2]],1,2),
        list(plot_list[[5]],1:2,1)
)



## Juga amb això!! Hi ha varies maneres...
require(gridBase)
require(gridExtra)

marrangeGrob(grobs=list(plot_list[[1]],plot_list[[2]]),nrow= 2,ncol=2)

grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[5]])

