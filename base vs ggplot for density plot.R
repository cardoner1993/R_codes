## Simulate normal distribution and make density plot


## declare seed for reproducibility
set.seed(123456)


x <- rnorm(n=1000, mean=100, sd=5)


## base package

plot(density(x),main = "density plot",col="red",xlab="")
abline(v = mean(x),lty=2)


## ggplot2 package

# if you don't have package, install "ggplot2" with the next code:
install.packages("ggplot2")


## 
library(ggplot2)

qplot(x,  geom="density",col=I("red"),  alpha=I(0.5),
      main="density plot", xlab="", ylab="Density")  +
      geom_vline(xintercept = mean(x),linetype=2) +
      theme_minimal()


# visit http://ggplot2.tidyverse.org/index.html to read more about ggplot2
