tally() function
================

### Example on mtcars data set

``` r
# for info about data set use ?mtcars

mtcars$vs
```

     [1] 0 0 1 1 0 1 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 1 0 0 0 1

``` r
#  (1)
table(mtcars$vs)   
```


     0  1 
    18 14 

Use tally() for obtain **(1)** and make plot

``` r
require(ggplot2)
require(dplyr)

data <- mtcars %>% 
           group_by(vs) %>% tally()

data
```

    # A tibble: 2 x 2
         vs     n
      <dbl> <int>
    1     0    18
    2     1    14

``` r
ggplot(data,aes(factor(vs),n),color="red") + geom_bar(stat="identity",fill = "#FF6666") + xlab("")
```

![](example_tally_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)
