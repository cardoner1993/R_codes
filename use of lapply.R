## use lapply for make action with if() statement

## Supose you have a data.frame with some numerics variables and you want to round it.

## make this


data.frame(lapply(dataset, function(y) if(is.numeric(y)) round(y, 3) else y)) 
