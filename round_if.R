## Example with iris data set


## round only if is.numeric().

data <- iris %>% group_by(Species) %>% summarise_if(is.numeric,funs(mean))   %>% 
  mutate_if(is.numeric, funs(round(., 2))) 
