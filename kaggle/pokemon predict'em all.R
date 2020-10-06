##############################################
Link kaggle:                               
https://www.kaggle.com/semioniy/predictemall 
##############################################

# attach packages
library(dplyr)
library(ggplot2)
library(reshape2)


# load data
pokemon<-read.csv2("300k/300k.csv",header = T, sep=",")

dim(pokemon)
View(pokemon)

pokemon %>% 
  summarise_all(funs(missings=100*mean(is.na(.)))) %>% filter_all(.,any_vars(. > 0))

# sum(is.na(pokemon))
## no missings in data !

## cellid parecen iguales en los metros, 
## coger 1 (la más cercana?)
## Aplicar log()
pokemon %>% select(grep("cellId",names(.),ignore.case = T)) %>% head(.) %>%
 mutate_all(.,funs(log(.)))  



pokemon %>% select(grep("cellId",names(.),ignore.case = T)) %>% head(1000) %>%
  mutate_all(.,funs(log(.))) %>% select(cellId_90m,cellId_180m) %>% 
  ## puntero en variables
  melt()  %>% 
  ggplot(aes(x=value, fill=variable)) + geom_histogram(alpha=0.25) + facet_wrap(~variable) 
  




## pokemon id

pokemon %>% group_by(pokemonId) %>% tally() %>% 
  ggplot(aes(x=reorder(pokemonId,-n),y=n)) +
  geom_bar(stat='identity') + xlab("Pokemons") + ylab("count") + theme(
                                               axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank())
  


## time of appeared ?

pokemon %>% select(grep("appeared",names(.),ignore.case=T)) %>%
  sample_n(.,size=30000,replace=F) %>% 
  select(-appearedLocalTime) %>% mutate_if(.,is.factor,as.integer) %>%   melt() %>% 
  ggplot(aes(x=value,fill=variable)) + geom_histogram(alpha=0.25,bins=25) +
  facet_wrap(~variable,ncol = 2,scales = 'free')
  
## quitamos day,month,year.. poco representativo XD

# nos podemos quedar con hora y minuto !


## tipo de terreno ?

pokemon %>% group_by(terrainType) %>% tally() %>% 
  ggplot(aes(x=terrainType,y=n)) + geom_line()

pokemon %>% group_by(terrainType) %>% tally()  %>% arrange(desc(n)) %>% head(5)
## tipo de terreno no equilibrado


## close to water??

pokemon %>% group_by(closeToWater) %>% tally()


## weather ? bastante asimetrico recategorizar

pokemon %>% group_by(weather) %>% tally() %>% 
  ggplot(aes(x=reorder(weather,-n),y=n)) + geom_bar(stat="identity") +
  xlab("Weather type") + ylab("count") + theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())

# variables climatologia (se puede jugar)

pokemon %>% select(temperature,windSpeed,windBearing,pressure) %>% 
  mutate_if(is.factor,as.character) %>% mutate_if(is.character,as.numeric) %>% 
   sample_n(10000,replace=FALSE) %>% 
  melt() %>% 
  ggplot(aes(x=value,fill=variable)) + geom_histogram(alpha=0.25,bins=25) +
  facet_wrap(~variable,ncol = 2,scales = 'free')


pokemon %>% select(weatherIcon) %>% mutate_if(is.factor,as.character) %>% 
  group_by(weatherIcon) %>% tally() %>% 
   ggplot(aes(x=reorder(weatherIcon,+n),y=n)) + geom_bar(stat='identity') + 
  xlab("") + coord_flip() 


## urban vs rural
table(pokemon$urban)
require(tidyverse)
pokemon %>% select(urban,rural) %>% mutate_if(is.factor,as.character) %>%
  gather(col,value,urban,rural) %>% 
  mutate(group = paste(col, value, sep = "_")) %>%
  group_by(group) %>%  tally() %>% 
   ggplot(aes(x=group,y=n)) + geom_bar(stat='identity') 


pokemon %>% select(grep("distance",names(.),ignore.case=T)) %>%
  mutate_if(.,is.factor,as.integer) %>%   melt() %>% 
  ggplot(aes(x=value,fill=variable)) + geom_histogram(alpha=0.25,bins=25) +
  facet_wrap(~variable,ncol = 2,scales = 'free')



# pokemon class

tipus1 <- pokemon %>% 
  mutate(tipus=ifelse(pokemonId %in% c(13:20,41,42),"Common",
                      ifelse(pokemonId %in% c(10:12,21:24,29:34,43:49,58:62,69:71,84,85,92:94,96:99,120,121,127),"Uncommon",
                             ifelse(pokemonId %in% c(88,89,106:108,113,129,130,137,142),"Very Rare",
                                    ifelse(pokemonId %in% c(83,132,144,145,146,150,151,115,122,131),"Super Rare","Rare"))))) %>% 
  select(pokemonId,tipus) %>% as.data.frame()



tipus1 %>% filter(tipus=='Common')  %>% distinct(pokemonId) %>% pull(pokemonId)

# vs quantile ?

tipus2 <- pokemon %>% group_by(pokemonId) %>% tally() %>%  as.data.frame()

# deciles  
quantile(tipus2$n,probs=seq(0,1,by=0.1))
summary(tipus2$n)

tipus2 %>% ggplot(aes(x=n)) + geom_density(alpha=0.3,fill="steelblue")

tipus2 %>% arrange(desc(n)) %>% head(10)


## class. from some reference pokemon page

clase_pokemon<-pokemon %>% 
  mutate(tipus=ifelse(pokemonId %in% c(13:20,41,42),"Common",
                      ifelse(pokemonId %in% c(10:12,21:24,29:34,43:49,58:62,69:71,84,85,92:94,96:99,120,121,127),"Uncommon",
                             ifelse(pokemonId %in% c(88,89,106:108,113,129,130,137,142),"Very Rare",
                                    ifelse(pokemonId %in% c(83,132,144,145,146,150,151,115,122,131),"Super Rare","Rare"))))) %>% 
  pull(tipus)



## relevant data

# clase_pokemon
names(pokemon[grep("distance",
     names(pokemon),ignore.case=T)])

# change of weather
asignacion_weather <- pokemon %>% group_by(weather) %>% tally() %>% arrange(desc(n)) %>% 
  mutate_if(is.factor,as.character) %>% pull(weather)



data <- pokemon %>% 
  select(
         appearedHour,
         appearedMinute,
         terrainType,
         closeToWater,
         weather,
         weatherIcon,
         temperature,
         windSpeed,
         windBearing,
         pressure,
         urban,
         rural,
         pokemonId,
         # reposta
         continent,
         gymDistanceKm,pokestopDistanceKm) %>% 
  # mutate(cellId_90m=log(cellId_90m)) %>% 
  mutate(loc=ifelse(continent %in% c("Africa","Australia",
                                     "Europe", "Indian", 
                                     "Atlantic","Asia"),"East","West")) %>% 
  mutate(tipus=ifelse(pokemonId %in% c(13:20,41,42),"Common",
                      ifelse(pokemonId %in% c(10:12,21:24,29:34,43:49,58:62,69:71,84,85,92:94,96:99,120,121,127),"Uncommon",
                             ifelse(pokemonId %in% c(88,89,106:108,113,129,130,137,142),"Very Rare",
                                    ifelse(pokemonId %in% c(83,132,144,145,146,150,151,115,122,131),"Super Rare","Rare"))))) %>% 
  # quitamos respuesta
  select(-continent,-pokemonId) %>% 
  ## niveles pequeños en 'others'
mutate(weatherIcon = ifelse(weatherIcon %in% c('cloudy','rain','wind','fog'),'others',weatherIcon)) %>% 
  
# weather según conteo dejar 3 y others (ver abajo)
  
mutate(weather=ifelse(weather %in% asignacion_weather,weather,'other')) %>%  

# problemas factor / character / numeric
 mutate(gymDistanceKm=as.numeric(as.character(gymDistanceKm)),
        # value = '?' => problem
        pokestopDistanceKm = as.numeric(as.character(pokestopDistanceKm)),
        temperature=as.numeric(as.character(temperature)),
        windSpeed=as.numeric(as.character(windSpeed)),
        windBearing=as.numeric(as.character(windBearing)),
        pressure=as.numeric(as.character(pressure))) %>%
# si es vacio al hacer numeric el ? => asignamos media general
    mutate(pokestopDistanceKm = ifelse(is.na(pokestopDistanceKm),
                                     mean(pokestopDistanceKm,na.rm=T),pokestopDistanceKm)) %>% 
  
  as.data.frame()

# View(data)

# # Asignacion weather # #
# pokemon %>% group_by(weather) %>% tally() %>% 
#   ggplot(aes(x=reorder(weather,-n),y=n)) + geom_bar(stat="identity") +
#   xlab("Weather type") + ylab("count") + theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank())
# asignacion_weather <- pokemon %>% group_by(weather) %>% tally() %>% arrange(desc(n)) %>% head(4) 
#   
# # # # ## # ### ### ### #


# head(data)
# names(data)
# sapply(data,class)
# as.numeric(as.character(data$pressure))
sapply(data,class)

library(xgboost)
features<- names(data)
for (f in features) {
  if (class(data[[f]])=="factor" | class(data[[f]])=="character" ) {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(data[[f]])
    data[[f]] <- as.numeric(factor(data[[f]], levels=levels))
  }
}

# todo numerico
sapply(data,class)
head(data)
names(data)

seed <- 1000
set.seed(seed)

idx <- sample(nrow(data),size = 0.7*nrow(data),replace = F)
train <- data[idx,]
test <- data[-idx,]


library(xgboost)
library(Matrix)

# xgboost quiere 1 y 0 // el 2 es 0
resposta <- train[,15] # loc
resposta[resposta==2] <- 0
#table(resposta)
resposta_test <- test[,15] # loc
resposta_test[resposta_test==2] <- 0

train<-train[,-15]
test<-test[,-15]

#sapply(train,class)

sparse.train <- sparse.model.matrix( ~  . -1 ,data=train)

sparse.test <- sparse.model.matrix( ~ . -1 , data = test)


dtrain <- xgb.DMatrix(data=sparse.train, label=resposta)
#View(as.matrix(sparse.train))
dtest <- xgb.DMatrix(data=sparse.test)

set.seed(100)
#param. bons aprox 30
xgb_params = list(
  booster = "gbtree",
  seed = 0, 
  eta=0.2, #learning rate (0,1) mes alt treballa menys
  objective = "binary:logistic",  #  output: probabilidad
  eval_metric='auc', ## mas proximo a 1 , mejor diagonal
  max_depth = 4  ##mayor en funcion de la complejidad del modelo.
)
#alpha = 1, lambda = 1

res = xgb.cv(xgb_params,
             dtrain,
             nrounds=50,
             nfold=5,
             early_stopping_rounds=30,
             print_every_n = 10,
             #watchlist=watchlist,
             nthread=4 #es la velocidad para los nucleos del core.
             #feval=xg_eval_mae2,
  )
             






best_nrounds = res$best_iteration
####Creem el model i predim
gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

# pred = abs(exp(predict(gbdt,dtest))-1)

pred = predict(gbdt,dtest)
pred0 = predict(gbdt,dtrain)

summary(pred)
summary(resposta_test)

## la mediana es pot interpretar como la prob(p==1) més dolenta.


View(cbind(pred,resposta_test))

max(pred)
min(pred)




plot(pred)
plot(pred0)

out <- data.frame(pred1=round(pred,0),resposta_test = resposta_test)
head(out)

table(out$pred1)
out %>%  select(pred1) %>% filter(pred1 > 0.4 & pred1 < 0.5 & resposta_test < 1) %>% 
#  summarise(max_min=max(max(pred1))) 
  tally()

#round(quantile(out$pred1,probs=seq(0,1,by=.05)),2)


require(caret)
confusionMatrix(out$pred1,out$resposta_test)

pred


