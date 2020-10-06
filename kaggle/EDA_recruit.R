## ref on kaggle: https://www.kaggle.com/c/recruit-restaurant-visitor-forecasting

## some script
require(data.table)
require(dplyr)
require(ggplot2)
require(knitr)
require(stringi)
require(lubridate)
library(reshape2)


# air_reserve <- fread("../input/air_reserve.csv", data.table = FALSE, stringsAsFactors = TRUE)

air_stores <- fread("data/input/air_store_info.csv", data.table = FALSE, stringsAsFactors = FALSE,
                    encoding="UTF-8")
## problems with some accents, change it
air_stores$air_area_name <- stri_trans_general(air_stores$air_area_name,"Latin-ASCII")


air_visits <- fread("data/input/air_visit_data.csv", data.table = FALSE, stringsAsFactors = FALSE,
                    encoding="UTF-8")
air_reserve <- fread("data/input/air_reserve.csv", data.table = FALSE, stringsAsFactors = FALSE,
                     encoding="UTF-8")
hpg_reserve <- fread("data/input/hpg_reserve.csv", data.table = FALSE, stringsAsFactors = FALSE,
                     encoding="UTF-8")
hpg_stores <- fread("data/input/hpg_store_info.csv", data.table = FALSE, stringsAsFactors = FALSE,
                    encoding="UTF-8")
store_mapping <- fread("data/input/store_id_relation.csv", data.table = FALSE, stringsAsFactors = FALSE,
                       encoding="UTF-8")
holidays <- fread("data/input/date_info.csv", data.table = FALSE, stringsAsFactors = FALSE,
                  encoding="UTF-8")
submission <- fread("data/input/sample_submission.csv", data.table = FALSE, stringsAsFactors = FALSE,
                    encoding="UTF-8")



head(store_mapping)
## only 150 ID's merge

## air stores
dim(air_stores)

# distinct id ?
air_stores %>% distinct(air_store_id) %>% tally()

# genre type of restaurant

kable(air_stores %>% group_by(air_genre_name) %>% tally() %>% arrange(desc(n)),caption = 'Type of restaurant')

genre_values <- air_stores %>% group_by(air_genre_name) %>% tally() %>% 
                   summarise(mean_n=round(mean(n,na.rm=T),2),
                             median_n = round(median(n,na.rm=T),2))  %>% 
                   rbind(c('line blue','line red'))
                       
## blue => mean
## red => median
air_stores %>% group_by(air_genre_name) %>% tally() %>% 
  ggplot(aes(x=reorder(air_genre_name,+n),y=n)) + geom_bar(stat='identity') + coord_flip() + 
  geom_hline(yintercept = as.numeric(genre_values$mean_n[1]),size=1,color='blue',linetype='dashed') +
               geom_hline(yintercept= as.numeric(genre_values$median_n[1]),size=1,color='red',linetype="dashed") +
                 xlab("Type restaurant") + ylab("counts AIR") 

kable(genre_values)



## There are 103 areas for AIR
## More areas near Tokyo (top10)
air_stores %>% group_by(air_area_name) %>% tally() %>% 
  arrange(desc(n)) %>% head(10)  %>% 
  ggplot(aes(x=reorder(air_area_name,+n),y=n)) + geom_bar(stat='identity') + coord_flip() + 
     xlab("Type restaurant") + ylab("counts") 

## areas without Tokyo in area (Tokyo represent 50% of data)
air_stores %>% 
  filter(!grepl("tokyo",air_area_name,ignore.case = T)) %>% tally()/dim(air_stores)[1]

air_stores %>% 
  filter(!grepl("tokyo",air_area_name,ignore.case = T)) %>% group_by(air_area_name) %>% tally() %>% 
  arrange(desc(n))


## Look Tokyo vs Fukuoka vs Osaka and compare with type genre of restaurant
air_stores %>% filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                               "Fukuoka area","Osaka area"))) %>% 
  group_by(area2,air_genre_name) %>%  tally() %>% 
  mutate(freq = n / sum(n)) %>% 
#    group_by(area2) %>% mutate(n_size = n()) %>% ungroup() %>%   
   # group_by(area2,air_genre_name) %>%  mutate(n2=n()/n_size) %>% 
    ggplot(aes(x=reorder(air_genre_name,+freq),y=freq,fill=area2)) + geom_bar(stat='identity') + coord_flip() + 
     scale_y_continuous(labels=percent) +  facet_wrap(~area2,nrow=3) + xlab("Type of restaurant") +
  ylab("Relative Freq.") + theme(strip.background = element_rect(fill="white",size = 12),
                        legend.position="none") 




tabla_freqs1 <- air_stores %>% filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                               "Fukuoka area","Osaka area"))) %>% 
  group_by(area2,air_genre_name) %>%  tally() %>% 
  mutate(freq = n / sum(n)) 


## with this classification
air_stores %>% filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 = ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                               "Fukuoka area","Osaka area"))) %>% 
  group_by(area2) %>% tally()

## Others areas (38)
air_stores %>% filter(!grepl("Tokyo|Fukuoka",air_area_name,ignore.case=T)) %>% 
  group_by(air_area_name) %>%   tally() %>%  arrange(desc(n)) %>% head(10) %>% kable()


# Air visits data
# class(as.Date(air_visits$visit_date[1:3]))

air_visits %>% mutate(visit_date=as.Date(visit_date),
                                     air_month = month(visit_date),
                                     air_wday = wday(visit_date)) %>% 
                                     # air_mday = mday(visit_date))
                          group_by(air_month) %>% 
               summarise(mean_vis = mean(visitors,na.rm=T) ,
                         median_vis = median(visitors,na.rm=T))

## Create 4 groups
air_stores <- air_stores %>% # filter(grepl("Tokyo|Fukuoka|Osaka",air_area_name,ignore.case=T)) %>% 
  mutate(area2 =        ifelse(grepl("Tokyo",air_area_name,ignore.case=T),
                        "Tokyo area",
                        ifelse(grepl("Fukuoka",air_area_name,ignore.case=T),
                        "Fukuoka area",
                        ifelse(grepl("Osaka",air_area_name,ignore.case=T),
                        "Osaka area","Others area"))))       
head(air_stores)


air_visits %>% sample_n(100) %>%  mutate(visit_date=as.Date(visit_date),
                      air_month = month(visit_date),
                      air_wday = wday(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  # air_mday = mday(visit_date))
  

  group_by(air_month,area2) %>% 
  summarise(mean_vis = mean(visitors,na.rm=T) ,
            median_vis = median(visitors,na.rm=T)) %>% 
 melt(., c('area2','air_month')) %>% 
  ggplot(aes(value,fill=variable)) + 
  geom_density() + facet_wrap(~area2)
  
  #ggplot(aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)  


# General median of year/area2 
air_visits_median_ya2 <- air_visits %>%   mutate(visit_date=as.Date(visit_date),
                                                 air_month = month(visit_date),
                                                 air_wday = wday(visit_date),
                                                 air_year = year(visit_date)) %>%
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>%
  # air_mday = mday(visit_date))
  group_by(air_year,area2) %>% summarise(median_ya2 = median(visitors,na.rm=T))
######################


## Median by year/month/area2
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                                         air_month = month(visit_date),
                                         air_wday = wday(visit_date),
                                         air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month,air_year,area2) %>% 
  summarise(median_vis = median(visitors,na.rm=T)) %>%
  ggplot(aes(factor(air_month),median_vis)) + geom_bar(stat='identity',aes(fill=area2)) +
  facet_wrap(~area2+air_year,nrow=4) + theme(strip.background = element_rect(fill="white",size = 12),
                                    legend.position="none") +
  xlab("Months") + ylab("Median of visitors")  +
 # geom_hline(yintercept = 12, 
 #             slope = 0,data=dplyr::filter(., area2 == 'Fukuoka area')) + 
  # geom_text(data = dplyr::filter(., year == 2016 & sub_area == "Activities"), 
  # # geom_vline(xintercept=c(5,10,15))
  # geom_vline(data=meanData, aes(xintercept=meanCDR), colour="red", lty=3) +
  # geom_line() +
  # geom_text(data=meanData, 
  #           aes(label=round(meanCDR,1), x=40, y=ypos), colour="red",
  #           hjust=1) 
geom_hline(data=air_visits_median_ya2, aes(yintercept=median_ya2),
           colour="black",linetype = 'dashed',size=1)
  

holidays$calendar_date <- as.Date(holidays$calendar_date)


#### MEAN by area/month/year with mark holiday_flag
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  # air_mday = mday(visit_date))
  group_by(air_month,air_year,area2,holiday_flg) %>% 
  summarise(mean_vis = mean(visitors,na.rm=T)) %>%
             
  ggplot(aes(factor(air_month),mean_vis,fill=factor(holiday_flg))) +
  
  geom_bar(stat='identity',position='dodge',width=0.75) +

  
  facet_wrap(~area2+air_year,nrow=4) + theme(strip.background = element_rect(fill="white",size = 12),
                                             legend.position="none") +
  xlab("Months") + ylab("Max of visitors") + 
      scale_fill_brewer(palette="Set2") 


## day of week median of visitors (since 2016 have similar trend in 2017)
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        # air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  group_by(day_of_week,air_year,air_genre_name) %>% 
  summarise(median = median(visitors,na.rm=T)) %>%
  ggplot(aes(median)) + geom_density(aes(fill=day_of_week)) + 
  facet_wrap(~factor(day_of_week,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday',
                                          'Saturday','Sunday')),nrow=2,scales='free_x') +
  theme(strip.background = element_rect(fill="white",size = 12),
        legend.position="none")       + ylab("Density") +
  scale_x_continuous(name="Median of visitors", breaks=c(0,15,30,45,60,75))  +
  # scale_fill_brewer(palette="Set2") 
  scale_fill_manual(values=c("#66CC99","#CC6666","#66CC99","#66CC99",
                             "#CC6666","#CC6666","#CC6666"))

## keep genres great blue line (mean)
bests_genre <- c('Izakaya','Cafe/Sweets','Dining bar','Italian/French','Bar/Cocktail','Japanase food')


## general median on weekends
general_median_weekends <- air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        # air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  # keep only weekend
  filter(day_of_week %in% c('Friday','Saturday','Sunday'),
         air_genre_name %in% bests_genre) %>% 
  summarise(median=median(visitors,na.rm=T)) %>% pull(median)


### day of week  = friday, saturday, sunday bests_genre by Area2
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        # air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  # keep only weekend
  filter(day_of_week %in% c('Friday','Saturday','Sunday'),
         air_genre_name %in% bests_genre) %>% 
  group_by(day_of_week,area2,air_genre_name) %>%
  summarise(median=median(visitors,na.rm=T))  %>% 
  ggplot(aes(median)) + geom_density(aes(fill=day_of_week)) + 
  facet_grid(day_of_week~area2,scales='free_x') +
  theme(strip.background = element_rect(fill="white",size = 12),
        legend.position="none") + xlab("Median of visitors")  + ylab("Density") +
   scale_fill_brewer(palette="Set2") +
  geom_vline(xintercept = general_median_weekends,size=1,color='blue',linetype='dashed')


## median of interaction weekend, genres in Area2 
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        # air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  # keep only weekend
  filter(day_of_week %in% c('Friday','Saturday','Sunday'),
         air_genre_name %in% bests_genre
         ) %>% 
  group_by(day_of_week,area2,air_genre_name,holiday_flg) %>%
  summarise(median=median(visitors,na.rm=T))  %>%
  ungroup() %>% 
  group_by(air_genre_name,holiday_flg) %>% 
  summarise(median=median(median,na.rm=T))  %>%
  ggplot(aes(air_genre_name,median)) + 
  geom_point(stat="identity",aes(color=factor(holiday_flg)),size=8) + coord_flip() +
  geom_text(aes(label = median),size=6,angle = 55,vjust=-0.2) +
  theme(strip.background = element_rect(fill="white",size = 12)) +
        scale_color_manual(values=c("#999999", "#E69F00"),
                           labels=c("No (0)", "Yes (1)")) +
                         
  guides(colour = guide_legend(title = "Holiday flag"),
         labels=c("double negative", "positive for a and/or b")) +
  ylab("Median") + xlab("Topic Air genres") + theme_minimal()

  
## median of interaction weekend, genres in Area2 
air_visits %>%   mutate(visit_date=as.Date(visit_date),
                        air_month = month(visit_date),
                        # air_wday = wday(visit_date),
                        air_year = year(visit_date)) %>% 
  left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
  left_join(holidays,by=c("visit_date"="calendar_date")) %>% 
  # keep only weekend
  filter(day_of_week %in% c('Friday','Saturday','Sunday'),
         air_genre_name %in% bests_genre
  ) %>% 
  group_by(day_of_week,area2,air_genre_name,holiday_flg) %>%
  summarise(median=median(visitors,na.rm=T))  %>%
  ungroup() %>% 
  group_by(area2,holiday_flg) %>% 
  summarise(median=median(median,na.rm=T))  %>%
  ggplot(aes(area2,median)) + 
  geom_point(stat="identity",aes(color=factor(holiday_flg)),size=8) + coord_flip() +
  geom_text(aes(label = median),size=6,angle = 55,vjust=-0.2) +
  theme(strip.background = element_rect(fill="white",size = 12)) +
  scale_color_manual(values=c("#999999", "#E69F00"),
                     labels=c("No (0)", "Yes (1)")) +
  ylab("Median") + xlab("Area") +
  guides(colour = guide_legend(title = "Holiday flag")) +
         theme_minimal()


## we look air_visits, air_stores, holidays
## look for reservers in air_reserve

# air_visits %>%   mutate(visit_date=as.Date(visit_date),
#                         air_month = month(visit_date),
#                         # air_wday = wday(visit_date),
#                         air_year = year(visit_date)) %>% 
#   left_join(air_stores, by = c("air_store_id" = "air_store_id")) %>% 
#   left_join(holidays,by=c("visit_date"="calendar_date")) %>% 

## divide time to analysis
head(air_reserve)
air_reserve$visit_datetime <- as.Date(air_reserve$visit_datetime, "%Y-%m-%d %H:%M:%S")
air_reserve$visit_datetime2 <- as.Date(air_reserve$visit_datetime,tz='CET')
air_reserve$reserve_datetime <- as.Date(air_reserve$reserve_datetime, "%Y-%m-%d %H:%M:%S")


air_visits$visit_date <- as.Date(air_visits$visit_date)
## 1 approach desde reserves
air_reserve %>% left_join(air_visits, by = c("visit_datetime2" = "visit_date",
                                                     "air_store_id","air_store_id"),all=TRUE) %>% 
  filter(air_store_id == 'air_db4b38ebe7a7ceff',visit_datetime2=='2016-01-01') %>% 
  group_by(visit_datetime2,air_store_id) %>% 
  mutate(suma_dia_id = sum(reserve_visitors)) %>% 
  ungroup() %>% 
  mutate(suma2 = visitors - suma_dia_id) %>%
  select(suma_dia_id,suma2,air_store_id,visit_datetime2,visit_datetime,visitors) %>% 
  distinct(suma_dia_id,suma2,visitors,air_store_id,visit_datetime2,.keep_all = TRUE)
   

# 314 distinct air ID
air_reserve %>% distinct(air_store_id) %>% tally()



## provem de mirar diferencia en funció de l'area i dia semana ?

holidays$calendar_date <- as.Date(holidays$calendar_date)

a <- air_reserve %>% 
  left_join(air_stores, by = c("air_store_id"="air_store_id")) %>% 
  left_join(air_visits,by = c("air_store_id"="air_store_id",
                              'visit_datetime2'='visit_date')) %>% 
  # filter(air_store_id == 'air_db4b38ebe7a7ceff',visit_datetime2=='2016-01-01') %>% 
  left_join(holidays,by=c("visit_datetime2"="calendar_date")) %>% as.data.frame()

# aux_a <-a %>% filter_all(.,any_vars(is.na(.)))



a %>% group_by(visit_datetime2,air_store_id) %>% 
  mutate(sum_reserve_day=sum(reserve_visitors,na.rm=T)) %>%
  ungroup() %>% distinct(air_store_id,visit_datetime2,.keep_all = TRUE) %>% 
  # filter(air_store_id == 'air_db4b38ebe7a7ceff',
  #        visit_datetime == '2016-01-01') %>%
  mutate(month = month(visit_datetime)) %>% 
   group_by(month,area2) %>%  
  summarise(reserve=median(sum_reserve_day),
            visitors=median(visitors,na.rm=T)) %>%  melt() %>% 
  filter(!variable == 'month') %>% 
  ggplot(aes(value,fill=variable)) + 
  geom_density() + facet_wrap(~area2) +
  theme(strip.background = element_rect(fill="white",size = 12),
        legend.position="bottom") +
scale_x_continuous(name="Median of visitors", breaks=c(0,10,20,30,40,50)) +
scale_fill_manual(values=c("#999999", "#E69F00"),
                  guide_legend(''))
  




# ungroup() %>% 
#   mutate(suma2 = visitors - suma_dia_id) %>%
#   select(suma_dia_id,suma2,air_store_id,visit_datetime2,visit_datetime,visitors) %>% 
#   distinct(suma_dia_id,suma2,visitors,air_store_id,visit_datetime2,.keep_all = TRUE)
# 
# 
# head(holidays)
