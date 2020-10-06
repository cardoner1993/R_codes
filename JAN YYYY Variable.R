### Type JAN YYYY Variable!

# Ex.

dates_mesos <- test$`Cal. year / month factor`
date1 <- as.yearmon(as.character(dates_mesos[1]),"%b %Y")
class(date1) ## yearmon

date1 <- as.Date(as.yearmon(dates_mesos, "%b %Y"))
class(date1) ## Date
