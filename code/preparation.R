library(data.table)
library(ggplot2)
# knitr::opts_chunk$set(echo = F)
map <- readRDS("data/gadm36_NOR_1_sp.rds")
#map <- rmapshaper::ms_simplify(map,keep = 0.5)
map <- broom::tidy(x=map,region="NAME_1")
setDT(map)
unique(map$id)
#saveRDS(map,"data/map_supervisor.RDS")
WeekC <- function (date = lubridate::today()) 
{
  wk <- as.numeric(format.Date(date, "%V"))
  wk <- formatC(wk, flag = "0", width = 2)
  return(wk)
}
Year <- function (date = lubridate::today()) 
{
  yr <- as.numeric(format.Date(date, "%G"))
  return(yr)
}
MonthC <- function (date = lubridate::today()) 
{
  m <- as.numeric(format.Date(date, "%m"))
  m <- formatC(m, flag = "0", width = 2)
  return(m)
}
Month <- function (date = lubridate::today()) 
{
  yr <- as.numeric(format.Date(date, "%m"))
  return(yr)
}
YearMonth <- function(date){
  return(sprintf("%s-%s", Year(date), MonthC(date)))
}
YearWeek <- function(date){
  return(sprintf("%s-%s", Year(date), WeekC(date)))
}
YearWeekN <- function(date){
  return(as.numeric(sprintf("%s%s", Year(date), WeekC(date))))
}
dr <- readxl::read_excel("data/casereports-2018-September-28 (1).xlsx")
dc <- readxl::read_excel("data/datacollectors-2018-September-28 (1).xlsx")
setDT(dr)
setDT(dc)
setnames(dr,"Data Collector","datacollector")
setnames(dr,"Males < 5","mu5")
setnames(dr,"Males ≥ 5","mo5")
setnames(dr,"Females < 5","fu5")
setnames(dr,"Females ≥ 5","fo5")
dr <- rbind(dr,dr,dr,dr,dr)
dr[,n:=floor(1:.N/2),by=datacollector]
dr[,Date:=lubridate::today()-n]
dr[,n:=NULL]
dr <- dr[`Health Risk`==params$healthRisk]
dr[,`Health Risk`:=NULL]
setnames(dr,"Lat. / Long.","latlong")
dr[,Region:=NULL]
dr[,District:=NULL]
dr[,Village:=NULL]
dr[,latlong:=NULL]
locs <- dc[,c("Display Name","Lat. / Long.","Region","District","Village")]
setnames(locs,c("datacollector","latlong","region","district","village"))
minDate <- min(dr$Date)
maxDate <- max(dr$Date)
weeks <- data.table(Date=seq.Date(minDate,maxDate,1))
weeks[,rollingWeek:=floor(as.numeric(difftime(max(Date),Date,units="days"))/7)]
weeks[,epiWeek:=YearWeek(Date)]
weeks[,epiWeekN := YearWeekN(Date)]
weeks[,rollingMinDate:=min(Date),by=rollingWeek]
weeks[,rollingMaxDate:=max(Date),by=rollingWeek]
weeks[,epiMinDate:=min(Date),by=epiWeek]
weeks[,epiMaxDate:=max(Date),by=epiWeek]
weeks[,rollingx:=max(rollingWeek)-rollingWeek]
weeks[,epix:=max(epiWeekN)-epiWeekN]
weeks[,epiMonth:=YearMonth(Date)]
weeksNoDate <- copy(weeks)
weeksNoDate[,Date:=NULL]
weeksNoDate <- unique(weeksNoDate)
skeleton <- vector("list",length=nrow(locs))
for(i in 1:nrow(locs)){
  skeleton[[i]] <- copy(weeks)
  skeleton[[i]][,datacollector:=locs$datacollector[i]]
}
skeleton <- rbindlist(skeleton)
nrow(skeleton)
skeleton <- merge(skeleton,locs,by=c("datacollector"),all.x=T,allow.cartesian = T)
nrow(skeleton)
d <- merge(skeleton,dr,by=c("Date","datacollector"),all.x=T,allow.cartesian = T)
nrow(d)
d[,isMessage:=ifelse(is.na(Time),0,1)]
xtabs(~d$isMessage)
d[,isMessageValid:=isMessage]
d[isMessage==1,isMessageValid:=sample(c(1,0),size=.N,replace=T,prob=c(0.7,0.3))]
d[is.na(mu5),mu5:=0]
d[is.na(mo5),mo5:=0]
d[is.na(fu5),fu5:=0]
d[is.na(fo5),fo5:=0]
dWide <- d
n <- names(d)[!names(d) %in% c("mu5","mo5","fu5","fo5")]
d <- melt.data.table(d,id.vars=n)
d[,sex:="Male"]
d[variable %in% c("fu5","fo5"),sex:="Female"]
d[,age:="Age <5"]
d[variable %in% c("mo5","fo5"),age:="Age 5+"]
