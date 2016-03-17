# Brandon Lewis
# March 15th, 2016

# ECON 294A Final

library(ggplot2)
library(dplyr)
library(RSQLite)
library(nycflights13)
library(knitr)

db<-src_sqlite("nycflights13", create = T)

flights_sqlite<-tbl(db,"flights")
airlines_sqlite <-tbl(db,"airlines")
airports_sqlite<-tbl(db,"airports")
planes_sqlite<-tbl(db,"planes")
weather_sqlite<-tbl(db,"weather")

flights=flights
airlines=airlines
airports=airports
planes=planes
weather=weather

flights_weather<-inner_join(
  tbl(db,"flights"),
  tbl(db,"weather"),
  by=c("year","month","day","hour"))%>%
  collect()%>%
  mutate(cancelled=(is.na(arr_time)),
         delayed=ifelse(dep_delay>0,1,0))     

flights_weather$month<-as.factor(flights_weather$month)

flights_planes<-inner_join(
  tbl(db,"flights"),
  tbl(db,"planes"),
  by=c("tailnum"))%>%
  collect()%>%
  mutate(canceled=(is.na(arr_time)),
         delayed= ifelse(dep_delay>0,1,0)) 

weather.c<-subset(flights_weather, cancelled==1)
weather.d<-subset(flights_weather, delayed==1)
plane.c<-subset(flights_planes, canceled==1)
plane.d<-subset(flights_planes, delayed==1)


Part (a)

probit11<-glm(cancelled~temp+dewp+humid+wind_speed+precip+pressure+visib,family=binomial(link="probit"), data=flights_weather)
summary(probit11)

probit12<-glm(delayed~temp+humid+wind_speed+precip+pressure+visib,family=binomial(link="probit"), data=flights_weather)
summary(probit12)

pa1<-ggplot(weather.c,aes(x=wind_speed,y=dep_delay))
pa1+geom_point(aes(color=visib))

ggplot(weather.c,aes(x=visib))+stat_bin(binwidth=1)
ggplot(weather.d,aes(x=visib))+stat_bin(binwidth=1)

ggplot(weather.c,aes(x=wind_speed))+stat_bin(binwidth=0.5)


Part (b)
































