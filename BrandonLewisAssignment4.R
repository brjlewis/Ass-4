# Brandon Lewis
# 2/17/2016

# Assignment 4

print("Name: Brandon Lewis
       SID:  1154992
      email: brjlewis@ucsc.edu")

# Question 1

library(foreign)

install.packages(dplyr)
library(dplyr)

install.packages("plyr")
library(plyr)

airports<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv",
                  stringsAsFactors = FALSE,header=TRUE)

planes<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",
                 stringsAsFactors=F,header=T)

weather<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv",
                  stringsAsFactors=F,header=T)

flights<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv",
                   stringsAsFactors=F,header=T)


# Question 2

flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)


# Question 3

flights.2a<-subset(flights,dest=="SFO" | dest=="OAK" )
nrow(flights.2a)
flights.2b<-subset(flights,dep_delay>=60)
nrow(flights.2b)
flights.2c<-subset(flights,arr_delay>2*dep_delay)
nrow(flights.2c)


# Question 4

library(dplyr)

select(flights,dep_delay)


# Question 5

# (a)

arrange(flights,-dep_delay)
head(flights,n=5)

# (b)

flights.5b<-flights
flights$delaydiff<-(flights.5b$dep_delay-flights.5b$arr_delay)
flights.5b<-arrange(flights.5b,delaydiff)
head(abs(flights.5b$delaydiff),n=5)

# Question 6

flights<-mutate(flights,speed=dist/(time/60))
flights<-mutate(flights,delta=(dep_delay-arr_delay))

# (a)

flights<-flights[order(-flights$speed),]
head(flights$speed,n=5)

# (b)

flights<-flights[order(flights$delta),]
head(abs(flights$delta),n=5)

# (c)

flights<-mutate(flights,deltaloss=(arr_delay-dep_delay))
flights.6c<-flights[order(flights$deltaloss),]
head(flights.6c$deltaloss,n=5)


# Question 7

by_carrier<-group_by(flights,carrier)

flights.7a<-by_carrier%>%
  summarise(
    cancelled_flights=sum(cancelled, na.rm=T),
    tot_flight=n(),
    percent_cancelled=((cancelled_flights/tot_flight)*100),
    min_delta=min(delta,na.rm=T),
    max_delta=max(delta,na.rm=T),
    median_delta=median(delta,na.rm=T),
    mean_delta=mean(delta,na.rm=T),
    first_quart=quantile(delta,0.25,na.rm=T),
    third_quart=quantile(delta,0.75,na.rm=T),
    ninety_quan=quantile(delta,0.90,na.rm=T)
  )

flights.7a<-arrange(flights.7a,desc(percent_cancelled))
summary(flights.7a)


day_delay<-dplyr::filter(flights,!is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(
    delay=mean(dep_delay),
    n=n()
  )


# Question 8

arrange(day_delay,date)
delay.8a<-day_delay %>%
  mutate(delay_today=delay,
         delay_yesterday=lag(delay,1),
         delay_inc=delay_today-delay_yesterday)

delay.8a<-arrange(delay.8a,-delay_inc)
head(delay.8a,n=5)


# Question 9

dest_delay<-dplyr::filter(flights,!is.na(dep_delay))%>%
  group_by(dest)%>%
  summarize(
    arr_delay=mean(arr_delay),
    n=n()
  )

airports<-airports %>%
  rename(dest=iata,name=airport)

df.9a<-left_join(dest_delay, airports, by=c("dest"="dest"))
df.9a<-arrange(df.9a,-arr_delay)
head(df.9a,n=5)

df.9b<-inner_join(dest_delay,airports,by=c("dest"="dest"))
nrow(df.9a)
nrow(df.9b)
print("The number of observations are close but not the same. They are different by 2.")

df.9c<-right_join(dest_delay,airports,by=c("dest"="dest"))
nrow(df.9c)
print("There are NAs in the arr_delay column, because airport has a different number of observations")

df.9d<-full_join(dest_delay,airports,by=c("dest"="dest"))
nrow(df.9d)
print("There are NAs in arr_delay, because airport has a different number of observations")


# Question 10

hourly_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date, hour)%>%
  summarise(
    delay = mean(dep_delay)
  )

hourly_delay$date <- as.Date(hourly_delay$date)

df.10a <- left_join(hourly_delay, weather, by=c("date"="date"))

arrange(df.10a,delay)
head(df.10a$conditions,n=20)

install.packages(data.table)
library(data.table)

weather_delays=data.table(df.10a$delay,df.10a$conditions)

head(weather_delays,n=18)





















