# Brandon Lewis
# 2/26/2016

# Assignment 5

print("Name: Brandon Lewis
      SID:  1154992
      email: brjlewis@ucsc.edu")

library(ggplot2)
library(scales)
library(dplyr)


# Question 1

df<-diamonds

# (a)

plot1<-ggplot(df, aes(x=x*y*z, y=price)) +
  geom_point(aes(color=clarity, size=carat), alpha=0.2) +
  scale_y_log10() + scale_x_log10() +
  scale_size(range=c(5,10))
  
plot1


# (b)

plot2<-ggplot(data=diamonds,aes(x=carat, y=..density.., fill = clarity)) +  
  geom_histogram(binwidth = 0.2) +
  facet_grid(cut ~ .) 

plot2


# (c)

plot3<- ggplot(data = diamonds,aes(y = price, x = cut)) + 
  geom_violin() + 
  geom_jitter(alpha = 0.01) 

plot3


# Question 2

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.RData"))

org_example<-subset(org_example, !is.na(rw))

bymonth<-org_example %>% 
  group_by(year,month) %>% 
  mutate(
    educ=educ,
    median.rm=median(rw),
    quar1st=quantile(rw, probs = 0.25, na.rm = T, names = TRUE, type = 7),
    quar3rd=quantile(rw, probs = 0.75 , na.rm = T, names = TRUE, type = 7), 
    decl1st=quantile(rw, probs = 0.1, na.rm = T, names = TRUE, type = 5) ,
    decl9th=quantile(rw, probs = 0.9, na.rm = T, names = TRUE, type = 5),
    total = n()
  ) %>% select(year, month, median.rm, quar1st, quar3rd, decl1st, decl9th, educ) %>%
  arrange(year,month)

bymonth<-bymonth %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )

plot4<-ggplot(data=bymonth,aes(date)) + 
  geom_line(aes(y = median.rm)) + 
  geom_ribbon(aes(ymin = quar1st, ymax = quar3rd), alpha=0.4) + 
  geom_ribbon(aes(ymin = decl1st, ymax = decl9th), alpha=0.2) + 
  ylim(0, 50) 

plot4


# (b)

bymonth<-subset(org_example, !is.na(educ))
bymonth<-org_example %>% 
  group_by(year,month,educ) %>% 
  mutate(
    median.rm=median(rw),
    total = n()
  ) %>% select(year, month, median.rm, educ) %>%
  arrange(year,month)

bymonth<-bymonth %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )

plot5<-ggplot(data=bymonth,aes(x=date,y=median.rm)) +  
  geom_line(aes(color=educ)) 

plot5



