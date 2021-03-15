library(tidyr)
library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
fileurl="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl,'./activity.zip', mode = 'wb')
unzip("activity.zip", exdir = getwd())
library(readr)
dat<-read_csv("activity.csv")
head(dat)
totwalk<-dat%>%
  group_by(date)%>%
  summarize(totwalk=sum(steps,na.rm=TRUE))
summ<-summary(totwalk$totwalk)
summ["Median"]
summ["Mean"]
with(totwalk,hist(totwalk,main=" Total Walk Histogram",xlab="Total Steps",col="darkmagenta"))
avewalk<-dat%>%
  group_by(interval)%>%
  summarize(avesteps=mean(steps,na.rm=TRUE))
with(avewalk,plot(interval,avesteps,type = "l",main="Average steps throughout day"))
avewalk%>%filter(avesteps==max(avesteps))%>%select(interval)

sum(is.na(dat))
dat<-dat%>%group_by(interval)%>%
  mutate(steps = replace_na(steps, mean(steps,na.rm=TRUE)))%>%
  ungroup()

totwalk<-dat%>%
  group_by(date)%>%
  summarize(totwalk=sum(steps,na.rm=TRUE))
summ<-summary(totwalk$totwalk)
summ["Median"]
summ["Mean"]
with(totwalk,hist(totwalk,main=" Total Walk Histogram",xlab="Total Steps",col="darkmagenta"))
weekavesteps<-dat%>%mutate(isweekday = case_when(
    weekdays(date)=="Monday"  ~ "Weekday",
    weekdays(date)=="Tuesday"  ~ "Weekday",
    weekdays(date)=="Wednesday"  ~ "Weekday",
    weekdays(date)=="Thursday"  ~ "Weekday",
    weekdays(date)=="Friday"  ~ "Weekday",
    weekdays(date)=="Saturday"  ~ "Weekend",
    weekdays(date)=="Sunday"  ~ "Weekend"),
    isweekday = factor(isweekday, levels=c("Weekday","Weekend")))%>%
  group_by(isweekday,interval)%>%
  summarize(avesteps=mean(steps,na.rm=TRUE))%>%ungroup()
p <- ggplot(weekavesteps, aes(interval, avesteps)) 
p+ geom_line()+facet_wrap(~ isweekday)
