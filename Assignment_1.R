if(!file.exists("reproducible_project1")) dir.create("reproducible_project1")
rm(list=ls())
activity <- read.csv("./reproducible_project1/activity.csv",colClasses = c("numeric", "character","integer"))

library(lubridate)
library(ggplot2)

total.steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
activity$date <- ymd(activity$date)

mean(total.steps)
median(total.steps)
steps <- activity %>%
filter(!is.na(steps)) %>%
group_by(date) %>%
summarize(steps = sum(steps)) %>%
print 

ggplot(steps, aes(x=date, y=steps))+geom_histogram(stat="identity")+ xlab("Dates")+ ylab("Steps")+ labs(title= "Total numbers of Steps per day")

daily <- activity %>%
filter(!is.na(steps)) %>%
group_by(interval) %>%
summarize(steps=mean(steps)) %>%
print

plot(daily, type = "l")

daily[which.max(daily$steps), ]$interval

missing <- sum(is.na(activity))
new <- activity %>%
group_by(interval) %>%
mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
summary(new)

new.steps <- new %>%
group_by(date) %>%
summarize(steps = sum(steps)) %>%
print 

ggplot(new.steps, aes(x=date, y=steps))+geom_histogram(stat="identity")+ xlab("Dates")+ ylab("Imputed Steps")+ labs(title= "Total numbers of Steps per day (missing data imputed)")

imputed.steps <- tapply(new$steps, new$date, FUN = sum, na.rm = TRUE)
new$date <- ymd(new$date)
mean(imputed.steps)

median(imputed.steps)
mean(total.steps)==mean(imputed.steps)
median(total.steps)==median(imputed.steps)
summary(total.steps)
summary(imputed.steps)
summary(imputed.steps) - summary(total.steps)

par(mfrow=c(2,1))
hist(imputed.steps,col="red")
hist(total.steps,col="blue")

dayofweek <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
new$daytype <- as.factor(sapply(new$date, dayofweek))
