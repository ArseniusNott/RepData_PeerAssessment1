---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
library(xtable)
library(tidyr)
data <- read.csv(unz("./activity.zip", "activity.csv"), header=TRUE, quote="\"", sep=",")
# see the first 5 rows of data
# head(data)
# show summary
# table <- xtable(summary(data))
# print(table, type="html")
# replace missing values with 0
# data <- data %>% mutate(steps = replace_na(steps, 0))
```

## What is mean total number of steps taken per day?

```r
# group data by date and sum the number of steps per day
num.of.steps.per.day <- data %>% group_by(date) %>% summarise(number.of.steps = sum(steps, na.rm = TRUE))
# generate plot
ggplot(num.of.steps.per.day) + geom_histogram(aes(x = number.of.steps), bins = 10) + xlab("Number of Steps") + ylab("Frequency") + ggtitle("Histogram of the Number of Steps Recorded Per Day")
```

![](PA1_template_files/figure-html/mean_total-1.png)<!-- -->

```r
# Calculate and report the mean and median of the total number of steps taken per day
centers <- num.of.steps.per.day %>% summarise(mean.number.of.steps = mean(number.of.steps, na.rm = TRUE), median.number.of.steps = median(number.of.steps, na.rm = TRUE))

print(centers)
```

```
## # A tibble: 1 x 2
##   mean.number.of.steps median.number.of.steps
##                  <dbl>                  <int>
## 1                9354.                  10395
```


## What is the average daily activity pattern?

```r
num.of.steps.per.interval <- data %>% group_by(interval) %>% summarise(average.steps = mean(steps, na.rm=TRUE))
ggplot(num.of.steps.per.interval, aes(x = interval, y = average.steps)) + geom_line()
```

![](PA1_template_files/figure-html/average_daily_pattern-1.png)<!-- -->


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
