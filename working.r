require("dplyr")
require("ggplot2")

get_data <- function(file = "activity.csv")
  read.csv(file, colClasses = c(date = "POSIXct"))

steps_per_day <- function(d = get_data(), na.rm = FALSE)
  d %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = na.rm))

steps_histogram <- function(d = steps_per_day(), binwidth = 1000)
  ggplot(d) +
    geom_histogram(aes(steps), binwidth = binwidth)

d          <- get_data()
spd        <- steps_per_day(d)
mean_spd   <- mean(spd$steps,   na.rm = TRUE)
median_spd <- median(spd$steps, na.rm = TRUE)

avg_per_interval <- function(d = get_data(), na.rm = TRUE)
  d %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = na.rm))

api          <- avg_per_interval(d)
max_interval <- api$interval[which.max(api$steps)]

### The `interval' variable identifies the starting minute of the day
### for a 5-minute interval. It is of the form hhmm where the hh
### digits give the hour and mm give the minutes after the hour. There
### are no leading zeros however.
###
### This function converts the interval id to the minute of the day
### which accurately represents that interval's place in a 24 hour
### period.

interval_to_minute <- function(i) 60*(i %/% 100) + (i %% 100)

interval_plot <- function(d = avg_per_interval())
  ggplot(d) +
    geom_line(aes(interval_to_minute(interval), steps)) +
    xlab("minute of day") + ylab("avg steps")

### The `summary' function shows that NA's are all in the first
### column.

summary(d)
nna <- sum(is.na(d$steps))
