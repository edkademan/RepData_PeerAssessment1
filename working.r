require("dplyr")
require("ggplot2")

act_data <- function(file = "activity.csv")
  read.csv(file, colClasses = c(date = "POSIXct"))

steps_per_day <- function(act = act_data(), na.rm = FALSE)
  act %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = na.rm))

steps_histogram <- function(spd = steps_per_day(), binwidth = 1000)
  ggplot(spd) +
    geom_histogram(aes(steps), binwidth = binwidth)

act        <- act_data()
spd        <- steps_per_day(act)
mean_spd   <- mean(spd$steps,   na.rm = TRUE)
median_spd <- median(spd$steps, na.rm = TRUE)

avg_per_interval <- function(act = act_data(), na.rm = TRUE)
  act %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = na.rm))

api          <- avg_per_interval(act)
max_interval <- api$interval[which.max(api$steps)]

### The `interval' variable identifies the starting minute of the day
### for a 5-minute interval. It is of the form hhmm where the hh
### digits give the hour and mm give the minutes after the hour. There
### are no leading zeros however.
###
### This function converts the interval id to the hour of the day
### which accurately represents that interval's place in a 24 hour
### period.

interval_to_hour <- function(i) (i %/% 100) + (i %% 100)/60

interval_plot <- function(api = avg_per_interval())
  ggplot(api) +
    geom_line(aes(interval_to_hour(interval), steps)) +
    xlab("hour of day") + ylab("avg steps")

### The `summary' function shows that NA's are all in the first
### column.

summary(act)
nna <- sum(is.na(act$steps))

### Return the activity data but where steps are missing for a
### particular day and interval replace them with the average number
### of steps for that interval.
imputed_data <- function(act = act_data(),
                         api = avg_per_interval(act)) { 
  i <- is.na(act$steps)
  act$steps[i] <- api$steps[match(act$interval[i], api$interval)]
  act}

## imp <- imputed_data(act, api)
## steps_histogram(steps_per_day(imp))
