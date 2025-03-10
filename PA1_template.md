# Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a single R markdown
document that can be processed by knitr and be transformed into an HTML
file.

### Loading and Preprocessing the Data

    # Load necessary libraries
    library(dplyr)
    library(ggplot2)

    # Load the data
    data <- read.csv("activity.csv")

    # Process/transform the data (convert date to Date type)
    data$date <- as.Date(data$date, format="%Y-%m-%d")

    # View the data
    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### Transform the Data into a suitable format

    activity_NONA <- na.omit(data)
    head(activity_NONA)

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

### What are the mean and median steps taken per day

    # Aggregate steps per day for visualization
    activity_per_day <- activity_NONA %>%
      group_by(date) %>%
      summarise(total_steps = sum(steps))

    head(activity_per_day)

    ## # A tibble: 6 × 2
    ##   date       total_steps
    ##   <date>           <int>
    ## 1 2012-10-02         126
    ## 2 2012-10-03       11352
    ## 3 2012-10-04       12116
    ## 4 2012-10-05       13294
    ## 5 2012-10-06       15420
    ## 6 2012-10-07       11015

### Plot a histogram of the total steps per day

    ggplot(activity_per_day, aes(x = total_steps)) +
      geom_histogram(binwidth = 500, fill = 'steelblue3', color = 'black') +
      ggtitle("Total Number of Steps Taken Each Day") +
      xlab("Total Steps") +
      ylab("Frequency")

![/RepData_PeerAssessment1/blob/master/Fig1.png](https://github.com/sbcybersolutions/RepData_PeerAssessment1/blob/master/Fig1.png)

### Summarize the statistics of total steps per day

    summary(activity_per_day)

    ##       date             total_steps   
    ##  Min.   :2012-10-02   Min.   :   41  
    ##  1st Qu.:2012-10-16   1st Qu.: 8841  
    ##  Median :2012-10-29   Median :10765  
    ##  Mean   :2012-10-30   Mean   :10766  
    ##  3rd Qu.:2012-11-16   3rd Qu.:13294  
    ##  Max.   :2012-11-29   Max.   :21194

### Aggregate average steps per interval for visualization

    activity_per_interval <- activity_NONA %>%
      group_by(interval) %>%
      summarise(mean_steps = mean(steps))

### Plot the time series of average steps per interval

    ggplot(activity_per_interval, aes(x = interval, y = mean_steps)) + 
      geom_line(color = "blue") +
      ggtitle("Average Number of Steps Taken per Interval") +
      xlab("Interval") +
      ylab("Average Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-31-1.png)

### Find the interval with the maximum average steps

    max_interval <- activity_per_interval[which.max(activity_per_interval$mean_steps),]
    print(max_interval)

    ## # A tibble: 1 × 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1      835       206.

### Count the number of missing values in the data set

    missing_values <- colSums(is.na(data))
    print(missing_values)

    ##    steps     date interval 
    ##     2304        0        0

### Impute missing values using the mean steps per interval

    activity_filled <- data %>%
      left_join(activity_per_interval, by = "interval") %>%
      mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
      select(-mean_steps)  # Remove the extra column created by join

    head(activity_filled)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

### Aggregate total steps per day after filling missing values

    activity_filled2 <- activity_filled %>%
      group_by(date) %>%
      summarise(total_steps = sum(steps))

### Plot histogram after imputing missing values

    ggplot(activity_filled2, aes(x = total_steps)) +
      geom_histogram(binwidth = 500, fill = 'red', color = 'black') +
      ggtitle("Total Number of Steps Taken Each Day - Missing Values Added") +
      xlab("Total Steps") +
      ylab("Frequency")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-36-1.png)

### Summary statistics after imputing missing values

    summary(activity_filled2)

    ##       date             total_steps   
    ##  Min.   :2012-10-01   Min.   :   41  
    ##  1st Qu.:2012-10-16   1st Qu.: 9819  
    ##  Median :2012-10-31   Median :10766  
    ##  Mean   :2012-10-31   Mean   :10766  
    ##  3rd Qu.:2012-11-15   3rd Qu.:12811  
    ##  Max.   :2012-11-30   Max.   :21194

### Categorize days as weekday or weekend

    activity_filled <- activity_filled %>%
      mutate(weekdayType = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))

### Compute mean steps per interval for weekdays vs weekends

    activity_filled3 <- activity_filled %>%
      group_by(interval, weekdayType) %>%
      summarise(mean_steps = mean(steps), .groups = "drop")

    head(activity_filled3)

    ## # A tibble: 6 × 3
    ##   interval weekdayType mean_steps
    ##      <int> <chr>            <dbl>
    ## 1        0 weekday         2.25  
    ## 2        0 weekend         0.215 
    ## 3        5 weekday         0.445 
    ## 4        5 weekend         0.0425
    ## 5       10 weekday         0.173 
    ## 6       10 weekend         0.0165

### Plot time series average steps by interval for weekdays and weekends

    ggplot(activity_filled3, aes(x = interval, y = mean_steps)) + 
      geom_line(color = "darkgreen") +
      facet_grid(rows = vars(weekdayType)) +
      ggtitle("Average Steps per Interval - Weekday vs. Weekend") +
      xlab("Interval") +
      ylab("Average Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-40-1.png)
