bellabeat-case-study
================
Eneye
7/31/2021

## How Can a Wellness Technology Company Play It Smart?

**Introduction**

Bellabeat is a high-tech manufacturer of health-focused products for
women. For this case-study, we will analyze smart device usage data in
order to gain insight into how consumers use non-Bellabeat smart
devices.

**Business Task**

For this analysis, we want to find out: *What are some trends in smart
device usage? *How could these trends apply to Bellabeat customers?
\*How could these trends help influence Bellabeat marketing strategy?

**Data Source**

We will use a public data that explores smart device users’ daily
habits.

FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available
through Mobius): This Kaggle data set contains personal fitness tracker
from thirty fitbit users. Thirty eligible Fitbit users consented to the
submission of personal tracker data, including minute-level output for
physical activity, heart rate, and sleep monitoring. It includes
information about daily activity, steps, and heart rate that can be used
to explore users’ habits.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

## Import Datasets

``` r
daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
heart_rate <- read.csv("heartrate_seconds_merged.csv")
weight_info <- read.csv("weightLogInfo_merged.csv")
```

## Explore and clean the imported data

Take a look at the data.

``` r
head(daily_activity)
```

    ##           Id ActivityDate TotalSteps TotalDistance TrackerDistance
    ## 1 1503960366    4/12/2016      13162          8.50            8.50
    ## 2 1503960366    4/13/2016      10735          6.97            6.97
    ## 3 1503960366    4/14/2016      10460          6.74            6.74
    ## 4 1503960366    4/15/2016       9762          6.28            6.28
    ## 5 1503960366    4/16/2016      12669          8.16            8.16
    ## 6 1503960366    4/17/2016       9705          6.48            6.48
    ##   LoggedActivitiesDistance VeryActiveDistance ModeratelyActiveDistance
    ## 1                        0               1.88                     0.55
    ## 2                        0               1.57                     0.69
    ## 3                        0               2.44                     0.40
    ## 4                        0               2.14                     1.26
    ## 5                        0               2.71                     0.41
    ## 6                        0               3.19                     0.78
    ##   LightActiveDistance SedentaryActiveDistance VeryActiveMinutes
    ## 1                6.06                       0                25
    ## 2                4.71                       0                21
    ## 3                3.91                       0                30
    ## 4                2.83                       0                29
    ## 5                5.04                       0                36
    ## 6                2.51                       0                38
    ##   FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes Calories
    ## 1                  13                  328              728     1985
    ## 2                  19                  217              776     1797
    ## 3                  11                  181             1218     1776
    ## 4                  34                  209              726     1745
    ## 5                  10                  221              773     1863
    ## 6                  20                  164              539     1728

``` r
head(heart_rate)
```

    ##           Id                 Time Value
    ## 1 2022484408 4/12/2016 7:21:00 AM    97
    ## 2 2022484408 4/12/2016 7:21:05 AM   102
    ## 3 2022484408 4/12/2016 7:21:10 AM   105
    ## 4 2022484408 4/12/2016 7:21:20 AM   103
    ## 5 2022484408 4/12/2016 7:21:25 AM   101
    ## 6 2022484408 4/12/2016 7:22:05 AM    95

``` r
head(sleep_day)
```

    ##           Id              SleepDay TotalSleepRecords TotalMinutesAsleep
    ## 1 1503960366 4/12/2016 12:00:00 AM                 1                327
    ## 2 1503960366 4/13/2016 12:00:00 AM                 2                384
    ## 3 1503960366 4/15/2016 12:00:00 AM                 1                412
    ## 4 1503960366 4/16/2016 12:00:00 AM                 2                340
    ## 5 1503960366 4/17/2016 12:00:00 AM                 1                700
    ## 6 1503960366 4/19/2016 12:00:00 AM                 1                304
    ##   TotalTimeInBed
    ## 1            346
    ## 2            407
    ## 3            442
    ## 4            367
    ## 5            712
    ## 6            320

``` r
head(weight_info)
```

    ##           Id                  Date WeightKg WeightPounds Fat   BMI
    ## 1 1503960366  5/2/2016 11:59:59 PM     52.6     115.9631  22 22.65
    ## 2 1503960366  5/3/2016 11:59:59 PM     52.6     115.9631  NA 22.65
    ## 3 1927972279  4/13/2016 1:08:52 AM    133.5     294.3171  NA 47.54
    ## 4 2873212765 4/21/2016 11:59:59 PM     56.7     125.0021  NA 21.45
    ## 5 2873212765 5/12/2016 11:59:59 PM     57.3     126.3249  NA 21.69
    ## 6 4319703577 4/17/2016 11:59:59 PM     72.4     159.6147  25 27.45
    ##   IsManualReport        LogId
    ## 1           True 1.462234e+12
    ## 2           True 1.462320e+12
    ## 3          False 1.460510e+12
    ## 4           True 1.461283e+12
    ## 5           True 1.463098e+12
    ## 6           True 1.460938e+12

``` r
colnames(daily_activity)
```

    ##  [1] "Id"                       "ActivityDate"            
    ##  [3] "TotalSteps"               "TotalDistance"           
    ##  [5] "TrackerDistance"          "LoggedActivitiesDistance"
    ##  [7] "VeryActiveDistance"       "ModeratelyActiveDistance"
    ##  [9] "LightActiveDistance"      "SedentaryActiveDistance" 
    ## [11] "VeryActiveMinutes"        "FairlyActiveMinutes"     
    ## [13] "LightlyActiveMinutes"     "SedentaryMinutes"        
    ## [15] "Calories"

``` r
colnames(heart_rate)
```

    ## [1] "Id"    "Time"  "Value"

``` r
colnames(sleep_day)
```

    ## [1] "Id"                 "SleepDay"           "TotalSleepRecords" 
    ## [4] "TotalMinutesAsleep" "TotalTimeInBed"

``` r
colnames(weight_info)
```

    ## [1] "Id"             "Date"           "WeightKg"       "WeightPounds"  
    ## [5] "Fat"            "BMI"            "IsManualReport" "LogId"

Notice that heart\_rate summarizes its observations per 5 seconds. We
want to aggregate it as day and get the average of the values.

``` r
heart_rate$Date <- as.Date(heart_rate$Time, format = "%m/%d/%y")
heart_rate2 <- aggregate(Value ~ Id + Date, data = heart_rate, FUN = mean)
```

We can see that the datasets have an ID field, which we can possibly use
to merge them.

## Understanding some summary statistics

How many observations are there in each dataframe?

``` r
nrow(daily_activity)
```

    ## [1] 940

``` r
nrow(heart_rate)
```

    ## [1] 2483658

``` r
nrow(sleep_day)
```

    ## [1] 413

``` r
nrow(weight_info)
```

    ## [1] 67

What are some quick summary statistics we’d want to know about each data
frame?

For the daily\_activity dataframe:

``` r
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         Calories) %>%
  summary()
```

    ##    TotalSteps    TotalDistance    SedentaryMinutes    Calories   
    ##  Min.   :    0   Min.   : 0.000   Min.   :   0.0   Min.   :   0  
    ##  1st Qu.: 3790   1st Qu.: 2.620   1st Qu.: 729.8   1st Qu.:1828  
    ##  Median : 7406   Median : 5.245   Median :1057.5   Median :2134  
    ##  Mean   : 7638   Mean   : 5.490   Mean   : 991.2   Mean   :2304  
    ##  3rd Qu.:10727   3rd Qu.: 7.713   3rd Qu.:1229.5   3rd Qu.:2793  
    ##  Max.   :36019   Max.   :28.030   Max.   :1440.0   Max.   :4900

For the heart\_rate dataframe:

``` r
heart_rate %>% 
  select(Value) %>% 
  summary()
```

    ##      Value       
    ##  Min.   : 36.00  
    ##  1st Qu.: 63.00  
    ##  Median : 73.00  
    ##  Mean   : 77.33  
    ##  3rd Qu.: 88.00  
    ##  Max.   :203.00

For the sleep\_day dataframe:

``` r
sleep_day %>% 
  select(TotalMinutesAsleep,
         TotalSleepRecords,
         TotalTimeInBed) %>% 
  summary()
```

    ##  TotalMinutesAsleep TotalSleepRecords TotalTimeInBed 
    ##  Min.   : 58.0      Min.   :1.000     Min.   : 61.0  
    ##  1st Qu.:361.0      1st Qu.:1.000     1st Qu.:403.0  
    ##  Median :433.0      Median :1.000     Median :463.0  
    ##  Mean   :419.5      Mean   :1.119     Mean   :458.6  
    ##  3rd Qu.:490.0      3rd Qu.:1.000     3rd Qu.:526.0  
    ##  Max.   :796.0      Max.   :3.000     Max.   :961.0

For the weight\_info dataframe:

``` r
weight_info %>% 
  select(WeightKg,
         WeightPounds,
         BMI) %>% 
  summary()
```

    ##     WeightKg       WeightPounds        BMI       
    ##  Min.   : 52.60   Min.   :116.0   Min.   :21.45  
    ##  1st Qu.: 61.40   1st Qu.:135.4   1st Qu.:23.96  
    ##  Median : 62.50   Median :137.8   Median :24.39  
    ##  Mean   : 72.04   Mean   :158.8   Mean   :25.19  
    ##  3rd Qu.: 85.05   3rd Qu.:187.5   3rd Qu.:25.56  
    ##  Max.   :133.50   Max.   :294.3   Max.   :47.54

Let’s show some visualizations to have a better understanding of how
some activities are related

## Plotting a few explorations

**What’s the relationship between steps taken in a day and sedentary
minutes?**

``` r
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Sedentary Minutes Vs TotalSteps")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](bellabeat-case-study_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
The plot shows a negative correlation between sedentary minutes and
total steps taken.

**What is the relationship between steps taken in a day and calories**

``` r
ggplot(data = daily_activity, aes(x=TotalSteps, y=Calories)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Calories Burned Vs TotalSteps")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](bellabeat-case-study_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
The plot shows a positive correlation between calories burned and total
steps taken.

## Merging the datasets together

Before we merge the data, it is important to check the number of unique
participants in each data frame.

``` r
n_distinct(daily_activity$Id)
```

    ## [1] 33

``` r
n_distinct(heart_rate$Id)
```

    ## [1] 14

``` r
n_distinct(sleep_day$Id)
```

    ## [1] 24

``` r
n_distinct(weight_info$Id)
```

    ## [1] 8

daily\_activity has the highest number of participants, followed by
sleep\_day.

Merging the four data means that we will have just eight participants
with complete records, which is too small for a sample size.

We will therefore merge just daily\_activity and sleep\_da.

``` r
combined_data <- merge(daily_activity, sleep_day, by="Id")
```

``` r
n_distinct(combined_data$Id)
```

    ## [1] 24

**Explore combined\_data**

``` r
head(combined_data)
```

    ##           Id ActivityDate TotalSteps TotalDistance TrackerDistance
    ## 1 1503960366     5/7/2016      11992          7.71            7.71
    ## 2 1503960366     5/7/2016      11992          7.71            7.71
    ## 3 1503960366     5/7/2016      11992          7.71            7.71
    ## 4 1503960366     5/7/2016      11992          7.71            7.71
    ## 5 1503960366     5/7/2016      11992          7.71            7.71
    ## 6 1503960366     5/7/2016      11992          7.71            7.71
    ##   LoggedActivitiesDistance VeryActiveDistance ModeratelyActiveDistance
    ## 1                        0               2.46                     2.12
    ## 2                        0               2.46                     2.12
    ## 3                        0               2.46                     2.12
    ## 4                        0               2.46                     2.12
    ## 5                        0               2.46                     2.12
    ## 6                        0               2.46                     2.12
    ##   LightActiveDistance SedentaryActiveDistance VeryActiveMinutes
    ## 1                3.13                       0                37
    ## 2                3.13                       0                37
    ## 3                3.13                       0                37
    ## 4                3.13                       0                37
    ## 5                3.13                       0                37
    ## 6                3.13                       0                37
    ##   FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes Calories
    ## 1                  46                  175              833     1821
    ## 2                  46                  175              833     1821
    ## 3                  46                  175              833     1821
    ## 4                  46                  175              833     1821
    ## 5                  46                  175              833     1821
    ## 6                  46                  175              833     1821
    ##                SleepDay TotalSleepRecords TotalMinutesAsleep TotalTimeInBed
    ## 1 4/12/2016 12:00:00 AM                 1                327            346
    ## 2 4/13/2016 12:00:00 AM                 2                384            407
    ## 3 4/15/2016 12:00:00 AM                 1                412            442
    ## 4 4/16/2016 12:00:00 AM                 2                340            367
    ## 5 4/17/2016 12:00:00 AM                 1                700            712
    ## 6 4/19/2016 12:00:00 AM                 1                304            320

``` r
colnames(combined_data)
```

    ##  [1] "Id"                       "ActivityDate"            
    ##  [3] "TotalSteps"               "TotalDistance"           
    ##  [5] "TrackerDistance"          "LoggedActivitiesDistance"
    ##  [7] "VeryActiveDistance"       "ModeratelyActiveDistance"
    ##  [9] "LightActiveDistance"      "SedentaryActiveDistance" 
    ## [11] "VeryActiveMinutes"        "FairlyActiveMinutes"     
    ## [13] "LightlyActiveMinutes"     "SedentaryMinutes"        
    ## [15] "Calories"                 "SleepDay"                
    ## [17] "TotalSleepRecords"        "TotalMinutesAsleep"      
    ## [19] "TotalTimeInBed"
