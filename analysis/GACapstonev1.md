# Install packages
``` r 
install.packages('tidyverse')
install.packages('tidyr')
install.packages('skimr')
install.packages('janitor')
install.packages('corrplot')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('VIM')
install.packages('mice')
install.packages('reshape')
install.packages('kableExtra')
```

# Load libraries
``` r
library(tidyverse) # Cleaning dataset
library(tidyr)
library(skimr)
library(janitor)
library(corrplot) # Find correlation between columns
library(ggplot2) # Plot graphs
library(ggpubr) # plot graphs side by side
library(VIM) # use to impute NAs
library(mice) # use to visualize NAs
library(kableExtra) # make outputs cleaner
```

# Import data sets
``` r
daily_activity <- read.csv("/Users/apple/GA Capstone/dailyActivity_merged.csv")
daily_steps <- read.csv("/Users/apple/GA Capstone/dailySteps_merged.csv")
sleep_days <- read.csv("/Users/apple/GA Capstone/sleepDay_merged.csv")
weight_log <- read.csv("/Users/apple/GA Capstone/weightLogInfo_merged.csv")
```

# Overview of data
``` r
kable(head(daily_activity))
kable(head(daily_steps))
kable(head(sleep_days))
kable(head(weight_log))
```

# Make column names consistent
``` r
daily_activity <- clean_names(daily_activity)
daily_steps <-clean_names(daily_steps)
sleep_days <- clean_names(sleep_days)
weight_log <- clean_names(weight_log)
```

# Change date and time to datetime standard
``` r
daily_activity <- daily_activity %>%
  rename(date = activity_date) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_steps <- daily_steps %>%
  rename(date = activity_day) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

sleep_days <- sleep_days %>%
  rename(date = sleep_day) %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y", tz=Sys.timezone()))

weight_log <- weight_log %>%
  rename(date = date) %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))

# Add day of the week column for each data set
daily_activity$day_of_week <- wday(daily_activity$date, label = TRUE)
daily_steps$day_of_week <- wday(daily_steps$date, label = TRUE)
sleep_days$day_of_week <- wday(sleep_days$date, label = TRUE)
weight_log$day_of_week <- wday(weight_log$date, label = TRUE)

#Identify unique users and duplicates in data sets
n_distinct(daily_activity$id)
n_distinct(daily_steps$id)
n_distinct(sleep_days$id)
n_distinct(weight_log$id)

sum(duplicated(daily_activity))
sum(duplicated(daily_steps))
sum(duplicated(sleep_days))
sum(duplicated(weight_log))

# Remove duplicates and NA in 'sleep_days' and other data sets
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_steps <- daily_steps %>%
  distinct() %>%
  drop_na()

sleep_days <- sleep_days %>%
  distinct() %>%
  drop_na()
```

# Find correlation for 'daily_activity' data set using correlation plot
# Select columns with numerical data
``` r
corrmatrices <- select(daily_activity, total_steps, total_distance, tracker_distance, 
                       very_active_distance, moderately_active_distance, light_active_distance, 
                       sedentary_active_distance, very_active_minutes, fairly_active_minutes, 
                       lightly_active_minutes, sedentary_minutes, calories) %>%
  cor()
kable(head(round(corrmatrices,2)))

corrplot(corrmatrices, method = "number", type = "lower", addCoef.col = "black", tl.col = "black", 
         tl.srt = 70, tl.cex = 0.50) # Columns 'very_active_distance' and 'very_active_minutes' have high correlation to other columns
```
<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/01correlation_plot.png" />

We can see strong positive correlations between total_steps, total_distance, and tracker_distance which might be measuring the same thing, so we will use a couple of them. Also we see high activity levels for very_active and fairly_active. However sedentary time has very little correlation from activity levels. 

# Create plots based on columns with strong correlation
``` r
ggarrange(ggplot(daily_activity, aes(x=very_active_distance, y=total_distance)) + 
  geom_point(color = "#800080") + geom_jitter(color = "#800080") + geom_smooth(color="green") + 
  labs(title = "Total Distance over Very Active Distance", x="Very Active Distance", y="Total Distance") + 
  theme_minimal(), ggplot(daily_activity, aes(x=very_active_distance, y=total_steps)) + 
    geom_point(color = "#800000") + geom_jitter(color = "#800000") + geom_smooth(color="green") + labs(title = "Total Steps over Very Active Distance", 
                                        x="Very Active Distance", y="Total Steps") + 
    theme_minimal()) # A mixed bag of people around '5' very active distance some around '10'

ggarrange(ggplot(daily_activity, aes(x=very_active_minutes, y=total_distance)) + 
            geom_point(color = "#CCCCFF") + geom_jitter(color = "#CCCCFF") + geom_smooth(color = "green") + 
            labs(title = "Total Distance over Very Active Minutes", x="Very Active Minutes", y="Total Distance") + 
            theme_minimal(), ggplot(daily_activity, aes(x=very_active_minutes, y=total_steps)) + 
            geom_point(color = "#FF7F50") + geom_jitter(color = "#FF7F50") + geom_smooth(color = "green") + labs(title = "Total Steps over Very Active Minutes", 
                                                                                                   x="Very Active Minutes", y="Total Steps") + 
            theme_minimal()) # Both plots show users very active between 0 to 75 mins
```
<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/02scatterplot%20of%20correlated%20columns.png" />

<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/03total%20distance%20and%20steps%20over%20very%20active%20minutes.png" />

The curvature in the first set of scatter plots shows a positive correlation, however a very strong relation can be seen in the 0-5 range of Very Active Distance. We can assume that people who are very active naturally cover more ground or distance and have more steps.

The second set of scatter plots shows data points more concentrated in the 0-75 minute range indicating that most users have very active periods within this time frame. The confidence interval tells us that there is a margin of error surrounding the green line. In total, the plots indicate that being very active for a short time has a big impact on total distance and total steps. 

# Plot total distance and total steps on a given day
``` r
ggarrange(ggplot(daily_activity, aes(x=day_of_week, y=total_distance)) + geom_col(fill = c("green")) + 
            labs(title = "Total Distance for Days", x="Day of Week", y="Total Distance") + 
            theme_bw(), ggplot(daily_activity, aes(x=day_of_week, y=total_steps)) + 
            geom_col(fill = c("blue")) + labs(title = "Total Steps for Days", x="Day of Week", y="Total Steps") + 
            theme_bw()) # Looks like Tuesday seems to be the most active day for users
```
<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/04total%20distance%20and%20steps%20for%20days.png" />

Both bar graphs show that there is a strong correlation between steps and distance. We can understand from these that users were most active on Tuesday and then became less active winding down to Friday. A spike occurs on Saturday possibly around the daytime leading to a quiet Sunday. We can summarize that weekdays tend to be more active than weekends.

# Calculate y-intercept for sleep_days data set
``` r
model <- lm(total_minutes_asleep~total_time_in_bed, data = sleep_days)
y_intercept <- coef(model)[1]
```

# Create plot with y-intercept showing minutes asleep and in bed on given day
``` r
ggarrange(ggplot(sleep_days, aes(x=day_of_week, y=total_minutes_asleep)) + 
            geom_col(fill = c("orange")) 
          + geom_hline(yintercept = y_intercept, linetype = "dashed", color = "black") + labs(title = "Minutes Asleep by Day", x="", y="") + theme_bw(), 
ggplot(sleep_days, aes(x=day_of_week, y=total_time_in_bed)) + geom_col(fill = c("cyan")) + 
  geom_hline(yintercept = y_intercept, linetype = "dashed", color = "black") + 
  labs(title = "Minutes in Bed by Day", x="", y="") 
+ theme_bw()) # Wednesday seems to the day where people slept a lot and stayed in bed
```
<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/05mins%20asleep%20and%20in%20bed%20by%20day%20y-intercept.png" />

Here we can see that both bar graphs depict that the highest amount of sleep time and time in bed happens on Wednesday and Thursday. There seems to be a noticeable pattern where users spend more time in bed rather than sleeping, possibly due to smartphone usage, difficult falling asleep, or some other factor. What we can gather is that people tend to sleep less on Monday with increase in sleep duration by mid week. 


# Get insights for weight_log, sleep_days, and daily activity data sets
``` r
daily_sleep_weight <- merge(x=daily_activity, y=weight_log, by=c(
  "id", "date", "day_of_week"), all.x=TRUE, all.y=TRUE)
daily_sleep_weight <- merge(x=daily_sleep_weight, y=sleep_days, by=c(
  "id", "date", "day_of_week"), all.x=TRUE, all.y=TRUE)
```

# Check NANs
``` r
kable(colSums(is.na(daily_sleep_weight)))
```

# Visual overview of NANs
``` r
nan_table <- md.pattern(daily_sleep_weight, plot = TRUE, rotate.names = TRUE)
```
<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/visual%20of%20NANs%20mdpattern.png" />

# Use kNN to create impute value columns
``` r
impute_columns <- c("total_steps", "total_distance", "tracker_distance", 
                    "logged_activities_distance", "very_active_distance", 
                    "moderately_active_distance", "light_active_distance", 
                    "sedentary_active_distance", "very_active_minutes", 
                    "fairly_active_minutes", "lightly_active_minutes", "sedentary_minutes", 
                    "calories", "total_sleep_records", "total_minutes_asleep", "total_time_in_bed", 
                    "weight_kg", "weight_pounds", "bmi", "is_manual_report", "log_id", "fat")
```

# Impute missing values from columns using kNN
``` r
imputed_data <- kNN(daily_sleep_weight, variable = impute_columns)
```

# Plot graphs comparing weight to daily activities and sleeping time
``` r
# Total Time in Bed by Weight(kg)
ggplot(imputed_data, aes(x=total_time_in_bed, y=weight_kg)) + geom_point(color="#8B4513") + 
  geom_jitter(color="#8B4513") + labs(title = "Total Time in Bed by Weight (kg)", 
    x="weight (kg)", y="total time in bed") + theme_minimal() # time in bed vs weight

# Lightly Active Minutes over Total Sleep Minutes Asleep
ggplot(imputed_data, aes(x=lightly_active_minutes, y=total_minutes_asleep)) + 
  geom_point(color="#8B0A50") + geom_jitter(color="#8B0A50")+ labs(title = "Lightly Active Mins over Total Mins Asleep", 
                    x="Lightly Active Mins", y="Total Mins Asleep") + 
  theme_minimal() # Scatter plots show majority of people slept over 500 mins and had between 100-300 of lightly active mins
```
<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/06time%20in%20bed%20by%20weight.png" />

<img src="https://github.com/Vitz2007/bellabeat_capstone_cert/blob/main/images/07lightly%20active%20min%20over%20mins%20asleep.png" />

From the looks of it, the first scatter plot shows there is zero correlation between total time spent in bed and user's weight. Even having imputed the missing values, zero relationship can be found in this data set. 

The second scatter plot depicts a strong cluster between 400 to 600 minutes of sleep with most light activity minutes under 400 minutes. Overall, again the scatter plot shows us that both total minutes asleep and light active minutes are independent from one another. 


## Summary and recommendations based on findings ##

Beallabeat's Time product which tracks a users activity, sleep, and stress. 

Based on our analysis above and visualizations plotted, our insights uncovered that there were some high to medium correlations for total steps, total distance, very active minutes and distance, and lightly active minutes. This lead to other insights when combined with the sleep dataset and categorizing by day. 

Our recommendations are the following:

### Activity Levels

- Bellabeat implement a new feature that encourages users to maintain their activity levels during the middle of the week such as reminders to workout and increased weekend activity. Weak activity levels on Sunday gives Bellabeat an opportunity to improve this area and profit.

### Sleeping Consistency

- Bellabeat introduce games or scores showing sleep quality to maintain consistent sleep time and reduce time in bed. 

### Very Active Minutes 

- Bellabeat can benefit by putting prioritization on very active minutes. Because there is strong correlation for very active minutes and calorie in our correlation chart, Bellabeat can encourage and target users aiming for better weight management to focus more on intense activities such as running.
