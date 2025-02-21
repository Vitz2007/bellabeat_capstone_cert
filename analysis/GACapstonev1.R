# Install packages
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

# Load libraries
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

# Import data sets 
daily_activity <- read.csv("/Users/apple/GA Capstone/dailyActivity_merged.csv")
daily_steps <- read.csv("/Users/apple/GA Capstone/dailySteps_merged.csv")
sleep_days <- read.csv("/Users/apple/GA Capstone/sleepDay_merged.csv")
weight_log <- read.csv("/Users/apple/GA Capstone/weightLogInfo_merged.csv")

# Overview of data
kable(head(daily_activity))
kable(head(daily_steps))
kable(head(sleep_days))
kable(head(weight_log))

# Make column names consistent
daily_activity <- clean_names(daily_activity)
daily_steps <-clean_names(daily_steps)
sleep_days <- clean_names(sleep_days)
weight_log <- clean_names(weight_log)

# Change date and time to datetime standard
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

# Find correlation for 'daily_activity' data set using correlation plot
# Select columns with numerical data
corrmatrices <- select(daily_activity, total_steps, total_distance, tracker_distance, 
                       very_active_distance, moderately_active_distance, light_active_distance, 
                       sedentary_active_distance, very_active_minutes, fairly_active_minutes, 
                       lightly_active_minutes, sedentary_minutes, calories) %>%
  cor()
kable(head(round(corrmatrices,2)))

corrplot(corrmatrices, method = "number", type = "lower", addCoef.col = "black", tl.col = "black", 
         tl.srt = 90, tl.cex = 0.50) # Columns 'very_active_distance' and 'very_active_minutes' have high correlation to other columns

# Create plots based on columns with strong correlation
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

# Plot total distance and total steps on a given day
ggarrange(ggplot(daily_activity, aes(x=day_of_week, y=total_distance)) + geom_col(fill = c("green")) + 
            labs(title = "Total Distance for Days", x="Day of Week", y="Total Distance") + 
            theme_bw(), ggplot(daily_activity, aes(x=day_of_week, y=total_steps)) + 
            geom_col(fill = c("blue")) + labs(title = "Total Steps for Days", x="Day of Week", y="Total Steps") + 
            theme_bw()) # Looks like Tuesday seems to be the most active day for users

# Calculate y-intercept for sleep_days data set
model <- lm(total_minutes_asleep~total_time_in_bed, data = sleep_days)
y_intercept <- coef(model)[1]

# Create plot with y-intercept showing minutes asleep and in bed on given day
ggarrange(ggplot(sleep_days, aes(x=day_of_week, y=total_minutes_asleep)) + 
            geom_col(fill = c("orange")) 
          + geom_hline(yintercept = y_intercept, linetype = "dashed", color = "black") + labs(title = "Minutes Asleep by Day", x="", y="") + theme_bw(), 
ggplot(sleep_days, aes(x=day_of_week, y=total_time_in_bed)) + geom_col(fill = c("cyan")) + 
  geom_hline(yintercept = y_intercept, linetype = "dashed", color = "black") + 
  labs(title = "Minutes in Bed by Day", x="", y="") 
+ theme_bw()) # Wednesday seems to the day where people slept a lot and stayed in bed

# Get insights for weight_log, sleep_days, and daily activity data sets
daily_sleep_weight <- merge(x=daily_activity, y=weight_log, by=c(
  "id", "date", "day_of_week"), all.x=TRUE, all.y=TRUE)
daily_sleep_weight <- merge(x=daily_sleep_weight, y=sleep_days, by=c(
  "id", "date", "day_of_week"), all.x=TRUE, all.y=TRUE)

# Check NANs
kable(colSums(is.na(daily_sleep_weight)))

# Visual overview of NANs
nan_table <- md.pattern(daily_sleep_weight, plot = TRUE, rotate.names = TRUE)

# Use kNN to create impute value columns
impute_columns <- c("total_steps", "total_distance", "tracker_distance", 
                    "logged_activities_distance", "very_active_distance", 
                    "moderately_active_distance", "light_active_distance", 
                    "sedentary_active_distance", "very_active_minutes", 
                    "fairly_active_minutes", "lightly_active_minutes", "sedentary_minutes", 
                    "calories", "total_sleep_records.x", "total_minutes_asleep.x", "total_time_in_bed.x", 
                    "weight_kg", "weight_pounds", "bmi", "is_manual_report", "log_id", "fat")

# Impute missing values from columns using kNN
imputed_data <- kNN(daily_sleep_weight, variable = impute_columns)

# Plot graphs comparing weight to daily activities and sleeping time
# Total Time in Bed by Weight(kg)
ggplot(imputed_data, aes(x=total_time_in_bed.x, y=weight_kg)) + geom_point(color="#8B4513") + 
  geom_jitter(color="#8B4513") + labs(title = "Total Time in Bed by Weight (kg)", 
    x="weight (kg)", y="total time in bed") + theme_minimal() # time in bed vs weight

# Lightly Active Minutes over Total Sleep Minutes Asleep
ggplot(imputed_data, aes(x=lightly_active_minutes, y=total_minutes_asleep.x)) + 
  geom_point(color="#8B0A50") + geom_jitter(color="#8B0A50")+ labs(title = "Lightly Active Mins over Total Mins Asleep", 
                    x="Lightly Active Mins", y="Total Mins Asleep") + 
  theme_minimal() # Scatter plots show majority of people slept over 500 mins and had between 100-300 of lightly active mins

