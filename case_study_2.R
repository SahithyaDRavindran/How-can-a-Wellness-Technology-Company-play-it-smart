#installing the required packages
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(sqldf)
library(ggplot2)
install.packages("janitor")
library(janitor)
library(plottrix)
library(matplotlib)
install.packages("matplotlib")
install.packages("plottrix")

  
#importing the data required for analysis

daily_activity <- read.csv("/Users/Admin/Documents/sahi_things/Google_data_analytics/case_study_2/archive/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

daily_calories <- read.csv("/Users/Admin/Documents/sahi_things/Google_data_analytics/case_study_2/archive/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

daily_sleep <- read.csv("/Users/Admin/Documents/sahi_things/Google_data_analytics/case_study_2/archive/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

daily_intensities <- read.csv("/Users/Admin/Documents/sahi_things/Google_data_analytics/case_study_2/archive/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")

weight_log <- read.csv("/Users/Admin/Documents/sahi_things/Google_data_analytics/case_study_2/archive/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

#exploring the dataset

head(daily_activity)
colnames(daily_activity)
glimpse(daily_activity) #we see that total rows count is 940

head(daily_calories)
colnames(daily_calories)
glimpse(daily_calories) #we see that total rows count is 940

head(daily_sleep)
colnames(daily_sleep)
glimpse(daily_sleep)

head(daily_intensities)
colnames(daily_intensities)
glimpse(daily_intensities) #we see that total rows count is 940

head(weight_log)
colnames(weight_log)
glimpse(weight_log)

#the data frames daily activity, daily intensity and daily calories have the same 940 rows. but the data frame daily activity already contains the 
#calories data and intensity data, let's cross check it.
daily_activity_2 <- daily_activity %>% 
  select(Id, ActivityDate, Calories)

head(daily_activity_2)

#let's cross check with the daily_activity data.
head(daily_calories)  #since daily calories data had column name as activity day instead of activity date, we have changed the column name to activity date
colnames(daily_calories)[2] = 'ActivityDate'

#we can now observe that daily activity data and daily calories data is matching.

#let's see if daily activity and daily internsity data are matching
daily_activity_3 <- daily_activity %>% 
  select(Id, ActivityDate, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, SedentaryActiveDistance,
         LightActiveDistance, ModeratelyActiveDistance, VeryActiveDistance)

head(daily_activity_3)

#let's cross check with the daily_activity data.
head(daily_intensities) #since daily calories data had column name as activity day instead of activity date, we have changed the column name to activity date
colnames(daily_intensities)[2] = 'ActivityDate'

#both data frame data are matching.
#so its enough if we take the data frames daily activity, daily sleep and weight_log into consideration and proceed with analysis.

#ANALYSIS
#daily activity data frame has more values than daily sleep and weight log data. Lets cross check if there are more observations in daily activity data frame
length(unique(daily_activity$Id)) #-- distinct ID count of daily activity data frame

length(unique(daily_sleep$Id)) #-- distinct ID count of daily sleep data frame

length(unique(weight_log$Id)) #-- distinct ID count of weight log data frame

#summary of daily activity
daily_activity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes, VeryActiveMinutes) %>% 
  summary()

#summary of daily sleep
daily_sleep %>% 
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()

#summary of weight log
weight_log %>% 
  select(WeightPounds, BMI) %>% 
  summary()

#Visualizations
#relationship between steps taken in a day and sedentary Minutes.
ggplot(data = daily_activity, aes(x=TotalSteps, y = SedentaryMinutes, color = Calories))+geom_point()
#-- the relationship seems to be negative because one cannot move when he/she is inactive.

#relationship between calories and total steps
ggplot(data = daily_activity, aes(x=TotalSteps, y= Calories, color = SedentaryMinutes))+geom_point()+
  geom_smooth(method = lm) + scale_color_gradient(low="beige", high="goldenrod4") +
  labs(title="Total Steps vs Calories") 

#In this scatter plot, we can see the total steps vs the calories burned.There is a clear 
#relationship between the two.
#There are users who have not taken any steps, yet burned 1000 to 2500 calories.
#There are also users who took have taken more 20000 steps, but the calories burned are as 
#with other people only

# We can also intimate people
#that to burn more calories, one has to do more workouts.

#relationship between sleep and time in bed
ggplot(data = daily_sleep, aes(x = TotalMinutesAsleep, y=TotalTimeInBed))+geom_point()
#there are some outliers like some people tend to spend more time in bed without sleeping and some
#who spend equal time of being asleep and spending time in bed. We can easily market to customers to notice
#their sleep time with regard to time they spend being in bed.

#total minutes asleep vs weekday
sleep_day <- daily_sleep %>% mutate(Weekday = weekdays(as.Date(SleepDay, "%m/%d/%Y")))
ggplot(data=sleep_day, aes(x=Weekday, y=TotalMinutesAsleep)) + 
  geom_step()

#we can infer that users sleep the most on Saturday and Monday and the least on Wednesday, Thursday
#and Friday

#total distance vs total steps
ggplot(data=daily_activity, aes(x=TotalSteps, y = TotalDistance, color=TotalDistance))+ 
  geom_point() + 
  geom_jitter() +
  scale_y_log10() +
  geom_smooth(method="gam", formula = y ~s(x)) +
  scale_color_gradient(low="yellowgreen", high="deepskyblue") +
  labs(title="Total Steps vs. Total Distance") +
  theme(legend.position="none")

#In this scatter plot, we can see the relationship between the total steps took and total distance'
#walked (in km).
#we can see that there's a positive relationship between the 2 variables and mostly we can also infer that
#users have walked upto 10 km per day.


# Percentage of activity in minutes
daily_activity %>% mutate(sum(daily_activity$VeryActiveMinutes)) #sum of very_activity_min
daily_activity %>% mutate(sum(daily_activity$FairlyActiveMinutes)) #sum of FairlyActiveMinutes
daily_activity %>% mutate(sum(daily_activity$LightlyActiveMinutes)) #sum of LightlyActiveMinutes
daily_activity %>% mutate(sum(daily_activity$SedentaryMinutes)) #sum of SedentaryMinutes

slices <- c(19895,12751,181244,931738)
slices

piepercent <- round(100*slices/sum(slices),1)
colors = c("khaki","blueviolet","hotpink2","gold")

pie3D(slices,labels = paste0(piepercent,"%"),col=colors,main = "Percentage of Activity in Minutes")
legend("bottomright",c("VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","SedentaryMinutes"),cex=0.9,fill = colors)

# We can see that percentage of sedentary minutes is huge compared to other activities with a total of 81.3% which indicates that users are inactive for most of the time.
# Very active and fairly active minutes percetnage is also vert=Y less.


#Percentage of activity in distance
daily_activity %>% mutate(sum(daily_activity$VeryActiveDistance)) #sum of VeryActiveDistance - 1412.52
daily_activity %>% mutate(sum(daily_activity$ModeratelyActiveDistance)) #sum of ModeratelyActiveDistance - 533.49
daily_activity %>% mutate(sum(daily_activity$LightActiveDistance)) #sum of LightActiveDistance - 3140.37

slices <- c(1412.52,533.49,3140.37)
slices

piepercent <- round(100*slices/sum(slices),1)
colors = c("khaki","blueviolet","hotpink2","gold")

pie3D(slices,labels = paste0(piepercent,"%"),col=colors,main = "Percentage of Activity in Minutes")
legend("bottomright",c("VeryActiveDistance","ModeratelyActiveDistance","LightActiveDistance"),cex=1.4,fill = colors)

# The percentage of lightlyactivedistance is more with 61.7% and moderately active distance is that of 10.5
# and very active distance is of 27.8%. 
# Percentage of very active distance is good, but it can be increased to achieve the fitness goals.


# combining the datasets for processing in Tableau.
daily_activity_sleep_combined <- merge(daily_activity, daily_sleep, by = "Id")
head(daily_activity_sleep_combined)

daily_activity_sleep_combine <- merge(daily_activity, daily_sleep, by = "Id", all = TRUE)
length(unique(daily_activity_sleep_combine$Id)) #-- distinct ID count of daily activity data frame

daily_activity_sleep_weight_combined <- merge(daily_activity_sleep_combined, weight_log , by = "Id", all = TRUE)
length(unique(daily_activity_sleep_weight_combined$Id)) #-- distinct ID count of daily activity data frame

write_csv(daily_activity_sleep_weight_combined, file = "final_data_for_viz.csv")
