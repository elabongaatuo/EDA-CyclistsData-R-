#This is my Google Data Analytics Capstone Project. 
#In this document, I will be analyzing data from a Bicycle Rental Company to understand Customer behaviour
#------------------------------------------------------------#

#installing the required packages
#tidyverse for data importing and wrangling
#lubridate for date functions
#ggplot for data visualization

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
 

#loading the necessary packages

library(tidyverse)
library(lubridate)
library(ggplot2)


#displaying and setting up directory

getwd() #displaying current directory
setwd("the\\path\\to\\your\\directory\\divvy-tripdata-csv")
getwd() #double checking to ensure we have chosen the right directory


#STEP ONE : COLLECTING DATA 

#Uploading Divvy dataset as CSV files

q2_2021<- read.csv("Q2_2021_CSV.csv")
q3_2021<- read.csv("Q3_2021_CSV.csv")
q4_2021<- read.csv("Q4_2021_CSV.csv")
q1_2022<- read.csv("Q1_2022_CSV.csv")

#STEP TWO: WRANGLE DATA AND COMBINE INTO A SINGLE DATASET

#comparing the column names of each of the files

colnames(q2_2021)
colnames(q3_2021)
colnames(q4_2021)
colnames(q1_2022)


#Renaming columns so that they are consistent with q2_2021 as this will be used as the standard table going forward

q2_2021 <- rename(q2_2021
                  ,trip_id = ride_id 
                  ,bikeid = rideable_type  
                  ,start_time = started_at 
                  ,end_time = ended_at  
                  ,from_station_name = start_station_name 
                  ,from_station_id = start_station_id  
                  ,to_station_name = end_station_name 
                  ,to_station_id = end_station_id 
                  ,usertype = member_casual)


q3_2021 <- rename(q3_2021
                  ,trip_id = ride_id 
                  ,bikeid = rideable_type  
                  ,start_time = started_at 
                  ,end_time = ended_at  
                  ,from_station_name = start_station_name 
                  ,from_station_id = start_station_id  
                  ,to_station_name = end_station_name 
                  ,to_station_id = end_station_id 
                  ,usertype = member_casual)


q4_2021 <- rename(q4_2021
                   ,trip_id = ride_id 
                   ,bikeid = rideable_type  
                   ,start_time = started_at 
                   ,end_time = ended_at  
                   ,from_station_name = start_station_name 
                   ,from_station_id = start_station_id  
                   ,to_station_name = end_station_name 
                   ,to_station_id = end_station_id 
                   ,usertype = member_casual)

q1_2022 <- rename(q1_2022
                   ,trip_id = ride_id 
                   ,bikeid = rideable_type  
                   ,start_time = started_at 
                   ,end_time = ended_at  
                   ,from_station_name = start_station_name 
                   ,from_station_id = start_station_id  
                   ,to_station_name = end_station_name 
                   ,to_station_id = end_station_id 
                   ,usertype = member_casual)

# Inspect the data-frames and look for inconsistencies

str(q2_2021)
str(q3_2021)
str(q4_2021)
str(q1_2022)

#converting the trip_id and bikeid type to character so that they can stack correctly
q2_2021 <-  mutate(q2_2021, trip_id = as.character(trip_id), bikeid =as.character(bikeid))
q3_2021 <-  mutate(q3_2021, trip_id = as.character(trip_id), bikeid =as.character(bikeid))
q4_2021 <-  mutate(q4_2021, trip_id = as.character(trip_id), bikeid =as.character(bikeid))
q1_2022 <-  mutate(q1_2022, trip_id = as.character(trip_id), bikeid =as.character(bikeid))



#stacking individual quarter data-frames into one big data-frame
all_trips<- bind_rows(q2_2021, q3_2021, q4_2021, q1_2022)
str(all_trips)

#removing lat,lng fields
all_trips<-  all_trips %>%
  select(-c(start_lat,start_lng,end_lat,end_lng,ride_length, day_of_week)) #-c means deleting the selected columns

#STEP THREE : CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

#inspecting the newly created table
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)  

#There are a few problems that need fixing
#(1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
#(2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
#(3) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.
            
table(all_trips$usertype) #This function allows us to see which observations are in the usertype column

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$start_time, "%m/%d/%Y") 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$start_time<- as.POSIXct(strptime(all_trips$start_time, format = "%m/%d/%Y  %H:%M"))
all_trips$end_time<- as.POSIXct(strptime(all_trips$end_time, format = "%m/%d/%Y  %H:%M"))
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time, units = "secs")
str(all_trips)
head(all_trips) #checking for ride_length consistency



# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
#check for negative values, remove any rows with nulls, see if there is actually a station that is called HQ QR (Which cases can nulls be considered appropriate?),The UNIFY function solved the issues of removing duplicates.
all_trips_v2 <- all_trips[!(all_trips$from_station_name == "HQ QR" | all_trips$ride_length <0),]
table(all_trips_v2$from_station_name) #checking for HQ QR
all_trips_v2 %>% drop_na() #drops/removes any row/observation with a missing variable(s) in any of the columns
str(all_trips_v2) 
#66 rows have been omitted

## STEP 4 : CONDUCT DESCRIPTIVE ANALYSIS

#Descriptive analysis in ride_length (units in seconds)

mean(all_trips_v2$ride_length) #finding the average ride time
median(all_trips_v2$ride_length) #finding the midpoint of ride time
max(all_trips_v2$ride_length) #finding the longest ride time
min(all_trips_v2$ride_length) #finding the shortest ride time

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean) #group ride length by members who are casual and find the mean
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday() with TRUE label returning it as an ordered factor
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration , number of rides equals the number of rows, each row is a trip
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(usertype, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

#customize the labels, as in add descriptions for the exponential thingy for those who don't get e function and for people to understand the units used in duration]


# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")


# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS

# Create a csv file that we will visualize in Excel, Tableau, or my presentation software

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'your\\choice\\storage\\directory\\avg_ride_length.csv')

















 










