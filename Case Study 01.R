
#=====================
# STEP 1: COLLECT DATA
#=====================

## Installing and Loading packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("here")
update.packages()
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(here)
library(scales)
getwd()
setwd

## Importing files
tripdata_202006 <- read_csv("Bike data/202006-divvy-tripdata.csv", 
                            +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                   +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                   +         start_station_id = col_character(), 
                                                   +         end_station_id = col_character()))
 View(tripdata_202006)                  july_2020 <- read_csv("Bike data 12 months/202007-divvy-tripdata/202007-divvy-tripdata.csv")

 tripdata_202007 <- read_csv("Bike data/202007-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         start_station_id = col_character(), 
                                                     +         end_station_id = col_character()))
 View(tripdata_202007)  

 tripdata_202008 <- read_csv("Bike data/202008-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         start_station_id = col_character(), 
                                                     +         end_station_id = col_character()))
 View(tripdata_202008)    

 tripdata_202009 <- read_csv("Bike data/202009-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         start_station_id = col_character(), 
                                                     +         end_station_id = col_character()))
 View(tripdata_202009)   

 tripdata_202010 <- read_csv("Bike data/202010-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         start_station_id = col_character(), 
                                                     +         end_station_id = col_character()))
 View(tripdata_202010)

 tripdata_202011 <- read_csv("Bike data/202011-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         start_station_id = col_character(), 
                                                     +         end_station_id = col_character()))
 View(tripdata_202011)

 tripdata_202012 <- read_csv("Bike data/202012-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
 View(tripdata_202012)

 tripdata_202101 <- read_csv("Bike data/202101-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
 View(tripdata_202101)   

 tripdata_202102 <- read_csv("Bike data/202102-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
 View(tripdata_202102)

 tripdata_202103 <- read_csv("Bike data/202103-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
 View(tripdata_202103)    

 tripdata_202104 <- read_csv("Bike data/202104-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
 View(tripdata_202104)

 tripdata_202105 <- read_csv("Bike data/202105-divvy-tripdata.csv", 
                              +     col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                                     +         ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
 View(tripdata_202105)  


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

## Combining each data frame to get twelve months of data bike
combined_databike <- bind_rows(tripdata_202006,tripdata_202007,tripdata_202008,tripdata_202009,
                               tripdata_202010,tripdata_202011,tripdata_202012,tripdata_202101,
                               tripdata_202102,tripdata_202103,tripdata_202104,tripdata_202105)
View(combined_databike)
str(combined_databike)


## More data cleaning

glimpse(combined_databike) ## With glimpse I have 3,745,465 rows and 13 columns
head(combined_databike) ## head allows a preview of the column names.
# Using Janitor to remove empty spaces
combined_databike <- janitor::remove_empty(combined_databike, which = c("cols"))
combined_databike <- janitor::remove_empty(combined_databike, which = c("rows"))
dim(combined_databike) # to check if there was any changes after checking for empty spaces

## Remove columns not required or beyond the scope of project
combined_databike <- combined_databike %>% 
  select(-start_station_id,-end_station_id) %>% 
combined_databike <- combined_databike %>% 
  select(-start_lat,-start_lng,-end_lat,-end_lng)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

colnames(combined_databike) #List of column names
nrow(combined_databike) #How many rows are in data frame?
dim(combined_databike) #Dimensions of the data frame?
head(combined_databike) #See the first 6 rows of data frame.
str(combined_databike) # See list of columns and data types (numeric, character, etc)
summary(combined_databike) # Statistical summary of data. Mainly for numerics

#
combined_databike$date <- as.Date(combined_databike$started_at) # The default format is yyyy-mm-dd
combined_databike$month <- format(as.Date(combined_databike$date),"%B")
combined_databike$day <- format(as.Date(combined_databike$date), "%d")
combined_databike$year <- format(as.Date(combined_databike$date),"%Y")
combined_databike$day_of_week <- format(as.Date(combined_databike$date), "%A")
## Add a "ride_length" calculation to all_trips (in seconds)
combined_databike$ride_length <- difftime(combined_databike$ended_at,combined_databike$started_at)

# Inspect the structure of the columns
str(combined_databike)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(combined_databike$ride_length)
combined_databike$ride_length <- as.numeric(as.character(combined_databike$ride_length))
is.numeric(combined_databike$ride_length)
# # Remove "bad" data
combined_databike_2 <- combined_databike[!(combined_databike$start_station_name == "HQ QR" | combined_databike$ride_length<0),] 



# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)
mean(combined_databike_2$ride_length) #straight average (total ride length / rides)
median(combined_databike_2$ride_length)  #midpoint number in the ascending array of ride lengths
max(combined_databike_2$ride_length) #longest ride
min(combined_databike_2$ride_length) #shortest ride
summary(combined_databike_2$ride_length)

# Compare members and casual users
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual, FUN = mean)
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual, FUN = median)
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual, FUN = max)
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual + combined_databike_2$day_of_week, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
combined_databike_2$day_of_week <- ordered(combined_databike_2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual + combined_databike_2$day_of_week, FUN = mean)

## See the average ride time by each month for members vs casuals users
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual + combined_databike_2$month, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
combined_databike_2$month <- ordered(combined_databike_2$month,c("June", "July", "August", "September", "October", "November", "December","January","February","March","April","May"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(combined_databike_2$ride_length ~ combined_databike_2$member_casual + combined_databike_2$month, FUN = mean)


## # Let's visualize the number of rides by rider type

## Number of rides completed by user type
ggplot(combined_databike_2, aes(x=member_casual))+
  geom_bar(fill="Red") +
  labs(
    title = "Number of rides completed by user type",
    subtitle = "For the period between June 2020 and May 2021",
    x = "User type",
    y = "Number of rides (in millions)") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  geom_text(stat='count', aes(label=..count..), vjust=+2, color="white")


## Number of rides each day by rider type
combined_databike_2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title= "Number of rides each day by rider type",
       subtitle= "For the period between June 2020 and May 2021") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  geom_text(aes(label=number_of_rides),position = position_dodge(0.9),hjust=+3, color="black",angle= 90)


# Let's create a visualization for average duration
combined_databike_2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title= "Average rides each day by rider type",
       subtitle= "For the period between June 2020 and May 2021") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  geom_text(aes(label=average_duration),position = position_dodge(0.85),hjust=0.9, color="black",angle= 90)



## 4.Bike preference by user type.
combined_databike_2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = rideable_type)) +
  geom_bar(stat = "identity") + labs(title= "Bike preference and member_casual",
                                      subtitle= "June 2020 to May 2021") +
  coord_flip() +
  geom_text(aes(label=number_of_rides),position = position_stack(vjust = .5), color="black") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))



######
######

##Number of rides completed by month by user name
combined_databike_2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title= "Numbers of rides completed by month",
                                        subtitle= "June 2020 to May 2021")

## Average of rides duration completed by month and member_casual
combined_databike_2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title= "Average of rides completed by month by member_casual",
                                      subtitle= "June 2020 to May 2021") 



###
###
## Top 5 start station by user types
Table1 <- combined_databike_2 %>%
  group_by(member_casual, start_station_name) %>%
  summarise(count_of=n()) %>%
  arrange(desc(count_of)) %>%
  na.omit(start_station_name)

## Table 1.1 - By casual riders ##
Table1.1 <- filter(Table1, member_casual =="casual") %>%
  rename(number_of_rides = count_of) %>%
  slice(1:10)

#Table 1.2 - By members ##
Table1.2 <- filter(Table1, member_casual =="member") %>%
  rename(number_of_rides = count_of) %>%
  slice(1:10)

print(Table1.1)
## Top 10 start station by casual customer
  ggplot(Table1.1,aes(x = start_station_name, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() +
  labs(title = "Top 10 start station by casual customer", subtitle = "June 2020 to May 2021") 
    
print(Table1.2)
## top 10 start station by member customer
  ggplot(Table1.2,aes(x = start_station_name, y = number_of_rides, fill = member_casual)) +
    geom_bar(stat = "identity", width = 0.2) +
    coord_flip() +
    labs(title = "Top 10 start station by member customer",subtitle= "June 2020 to May 2021")

  
##Top 10 end station by user types
  Table2 <- combined_databike_2 %>%
    group_by(member_casual, end_station_name) %>%
    summarise(count_of=n()) %>%
    arrange(desc(count_of)) %>%
    na.omit(end_station_name)
  
  ## Table 2.1 - By casual riders ##
  Table2.1 <- filter(Table2, member_casual =="casual") %>%
    rename(number_of_rides = count_of) %>%
    slice(1:10)
  
  #Table 2.2 - By members ##
  Table2.2 <- filter(Table2, member_casual =="member") %>%
    rename(number_of_rides = count_of) %>%
    slice(1:10)

print(Table2.1)
##

ggplot(Table2.1,aes(x = end_station_name, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() +
  labs(title = "Top 10 end station by casual customer",subtitle= "June 2020 to May 2021")

print(Table2.2)
## top 10 start station by member customer
ggplot(Table2.2,aes(x = end_station_name, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() +
  labs(title = "Top 10 end station name by member customer",subtitle= "June 2020 to May 2021")


