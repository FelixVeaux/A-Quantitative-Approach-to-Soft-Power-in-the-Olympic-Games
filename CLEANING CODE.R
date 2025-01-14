data = read.csv("C:/Users/felix/Documents/McGill MASTERS Class Work/1-Fall 2024/MGSC 661/Final Project/Dataset 2 — Olympic events data.csv")
attach(data)
View(data)
names(data)
table(NOC)

colnames(data)[colSums(is.na(data)) > 0]
data[is.na(data$Age), ]

library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
require(ggpubr)


##### FEATURES ----
names(data)

# Keep only rows where Sport is "Athletics"
data <- data[data$Sport == "Athletics", ]

attach(data)


###########################"
#### HANDELING NA VALUES ----
############################"

#### delete all rows where the Sport has less than 30 representations #under 30 data point does not allow for meaningful analysis
# Count the number of representations for each sport
sport_counts <- data %>%
  group_by(Sport) %>%
  summarise(Count = n(), .groups = "drop")
data <- data %>%
  inner_join(sport_counts, by = "Sport") %>%  
  filter(Count >= 30) %>%
  mutate(Count = NULL)  

# Count the number of representations for each NOC
noc_counts <- data %>%
  group_by(NOC) %>%
  summarise(Count = n(), .groups = "drop")
data <- data %>%
  inner_join(noc_counts, by = "NOC") %>%  
  filter(Count >= 30) %>%
  mutate(Count = NULL) 

# Count the number of representations for each event
event_counts <- data %>%
  group_by(Event) %>%
  summarise(Count = n(), .groups = "drop")
data <- data %>%
  inner_join(event_counts, by = "Event") %>%  
  filter(Count >= 30) %>%
  mutate(Count = NULL)  

attach(data)

table(NOC)
table(Sport)
table(Event)



#### Fill Weight and Height, missing values 

## Checking
sum(is.na(data$Weight)) 
sum(is.na(data$Height)) 

# Fill using avg of based on Sport, Sex, and Year
data$Weight[is.na(data$Weight)] <- ave(data$Weight, data$Sport, data$Sex, data$Year,
                                       FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)))[is.na(data$Weight)]

data$Height[is.na(data$Height)] <- ave(data$Height, data$Sport, data$Sex, data$Year,
                                       FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)))[is.na(data$Height)]

# Fill remaining missing values using Sport and Sex
data$Weight[is.na(data$Weight)] <- ave(data$Weight, data$Sport, data$Sex,
                                       FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)))[is.na(data$Weight)]

data$Height[is.na(data$Height)] <- ave(data$Height, data$Sport, data$Sex,
                                       FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)))[is.na(data$Height)]

# Fill remaining missing values using overall Sport mean
data$Weight[is.na(data$Weight)] <- ave(data$Weight, data$Sport,
                                       FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)))[is.na(data$Weight)]

data$Height[is.na(data$Height)] <- ave(data$Height, data$Sport,
                                       FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)))[is.na(data$Height)]

# Check if all NA gone
sum(is.na(data$Weight)) 
sum(is.na(data$Height))


# LAST RESORT hard inpute #I dont want to lose data point and the mean is a decent represenation due to law of large numbers
data$Weight[is.na(data$Weight)] <- mean(data$Weight, na.rm = TRUE)
data$Height[is.na(data$Height)] <- mean(data$Height, na.rm = TRUE)

sum(is.na(data$Weight)) 
sum(is.na(data$Height)) 


#### Fill Age NA
# Fill remaining Age using sport-season median
# Calculate the median Age for each sport-season combination
sport_season_age_stats <- data %>%
  group_by(Sport, Season) %>%
  summarise(Median_Age = median(Age, na.rm = TRUE), .groups = "drop")
data <- data %>%
  left_join(sport_season_age_stats, by = c("Sport", "Season")) %>%
  mutate(Age = ifelse(is.na(Age), Median_Age, Age))
data <- subset(data, select = -Median_Age)
summary(data$Age)
colnames(data)[colSums(is.na(data)) > 0]
data[is.na(data$Height), ]


attach(data)


###########################"
#### FEATURE ENGENERING ----
############################"
## Creating BMI - This reduces dimensionality and holds the relevant values
data$BMI = data$Weight / (((data$Height)/100)^2)
data$Medal[is.na(data$Medal)] = "no"

## Encoding One if a medal was won
data$TotalMedalBinary = ifelse(data$Medal != "NO", 1, 0)

# Calculate Medal Efficiency
data <- data %>%
  group_by(ID) %>%
  mutate(Total_Participations = n()) %>%  
  mutate(Medal_Count = sum(Medal %in% c("Gold", "Silver", "Bronze"))) %>%  
  ungroup() %>%
  mutate(Medal_Efficiency = Medal_Count / Total_Participations) 

# Calculate Athlete Gold Medals
data <- data %>%
  group_by(ID) %>%
  mutate(Athlete_Gold_Medals = sum(Medal == "Gold")) %>%
  ungroup()

# Calculate First Year (Earliest Year of Participation)
data <- data %>%
  group_by(ID) %>%
  mutate(First_Year = min(Year)) %>%
  ungroup()

# Calculate Number of Sports Participated (Number of Unique Sports)
data <- data %>%
  group_by(ID) %>%
  mutate(Number_Sports_Participated = n_distinct(Sport)) %>%
  ungroup()

# Calculate Number of Events Participated (Number of Unique Events)
data <- data %>%
  group_by(ID) %>%
  mutate(Number_Events_Participated = n_distinct(Event)) %>%
  ungroup()

# Create a new column for the count of distinct events where athletes won a medal
data <- data %>%
  group_by(ID) %>%
  mutate(Distinct_Events_Won_Medal = n_distinct(Event[Medal %in% c("Gold", "Silver", "Bronze")])) %>%
  ungroup()

data$Career_Length <- data$Year - data$First_Year + 1

# Find Primary Event (Most Participated Event for Each Athlete)
data <- data %>%
  group_by(ID, Event) %>%
  mutate(Participation_Count = n()) %>%  
  ungroup() %>%
  group_by(ID) %>%
  mutate(Primary_Event = Event[which.max(Participation_Count)]) %>%  
  ungroup()

#Update MedalOtherEvent based on the condition "Distinct_Events_Won_Medal > 1"
data <- data %>%
  group_by(ID) %>%
  mutate(
    MedalOtherEvent = ifelse(Distinct_Events_Won_Medal > 1, 1, 0)  
  ) %>%
  ungroup()

##### Dumify Variables #####
data <- data %>%
  mutate(Sex_male = ifelse(Sex == "M", 1, 0),
         Sex_female = ifelse(Sex == "F", 1, 0))

unique_medals <- unique(data$Medal)
for (medal in unique_medals) {
  data[[medal]] <- apply(data, 1, function(x) as.integer(medal %in% x["Medal"]))
}


#### SAVING New Dataset ----
file_path <- "C:/Users/felix/Documents/McGill MASTERS Class Work/1-Fall 2024/MGSC 661/Final Project/Final Code/final_dataset_ALL2_1985.csv"
write.csv(data, file = file_path, row.names = FALSE)
cat("Dataset saved successfully to:", file_path, "\n")
View(data)







#############################"
#### OLD PROCEEDURES AND CHECKS ----
#############################"



#### Dummyfication for Random Forest ----
datarf <- data


colnames(data)[colSums(is.na(data)) > 0]

attach(datarf)
table(NOC)
table(Sport)
table(Event)

######################
#### Final Check ----
#####################

# Count the number of representations for each sport
sport_counts <- data %>%
  group_by(Sport) %>%
  summarise(Count = n(), .groups = "drop")

# Filter for sports with less than 30 representations
sports_less_than_30 <- sport_counts %>%
  filter(Count < 30)

# View the table for sports with less than 30 representations
print(sports_less_than_30)

# Count the number of representations for each NOC
noc_counts <- data %>%
  group_by(NOC) %>%
  summarise(Count = n(), .groups = "drop")

# Filter for NOCs with less than 30 representations
nocs_less_than_30 <- noc_counts %>%
  filter(Count < 30)

# View the table for NOCs with less than 30 representations
print(nocs_less_than_30)

# Count the number of representations for each event
event_counts <- data %>%
  group_by(Event) %>%
  summarise(Count = n(), .groups = "drop")

# Filter for events with less than 30 representations
events_less_than_30 <- event_counts %>%
  filter(Count < 30)

print(events_less_than_30)




###########################################'
##########Data Dictionary ############## ----
###########################################'
###########################################'
###########################################'# Sports disctionary category Team, Individual Technical, Individual Endurance, 
sport_dictionary <- c(
  "Basketball" = "Team",
  "Judo" = "Individual Technical",
  "Football" = "Team",
  "Tug-Of-War" = "Team",
  "Speed Skating" = "Individual Endurance",
  "Cross Country Skiing" = "Individual Endurance",
  "Athletics" = "Individual Athletics",
  "Ice Hockey" = "Team",
  "Swimming" = "Individual Endurance",
  "Badminton" = "Individual Technical",
  "Sailing" = "Team",
  "Biathlon" = "Individual Endurance",
  "Gymnastics" = "Individual Technical",
  "Art Competitions" = "Individual Technical",
  "Alpine Skiing" = "Individual Technical",
  "Handball" = "Team",
  "Weightlifting" = "Individual Technical",
  "Wrestling" = "Individual Technical",
  "Luge" = "Individual Technical",
  "Water Polo" = "Team",
  "Hockey" = "Team",
  "Rowing" = "Team",
  "Bobsleigh" = "Team",
  "Fencing" = "Individual Technical",
  "Equestrianism" = "Individual Technical",
  "Shooting" = "Individual Technical",
  "Boxing" = "Individual Technical",
  "Taekwondo" = "Individual Technical",
  "Cycling" = "Individual Endurance",
  "Diving" = "Individual Technical",
  "Canoeing" = "Individual Endurance",
  "Tennis" = "Individual Technical",
  "Modern Pentathlon" = "Individual Endurance",
  "Figure Skating" = "Individual Technical",
  "Golf" = "Individual Technical",
  "Softball" = "Team",
  "Archery" = "Individual Technical",
  "Volleyball" = "Team",
  "Synchronized Swimming" = "Team",
  "Table Tennis" = "Individual Technical",
  "Nordic Combined" = "Individual Endurance",
  "Baseball" = "Team",
  "Rhythmic Gymnastics" = "Individual Technical",
  "Freestyle Skiing" = "Individual Technical",
  "Rugby Sevens" = "Team",
  "Trampolining" = "Individual Technical",
  "Beach Volleyball" = "Team",
  "Triathlon" = "Individual Endurance",
  "Ski Jumping" = "Individual Technical",
  "Curling" = "Team",
  "Snowboarding" = "Individual Technical",
  "Rugby" = "Team",
  "Short Track Speed Skating" = "Individual Endurance",
  "Skeleton" = "Individual Technical",
  "Lacrosse" = "Team",
  "Polo" = "Team",
  "Cricket" = "Team",
  "Racquets" = "Individual Technical",
  "Motorboating" = "Individual Technical",
  "Military Ski Patrol" = "Team",
  "Croquet" = "Individual Technical",
  "Jeu De Paume" = "Individual Technical",
  "Roque" = "Individual Technical",
  "Alpinism" = "Individual Endurance",
  "Basque Pelota" = "Individual Technical",
  "Aeronautics" = "Individual Technical"
)



