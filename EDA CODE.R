olympics_data = read.csv("C:/Users/felix/Documents/McGill MASTERS Class Work/1-Fall 2024/MGSC 661/Final Project/Dataset 2 — Olympic events data.csv")

attach(olympics_data)
names(olympics_data)

summary(olympics_data)


library(rpart.plot)	
library(ggfortify)
library(ggplot2)
library(GGally)
library(dplyr)
require(ggpubr)

ggplot(olympics_data, aes(x = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "Value") +
  ggtitle("Boxplot of Year")


##### Overview  ----
# Distribution of sports
filtered_data <- olympics_data %>%
  group_by(Sport) %>%
  filter(n() > 3000) %>%
  ungroup()

ggplot(filtered_data, aes(x = reorder(Sport, table(Sport)))) + 
  geom_bar(fill = "steelblue", color = "black") +  
  labs(title = "Distribution of Sports in the Olympics Dataset",
       x = "Sport",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

### Distribution of Unique event within Athletics, Gymnastics, and Swimming
sports_data <- olympics_data %>%
  filter(Sport %in% c("Athletics", "Gymnastics", "Swimming"))
event_count <- sports_data %>%
  group_by(Sport) %>%
  summarise(Unique_Events = n_distinct(Event))  #
ggplot(event_count, aes(x = Sport, y = Unique_Events, fill = Sport)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Number of Unique Events in Athletics, Gymnastics, and Swimming",
       x = "Sport",
       y = "Number of Unique Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  


#### Heatmap of Medal Type by Sport and Gender
ggplot(olympics_data, aes(x = Sport, y = Sex, fill = Medal)) +
  geom_tile() +
  scale_fill_manual(values = c("gold" = "gold", "silver" = "silver", "bronze" = "brown", "NA" = "lightgray")) +
  labs(title = "Heatmap of Medal Type by Sport and Gender", x = "Sport", y = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### FILTERING ----
# Keep only rows where the Sport is Athletics
olympics_data <- olympics_data %>%
  filter(Sport == "Athletics")
summary(olympics_data)
attach(olympics_data)


#### VISULISQTION ----
# Histogram for Age
ggplot(olympics_data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal()

# Histogram for Height
ggplot(olympics_data, aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Height", x = "Height (cm)", y = "Count") +
  theme_minimal()

# Histogram for Weight
ggplot(olympics_data, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Weight", x = "Weight (kg)", y = "Count") +
  theme_minimal()

ggplot(olympics_data, aes(x = Sex)) +
  geom_bar(fill = c("pink", "lightblue"), alpha = 0.7) +
  labs(title = "Gender Distribution of Athletes", x = "Sex", y = "Count") +
  theme_minimal()

ggplot(olympics_data, aes(x = Medal)) +
  geom_bar(fill = c("gold", "gray", "brown", "red"), alpha = 0.7) +
  labs(title = "Distribution of Medals", x = "Medal Type", y = "Count") +
  theme_minimal()


### Countries Medal
top_countries <- olympics_data %>%
  group_by(NOC) %>%
  summarise(medal_count = n()) %>%
  arrange(desc(medal_count)) %>%
  head(10)

ggplot(top_countries, aes(x = reorder(NOC, medal_count), y = medal_count)) +
  geom_bar(stat = "identity", fill = "dodgerblue", alpha = 0.7) +
  labs(title = "Top 10 Countries by Athletics Medal Count", x = "Country", y = "Medal Count") +
  theme_minimal() +
  coord_flip()


## Wright VS HEight
ggplot(olympics_data, aes(x = Height, y = Weight)) +
  geom_point(aes(color = Sex), alpha = 0.6) +
  labs(title = "Height vs. Weight of Athletes", x = "Height (cm)", y = "Weight (kg)") +
  theme_minimal()

### Age vs. Medal Type
ggplot(olympics_data, aes(x = Age, fill = Medal)) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.7) +
  labs(title = "Age Distribution by Medal Type", x = "Age", y = "Count") +
  theme_minimal()

# Age vs Medal Type (Ignoring NA values for Medal)
ggplot(na.omit(olympics_data), aes(x = Age, fill = Medal)) +  
  geom_histogram(binwidth = 1, position = "dodge", alpha = .8) +
  labs(title = "Age Distribution by Medal Type", x = "Age", y = "Count") +
  theme_minimal()

### AGE STATISTICS ----
# Remove rows with NA values for Medal and calculate mean and standard deviation by Medal type
age_stats <- olympics_data %>%
  filter(!is.na(Medal)) %>% 
  group_by(Medal) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE)
  )

# Print the statistics
print(age_stats)




######################"
##### Before after 1985 ----
#######################"

# Split data into before and after 1985
before_1985 <- olympics_data %>% filter(Year < 1985)
after_1985 <- olympics_data %>% filter(Year >= 1985)

# Combine both datasets with an indicator variable
combined_data <- olympics_data %>%
  mutate(Period = ifelse(Year < 1985, "Before 1985", "After 1985"))


# Plot the age distribution before and after 1985
ggplot(combined_data, aes(x = Age, fill = Period)) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.7) +
  labs(title = "Age Distribution Before and After 1985", x = "Age", y = "Count") +
  scale_fill_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal()

combined_data <- olympics_data %>%
  mutate(
    Period = ifelse(Year < 1985, "Before 1985", "After 1985"),
    Medal_Status = ifelse(is.na(Medal), "No Medal", "Medal")  
  )

# Create the box plot for age distribution, separated by Period and Medal_Status
ggplot(combined_data, aes(x = Period, y = Age, fill = Medal_Status)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Age Distribution of Medal Winners and Non-Winners Before and After 1985",
       x = "Period",
       y = "Age") +
  scale_fill_manual(values = c("Medal" = "gold", "No Medal" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#### Age Distribution Before and After 1985
ggplot(combined_data, aes(x = Period, y = Age, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Age Distribution Before and After 1985", x = "Period", y = "Age") +
  scale_fill_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal()

####  Height Distribution Before and After 1985
ggplot(combined_data, aes(x = Period, y = Height, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Height Distribution Before and After 1985", x = "Period", y = "Height (cm)") +
  scale_fill_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal()

ggplot(combined_data, aes(x = Period, y = Weight, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Weight Distribution Before and After 1985", x = "Period", y = "Weight (kg)") +
  scale_fill_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal()

# Calculate Mean and Standard Deviation for Height and Weight, grouped by Period
summary_stats <- combined_data %>%
  group_by(Period) %>%
  summarise(
    Mean_Height = mean(Height, na.rm = TRUE),
    SD_Height = sd(Height, na.rm = TRUE),
    Mean_Weight = mean(Weight, na.rm = TRUE),
    SD_Weight = sd(Weight, na.rm = TRUE)
  )
print(summary_stats)


# Create a scatter plot of Weight vs Height, colored by Period
ggplot(combined_data, aes(x = Height, y = Weight, color = Period)) +
  geom_point(alpha = 0.7) +  # Scatter plot with some transparency
  labs(title = "Weight vs Height Before and After 1985",
       x = "Height (cm)",
       y = "Weight (kg)") +
  scale_color_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal() +
  theme(legend.title = element_blank()) 



# Split data into before and after 1985
before_1985 <- olympics_data %>% filter(Year < 1985)
after_1985 <- olympics_data %>% filter(Year >= 1985)

# Combine both datasets with period and medal status indicators
combined_data <- olympics_data %>%
  mutate(
    Period = ifelse(Year < 1985, "Before 1985", "After 1985"),
    Medal_Status = ifelse(is.na(Medal), "No Medal", "Medal") 
  )

# Boxplot for Height
ggplot(combined_data, aes(x = Medal_Status, y = Height, fill = Period)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  labs(
    title = "Boxplot of Athlete Heights by Medal Status and Period",
    x = "Medal Status",
    y = "Height (cm)"
  ) +
  scale_fill_manual(values = c("Before 1985" = "lightcoral", "After 1985" = "lightgreen")) +
  theme_minimal() +
  facet_wrap(~Period)

# Boxplot for Weight
ggplot(combined_data, aes(x = Medal_Status, y = Weight, fill = Period)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  labs(
    title = "Boxplot of Athlete Weights by Medal Status and Period",
    x = "Medal Status",
    y = "Weight (kg)"
  ) +
  scale_fill_manual(values = c("Before 1985" = "lightcoral", "After 1985" = "lightgreen")) +
  theme_minimal() +
  facet_wrap(~Period)


#### map the count of sports before and after in a bar chart for those sports with more than 30 represntatiation
sport_counts <- olympics_data %>%
  mutate(Period = ifelse(Year < 1985, "Before 1985", "After 1985")) %>%
  group_by(Sport, Period) %>%
  summarise(count = n()) %>%
  filter(count > 30)
ggplot(sport_counts, aes(x = reorder(Sport, -count), y = count, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Count of Sports Before and After 1985 (More Than 30 Representations)",
       x = "Sport", y = "Count of Representations") +
  scale_fill_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




### Medal count by country by period
# Calculate the top 10 countries by medal count, grouped by Period
top_countries_period <- combined_data %>%
  group_by(NOC, Period) %>%
  summarise(medal_count = n()) %>%
  arrange(Period, desc(medal_count)) %>%
  group_by(Period) %>%
  top_n(10, medal_count) %>%
  ungroup()
ggplot(top_countries_period, aes(x = reorder(NOC, medal_count), y = medal_count, fill = Period)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Top 10 Countries by Athletics Medal Count (Before and After 1985)", x = "Country", y = "Medal Count") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("Before 1985" = "dodgerblue", "After 1985" = "tomato"))  # Custom colors for periods
combined_data <- combined_data %>%
  mutate(
    Medal_Status = case_when(
      Medal == "Gold" ~ "Gold",
      Medal == "Silver" ~ "Silver",
      Medal == "Bronze" ~ "Bronze",
      TRUE ~ "No Medal"
    )
  )
medal_count_by_country <- combined_data %>%
  group_by(NOC, Period, Medal_Status) %>%
  summarise(medal_count = n()) %>%
  filter(Medal_Status != "No Medal") %>%
  arrange(Period, desc(medal_count)) %>%
  group_by(Period) %>%
  top_n(20, medal_count) %>%
  ungroup()

# Create a stacked bar chart for the medal distribution by country and period (Top 10 countries)
ggplot(medal_count_by_country, aes(x = reorder(NOC, medal_count), y = medal_count, fill = Medal_Status)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
  labs(title = "Top 15 Countries Athletics Medal Distribution by Period (Before and After 1985)",
       x = "Country", y = "Medal Count") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown")) +  
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ Period)  


#### Top 10 Calculate the total count of representations per sport across both periods
sport_counts_total <- olympics_data %>%
  mutate(Period = ifelse(Year < 1985, "Before 1985", "After 1985")) %>%
  group_by(Sport) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
top_sports_data <- olympics_data %>%
  mutate(Period = ifelse(Year < 1985, "Before 1985", "After 1985")) %>%
  filter(Sport %in% sport_counts_total$Sport)
sport_counts <- top_sports_data %>%
  group_by(Sport, Period) %>%
  summarise(count = n())
ggplot(sport_counts, aes(x = reorder(Sport, -count), y = count, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Top 10 Most Represented Sports Before and After 1985",
       x = "Sport", y = "Count of Representations") +
  scale_fill_manual(values = c("Before 1985" = "lightblue", "After 1985" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






