##### SETUP ----
data = read.csv("C:/Users/felix/Documents/McGill MASTERS Class Work/1-Fall 2024/MGSC 661/Final Project/Final Code/final_dataset_ALL_1985.csv")
attach(data)

library(gbm)
library(ggfortify)
library(ggplot2)
library(caret)
library(cluster)
library(GGally)
library(dplyr)

##########################"
### Splittimg Dataset ----

Data_display = data

data <- data %>%
  group_by(ID) %>%
  mutate(
    Is_Winner = ifelse(any(Medal %in% c("Gold", "Silver", "Bronze")), 1, 0)
  ) %>%
  ungroup()



winners_original <- data %>% filter(Is_Winner == 1)
winners_original$Is_Winner = NULL
non_winners_original <- data %>% filter(Is_Winner == 0)
non_winners_original$Is_Winner = NULL


##### DROPPING ----
#data$MedalBinary <- ifelse(data$Medal == "no", 0, 1)
data$Sport = NULL
data$Number_Sports_Participated = NULL
#data$ID = NULL
data$First_Year = NULL
data$Name = NULL
data$Sex = NULL
data$Team = NULL
data$Games = NULL
data$City = NULL
data$no = NULL
data$Sex_male = NULL
data$TotalMedalBinary = NULL
#data$Gold = NULL
#data$Silver = NULL
#data$Bronze = NULL
data$Season_Winter = NULL
data$Athlete_Gold_Medals = NULL
data$Medal_Count = NULL
#data$Medal_Efficiency = NULL

data$Distinct_Events_Won_Medal = NULL
data$MedalOtherSport = NULL
#data$Year = NULL
#data$NOC = NULL
data$Primary_Event = NULL
data$Participation_Count = NULL

#data$Weight = NULL
#data$Height = NULL

data$Total_Participations = NULL
#data$Distinct_Events_Won_Medal = NULL
#data$Event = NULL
data$Season = NULL

data$BMI =NULL

#data$Primary_Event = NULL
#data$Medal = NULL
summary(data)
table(Event)

data_Is_Winner  = data



attach(data)

#########################"
##### Further Cleaning (correlation) -----
#########################"

# Calculate the correlation matrix for numeric columns
cor_matrix <- cor(data[, sapply(data, is.numeric)])

# Set the threshold for correlation (absolute value greater than 0.5)
threshold <- 0.5

# Find correlations with absolute values greater than the threshold (excluding self-correlations)
cor_filtered <- cor_matrix[abs(cor_matrix) > threshold & abs(cor_matrix) < 1]

# Print the filtered correlations with variable names
for (i in 1:nrow(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) > threshold & abs(cor_matrix[i, j]) < 1) {
      cat(paste(colnames(cor_matrix)[i], "<->", colnames(cor_matrix)[j], ": ", cor_matrix[i, j], "\n"))
    }
  }
}

data$Is_Winner = NULL

######################"
##### Scaling ----
######################"

numeric_data <- data %>% select_if(is.numeric)
data_scaled <- as.data.frame(scale(numeric_data))

# Add back non-numeric columns for grouping purposes
data_scaled$ID <- data$ID
data_scaled$Medal <- data$Medal
#data_scaled$NOC <- data$NOC

# Group by ID and check if the athlete has won any medal
data_scaled <- data_scaled %>%
  group_by(ID) %>%
  mutate(
    Is_Winner = ifelse(any(Medal %in% c("Gold", "Silver", "Bronze")), 1, 0)
  ) %>%
  ungroup()

# Drop ID and Medal columns as they're no longer needed
data_scaled$ID= NULL
data_scaled$Medal= NULL

# Split data into winners and non-winners
winners <- data_scaled %>% filter(Is_Winner == 1)
winners$Is_Winner = NULL
non_winners <- data_scaled %>% filter(Is_Winner == 0)
non_winners$Is_Winner = NULL

# Check the counts of winners and non-winners
cat("Number of Winners:", nrow(winners), "\n")
cat("Number of Non-Winners:", nrow(non_winners), "\n")

View(winners)


#########################"
##### PCA -----
#########################"
# Filter numeric variables for PCA
pca_data <- data[, sapply(data, is.numeric)]
pca_data$ID = NULL

# Perform PCA with scaling
pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)

summary(pca)

explained_variance <- summary(pca)$importance[2, ]

autoplot(pca, data = data, colour = 'grey', loadings = TRUE, loadings.label = TRUE) +
  labs(title = "PCA Analysis") +
  xlab(paste("Principal Component 1 (", round(explained_variance[1] * 100, 1), "% variance)", sep = "")) +
  ylab(paste("Principal Component 2 (", round(explained_variance[2] * 100, 1), "% variance)", sep = "")) +
  theme_minimal()

#########################"
##### K-means Clustering (common) -----
#########################"
set.seed(123)  # For reproducibility

##### Elbow Method
wss <- sapply(1:10, function(k) {
  kmeans(data_scaled[, -ncol(data_scaled)], centers = k)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Plot")

# Apply K-Means clustering with 3 centers
kmeans_model <- kmeans(data_scaled, centers = 4)

# Add cluster labels to the original dataset
data_scaled$Cluster <- as.factor(kmeans_model$cluster)

#sil <- silhouette(kmeans_model$cluster, dist(data_scaled))
# Compute the average silhouette width (score)
#silhouette_score <- mean(sil[, 3])
#silhouette_score   
#0.395



kmeans_model

##### ANALYSIS ----
#### labels to dataset unscaled
# Add cluster labels to the original dataset
data$Cluster <- as.factor(kmeans_model$cluster)

# Get the summary statistics for the clusters in the winners dataset
cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE), 
                                      sd = ~sd(. , na.rm = TRUE)), 
                   .names = "{.col}_{.fn}"))

# Print winners summary to check
View(cluster_summary)

# Visualize clusters 
ggplot(data, aes(x = Weight, y = Height, color = Cluster)) +
  geom_point() +
  labs(
    title = "K-Means Clustering",
    x = "Weight",
    y = "Height"
  )









################################################"
##### !! OTHER MODELS USED FOR TRIAL !! -----
################################################"



##################################"
##### Hierarchical Clustering -----
##################################"
# Compute the distance matrix
dist_matrix <- dist(data_scaled)  # Euclidean distance
# Apply hierarchical clustering
hc_model <- hclust(dist_matrix)
# Plot the dendrogram
plot(hc_model, main = "Hierarchical Clustering", xlab = "Data Points", sub = "")
# Cut the dendrogram into 3 clusters



num_clusters <- 4
cluster_labels <- cutree(hc_model, k = num_clusters)

Data_display$Cluster <- as.factor(cluster_labels)
print(table(Data_display$Cluster))


#sil <- silhouette(cluster_labels, dist_matrix)
#silhouette_score <- mean(sil[, 3])
#cat("Average Silhouette Score:", silhouette_score, "\n")


ggplot(Data_display, aes(x = Weight, y = Height, color = Cluster)) + 
  geom_point() +
  labs(
    title = paste("Hierarchical Clustering with", num_clusters, "Clusters"),
    x = "Weight",
    y = "Height"
  )


# Assign cluster labels to the original dataset
Data_display$Cluster <- as.factor(cluster_labels)

# Check the cluster distribution
print(table(Data_display$Cluster))

# Compute summary statistics for the clusters in the dataset
cluster_summary <- Data_display %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE), 
                                      sd = ~sd(. , na.rm = TRUE)), 
                   .names = "{.col}_{.fn}"))

# View the cluster summary
View(cluster_summary)

# Silhouette score calculation (already provided in your code)
#sil <- silhouette(cluster_labels, dist_matrix)
#silhouette_score <- mean(sil[, 3])
#cat("Average Silhouette Score:", silhouette_score, "\n")

# Visualize the clusters (scatter plot of Weight vs. Height)
ggplot(data, aes(x = Weight, y = Height, color = Cluster)) + 
  geom_point() +
  labs(
    title = paste("Hierarchical Clustering with", num_clusters, "Clusters"),
    x = "Weight",
    y = "Height"
  )



#################################"
#### Statistical Comparison ----
#################################"

# Compare means for a key metric (e.g., Height)
t_test_result <- t.test(winners$Height, non_winners$Height, na.rm = TRUE)
print(t_test_result)


ggplot(data, aes(x = Medal, y = Weight, fill = Medal)) +
  geom_boxplot() +
  labs(title = "Height Distribution: Winners vs. Non-Winners", x = "Medal Status", y = "Height")
ggplot(data, aes(x = Medal, y = Height, fill = Medal)) +
  geom_boxplot() +
  labs(title = "Weight Distribution: Winners vs. Non-Winners", x = "Medal Status", y = "Weight")



######## !!!!!!!!!!!!!! ################"



#################################"
#### Clustering Within Groups (Kmeans) ----
#################################"


##### Elbow Method
wss <- sapply(1:10, function(k) {
  kmeans(non_winners[, -ncol(non_winners)], centers = k)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Plot")


wss <- sapply(1:10, function(k) {
  kmeans(winners[, -ncol(winners)], centers = k)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Plot")



###############"
#### START ####

set.seed(123)

winners_clusters <- kmeans(winners, centers = 3)
non_winners_clusters <- kmeans(non_winners, centers = 4)

##### labels to datasets Scalled -----
winners$Cluster <- as.factor(winners_clusters$cluster)
non_winners$Cluster <- as.factor(non_winners_clusters$cluster)

winners_clusters$centers
non_winners_clusters$centers


#### Perfomance ----
winners_clusters$tot.withinss
non_winners_clusters$tot.withinss

#winners_silhouette <- silhouette(winners_clusters$cluster, dist(winners))
#non_winners_silhouette <- silhouette(non_winners_clusters$cluster, dist(non_winners))

#winners_silhouette_avg <- mean(winners_silhouette[, 3])
#non_winners_silhouette_avg <- mean(non_winners_silhouette[, 3])

#cat("Average silhouette score for winners:", winners_silhouette_avg, "\n")
#cat("Average silhouette score for non-winners:", non_winners_silhouette_avg, "\n")


######## Visualize ----
#Visualize clusters for winners
ggplot(winners, aes(x = Height, y = Weight, color = Cluster)) +
  geom_point() +
  labs(title = "Clusters of Winning Athletes", x = "Height", y = "Weight")

# Visualize clusters for non-winners
ggplot(non_winners, aes(x = Height, y = Weight, color = Cluster)) +
  geom_point() +
  labs(title = "Clusters of Non-Winning Athletes", x = "Height", y = "Weight")


##### ANALYSIS ----
#### labels to dataset unscaled
# Add cluster labels to the original dataset
winners_original$Cluster <- as.factor(winners_clusters$cluster)
non_winners_original$Cluster <- as.factor(non_winners_clusters$cluster)

# Get the summary statistics for the clusters in the winners dataset
winners_summary <- winners_original %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE), 
                                      sd = ~sd(. , na.rm = TRUE)), 
                   .names = "{.col}_{.fn}"))

# Print winners summary to check
View(winners_summary)

# Get the summary statistics for the clusters in the non-winners dataset
non_winners_summary <- non_winners_original %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(mean = ~mean(. , na.rm = TRUE), 
                                      sd = ~sd(. , na.rm = TRUE)), 
                   .names = "{.col}_{.fn}"))

# Print non-winners summary to check
View(non_winners_summary)



#### TEAM DISTRIBUTION -----
# Distribution of 'Team' in each cluster for winners with percentages
winners_team_distribution <- winners_original %>%
  group_by(Cluster, Team) %>%
  tally() %>%
  mutate(Percentage = n / sum(n) * 100) %>%  
  arrange(Cluster, desc(Percentage)) %>%
  group_by(Cluster) %>%
  slice_head(n = 10)  
print(winners_team_distribution, n = 400)

# Distribution of 'Team' in each cluster for non-winners with percentages
non_winners_team_distribution <- non_winners_original %>%
  group_by(Cluster, Team) %>%
  tally() %>%
  mutate(Percentage = n / sum(n) * 100) %>%  
  arrange(Cluster, desc(Percentage)) %>%
  group_by(Cluster) %>%
  slice_head(n = 10)  
print(non_winners_team_distribution, n=40)


cat("\nCluster Distribution for Winners:\n")
print(table(winners_original$Cluster))

cat("\nCluster Distribution for Non-Winners:\n")
print(table(non_winners_original$Cluster))




###################"
###### Stats ----



# Evaluate performance of k-means clustering
winners_clusters_stats <- data.frame(
  TotalWithinSS = winners_clusters$tot.withinss,
  Size = winners_clusters$size
)

non_winners_clusters_stats <- data.frame(
  TotalWithinSS = non_winners_clusters$tot.withinss,
  Size = non_winners_clusters$size
)

# Print cluster statistics for winners and non-winners
print("Winners Cluster Statistics")
print(winners_clusters_stats)

print("Non-Winners Cluster Statistics")
print(non_winners_clusters_stats)


#### Winner VS Non Winner
# Combine winners and non-winners data with a label
winners$Type <- "Winner"
non_winners$Type <- "Non-Winner"
combined_data <- rbind(winners, non_winners)

# Scatter plot comparing winners and non-winners
ggplot(combined_data, aes(x = Height, y = Weight, color = Type)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Comparison of Winners and Non-Winners: Height vs Weight",
    x = "Height (cm)",
    y = "Weight (kg)"
  ) +
  theme_minimal()

# Boxplot comparing Age distribution
ggplot(combined_data, aes(x = Type, y = Age, fill = Type)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Winners and Non-Winners: Age Distribution",
    x = "Type",
    y = "Age"
  ) +
  theme_minimal()



##### Visualize Key Features by Cluster
# Scatter plot for winners
ggplot(winners, aes(x = Height, y = Weight, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Winners Clustering: Height vs Weight",
    x = "Height (cm)",
    y = "Weight (kg)"
  ) +
  theme_minimal()

# Boxplot for Age distribution by cluster (winners)
ggplot(winners, aes(x = Cluster, y = Age, fill = Cluster)) +
  geom_boxplot() +
  labs(
    title = "Winners Clustering: Age Distribution by Cluster",
    x = "Cluster",
    y = "Age"
  ) +
  theme_minimal()

# Scatter plot for non-winners
ggplot(non_winners, aes(x = Height, y = Weight, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Non-Winners Clustering: Height vs Weight",
    x = "Height (cm)",
    y = "Weight (kg)"
  ) +
  theme_minimal()

# Boxplot for Age distribution by cluster (non-winners)
ggplot(non_winners, aes(x = Cluster, y = Age, fill = Cluster)) +
  geom_boxplot() +
  labs(
    title = "Non-Winners Clustering: Age Distribution by Cluster",
    x = "Cluster",
    y = "Age"
  ) +
  theme_minimal()


#### Analyze Cluster Centers
# Compare cluster centers for winners and non-winners
summary(winners_clusters$centers)
summary(non_winners_clusters$centers)


# Cluster centers for winners and non-winners
print("Winners Cluster Centers:")
print(winners_clusters$centers)

print("Non-Winners Cluster Centers:")
print(non_winners_clusters$centers)

# Convert cluster centers to data frame for visualization
winners_centers <- as.data.frame(winners_clusters$centers)
winners_centers$Cluster <- as.factor(1:nrow(winners_centers))

non_winners_centers <- as.data.frame(non_winners_clusters$centers)
non_winners_centers$Cluster <- as.factor(1:nrow(non_winners_centers))

# Visualization of cluster centers (e.g., height vs weight)
ggplot(winners_centers, aes(x = Height, y = Weight, label = Cluster)) +
  geom_point(size = 4, color = "blue") +
  geom_text(vjust = -1) +
  labs(
    title = "Winners Cluster Centers: Height vs Weight",
    x = "Height (cm)",
    y = "Weight (kg)"
  ) +
  theme_minimal()

ggplot(non_winners_centers, aes(x = Height, y = Weight, label = Cluster)) +
  geom_point(size = 4, color = "red") +
  geom_text(vjust = -1) +
  labs(
    title = "Non-Winners Cluster Centers: Height vs Weight",
    x = "Height (cm)",
    y = "Weight (kg)"
  ) +
  theme_minimal()





