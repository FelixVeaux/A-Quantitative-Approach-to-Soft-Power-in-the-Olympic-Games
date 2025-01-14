data = read.csv("C:/Users/felix/Documents/McGill MASTERS Class Work/1-Fall 2024/MGSC 661/Final Project/Final Code/final_dataset_ALL_1985.csv")
attach(data)


library(randomForest)
library(rpart)					
library(ggplot2)
library(dplyr)
require(caret)
library(gbm)

data = data[data$Year > 1985, ] 

data$MedalBinary <- ifelse(data$Medal == "no", 0, 1)

data$Sport = NULL
data$ID = NULL
data$First_Year = NULL
data$Name = NULL
data$Sex = NULL
data$Team = NULL
data$Games = NULL
data$City = NULL
data$no = NULL
data$Sex_male = NULL
data$TotalMedalBinary = NULL
data$Gold = NULL
data$Silver = NULL
data$Bronze = NULL
data$Season_Winter = NULL

data$Athlete_Gold_Medals = NULL

data$Medal_Count = NULL
data$Medal_Efficiency = NULL


data$Distinct_Events_Won_Medal = NULL
#data$MedalOtherSport = NULL
data$Year = NULL
#data$NOC = NULL
#data$Primary_Event = NULL
#data$Participation_Count = NULL
#data$Weight = NULL
#data$Height = NULL
#data$Total_Participations = NULL
data$Number_Sports_Participated = NULL
#data$Distinct_Events_Won_Medal = NULL
#data$Event = NULL
data$Season = NULL
#data$Primary_Event = NULL
data$Medal = NULL

summary(data)
table(Event)
#########################"
##### Further Cleaning -----
#########################"

# Count the frequency of each team in the NOC column
team_counts <- table(data$NOC)
top_49_teams <- names(sort(team_counts, decreasing = TRUE))[1:49]
data$NOC <- ifelse(data$NOC %in% top_49_teams, as.character(data$NOC), "Other")
table(data$NOC)


#########################"
##### Modeling -----
#########################"


### Factoring
# Convert all character/logical columns to factors
categorical_columns <- names(data)[sapply(data, function(x) is.character(x) | is.logical(x))]
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)
datatest = data

attach(data)
names(data)


############################
### Undersampling ----

# Count the number of instances for each class
class_counts <- table(data$MedalBinary)
minority_class <- min(class_counts)  
majority_class <- max(class_counts) 

# Calculate the desired majority class size (2 times the minority class)
desired_majority_size <- 2 * minority_class

# Separate the data into majority and minority classes
majority_data <- data %>% filter(MedalBinary == 0)
minority_data <- data %>% filter(MedalBinary == 1)

# Undersample the majority class to the desired size
set.seed(123)
undersampled_majority <- majority_data %>%
  sample_n(size = floor(desired_majority_size))

# Combine the undersampled majority class with the minority class
balanced_data <- bind_rows(minority_data, undersampled_majority)
data = balanced_data
attach(data)


#########################"
##### Split -----
#########################"
set.seed(123)  
# Split the data into training (70%) and testing (30%)
train_index <- createDataPartition(data$MedalBinary, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

summary(train_data)






######################################"
##### Random Forest (MAIN MODEL) -----
######################################"

# Convert MedalBinary to a factor to indicate it's a classification problem
train_data$MedalBinary <- as.factor(train_data$MedalBinary)

# Train the random forest model (classification)
num_predictors <- ncol(train_data) - 1
default_mtry <- floor(sqrt(num_predictors))

rf_model_After_1985 <- randomForest(MedalBinary ~ ., 
                         data = train_data, 
                         ntree = 1000,      
                         mtry = default_mtry, 
                         importance = TRUE)

print(rf_model_After_1985)


importance(rf_model_After_1985)
varImpPlot(rf_model_After_1985)

######## Predict on the test dataset
predictions <- predict(rf_model_After_1985, newdata = test_data)
predicted_probabilities <- predict(rf_model_After_1985, newdata = test_data, type = "prob")[, 2]

confusion_matrix <- table(Predicted = predictions, Actual = test_data$MedalBinary)
print(confusion_matrix)

# Accuracy
accuracy <- sum(predictions == test_data$MedalBinary) / length(predictions)
print(paste("Accuracy:", accuracy))
#78.6
## Indiv Error; 0 16%, 1 32%

# Precision, Recall, F1-score
confusion <- confusionMatrix(as.factor(predictions), as.factor(test_data$MedalBinary))
print(confusion)

# Display feature importance
importance(rf_model_After_1985)
varImpPlot(rf_model_After_1985)



##########################################################
################Boosted with cross validation ############
##########################################################
set.seed(1)

categorical_vars <- names(data)[sapply(data, function(col) !is.numeric(col))]
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)
str(data)

# Train the GBM model with 10-fold cross-validation
boosted_cv <- gbm(
  MedalBinary ~ .,
  data = data,
  distribution = "bernoulli",
  n.trees = 10000,         
  shrinkage = 0.01,         
  interaction.depth = 4,    
  cv.folds = 10,        
  verbose = FALSE            
)

best_iter <- gbm.perf(boosted_cv, method = "cv")
cat("Optimal number of trees:", best_iter, "\n")
#Optimal number of trees: 1673 

test_probs <- predict(boosted_cv, newdata = data, n.trees = best_iter, type = "response")
test_classes <- ifelse(test_probs > 0.5, 1, 0)

# Confusion Matrix
conf_matrix_b <- table(Predicted = test_classes, Actual = data$MedalBinary)
print("Confusion Matrix:")
print(conf_matrix_b)

# Calculate accuracy
accuracy_b <- sum(diag(conf_matrix_b)) / sum(conf_matrix_b)
cat("Cross-validated GBM Accuracy:", accuracy_b, "\n")
#86.82%
## Indiv Error; 0 12.7%, 14.2%


# Display feature importance
feature_importance <- summary(boosted_cv, n.trees = best_iter) 
print(feature_importance)

summary(boosted_cv, n.trees = best_iter, plotit = TRUE)







###########################################################################"
##### !! OTHER MODELS USED FOR TRIAL, BUT NOT RETAINED FOR FINAL REPORT !! -----
###########################################################################"









#########################"
##### Logistic -----
#########################"


logistic_model <- glm(MedalBinary ~ ., , 
                      data = train_data, 
                      family = binomial())

# Summary of the model to check coefficients and significance
summary(logistic_model)

predictions <- predict(logistic_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)
predicted_class <- as.factor(predicted_class)
confusion_matrix <- table(Predicted = predicted_class, Actual = test_data$MedalBinary)
print(confusion_matrix)
accuracy <- sum(predicted_class == test_data$MedalBinary) / nrow(test_data)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))


#########################"
##### Boosting -----
#########################"
unique(train_data$MedalBinary)
class(train_data$MedalBinary)
train_data$MedalBinary <- as.numeric(as.character(train_data$MedalBinary))

# Fit the GBM model for multi-class classification
boosted <- gbm(MedalBinary ~ ., 
               data = train_data, 
               distribution = "bernoulli", 
               n.trees = 1000, 
               interaction.depth = 3,
               cv.folds = 5,
               n.cores = NULL, 
               verbose = FALSE)

summary(boosted)

predictions <- predict(boosted, newdata = test_data, n.trees = 100, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(predicted_classes == test_data$MedalBinary)
print(paste("Boosting Accuracy: ", accuracy))

# Confusion Matrix
predicted_classes <- factor(predicted_classes, levels = c(0, 1))
test_data$MedalBinary <- factor(test_data$MedalBinary, levels = c(0, 1))
confusionMatrix(predicted_classes, test_data$MedalBinary)





##############################"
############ QDA (IGNORE) ----
##############################"


# Fit the QDA model
qda_model <- qda(MedalBinary ~ ., data = train_data)

summary(qda_model)

qda_predictions <- predict(qda_model, newdata = test_data)

predicted_class <- qda_predictions$class

confusion_matrix <- table(Predicted = predicted_class, Actual = test_data$MedalBinary)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predicted_class == test_data$MedalBinary) / nrow(test_data)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))

confusion <- confusionMatrix(predicted_class, test_data$MedalBinary)
print(confusion)
