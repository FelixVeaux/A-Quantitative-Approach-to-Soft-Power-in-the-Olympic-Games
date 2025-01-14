# A-Quantitative-Approach-to-Soft-Power-in-the-Olympic-Games
An analysis to find quantitative proof of country influence passed in the Olympic games. 

The Olympic Games have evolved from a showcase of athleticism to a global stage where nations project influence and prestige. Originally focused on the amateur spirit of sports, the games underwent a significant shift in 1985 with the introduction of sponsorships and the rise of television broadcasts. This change transformed the Olympics into a platform where countries compete not only for athletic excellence but also for soft power and global recognition.

This project analyzes Olympic medal outcomes using quantitative methods to predict an athlete's chances of winning a medal based on demographic data, event participation, and country affiliation. The goal is to uncover patterns that can optimize performance strategies and explore the influence of countries in shaping success.

## Data Overview and Initial Exploration

The dataset spans Olympic events from Athens 1896 to Rio 2016, featuring athlete demographics (sex, age, height, and weight), event participation, and medal outcomes. With over 271,000 data points, exploratory data analysis (EDA) was crucial for understanding the dataset’s trends.

### Key Observations:
- **Athletics as a Core Focus**: Athletics (Track and Field) was the most represented sport, contributing over 38,000 entries.
- **Demographic Trends**: Men were generally taller and heavier than women. Post-1985, athletes extended their careers longer, with more medalists in their mid-30s.
- **Age-Related Trends**: Advances in training and recovery led to longer careers and a rise in older medalists, even as height and weight distributions remained consistent across eras.

## Feature Engineering and Data Processing

To prepare the data for modeling, missing values were addressed, and new features were engineered to enhance prediction accuracy:

- **Imputation**: Missing height and weight values were filled using the mean (stratified by sex and year), while median values were used for missing ages.
- **Data Filtering**: Records with fewer than 30 occurrences per National Olympic Committee (NOC) or event were removed for statistical robustness.
- **Feature Engineering**:
  - **Body Mass Index (BMI)**: Used to gauge physical conditioning.
  - **Medal Efficiency**: The ratio of medals won to total participation.
  - **Career Length**: The number of years from the athlete's first to last Olympic appearance.
  - **Distinct Events Won Medals**: The number of different events in which an athlete won medals.

These features helped improve the dataset's depth and accuracy.

## Classification Models: Predicting Medal Outcomes

Various classification models were tested, including logistic regression, decision trees, random forests, and boosting algorithms.

### Initial Findings:
- **All Medal Types**: Predicting gold, silver, and bronze medals achieved a 51% accuracy—slightly better than random chance.
- **Binary Classification Shift**: By framing the problem as predicting "medal vs. no medal," accuracy improved to 75% using random forest models with under-sampling.
- **Final Model**: A boosting model with 10-fold cross-validation reached the highest accuracy at **86.8%**, with individual error rates of 12.7% for "no medal" and 14.2% for "medal."
- **Feature Importance**: Post-1985, NOC (country affiliation) became a stronger predictor of success.

## Clustering Analysis: Identifying Success Patterns

To gain deeper insights, clustering was applied to group athletes with shared characteristics:

- **K-Means Clustering**: Four clusters emerged after scaling variables to prevent skew from features like weight and height.
  - **Cluster 1**: Older athletes (mean age: 29) with long careers, suggesting that experience plays a significant role.
  - **Cluster 2**: Athletes with the highest medal efficiency (0.66) and slightly shorter average height, indicating the importance of technique and specialization.
  - **Cluster 3**: Athletes with lower physical metrics and lower medal efficiency, emphasizing the need for well-rounded training.
  - **Cluster 4**: Represented athletes with poor performance despite a large number of points, suggesting inconsistent outcomes.

The silhouette score of 0.43 indicated moderate separation between clusters, showing that meaningful trends could still be extracted.

## Key Insights and Observations

1. **Growing Role of Country Influence**: Post-1985, countries like Spain and China emerged as Olympic powerhouses. NOC (country affiliation) became a stronger predictor of success after this period.
2. **The Advantage of Versatility**: Athletes who competed in multiple events had a higher likelihood of winning medals.
3. **Longevity Factor**: Career length correlated with success, with athletes in longer careers often gaining valuable experience and strategic insights.

However, physical metrics and NOC affiliation alone don’t provide the full picture. Modern sports success also depends on external factors such as training resources, diet, sponsorships, and access to advanced facilities.

## Conclusions and Recommendations
Looking at the given variables and characteristics of the athlete gives us limited insight into the possibility of victory, which hints at other factors not included in this dataset playing a role. This will be particularly true in modern sports where athletes are optimized through technology and medicine, hinting at the role the athletes' training and affiliations play in their success above their physical characteristics.

The race is no longer won 100% due to physical ability, and the optimization of athletes is going deeper with recovery, diet, and more. All aspects of the athletes are now controlled; therefore, further data points are required.

Ultimately, further research to complement these ones is required to understand which factors lead to athletic success. More data such as diet, training hours, sponsors, family information, training camps, facilities, and motivations could be analyzed to more accurately evaluate the athlete's potential to success. 

Additionally, proof from this report is limited to answering the question, "Was there a shift in countries' desire to win?". Given the random forest features importance, we can see that countries now play a more significant role than before, but it would be a reach to conclude this solely based on this research. 

However, the emergence of certain countries on the leaderboard clearly gives reason to believe this is a possibility, and further qualitative and quantitative research is needed to elucidate and prove this phenomenon. 

#### The Olympic Games have transformed from a competition of physical prowess to one shaped by a complex array of factors. Understanding these shifts is crucial for athletes, coaches, and nations aiming to optimize performance on the global stage.

[**Full Report**](https://drive.google.com/file/d/1DkaMo788KPi8cZ2qVH48qeQiPo3XusU3/view?usp=sharing)
