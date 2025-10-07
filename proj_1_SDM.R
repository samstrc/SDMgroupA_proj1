# Group A project 1 - Sam, Chelsea, Robert
url <- "https://raw.githubusercontent.com/samstrc/SDMgroupA/refs/heads/main/student_habits.csv"
df_before_sample <- read.csv(url, header = TRUE)
set.seed(1234)
df <- df_before_sample[sample(nrow(df_before_sample), 150), ]
numeric_df <- df[sapply(df, is.numeric)] # Stores numeric columns only in new df

# Basic Analysis
names(df) # Print the names of each column 
summary(df) 
head(df, 20) # First 20 data points

# Correlation Analysis
pairs(numeric_df, pch=6, col='black', lower.panel=panel.smooth) # Pair plot for every numeric variable
cor(numeric_df) # Print correlations for all variables
cor.test(numeric_df$mental_health_rating, numeric_df$exam_score) # Test to see if mental health and exam score's relationship is similar outside of this dataset

# 3 Cont. variables: study_hours_per_day, sleep_hours, exam_score
# 2 Categ. variables: gender, diet_quality
counts_gender <- table(df$gender)
counts_gender # Kinda balanced

counts_diet_quality <- table(df$diet_quality)
counts_diet_quality # Mostly balanced


# ----- Data Visualizations -----
# QQ Plots to visualize normality of study_hours_per_day, sleep_hours, exam_score
# QQ plots for normality check
par(mfrow = c(1, 3))  # show 3 plots side-by-side

qqnorm(df$study_hours_per_day, main = "QQ Plot: Study Hours per Day")
qqline(df$study_hours_per_day, col = "grey")

qqnorm(df$sleep_hours, main = "QQ Plot: Sleep Hours")
qqline(df$sleep_hours, col = "grey")

qqnorm(df$exam_score, main = "QQ Plot: Exam Score")
qqline(df$exam_score, col = "grey")

par(mfrow = c(1, 1))  # reset plot layout


# Loop for histograms 
for (col in names(numeric_df)) { # For each column in the dataframe, make histogram
    hist(numeric_df[[col]], 
         main = paste("Histogram of", col), # Concatenates two strings for the title
         xlab = col, # Column label/name/variable
         col = 'grey')
}

# Scatter plot 
plot(df$study_hours_per_day, df$exam_score, xlab="Hours Studied (per day)", ylab="Exam Score (%)", type="p")

# Bar plot 
barplot(counts_gender, xlab="Gender", ylab="count", col="grey")
barplot(counts_diet_quality, xlab="Diet Quality", ylab="count", col="grey")

# Box plot of sleep hours across gender
boxplot(sleep_hours ~ gender, data = df,
        notch = TRUE,
        xlab = "Gender",
        ylab = "Hours of Sleep (per night)",
        col  = "grey")

# Tables to show practical differences in summaries between groups
aggregate(exam_score ~ gender, data = df, mean) # Women and men have similar mean exam scores
aggregate(exam_score ~ diet_quality, data = df, mean) # People with poorer diets tended to score worse on average


# Table to look at count/frequency differences
table(df$gender, df$diet_quality) # More women report having a poor diet, men are more confident in their diets

#comment 3 