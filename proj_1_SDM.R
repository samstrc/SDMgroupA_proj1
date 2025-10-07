# Group A project 1 
directory <- "https://raw.githubusercontent.com/samstrc/SDMgroupA/refs/heads/main/student_habits.csv"
df <- read.csv(directory, header = TRUE)
numeric_df <- df[sapply(df, is.numeric)] # Stores numeric columns only in new df

names(df) # Print the names of each column 
summary(df)
head(df, 20) 


# Pair plot for every numeric variable
pairs(numeric_df, pch=6, col='black', lower.panel=panel.smooth) 
cor(numeric_df) # Print correlations for all variables
cor.test(numeric_df$mental_health_rating, numeric_df$exam_score) # Test to see if mental health and exam score's relationship is similar outside of this dataset

# 3 Cont. variables: study_hours_per_day, sleep_hours, exam_score
# 2 Categ. variables: gender, diet_quality

counts_gender <- table(df$gender)
counts_gender # Kinda balanced

counts_diet_quality <- table(df$diet_quality)
counts_diet_quality # Mostly balanced


# ----- PLOTS -----
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

# Boxplot of sleep hours across gender
boxplot(df$sleep_hours ~ df$gender, data = df,
        notch = TRUE,
        xlab = "Gender",
        ylab = "Hours of Sleep (per night)",
        col  = "grey")

