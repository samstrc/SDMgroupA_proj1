# Group A project 1 
data_dir = "/Users/samstrickler/School/Statistical Database Management/Project 1"
fp <- function(...) file.path(data_dir, ...)


df <- read.csv(fp("student_habits.csv"), header = TRUE)
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

counts_sleepqual <- table(df$gender)
counts_sleepqual # Kinda balanced

counts_occupation <- table(df$diet_quality)
counts_occupation # Mostly balanced


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
barplot(counts_occupation, xlab="Occupation", ylab="count", col="tan")
barplot(counts_sleepqual, xlab="Sleep Quality", ylab="count", col="tan")

# Boxplot of BMI across occupations..this is silly baha
boxplot(BMI ~ Occupation, data = df,
        notch = TRUE,
        xlab = "Occupation",
        ylab = "BMI",
        col  = "saddlebrown")

