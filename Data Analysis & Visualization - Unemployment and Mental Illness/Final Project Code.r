# Load necessary libraries
# Packages already installed

library(readr)              # Faster way to read in rectangular data
library(openxlsx)           # Reading, writing, editing Excel files
library(corrplot)           # Visualizing correlation matrices, offers various methods
library(dplyr)              # Manipulating data sets
library(tidyverse)          # For tidying up data set
library(ggplot2)            # For visualization
library(ggcorrplot)         # For correlation matrix plots, an extension of ggplot2
options(warn=-1)

# Set working directory
setwd("C:/Users/Aya K/Desktop/ERM 411 Final Project")

# Load original data
df <- read.xlsx("Cleaned Data.xlsx", sep.names = "_")

# Initial exploration to familiarize myself with the data set
glimpse(df)                

# Print column names
print(names(df))

# Rename the variables for better readability
df <- df %>%
  rename(
    Employed = `I_am_currently_employed_at_least_part-time`,
    Unemployed = `I_am_unemployed`,
    Mental_Illness = `I_identify_as_having_a_mental_illness`,
    Owns_Computer = `I_have_my_own_computer_separate_from_a_smart_phone`,
    Hospitalized_Before_Illness = `I_have_been_hospitalized_before_for_my_mental_illness`,
    Days_Hospitalized = `How_many_days_were_you_hospitalized_for_your_mental_illness`,
    Legally_Disabled = `I_am_legally_disabled`,
    Regular_Internet_Access = `I_have_my_regular_access_to_the_internet`,
    Lives_With_Parents = `I_live_with_my_parents`,
    Resume_Gap = `I_have_a_gap_in_my_resume`,
    Gap_Duration_Months = `Total_length_of_any_gaps_in_my_resume_in_months`,
    Annual_Income_USD = `Annual_income_(including_any_social_welfare_programs)_in_USD`,
    Reads_Outside_Work_School = `I_read_outside_of_work_and_school`,
    Welfare_AnnualIncome_USD = `Annual_income_from_social_welfare_programs`,
    Receives_FoodStamps = `I_receive_food_stamps`,
    On_Section8_Housing = `I_am_on_section_8_housing`,
    Times_Hospitalized = `How_many_times_were_you_hospitalized_for_your_mental_illness`)

# Double check for missing values
sum(is.na(df))         
colSums(is.na(df))

df$Region[is.na(df$Region)] <- "Unknown"

cols <- c("Tiredness", "Compulsive_behavior", "Panic_attacks", 
                         "Mood_swings", "Obsessive_thinking", "Lack_of_concentration")

# Find the rows that have NA in any of the columns of interest
NA_rows <- apply(df[cols], 1, function(x) any(is.na(x)))
NA_rows

# Exclude those rows from the dataframe
df <- df[!NA_rows, ]

unique(df$Age)
unique(df$Device_Type)
unique(df$Region)
unique(df$Education)
unique(df$Household_Income)

# Convert categorical variables to factors and order them if needed

# Ordered from highest to lowest
df$Education <- factor(df$Education, levels = c("Completed Phd", "Some Phd", "Completed Masters",
                                                "Some Masters", "Completed Undergraduate", "Some Undergraduate",
                                                "High School or GED", "Some highschool"))


df$Gender <- as.factor(df$Gender)

# Ordered from highest to lowest
df$Household_Income <- factor(df$Household_Income, levels = c("$200,000+", "$175,000-$199,999", "$150,000-$174,999",
                                                             "$125,000-$149,999", "$100,000-$124,999", "$75,000-$99,999",
                                                             "$50,000-$74,999", "$25,000-$49,999", "$10,000-$24,999",
                                                             "$0-$9,999"))

# Regions are ordered from the Westernmost to the Easternmost 
df$Region <- factor(df$Region, levels = c("Pacific", "Mountain", "West North Central", "West South Central",
                                          "East North Central", "East South Central", 
                                          "South Atlantic", "Middle Atlantic", "New England"))

# Ordered from Phones/Tablets to Desktops/Laptops for better readability
df$Device_Type <- factor(df$Device_Type, levels = c("Android Phone / Tablet", "iOS Phone / Tablet",
                                                    "Windows Desktop / Laptop", "MacOS Desktop / Laptop", "Other"))

# Ordered from oldest to youngest
df$Age <- factor(df$Age, levels = c("> 60", "45-60", "30-44", "18-29"))

# View the data set structure after being modified
str(df)

# Write changes to CSV 

write.csv(df, "Updated Unemployment & Mental Health Data.csv", row.names = FALSE)

# Summary statistics for numerical variables
print(summary(df %>% 
                select_if(is.numeric)))

# Frequency distribution for categorical variables
cat_cols <- c("Education", "Gender", "Age", "Household_Income", "Region", "Device_Type")

frequency_tables <- sapply(df[cat_cols], table)

frequency_tables

# Group-specific Summary Statistics
summary_table <- df %>%
  group_by(Unemployed) %>%
  summarise(
    Annual_Income_Mean = mean(Annual_Income_USD),
    Annual_Income_SD = sd(Annual_Income_USD),
    Annual_Income_Min = min(Annual_Income_USD),
    Annual_Income_Max = max(Annual_Income_USD),
    Welfare_Income_Mean = mean(Welfare_AnnualIncome_USD),
    Welfare_Income_SD = sd(Welfare_AnnualIncome_USD),
    Welfare_Income_Min = min(Welfare_AnnualIncome_USD),
    Welfare_Income_Max = max(Welfare_AnnualIncome_USD),
    ResumeGap_Duration_Months_Mean = mean(Gap_Duration_Months),
    ResumeGap_Duration_Months_SD = sd(Gap_Duration_Months),
    ResumeGap_Duration_Months_Min = min(Gap_Duration_Months),
    ResumeGap_Duration_Months_Max = max(Gap_Duration_Months),
    Internet_Access_Mean = mean(Regular_Internet_Access),
    Owns_Comp_Mean = mean(Owns_Computer),
    Times_Hospitalized_Mean = mean(Times_Hospitalized),
    Times_Hospitalized_SD = sd(Times_Hospitalized),
    Times_Hospitalized_Min = min(Times_Hospitalized),
    Times_Hospitalized_Max = max(Times_Hospitalized),
    Depression_Mean = mean(Depression),
    Anxiety_Mean = mean(Anxiety),
    Tiredness_Mean = mean(Anxiety),
    Compulsivebehavior_Mean = mean(Compulsive_behavior),
    PanicAttacks_Mean = mean(Panic_attacks),
    MoodSwings_Mean = mean(Mood_swings),
    ObsThinking_Mean = mean(Obsessive_thinking), 
    ConcentrationLack_Mean = mean(Lack_of_concentration), 
    Disabled_Mean = mean(Legally_Disabled),
    Mental_Illness_Mean = mean(Mental_Illness),
    Receives_FoodStamps_Mean = mean(Receives_FoodStamps),
    Section8_Housing_Mean = mean(On_Section8_Housing)  
  )

print(summary_table, n = Inf, width = Inf)

# Histograms for numerical variables

library(repr)            # Used for representing data objects in different formats. 

# Adjust figure size's width and height
options(repr.plot.width = 10, repr.plot.height = 6)

num_cols <- names(df %>% select_if(is.numeric))

# Loop through each numerical column to create histograms

for (col in num_cols) {
    print(
        ggplot(df, aes_string(x = col)) + 
        geom_histogram(bins = 15,
                       fill = "cornflowerblue", 
                       col = "white",
                       na.rm = TRUE) + 
        ggtitle(paste("Histogram of", col)) +
        theme_minimal()+
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 20, face = "bold")
        )
    )
}

# For each of non-binary numerical features:

# Calculating Kurtosis

# Acceptable values range from -10 to 10
# Large negative values indicate platykurtic (too flat), large positive values indicate leptokurtic (too pointy).

library(psych)          # Basic data analysis and psychometric analysis.
options(warn=-1)

kurtosi(df$Days_Hospitalized)
kurtosi(df$Times_Hospitalized)
kurtosi(df$Gap_Duration_Months)
kurtosi(df$Annual_Income_USD)
kurtosi(df$Welfare_AnnualIncome_USD)

# Calcualting Skewness 

# Acceptable values range from -3 to 3
# Large negative values indicate negative skew, large positive values indicate positive skew.

skew(df$Days_Hospitalized)
skew(df$Times_Hospitalized)
skew(df$Gap_Duration_Months)
skew(df$Annual_Income_USD)
skew(df$Welfare_AnnualIncome_USD)

# Bar Plots for Categorical Features

cat_cols <- names(df %>% select_if(is.factor))

options(repr.plot.width = 25, repr.plot.height = 15)

# Ensure this vector has enough colors for the categories
my_colors <- c("palevioletred", "cornflowerblue", "salmon", "plum", "khaki", 
               "sienna", "skyblue", "maroon", "tan", "thistle", 
               "violet") 

# Loop through each categorical column to create bar plots
for (col in cat_cols) {
    print(
        ggplot(df, aes_string(x = col, fill = col)) +
        geom_bar() +
        scale_fill_manual(values = my_colors) + 
        ggtitle(paste("Bar Plot of", col)) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 30, face = "bold"),
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 25, face = "bold"),
            axis.text.x = element_text(size = 25, face = "bold"),
            legend.position = "none") +
        coord_flip()
    )
}

# Correlation Matrix
num_data <- df[, num_cols]

# Compute correlation on the selected numeric data
cor_matrix <- cor(num_data, use = "complete.obs")
cor_matrix

# Spotting correlations from the matrix above is not as efficient as when they are color-coded in a heatmap 

library(reshape2)         # Reshaping data between long and wide formats, making it easier to manipulate and visualize data. 
library(hrbrthemes)       # Primarily used to enhance the visual appeal of the plots.
options(warn=-1)

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

options(repr.plot.width=15, repr.plot.height=15) 

# Plot the heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
geom_tile() +
scale_fill_distiller(palette = "RdBu") +
theme_ipsum(base_size = 25) +  # Apply the increased font size
theme(
    axis.text.x = element_text(angle = 90, size = 15, face = "bold"),  
    axis.text.y = element_text(size = 15, face = "bold")
) 

# List of features to plot against Employment
features <- c("Age", "Gender", "Region", "Education", "Household_Income")


df$Unemployed <- factor(df$Unemployed, levels = c(0, 1))

options(repr.plot.width = 25, repr.plot.height = 15)


for (feature in features) {
    print(
        ggplot(df, aes_string(x = feature, fill = "Unemployed")) +
        geom_bar(position = "fill") +
        labs(
            title = paste("Distribution of Unmployment by", feature),
            x = feature,
            y = "Proportion",
            fill = "Unemployment Status") +
        scale_fill_manual(
            values = c("0" = "lightblue", "1" = "firebrick1"),
            labels = c("Employed", "Unemployed")) +
        theme_minimal() +
       theme(
            plot.title = element_text(size = 30, face = "bold"),
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 25, face = "bold"),
            axis.text.y = element_text(size = 25, face = "bold"),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 25, face = "bold"),  
            legend.key.size = unit(2, "lines") 
            ) +
        coord_flip()
    )
}

# List of features to plot against Resume Gap
features <- c("Age", "Gender", "Region", "Education", "Household_Income")

df$Resume_Gap <- factor(df$Resume_Gap , levels = c(0, 1))

options(repr.plot.width = 25, repr.plot.height = 15)

for (feature in features) {
    print(
        ggplot(df, aes_string(x = feature, fill = "Resume_Gap")) +
        geom_bar(position = "fill") +
        labs(title = paste("Distribution of Resume Gap by", feature),
             x = feature,
             y = "Proportion",
             fill = "Resume Gap Presence") +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "firebrick1"),
                          labels = c("No Resume Gap", "With Resume Gap")) +
        theme_minimal() +
         theme(
            plot.title = element_text(size = 30, face = "bold"),
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 25, face = "bold"),
            axis.text.y = element_text(size = 25, face = "bold"),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 25, face = "bold"),  
            legend.key.size = unit(2, "lines") 
            ) +
        coord_flip()
    )
}

# List of features to plot against Mental Illness
features <- c("Age", "Gender", "Region", "Education", "Household_Income")

df$Mental_Illness <- factor(df$Mental_Illness, levels = c(0, 1))

options(repr.plot.width = 25, repr.plot.height = 15)

for (feature in features) {
    print(
        ggplot(df, aes_string(x = feature, fill = "Mental_Illness")) +
        geom_bar(position = "fill") +
        labs(title = paste("Distribution of Mental Illness by", feature),
             x = feature,
             y = "Proportion",
             fill = "Mental Illness Presence") +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "firebrick1"),
                          labels = c("No Mental Illness", "With Mental Illness")) +
        theme_minimal() +
       theme(
            plot.title = element_text(size = 30, face = "bold"),
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 25, face = "bold"),
            axis.text.y = element_text(size = 25, face = "bold"),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 25, face = "bold"),  
            legend.key.size = unit(2, "lines") 
            ) +
        coord_flip()
    )
}

# Chi-Square Test for Region and Education
chisq_test1 <- chisq.test(df$Region, df$Education)
print(chisq_test1)

# Chi-Square Test for Region and Household Income
chisq_test2 <- chisq.test(df$Region, df$Household_Income)
print(chisq_test2)

# I can make the assumption that this is also the case with Annual Income, but let's double check
# Boxplot for Annual Income and Region

options(repr.plot.width = 20, repr.plot.height = 15)
ggplot(df, aes(x = Region, y = Annual_Income_USD)) +
geom_boxplot(aes(fill=factor(Region))) +
labs(title = "Boxplot of Annual Income by Region",
     x = "Region",
     y = "Annual Income",
     fill = "Region")+
theme(plot.title = element_text(size = 25, face = "bold"),
      axis.title = element_text(size = 25, face = "bold"),
      axis.text.x = element_text(size = 25, face = "bold", angle = 90),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20, face = "bold"),  
      legend.key.size = unit(3, "lines")
     )

# Let's check if the relationship is significant or due to chance
logistic_model1 <- glm(Unemployed ~ Region + Age + Education,
                              family = "binomial", data = df)

summary(logistic_model1)

# Chi-Square Test for Education and Household Income 
chisq_test3 <- chisq.test(df$Household_Income, df$Education)
print(chisq_test3)

# Chi-Square Test for Age and Household Income
chisq_test4 <- chisq.test(df$Age, df$Household_Income)
print(chisq_test4)

# Since there is a significant relationship for Education & Age with Household Income:
# I can make the assumption that this is also the case with Annual Income

# Boxplot for Annual Income and Education

options(repr.plot.width = 20, repr.plot.height = 15)
ggplot(df, aes(x = Education, y = Annual_Income_USD)) +
geom_boxplot(aes(fill=factor(Education))) +
labs(title = "Boxplot of Annual Income by Education",
     x = "Education",
     y = "Annual Income",
     fill = "Education Level")+
theme(plot.title = element_text(size = 25, face = "bold"),
      axis.title = element_text(size = 25, face = "bold"),
      axis.text.x = element_text(size = 25, face = "bold", angle = 90),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20, face = "bold"),  
      legend.key.size = unit(3, "lines")
     )

# Boxplot for Annual Income and Age

options(repr.plot.width = 12, repr.plot.height = 10)
ggplot(df, aes(x = Age, y = Annual_Income_USD)) +
geom_boxplot(aes(fill=factor(Age))) +
labs(title = "Boxplot of Annual Income by Age",
     x = "Age",
     y = "Annual Income",
     fill = "Age")+
theme(plot.title = element_text(size = 25, face = "bold"),
      axis.title = element_text(size = 25, face = "bold"),
      axis.text.x = element_text(size = 25, face = "bold", angle = 90),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20, face = "bold"),  
      legend.key.size = unit(3, "lines")
     )

# Next are statistical tests for mental illness and unemployment 
# Unemployment & Mental Illness
logistic_model2 <- glm(Unemployed ~ Mental_Illness + Panic_attacks + Tiredness + Depression + Anxiety,
                      data = df, family = "binomial")
summary(logistic_model2)

# Unemployment & Resume Gaps
logistic_model3 <- glm(Unemployed ~ Resume_Gap + Gap_Duration_Months,
                      data = df, family = "binomial")
summary(logistic_model3)

# Unemployment and the Combination of Resume Gap with Mental Illness 
logistic_model4 <- glm(Unemployed ~ Resume_Gap * Anxiety, 
                              family = "binomial", data = df)

summary(logistic_model4)

logistic_model5 <- glm(Unemployed ~ Resume_Gap * Depression, 
                              family = "binomial", data = df)

summary(logistic_model5)

logistic_model6 <- glm(Unemployed ~ Resume_Gap * Mental_Illness, 
                              family = "binomial", data = df)
summary(logistic_model6)

logistic_model6 <- glm(Unemployed ~ Resume_Gap * Legally_Disabled, 
                              family = "binomial", data = df)
summary(logistic_model6)

# Linear Regression for Times Hospitalized based on different predictors

# number of days hospitalized is predicted based on employment status 
linear_model1 <- lm(Days_Hospitalized ~ Unemployed,
                    data = df)

summary(linear_model1)

# Linear Regression for Times Hospitalized based on different predictors

# To what extent can the resume gap and gap duration predict days hospitalized

linear_model2 <- lm(Days_Hospitalized ~ Resume_Gap + Gap_Duration_Months,
                    data = df)
summary(linear_model2)

features <- c("Annual_Income_USD", "Welfare_AnnualIncome_USD", "Days_Hospitalized", "Gap_Duration_Months")

# Convert Mental_Illness to a factor
df$Mental_Illness <- factor(df$Mental_Illness, levels = c(0, 1))

# Set plot dimensions
options(repr.plot.width = 13, repr.plot.height = 13)

# Loop through features and create plots
for (feature in features){
    print(
        ggplot(df, aes_string(x = feature, fill = "Mental_Illness")) +
        geom_histogram(binwidth = 10, col = "white") +
        labs(title = paste("Distribution of Mental Illness by", feature),
             x = feature,
             y = "Proportion",
             fill = "Mental Illness Presence") +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "firebrick1"),
                          labels = c("No Mental Illness", "With Mental Illness")) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 20, face = "bold", angle = 90),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),  
            legend.key.size = unit(1.5, "lines")
        )
    )
}

# Convert the Low Income Indicator Variables into Factors
df$On_Section8_Housing <- factor(df$On_Section8_Housing, levels = c(0, 1), labels = c("No", "Yes"))
df$Receives_FoodStamps <- factor(df$Receives_FoodStamps, levels = c(0, 1), labels = c("No", "Yes"))
df$Lives_With_Parents <- factor(df$Lives_With_Parents, levels = c(0, 1), labels = c("No", "Yes"))
df$Regular_Internet_Access <- factor(df$Regular_Internet_Access, levels = c(0, 1), labels = c("No", "Yes"))

conditions <- c("Regular_Internet_Access", "Receives_FoodStamps", "On_Section8_Housing", "Lives_With_Parents")

options(repr.plot.width = 13, repr.plot.height = 13)

for (feature in conditions){
    print(
        ggplot(df, aes_string(x = feature, fill = "Mental_Illness")) +
        geom_bar(binwidth = 10, col = "white") +
        labs(title = paste("Distribution of Mental Illness by", feature),
             x = feature,
             y = "Proportion",
             fill = "Mental Illness Presence") +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "firebrick1"),
                          labels = c("No Mental Illness", "With Mental Illness")) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),  
            legend.key.size = unit(1.5, "lines")
        )
    )
}

# Check effectiveness of Social Welfare Programs w/ Mental Illness & Unemployment
logistic_model5 <- glm(Mental_Illness ~ Receives_FoodStamps + Welfare_AnnualIncome_USD + 
                       On_Section8_Housing, data = df, family = "binomial")
summary(logistic_model5)

logistic_model6 <- glm(Unemployed ~ Receives_FoodStamps + Welfare_AnnualIncome_USD + 
                       On_Section8_Housing, data = df, family = "binomial")
summary(logistic_model6)

options(repr.plot.width = 13, repr.plot.height = 13)

for (feature in conditions){
    print(
        ggplot(df, aes_string(x = feature, fill = "Unemployed")) +
        geom_bar(binwidth = 10, col = "white") +
        labs(title = paste("Distribution of Employment by", feature),
             x = feature,
             y = "Proportion",
             fill = "Employment Status") +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "firebrick1"),
                          labels = c("Employed", "Unemployed")) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),  
            legend.key.size = unit(1.5, "lines")
        )
    )
}


df$Anxiety <- factor(df$Anxiety, levels = c(0, 1), labels = c("No", "Yes"))
df$Depression <- factor(df$Depression, levels = c(0, 1), labels = c("No", "Yes"))
df$Mental_Illness <- factor(df$Mental_Illness, levels = c(0, 1), labels = c("No", "Yes"))
df$Panic_attacks <- factor(df$Panic_attacks, levels = c(0, 1), labels = c("No", "Yes"))
df$Resume_Gap <- factor(df$Resume_Gap, levels = c(0, 1), labels = c("No", "Yes"))

conditions <- c("Anxiety", "Depression", "Mental_Illness", "Mental_Illness", "Panic_attacks", "Resume_Gap")

# Convert Mental_Illness to a factor
df$Unemployed <- factor(df$Unemployed, levels = c(0, 1))

options(repr.plot.width = 13, repr.plot.height = 13)

for (feature in conditions){
    print(
        ggplot(df, aes_string(x = feature, fill = "Unemployed")) +
        geom_bar(binwidth = 10, col = "white") +
        labs(title = paste("Distribution of Employment by", feature),
             x = feature,
             y = "Proportion",
             fill = "Employment Status") +
        scale_fill_manual(values = c("0" = "lightblue", "1" = "firebrick1"),
                          labels = c("Employed", "Unemployed")) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15, face = "bold"),  
            legend.key.size = unit(1.5, "lines")
        )
    )
}

