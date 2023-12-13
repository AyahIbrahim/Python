# Modifying my Kaggle Datasets: 
# 1) BRFSS Cardiovacular Diseases Risk Prediction
# 2) UCI Cleveland Heart Disease
# To be Used for Better Visualization on Tableau

#Load necessary libraries
library(tidyverse)
library(openxlsx)
library(data.table) #Reads .csv files with much greater speed than the 'read.csv' function

#Set working directory 
setwd("C:/Users/Aya K/Desktop")

#Load original data file - don't modify the original cvd_raw
cvd_raw <- fread("CVD_cleaned.csv", check.names = T) #Read the data using fread from package data.table
names(cvd_raw) <- str_replace_all(names(cvd_raw), "\\.", "") #Remove periods from the variable names

#Look at the original data
glimpse(cvd_raw)

# Recode the values in the "Diabetes" column
cvd_raw <- cvd_raw %>%
  mutate(Diabetes = case_when(
    Diabetes == "Yes, but female told only during pregnancy" ~ "Yes",
    Diabetes == "No, pre-diabetes or borderline diabetes" ~ "No",
    TRUE ~ Diabetes
  ))

#Add an index variable to each row to ensure we are working with the same individuals at various stages of the process
cvd_temp <- cvd_raw %>%
  mutate(Patient_ID = row_number()) %>% #Add ID equivalent to the row_number()
  select(Patient_ID, everything()) #Make Patient_ID the left-most column

#Create a data frame with the yes/no variables
cvd_yesno <- cvd_raw %>%
  select(Exercise:Arthritis, Smoking_History) %>% #Select only the columns with Yes/No responses
  mutate_all(~as.integer(. == "Yes")) %>% #as.integer converts to a 0 or 1
  rename_all(~paste0("Has_", ., "_Int")) #Add "Has_" to the beginning and "_Int" to the end of each variable name

#Combine cvd_yesno with the original data set
cvd <- bind_cols(cvd_temp, cvd_yesno)

#Check the coding (repeat for all values to ensure coding is as expected)
#Also for statistics EDA purposes to have an idea of the count of patients 
cvd %>%
  count(Exercise, Has_Exercise_Int) 

cvd %>%
  count(Heart_Disease, Has_Heart_Disease_Int) 

cvd %>%
  count(Skin_Cancer, Has_Skin_Cancer_Int) 

cvd %>%
  count(Other_Cancer, Has_Other_Cancer_Int) 

cvd %>%
  count(Depression, Has_Depression_Int) 

cvd %>%
  count(Diabetes, Has_Diabetes_Int) 

cvd %>%
  count(Smoking_History, Has_Smoking_History_Int) 

#The advantage to having integer values is that the mean is equivalent to the percentage of "yesses."
cvd %>%
  summarise_at(vars(starts_with("Has_")), ~mean(.))

#Remove temporary data frames
rm(cvd_temp, cvd_yesno)

# According to the questions in the BRFSS Survey, the features concerning dietary choices refer to monthly intake. 
# However, the quantity entered can be in times per day, week, or month. 
# Therefore, it would be useful to unify the unit of measurement across them all.

# List of columns to be considered for unit conversion
columns_to_convert <- c('Alcohol_Consumption', 'Fruit_Consumption', 'Green_Vegetables_Consumption', 'FriedPotato_Consumption')

# Create a function to normalize values to "per month"
normalize_to_per_month <- function(x) {
  ifelse(x > 7, x * 4, ifelse(x > 1, x * 30, x))
}

# Apply the normalization function to the selected columns
cvd[, (columns_to_convert) := lapply(.SD, normalize_to_per_month), .SDcols = columns_to_convert]

# Display the results with the original column names preserved
glimpse(cvd)

##################################

#Recode General_Health to be a factor. This allows us to set the order and convert to a numeric variable.
cvd %>%
  count(General_Health)

#Create a vector of the General_Health categories in the desired order
general_health_levels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

cvd <- cvd %>%
  mutate(General_Health_Factor = factor(General_Health, levels = general_health_levels),
         General_Health_Num = as.numeric(General_Health_Factor))

#Check coding and have an idea of the count of patients in each health level
cvd %>%
  count(General_Health_Factor, General_Health, General_Health_Num)

#Repeat the same process for Checkup
checkup_levels <- c("Never", 
                    "5 or more years ago", 
                    "Within the past 5 years",
                    "Within the past 2 years",
                    "Within the past year") 
  
cvd <- cvd %>%
  mutate(Checkup_Factor = factor(Checkup, levels = checkup_levels),
         Checkup_Num = as.numeric(Checkup_Factor))

cvd %>%
  count(Checkup_Factor, Checkup, Checkup_Num)

#Repeat the same process for Age Category
age_levels <- c("18-24", "25-29",
                "30-34", "35-39",
                "40-44", "45-49",
                "50-54", "55-59",
                "60-64", "65-69",
                "70-74", "75-79", "80+") 

cvd <- cvd %>%
  mutate(Age_Factor = factor(Age_Category, levels = age_levels),
         Age_Num = as.numeric(Age_Factor))

cvd %>%
  count(Age_Factor, Age_Category, Age_Num)

### Split data into separate tables to focus at one area at a time ###

#Build Patient Demographics & Background variables table
cvd_background <- cvd %>%
  select(Patient_ID, Sex:BMI)

glimpse(cvd_background)

summary(cvd_background)

#Build Patient Dietary Choices table (will include a few other demographic factors that might be of influence)
cvd_diet <- cvd %>%
  select(Patient_ID, Sex, Age_Category, BMI, ends_with("Consumption"))

summary(cvd_diet)

#Build Patient Health Level & Other Factors table (will include a few other demographic factors that might be of influence)
cvd_health <- cvd %>%
  select(Patient_ID, Sex, Age_Category, BMI, General_Health_Num, Checkup, Has_Exercise_Int, Has_Smoking_History_Int)

#Build Diseases/Disorders table (will include a few other demographic factors that might be of influence)
cvd_diseases <- cvd %>%
  select(Patient_ID, Sex, Age_Category, BMI, Has_Heart_Disease_Int:Has_Arthritis_Int)

any(is.na(cvd))

#Create named list of data frames
excel_output <- list("All Patients CVD Data" = cvd,
                     "Demographics & Background" = cvd_background,
                     "Dietary Intake" = cvd_diet,
                     "Health" = cvd_health,
                     "Diseases & Disorders" = cvd_diseases)

glimpse(cvd)

#Write list to Excel - Note you may need to open and re-save the file for it to work in Tableau
write.xlsx(excel_output, "CVD Data Updated.xlsx")

#####################################################################################################

#Load original data file 
cleveland_raw <- fread("original_cleveland.csv", check.names = T)    #Read the data using fread from package data.table

glimpse(cleveland_raw)

# Rename the variables for better readability
cleveland_raw <- cleveland_raw %>%
  rename(
    Age = `age`,
    Sex = `sex`,
    ChestPain_Type= `cp`,
    Blood_Pressure_Level = `trestbps`,
    Cholestrol_Level = `chol`,
    Sugar_Level = `fbs`,
    Electrocardiogram_Results = `restecg`,
    Max_Heart_Rate = `thalach`,
    Exercise_Angina_Presence = `exang`,
    Exercise_STsegment_Depression_Value = `oldpeak`,
    SlopePeak_Exercise_STsegment_Value = `slope`,
    Visible_Arteries = `ca`,
    Thalassemia_Type = `thal`,
    Heart_Disease = `target`)

glimpse(cleveland_raw)

any(is.na(cleveland_raw)) # There are null values

# Check for NA values in the entire dataset
na_columns <- colnames(cleveland_raw)[colSums(is.na(cleveland_raw)) > 0]

# Print the names of columns with NA values
cat("Columns with NA values: ", na_columns, "\n")

# Calculate the mean for the 'ca' and 'thal' columns were NA values were found
mean_ca <- mean(cleveland_raw$Visible_Arteries, na.rm = TRUE)
mean_thal <- mean(cleveland_raw$Thalassemia_Type, na.rm = TRUE)

mean_ca
mean_thal

# Impute the missing values with the means
cleveland_raw$Visible_Arteries[is.na(cleveland_raw$Visible_Arteries)] <- mean_ca
cleveland_raw$Thalassemia_Type[is.na(cleveland_raw$Thalassemia_Type)] <- mean_thal

# Verify that there are no more NA values
any(is.na(cleveland_raw)) 

# Now, there are no more NA values

#Write changes to CSV 
write.csv(cleveland_raw, "Cleveland CVD Data.csv", row.names = FALSE)

glimpse(cleveland_raw)


