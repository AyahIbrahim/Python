# Modifying my Cardiovacular Disease Dataset 
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


#Create named list of data frames
excel_output <- list("All Patients CVD Data" = cvd,
                     "Demographics & Background" = cvd_background,
                     "Dietary Intake" = cvd_diet,
                     "Health" = cvd_health,
                     "Diseases & Disorders" = cvd_diseases)

#Write list to Excel - Note you may need to open and re-save the file for it to work in Tableau
write.xlsx(excel_output, "CVD Data Updated.xlsx")



