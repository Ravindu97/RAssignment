install.packages("tidyverse") 
library(dplyr)


# You may need to change/include the path of your working directory

dat <- read.csv("HealthCareData_2024.csv", stringsAsFactors = TRUE)

# Separate samples of normal and malicious events
dat.class0 <- dat %>% filter(Classification == "Normal") # normal
dat.class1 <- dat %>% filter(Classification == "Malicious") # malicious

# Randomly select 400 samples from each class, then combine them to form a working dataset
set.seed(10583221)
rand.class0 <- dat.class0[sample(1:nrow(dat.class0), size = 400, replace = FALSE),]
rand.class1 <- dat.class1[sample(1:nrow(dat.class1), size = 400, replace = FALSE),]
# Your sub-sample of 800 observations
mydata <- rbind(rand.class0, rand.class1)
dim(mydata) # Check the dimension of your sub-sample


# Install necessary packages
install.packages(c("tidyverse",  "FactoMineR", "factoextra")) 

# Load packages
library(tidyverse)
library(FactoMineR)
library(factoextra)

#Part 1: Exploratory Data Analysis and Data Cleaning

#Steps

#1 Summarize Categorical Features

# Function to create a summary table
summarize_categorical <- function(data, features) {
  data %>%
    select(one_of(features)) %>%
    group_by_all() %>% 
    summarise(N = n(), Percent = N/nrow(data) * 100) %>%
    mutate(Percent = round(Percent, 1)) 
}

# Now you can use the function:
summary_categorical = summarize_categorical(mydata, c("AlertCategory", "NetworkEventType", "NetworkInteractionType", 
                              "SessionIntegrityCheck", "ResourceUtilizationFlag")) 

# Explanation:
  
# We define a function summarize_categorical to streamline the summarization process.
# This function selects the specified categorical features, groups the data by each unique combination of category levels, counts the occurrences (N), and calculates percentages (Percent).


# 2 Summarize Continuous/Numeric Features
# Function to create a summary table
summarize_numeric <- function(data, features) {
  data %>%
    select(one_of(features)) %>%
    summarise_all(list(~length(.), 
                       Missing = ~sum(is.na(.)), 
                       Min = ~min(.), 
                       Max = ~max(.),
                       Mean = ~mean(.),
                       Median = ~median(.),
                       Skewness = ~e1071::skewness(.))) 
}


# Apply the function to your data
summary_numeric=summarize_numeric(mydata, c("DataTransferVolume.out", "DataTransferVolume.in", 
                          "TransactionsPerSession", "NetworkAccessFrequency", 
                          "UserActivityLevel", "SystemAccessRate", "SecurityRiskLevel", 
                          "ResponseTime"))

# Explanation:
  
# We create a similar function, summarize_numeric, for continuous features.
# The summarise_all function allows calculating multiple summary statistics at once. We calculate the number of observations (N), missing values (Missing), minimum (Min), maximum (Max), mean (Mean), median (Median), and skewness of the distribution.

#3 Identifying and Addressing Data Issues

#Examine Summary Tables:  Carefully analyze the output of the summary tables created in steps 1 and 2. Look for the following:
  
  #Missing values: High percentages of missing data in particular features might require attention.
  #Unexpected categories: Investigate categorical features with categories that don't align with your understanding of the data.
  #Extreme outliers: Look for unusually large or small values in continuous features, especially in the minimum, maximum, and skewness values.

# Visualization: Create visualizations to enhance the identification of potential issues.

# Histograms for continuous features:
ggplot(mydata, aes(x = TransactionsPerSession)) +
  geom_histogram(bins = 30, aes(fill = ..density..), color = "black") +
  geom_density(alpha = 0.3, overlay = TRUE, color = "darkorange") +  # Adjust the number of bins as needed
  labs(title = "Distribution of Transactions per Session",
       x = "Transactions per Session",
       y = "Count") +
  scale_fill_viridis_c()  # Or other color palette from 'viridis'

# Summarize Continuous/Numeric Features

ggplot(mydata, aes(x = Classification, y = SystemAccessRate)) + 
  geom_boxplot(fill = c("#E69F00", "#56B4E9")) +  
  labs(title = "System Access Rate by Classification") +
  scale_fill_manual(values =  c("#E69F00", "#56B4E9")) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),  # Adjust gridlines as desired
        panel.grid.major.x = element_line(color = "grey80")) 

 
# 1. Missing Data
# Identifying High Proportion Missing Values:

# Calculate proportion of missing values per feature
colSums(is.na(mydata)) / nrow(mydata) 

# Example: Remove features with more than 40% missingness
threshold <- 0.4  
mydata_filtered <- mydata[, colSums(is.na(mydata))/nrow(mydata) < threshold]

# Investigating Patterns (using 'mice' package):

install.packages("mice")
library(mice)

md.pattern(mydata) # Visualize missing data patterns

# Imputation

#Mean/Median Imputation:
for(col in colnames(mydata)) {
  if(is.numeric(mydata[, col])) {
    mydata[is.na(mydata[, col]), col] <- mean(mydata[, col], na.rm = TRUE) # Mean imputation
  } 
}

# Predictive Models (example with 'mice' package):
imp <- mice(mydata, m = 5, maxit = 10, method = "pmm")  # Create multiple Imputations
complete_data <- complete(imp, 1)  # Select the first imputed dataset

# 2. Invalid Categories

#Identify and Correct
# Example: Assuming a feature named 'NetworkEventType'
unique(mydata$NetworkEventType) # View unique categories

# Correct typos (e.g., "Regualr" -> "Regular")
mydata$NetworkEventType[mydata$NetworkEventType == "Regualr"] <- "Regular"

# Combine or Remove:
mydata$NetworkEventType <- factor(mydata$NetworkEventType) # Ensure it's a factor

# Example: Combine categories appearing less than 5% of the time into "Other"
mydata$NetworkEventType[levels(mydata$NetworkEventType)[table(mydata$NetworkEventType) < nrow(mydata) * 0.05]] <- "Other"

# 3. Outliers

# Verification (Boxplots + Domain Knowledge):
ggplot(mydata, aes(y = SystemAccessRate)) + geom_boxplot()  # Replace with your feature

# Use your domain knowledge to assess if extreme values make sense in the context of your data 

# Removal:
# Example: Remove rows where 'SystemAccessRate ' is above 200
mydata_filtered <- mydata[mydata$SystemAccessRate <= 200, ] 

