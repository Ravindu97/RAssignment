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

# (ii) Perform PCA

# Load necessary packages
library(FactoMineR)
library(factoextra)

# Select numerical features (assuming you know data types)
pca_data_numeric <- mydata %>% select(DataTransferVolume_OUT, DataTransferVolume_IN, TransactionsPerSession, 
                                      NetworkAccessFrequency, UserActivityLevel, SystemAccessRate,
                                      SecurityRiskLevel, ResponseTime)

# Perform PCA
pca_result <- PCA(pca_data_numeric, scale.unit = TRUE, graph = FALSE)


# Variance explained by each component
pca_result$eig[,c(2,3)]  

# Identify number of PCs for capturing adequate variance 
get_pca_var(pca_result)$cumprop 

# Loadings
pca_result$var$coord 


#(iii) Create Biplot
install.packages("factoextra")
library(factoextra)
fviz_biplot(pca_result, col.var = "Classification",  
            geom.var = list(size = 2), # Adjust arrow size if needed
            addEllipses = TRUE)  # Add confidence ellipses 

# (v) Identify Dimensions for Classification
# Scree plot (to help with decision)
fviz_eig(pca_result) 




