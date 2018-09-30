###############################################################################
# Programmer: Matthew Landes                                                  #
# Date:       September 17, 2018                                              #
# Data:       Telco Customer                                                  #
#             https://www.kaggle.com/blastchar/telco-customer-churn/version/1 #
# Purpose:    Demonstrate K-means clustering                                  #
###############################################################################


#################### Section 1: Packages/Read Data ############################

# Packages
library(dplyr)

# Reads data from folder
data = read.csv("C:/Users/Matthew Landes/Documents/Matt/Kaggle/Telco_Customers/WA_Fn-UseC_-Telco-Customer-Churn.csv",
                header = TRUE)

###############################################################################

#################### Section 2: Summarize Data/Remove Missing #################

# Summary of data
summary(data)

# Filters out missing Total Charges
data = data %>% filter(!is.na(data$TotalCharges))

###############################################################################

#################### Section 3: Category to Numeric Featues ###################

# Encodes categorical feature as numeric
data2 = matrix(nrow=nrow(data), ncol= 20)

for (i in 1:nrow(data)){
  
  # Gender numeric encodeing
  if(data$gender[i] == "Female") {data2[i,1] = 0} else {data2[i,1] = 1}
  
  # Copy Senior Citzen values
  data2[i,2] = as.numeric(data$SeniorCitizen[i])
  
  # Partner numeric encoding
  (if(data$Partner[i] == "No") {data2[i,3] = 0} else {data2[i,3] = 1})
  
  # Dependents numeric encoding
  (if(data$Dependents[i] == "No") {data2[i,4] = 0} else {data2[i,4] = 1})
  
  # Copying Tenure values
  data2[i,5] = as.numeric(data$tenure[i])
  
  # Phone Serive numeric encoding
  if(data$PhoneService[i] == "No") {data2[i,6] = 0} else {data2[i,6] = 1}
  
  # Multiple Lines numeric encoding
  if(data$MultipleLines[i] == "No phone service") {data2[i,7] = -1} 
    else if(data$MultipleLines[i] == "No") {data2[i,7] = 0} else {data2[i,7] = 1}
  
  # Internet Service numeric encoding
  if(data$InternetService[i] == "No") {data2[i,8] = -1} 
    else if(data$InternetService[i] == "DSL") {data2[i,8] = 0} else {data2[i,8] = 1}
  
  # Online Security numeric encoding 
  if(data$OnlineSecurity[i] == "No internet service") {data2[i,9] = -1} 
    else if(data$OnlineSecurity[i] == "No") {data2[i,9] = 0} else {data2[i,9] = 1}
  
  # Online Backup numeric encoding
  if(data$OnlineBackup[i] == "No internet service") {data2[i,10] = -1} 
    else if(data$OnlineBackup[i] == "No") {data2[i,10] = 0} else {data2[i,10] = 1}
  
  # Device Protection numeric encoding
  if(data$DeviceProtection[i] == "No internet service") {data2[i,11] = -1} 
    else if(data$DeviceProtection[i] == "No") {data2[i,11] = 0} else {data2[i,11] = 1}
  
  # Tech Support numeric encoding
  if(data$TechSupport[i] == "No internet service") {data2[i,12] = -1} 
    else if(data$TechSupport[i] == "No") {data2[i,12] = 0} else {data2[i,12] = 1}
  
  # Streaming TV numeric encoding
  if(data$StreamingTV[i] == "No internet service") {data2[i,13] = -1} 
    else if(data$StreamingTV[i] == "No") {data2[i,13] = 0} else {data2[i,13] = 1}
  
  # Streaming Movies numeric encoding
  if(data$StreamingMovies[i] == "No internet service") {data2[i,14] = -1} 
    else if(data$StreamingMovies[i] == "No") {data2[i,14] = 0} else {data2[i,14] = 1}
  
  # Contract numeric encoding
  if(data$Contract[i] == "Month-to-month") {data2[i,15] = -1} 
    else if(data$Contract[i] == "One year") (data2[i,15] = 0) else {data2[i,15] = 1}
  
  # Paperless Billing numeric encoding
  if(data$PaperlessBilling[i] == "No") {data2[i,16] = 0} else {data2[i,16] = 1}
  
  # Payment Method numeric encoding
  if(data$PaymentMethod[i] == "Mailed check") {data2[i,17] = -1}
    else if (data$PaymentMethod[i] == "Electronic check") {data2[i,17] = 0}
    else if (data$PaymentMethod[i] == "Bank transfer (autometic)") {data2[i,17] = 1} else {data2[i,17] = 2}
  
  # Copying Monthly Charges 
  data2[i,18] = as.numeric(data$MonthlyCharges[i])
  
  # Copying Total Charges
  data2[i,19] = as.numeric(data$TotalCharges[i])
  
  # Color coding Churn
  if(data$Churn[i] == "No") {data2[i,20] = "green"} else {data2[i,20] = "red"}
}

# Changes the matrice into a dataframe
data2 = as.data.frame(data2)

# Adds labels from original dataset
names(data2) = names(data)[2:21]

# Changes classes from factors to numeric
data2$gender = as.numeric(data2$gender)
data2$SeniorCitizen = as.numeric(data2$SeniorCitizen)
data2$Partner = as.numeric(data2$Partner)
data2$Dependents = as.numeric(data2$Dependents)
data2$tenure = as.numeric(data2$tenure)
data2$PhoneService = as.numeric(data2$PhoneService)
data2$MultipleLines = as.numeric(data2$MultipleLines)
data2$InternetService = as.numeric(data2$InternetService)
data2$OnlineSecurity = as.numeric(data2$OnlineSecurity)
data2$OnlineBackup = as.numeric(data2$OnlineBackup)
data2$DeviceProtection = as.numeric(data2$DeviceProtection)
data2$TechSupport = as.numeric(data2$TechSupport)
data2$StreamingTV = as.numeric(data2$StreamingTV)
data2$StreamingMovies = as.numeric(data2$StreamingMovies)
data2$Contract = as.numeric(data2$Contract)
data2$PaperlessBilling = as.numeric(data2$PaperlessBilling)
data2$PaymentMethod = as.numeric(data2$PaymentMethod)
data2$MonthlyCharges = as.numeric(data2$MonthlyCharges)
data2$TotalCharges = as.numeric(data2$TotalCharges)

###############################################################################


#################### Section 4: Clustering ####################################

# Two clusters without scaling 
km2 = kmeans(data2[,1:19], 2)
table(km2$cluster, data$Churn)

# Scales dataset
data3 = scale(data2[,1:19])

# Two clusters with scaling
km2.1 = kmeans(data3[,1:19], 2)
table(km2.1$cluster, data$Churn)

# Three clusters 
km3 = kmeans(data3[,1:19], 3)
table(km3$cluster, data$Churn)

# Four clusters 
km4 = kmeans(data3[,1:19], 4)
table(km4$cluster, data$Churn)

# Five clusters 
km5 = kmeans(data3[,1:19], 5)
table(km5$cluster, data$Churn)

# Six clusters 
km6 = kmeans(data3[,1:19], 6)
table(km6$cluster, data$Churn)

###############################################################################