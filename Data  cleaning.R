# Data cleaning 

# Install packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")


# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(readr)

setwd("D:/Digital health project")

# 1. Load data
data <- read.csv("CVD Risk Assessment Dataset.csv")
# 2. EXPLORE STRUCTURE
# ==============================

# View first rows
head(data)

# Check structure (data types)
str(data)

# Summary statistics
summary(data)

# Check column names
names(data)
# Check dimensions
dim(data)

# ==============================
# 3. CHECK MISSING VALUES
# ==============================

# Total missing values per column
colSums(is.na(data))

# ==============================
# 4. REMOVE DUPLICATES
# ==============================

data <- data[!duplicated(data), ]

# ==============================
# 5. CLEAN COLUMN NAMES (optional but useful)
# ==============================

# install.packages("janitor")
library(janitor)
data <- clean_names(data)

# ==============================
# 6. HANDLE MISSING VALUES (basic approach)
# ==============================

# Option A: remove rows with missing values
data_clean <- na.omit(data)


# ==============================
# 7. FINAL CHECK
# ==============================

str(data_clean)
summary(data_clean)

# Check for factor variables
sapply(data_clean, class)

# Convert character to factor if needed
data_clean[] <- lapply(data_clean, function(x) {
  if (is.character(x)) as.factor(x) else x
})
# Save cleand data
write.csv(data_clean, 
          "D:/Digital health project/CVD_cleaned_dataset.csv", 
          row.names = FALSE)
