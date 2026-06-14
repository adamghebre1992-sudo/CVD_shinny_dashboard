# Data Visualization 
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("reshape2")

library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# Load your cleaned data
clean_data <- read_csv("CVD_cleaned_dataset.csv")

# Quick look at the data
head(clean_data)
str(clean_data)
summary(clean_data)

# ==============================
table(data_clean$cvd_risk_level)


# Frequency table
cvd_table <- table(data_clean$cvd_risk_level)
cvd_table

# Percentage distribution
cvd_percent <- prop.table(cvd_table) * 100
round(cvd_percent, 2)

# Combined summary table
cvd_summary <- data.frame(
  Risk_Level = names(cvd_table),
  Count = as.vector(cvd_table),
  Percentage = round(as.vector(cvd_percent), 2)
)

cvd_summary

#CVD Risk Association: Visualization Template

# ==============================
# 1. LOAD CLEAN DATA
# ==============================
data <- read.csv("D:/Digital health project/CVD_cleaned_dataset.csv")

library(ggplot2)

# ==============================
# 2. SET OUTCOME AS FACTOR
# ==============================
data$cvd_risk_level <- as.factor(data$cvd_risk_level)

# ==============================
# 3. AGE vs CVD RISK (MOST IMPORTANT)
# ==============================

ggplot(data, aes(x = cvd_risk_level, y = age, fill = cvd_risk_level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Age Distribution Across CVD Risk Levels",
       x = "CVD Risk Level",
       y = "Age")

# ==============================
# 4. BLOOD PRESSURE vs CVD RISK
# ==============================

ggplot(data, aes(x = cvd_risk_level, y = systolic_bp, fill = cvd_risk_level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Systolic BP Across CVD Risk Levels",
       x = "CVD Risk Level",
       y = "Systolic Blood Pressure")

# ==============================
# 5. CHOLESTEROL vs CVD RISK
# ==============================

ggplot(data, aes(x = cvd_risk_level, y = total_cholesterol_mg_d_l, fill = cvd_risk_level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Total Cholesterol Levels Across CVD Risk Levels",
       x = "CVD Risk Level",
       y = "Total Cholesterol")

# ==============================
# 6. GLUCOSE vs CVD RISK
# ==============================

ggplot(data, aes(x = cvd_risk_level, y = fasting_blood_sugar_mg_d_l, fill = cvd_risk_level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Fasting Blood sugar Levels Across CVD Risk Levels",
       x = "CVD Risk Level",
       y = "Fasting Blood Sugar ")

# ==============================
# 7. SMOKING vs CVD RISK
# ==============================

ggplot(data, aes(x = smoking_status, fill = cvd_risk_level)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Smoking Status Distribution by CVD Risk Level",
       x = "Smoking Status",
       y = "Proportion")

# ==============================
# 8. SEX vs CVD RISK
# ==============================

ggplot(data, aes(x = sex, fill = cvd_risk_level)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Sex Distribution by CVD Risk Level",
       x = "Sex",
       y = "Proportion")

# ==============================
# 9. BMI vs CVD RISK
# ==============================
ggplot(data, aes(x = cvd_risk_level, y = bmi, fill = cvd_risk_level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "BMI Across CVD Risk Levels",
       x = "CVD Risk Level",
       y = "BMI")
# ==============================
# 10. PHYSICAL ACTIVITY vs CVD RISK
# ==============================

ggplot(data, aes(x = physical_activity_level, fill = cvd_risk_level)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Physical Activity Level vs CVD Risk Level",
    x = "Physical Activity Level",
    y = "Proportion of Patients",
    fill = "CVD Risk Level"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ==============================
# 11. BLOOD PRESSURE CATEGORY vs CVD RISK
# ==============================

ggplot(data, aes(x = blood_pressure_category, fill = cvd_risk_level)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Blood Pressure Category vs CVD Risk Level",
    x = "Blood Pressure Category",
    y = "Proportion of Patients",
    fill = "CVD Risk Level"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ==============================
# 12. DIABETES vs CVD RISK 
# ==============================

ggplot(data, aes(x = diabetes_status, fill = cvd_risk_level)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Diabetes Status vs CVD Risk Level",
    x = "Diabetes Status",
    y = "Proportion of Patients",
    fill = "CVD Risk Level"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



