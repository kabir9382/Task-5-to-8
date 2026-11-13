diabetes_original <- read.csv("data/diabetes_dataset.csv", header = TRUE, sep = ",")

nrow(diabetes_original)
ncol(diabetes_original)

str(diabetes_original)
head(diabetes_original[, 1:8])

summary(diabetes_original[, c("age", "bmi", "glucose_fasting", "hba1c", "insulin_level")])
summary(factor(diabetes_original$diagnosed_diabetes))
summary(factor(diabetes_original$diagnosed_diabetes)) / nrow(diabetes_original) * 100
summary(factor(diabetes_original$diabetes_stage))

sum(is.na(diabetes_original))
nrow(na.omit(diabetes_original))

sum(diabetes_original$age < 18 | diabetes_original$age > 100)
sum(diabetes_original$sleep_hours_per_day < 0 | diabetes_original$sleep_hours_per_day > 24)

n <- nrow(diabetes_original)
modified <- diabetes_original

modified$insulin_level[seq(20, n, by = 20)] <- NA
modified$bmi[seq(3, n, by = 33)] <- NA
modified$smoking_status[seq(11, n, by = 34)] <- NA

modified$age[seq(500, n, by = 2500)] <- -5
modified$sleep_hours_per_day[seq(1000, n, by = 5000)] <- 30
modified$diastolic_bp[seq(300, n, by = 3333)] <- 200

modified$cholesterol_total[seq(250, n, by = 1667)] <- 700
modified$triglycerides[seq(400, n, by = 2500)] <- 1000

modified <- modified[c(1:n, 1:200), ]

diabetes <- modified
nrow(diabetes)
ncol(diabetes)
colSums(is.na(diabetes))
sum(diabetes$age < 18 | diabetes$age > 100)
sum(diabetes$sleep_hours_per_day > 24)
sum(diabetes$systolic_bp < diabetes$diastolic_bp)
max(diabetes$cholesterol_total)
max(diabetes$triglycerides)


colSums(is.na(diabetes)) / nrow(diabetes) * 100
nrow(diabetes) - nrow(na.omit(diabetes))

afterDeletedDataset <- na.omit(diabetes)
nrow(afterDeletedDataset)
ncol(afterDeletedDataset)
(nrow(diabetes) - nrow(afterDeletedDataset)) / nrow(diabetes) * 100
sum(is.na(afterDeletedDataset))

summary(diabetes$insulin_level)
insulin_mean <- diabetes$insulin_level
insulin_mean[is.na(insulin_mean)] <- mean(diabetes$insulin_level, na.rm = TRUE)
sum(is.na(insulin_mean))
summary(insulin_mean)

summary(diabetes$bmi)
bmi_mean <- diabetes$bmi
bmi_mean[is.na(bmi_mean)] <- mean(diabetes$bmi, na.rm = TRUE)
sum(is.na(bmi_mean))
summary(bmi_mean)

summary(diabetes$insulin_level)
insulin_median <- diabetes$insulin_level
insulin_median[is.na(insulin_median)] <- median(diabetes$insulin_level, na.rm = TRUE)
sum(is.na(insulin_median))
summary(insulin_median)


smoking_counts <- table(diabetes$smoking_status)
smoking_counts
smoking_mode <- names(which.max(smoking_counts))
smoking_mode
smoking_filled <- diabetes$smoking_status
smoking_filled[is.na(smoking_filled)] <- smoking_mode
sum(is.na(smoking_filled))
table(smoking_filled)


cleaned_dataset <- diabetes
cleaned_dataset$insulin_level <- insulin_median
cleaned_dataset$bmi <- bmi_mean
cleaned_dataset$smoking_status <- smoking_filled
sum(is.na(cleaned_dataset))
nrow(cleaned_dataset)
ncol(cleaned_dataset)


