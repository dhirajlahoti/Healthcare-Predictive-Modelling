install.packages("nnet")
library(nnet)

my_data <- read.csv("C:/Users/dhira/OneDrive/Desktop/filtered_data.csv")
categorical_columns <- c("Diabetes", "Sex", "Age", "COVIDPOS", "Smoker", "Physical_activity", "Food_shortage", "MEDCOST1", "General_health", "Income_category", "Stroke", "Walking_difficulty", "HeartDiseaseorAttack")

#specified columns to categorical
my_data[categorical_columns] <- lapply(my_data[categorical_columns], as.factor)


set.seed(123)  # For reproducibility
sample_indices <- sample(1:nrow(my_data), 0.8 * nrow(my_data))  # 70% train, 30% test
train_data <- my_data[sample_indices, ]
test_data <- my_data[-sample_indices, ]

model <- multinom(Diabetes ~ ., data = train_data)
summary(model)

#Wald tests for significance
wald_test <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(wald_test)))
print(p_values)



# Coefficient values for predictor variables (Category 1)
coefficients_category1_vs_reference <- c(
  Sex1 = 0.1455622,
  Age2 = 0.04369554,
  Age3 = 0.2495164,
  Age4 = 0.4485056,
  Age5 = 0.588316,
  Age6 = 1.102796,
  Age7 = 1.209242,
  Age8 = 1.264853,
  Age9 = 1.432004,
  Age10 = 1.406094,
  Age11 = 1.556476,
  Age12 = 1.523066,
  Age13 = 1.485228,
  BMI = 0.06464372,
  COVIDPOS1 = 0.0435261,
  COVIDPOS3 = 0.2332161,
  Smoker1 = -0.11807522,
  Physical_activity1 = 0.001495306,
  Food_shortage1 = 0.20604072,
  Food_shortage3 = 0.45621038,
  Food_shortage4 = 0.2340694,
  Food_shortage5 = 0.1001708,
  Average_drink = 0.0002236229,
  MEDCOST11 = 0.1141904,
  General_health2 = 0.4326584,
  General_health3 = 0.8448622,
  General_health4 = 1.011853,
  General_health5 = 0.9775213,
  Income_category2 = 0.01006929,
  Income_category3 = -0.06978511,
  Income_category4 = -0.2017966,
  Income_category5 = -0.09065265,
  Income_category6 = -0.14697270,
  Income_category7 = -0.39874309,
  Income_category8 = -0.54929835,
  Income_category9 = -0.61309778,
  Income_category10 = -0.73147929,
  Income_category11 = -0.9150418,
  Stroke1 = -0.02014588,
  Walking_difficulty1 = 0.1993777,
  HeartDiseaseorAttack1 = 0.2215800,
  Alcohol_consumed = -0.01368659
)

# Coefficient values for predictor variables (Category 2) 
coefficients_category2_vs_reference <- c(
  Sex1 = 0.5135587,
  Age2 = 0.21720720,
  Age3 = 0.5623134,
  Age4 = 0.9501481,
  Age5 = 1.368684,
  Age6 = 1.860338,
  Age7 = 2.110938,
  Age8 = 2.431134,
  Age9 = 2.549917,
  Age10 = 2.711244,
  Age11 = 2.857342,
  Age12 = 2.942166,
  Age13 = 2.735114,
  BMI = 0.07081753,
  COVIDPOS1 = 0.1179766,
  COVIDPOS3 = -0.1084739,
  Smoker1 = 0.04010873,
  Physical_activity1 = -0.170002056,
  Food_shortage1 = -0.09827026,
  Food_shortage3 = -0.06189959,
  Food_shortage4 = -0.1708714,
  Food_shortage5 = -0.3014448,
  Average_drink = 0.0010644802,
  MEDCOST11 = -0.1062249,
  General_health2 = 0.8197880,
  General_health3 = 1.5401960,
  General_health4 = 1.939174,
  General_health5 = 2.0785582,
  Income_category2 = -0.08556109,
  Income_category3 = -0.06643024,
  Income_category4 = 0.0354277,
  Income_category5 = 0.02844706,
  Income_category6 = -0.00502135,
  Income_category7 = -0.07446621,
  Income_category8 = -0.07239704,
  Income_category9 = -0.06128878,
  Income_category10 = -0.04058695,
  Income_category11 = -0.2288525,
  Stroke1 = 0.25634789,
  Walking_difficulty1 = 0.1122935,
  HeartDiseaseorAttack1 = 0.4417612,
  Alcohol_consumed = -0.03468563
)


log_odds <- coef(model)
log_odds


odds_ratios <- exp(log_odds)
odds_ratios


# most significant


# vector of p-values for Category 1
p_values_category1 <- c(
  Sex1 = 0.006424617,
  Age2 = 0.8573147,
  Age3 = 0.27432932,
  Age4 = 3.939556e-02,
  Age5 = 6.008582e-03,
  Age6 = 7.831794e-08,
  Age7 = 2.069732e-09,
  Age8 = 3.244887e-10,
  Age9 = 3.901324e-13,
  Age10 = 1.437517e-12,
  Age11 = 6.661338e-15,
  Age12 = 2.73559e-13,
  Age13 = 3.378631e-12,
  BMI = 0,
  COVIDPOS1 = 4.614915e-01,
  COVIDPOS3 = 0.06394489,
  Smoker1 = 0.02941822,
  Physical_activity1 = 9.816119e-01,
  Food_shortage1 = 0.4778913,
  Food_shortage3 = 0.05344686,
  Food_shortage4 = 0.3209425,
  Food_shortage5 = 0.658975998,
  Average_drink = 0.9272068,
  MEDCOST11 = 0.2282108,
  General_health2 = 2.749548e-05,
  General_health3 = 2.220446e-16,
  General_health4 = 0,
  General_health5 = 9.402474e-09,
  Income_category2 = 0.9621908,
  Income_category3 = 0.7321227,
  Income_category4 = 0.3007129,
  Income_category5 = 0.6074096,
  Income_category6 = 0.4034773,
  Income_category7 = 0.02311896,
  Income_category8 = 0.002293702,
  Income_category9 = 0.0007167304,
  Income_category10 = 0.0002851345,
  Income_category11 = 9.199221e-06,
  Stroke1 = 8.792405e-01,
  Walking_difficulty1 = 0.008687915,
  HeartDiseaseorAttack1 = 0.01076856,
  Alcohol_consumed = 4.467814e-06
)


# vector of p-values for Category 2
p_values_category2 <- c(
  Sex1 = 0.000000000,
  Age2 = 0.2692348,
  Age3 = 0.00192008,
  Age4 = 2.773331e-08,
  Age5 = 2.220446e-16,
  Age6 = 0.000000e+00,
  Age7 = 0.000000e+00,
  Age8 = 0.000000e+00,
  Age9 = 0.000000e+00,
  Age10 = 0.000000e+00,
  Age11 = 0.000000e+00,
  Age12 = 0.00000e+00,
  Age13 = 0.000000e+00,
  BMI = 0,
  COVIDPOS1 = 6.907282e-05,
  COVIDPOS3 = 0.13969274,
  Smoker1 = 0.13971985,
  Physical_activity1 = 7.823468e-08,
  Food_shortage1 = 0.4997725,
  Food_shortage3 = 0.59935348,
  Food_shortage4 = 0.1401623,
  Food_shortage5 = 0.006061061,
  Average_drink = 0.3903356,
  MEDCOST11 = 0.0558602,
  General_health2 = 0.000000e+00,
  General_health3 = 0.000000e+00,
  General_health4 = 0,
  General_health5 = 0.000000e+00,
  Income_category2 = 0.4831430,
  Income_category3 = 0.5687604,
  Income_category4 = 0.7447907,
  Income_category5 = 0.7791327,
  Income_category6 = 0.9603855,
  Income_category7 = 0.45684673,
  Income_category8 = 0.475667739,
  Income_category9 = 0.5474979776,
  Income_category10 = 0.7098139594,
  Income_category11 = 3.899406e-02,
  Stroke1 = 7.376336e-06,
  Walking_difficulty1 = 0.002704062,
  HeartDiseaseorAttack1 = 0.00000000,
  Alcohol_consumed = 0.000000e+00
)









# most predictive features- category 1


# data frame to combine predictor names, coefficients, and p-values
data <- data.frame(
  Predictor = names(coefficients_category1_vs_reference),
  Coefficient = coefficients_category1_vs_reference,
  P_Value = p_values_category1
)

# most predictive feature based on a significance threshold ( 0.05)
significant_feature <- data[data$P_Value < 0.05, ]

# Sort - data frame by absolute coefficient values in descending order
significant_feature <- significant_feature[order(abs(significant_feature$Coefficient), decreasing = TRUE), ]

# Print - most predictive feature
print(significant_feature[1, ])

# category 2


#  data frame to combine predictor names, coefficients, and p-values
data_category2 <- data.frame(
  Predictor = names(coefficients_category2_vs_reference),
  Coefficient = coefficients_category2_vs_reference,
  P_Value = p_values_category2
)

#  the most predictive feature based on a significance threshold (0.05)
significant_feature_category2 <- data_category2[data_category2$P_Value < 0.05, ]

# Sort- data frame by absolute coefficient values in descending order
significant_feature_category2 <- significant_feature_category2[order(abs(significant_feature_category2$Coefficient), decreasing = TRUE), ]

# Print - the most predictive feature for Category 2
print(significant_feature_category2[1, ])





# Predicting the class labels for the test data
predicted_classes <- predict(model, newdata = test_data, "class")

# Calculate accuracy
correct_predictions <- sum(predicted_classes == test_data$Diabetes)
total_predictions <- length(predicted_classes)
accuracy <- correct_predictions / total_predictions
cat("Accuracy:",accuracy,"\n")


