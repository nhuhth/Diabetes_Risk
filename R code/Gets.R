# Start: general model
# a) test all insignificant variables jointly
#     i) yes - then get rid of them at once
#     ii) no - then apply step-by-step approach

# Stop: final model without insignificant variables

# Specific-to-General Approach

if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(car)) {
  install.packages("car")
}
library("car")
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caret)
library(glmnet)
library(MASS)

Sys.setenv(LANG = "en")
setwd("~/Desktop/AI/Health_project/")
load("data/data_full.RData")

glimpse(df)

# Ensure that Diabetes is converted to numeric if it is a factor
df$Diabetes <- as.numeric(as.character(df$Diabetes))

# Assuming df is the dataset you want to copy
health_data <- df

# Inspect the copied dataset
str(health_data)   # Check structure
head(health_data)  # View first few rows
summary(health_data) # Get summary statistics

# plot diabetes columns' distribution with differnet colours for different values of diabetes
ggplot(health_data, aes(x=Diabetes, fill=Diabetes)) + 
  geom_bar() + 
  theme_minimal() + 
  labs(title="Diabetes Distribution", x="Diabetes", y="Count") + 
  scale_fill_manual(values=c("red", "blue"))

# show the distribution of diabetes counts
table(health_data$Diabetes)

# show the distribution of diabetes counts in percentage
prop.table(table(health_data$Diabetes)) * 100

# show the column names
colnames(health_data[1:19])
# print columns line by line without ""
for (col in colnames(health_data[1:19])) {
  print(col)
}

# correlation matrix with plot, target variable Diabetes
# Select only numerical columns for the correlation matrix, excluding the target variable
health_ori <- health_data[1:19] 
numeric_cols <- sapply(health_ori , is.numeric)
correlation_matrix <- cor(health_ori [, numeric_cols], use = "complete.obs")
correlation_matrix

# Visualize the correlation matrix
corrplot::corrplot(correlation_matrix, method = "color", tl.col = "black", tl.cex = 0.8,)

#---> not high correlation



#------ method -------

# Step 1
# general model

# List the column names to ensure correct referencing
colnames(health_data) 
## created variables for factor level before
# # Model with selected general variables
general<- lm(Diabetes ~ ., data = health_data[, !(names(df) %in% c("Education","Age", "Income"))])
summary(general)

# Model with all variables
model1 <- lm(Diabetes ~ ., data = health_data[, 1:19])
summary(model1)

#--> several vars not significant


# Model with individually specified variables based on general)
model1a <- lm(Diabetes ~ HighBP + HighChol + BMI + Smoker + Stroke 
              + HeartDiseaseorAttack + PhysActivity + Veggies + HvyAlcoholConsump 
              + AnyHealthcare + GenHlth + MentHlth + PhysHlth + Sex +A40_44+ A45_49 + A50_54 + A55_59 + A65_69 
              + A70_74 + A75_79 + A80_older +I20K + I25K + I35K + I50K + I75K + I75K_more, data = health_data)

summary(model1a)
# Perform ANOVA to test nested models
anova(model1, model1a)


# H0: they are jointly insignificant
# all insignificant variables are jointly not significant
# therefore we have to drop variables in the way one after another
# let's drop "the most insignificant" variable from model1
# that is high_school

# Step 2
# Tạo mô hình glm
selected_columns <- c(1:22, "edu_no", "edu_kinder", "eduhschool_gra", "educollege_ungra", "educollege_gra")

model2 <- lm(Diabetes ~ 		HighBP + HighChol + BMI + Smoker
             +	Stroke + HeartDiseaseorAttack + PhysActivity + Fruits + Veggies
             +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
             +	PhysHlth + Sex + Age +	Income
             +  elementary + high_school_graduate + college + college_graduate
             #+ A25_29 +	A30_34 + A35_39 + A40_44 + A45_49 + A50_54
             #+	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
             #+ I15K + I20K + I25K +	I35K + I50K + I75K
              ,data = health_data)

summary(model2)

# # let's drop "the most insignificant" variable from model2 that is Fruits
# # and test joint hypothesis: beta_high_school=beta_Fruits=0

linearHypothesis(general, c("high_school=0", "Fruits=0"))

## --> p -value = 0.994  -> continue to remove


model3 = lm(Diabetes~ HighBP + HighChol + BMI + Smoker
                      +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
                      +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
                      +	PhysHlth + Sex + Age +	Income
                      +  elementary + high_school_graduate + college + college_graduate
                      ,data = health_data)
summary(model3)

# # we would like to drop I15K from model3
# # to do so, we have to verify joint hypothesis that
# # and test joint hypothesis: beta_high_school=beta_Fruits= beta_I15K=0
linearHypothesis(general, c("high_school=0", "Fruits=0","I15K=0"))

# p-value = 0.9708 > 5% -> continue


# # Step 4
model4 <- lm(Diabetes ~ 		HighBP + HighChol + BMI + Smoker
             +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
             +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
             +	PhysHlth + Sex + Age
             +  elementary + high_school_graduate + college + college_graduate
             #+ A25_29 +	A30_34 + A35_39 + A40_44 + A45_49 + A50_54
             #+	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
              + I20K + I25K +	I35K + I50K + I75K +I75K_more
             ,data = health_data)
summary(model4)
# # we would like to drop Age25_29 from model4
# # to do so, we have to verify joint hypothesis that
# # and test joint hypothesis: beta_high_school=beta_Fruits= beta_I15K= beta_Age25_29=0
linearHypothesis(general, c("high_school=0", "Fruits=0","I15K=0", "A25_29=0"))


# p =0.9704, we cannot reject the null, so Age25_29 might be dropped from model4 model

# Step 5
model5 <- lm(Diabetes ~ 		HighBP + HighChol + BMI + Smoker
             +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
             +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
             +	PhysHlth + Sex
             +  elementary + high_school_graduate + college + college_graduate
             +	A30_34 + A35_39 + A40_44 + A45_49 + A50_54
             +	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
             + I20K + I25K +	I35K + I50K + I75K + I75K_more
             ,data = health_data)

summary(model5)

# we would like to drop A30_34  from model5
# to do so, we have to verify joint hypothesis that
# # and test joint hypothesis: beta_high_school=beta_Fruits= beta_I15K= beta_A35_39= beta_A30_34=0
linearHypothesis(general, c("high_school=0", "Fruits=0","I15K=0", "A35_39=0","A30_34=0"))

# p =0.2024 we reject the null,  we cannot reject the null, so A30_34 might be dropped from model5 model

# Step 6

model6 <- lm(Diabetes ~ 		HighBP + HighChol + BMI + Smoker
             +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
             +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
             +	PhysHlth + Sex
             +  elementary + high_school_graduate + college + college_graduate
             + A35_39 + A40_44 + A45_49 + A50_54
             +	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
             + I20K + I25K +	I35K + I50K + I75K + I75K_more
             ,data = health_data)


summary(model6)

# All variables are significant in this step, so
# this ends general-to-specific procedure.

library("stargazer")
stargazer(general, model6, type="text")



