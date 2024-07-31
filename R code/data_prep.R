# lets install and load needed packages

#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
# lets import the data

df <- read.csv("data/diabetes_012_health_indicators_BRFSS2015.csv")

#1. Summary data
glimpse(df)
summary(df)

#NA
colSums(is.na(df)) %>% 
  sort() #--> no NA

sapply(df[, 1:ncol(df)], 
       function(x) 
         unique(x) %>% 
         length()) %>% 
  sort()


#3.  convert Target Variable 
table(df$Diabetes_012)

# lets check the distribution (histogram) 
# of the dependent variable Diabetes_012

ggplot(df,
       aes(x = Diabetes_012)) +
  geom_histogram(fill = "blue",
                 bins = 100) +
  theme_bw()



#For binary classification of diabetes versus no diabetes, prediabetic individuals can be classified in several ways: 
#by combining them with diabetics, with non-diabetics, or excluding them altogether. 
#To focus on diabetes risk, we'll include prediabetics in the diabetic group, 
#as a prediabetes diagnosis indicates a higher risk of developing diabetes.

df <- df %>%
  mutate(Diabetes = case_when(
    Diabetes_012 == 0 ~ 0,
    Diabetes_012 %in% c(1, 2) ~ 1
  ))

# Drop the Diabetes_012 column
df$Diabetes_012 <- NULL

table(df$Diabetes)

#based on literature review, --> remove with variables not use
df$NoDocbcCost<- NULL
df$CholCheck<- NULL
df$DiffWalk<- NULL

#2. convert to factor to column
# see variabels: https://www.cdc.gov/brfss/annual_data/2014/pdf/CODEBOOK14_LLCP.pdf
# see codebook  



table(df$GenHlth)
table(df$Age)
table(df$Education)
table(df$Income)

plot_age <- ggplot(df, aes(x = Age)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Bar Plot of Age", x = "Age", y = "Count") +
  theme_minimal()

# Bar plot for GenHlth
plot_genhlth <- ggplot(df, aes(x = GenHlth)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Plot of General Health", x = "General Health", y = "Count") +
  theme_minimal()

# Bar plot for Education
plot_education <- ggplot(df, aes(x = Education)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Bar Plot of Education", x = "Education", y = "Count") +
  theme_minimal()

# Bar plot for Income
plot_income <- ggplot(df, aes(x = Income)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Bar Plot of Income", x = "Income", y = "Count") +
  theme_minimal()

# Arrange the plots in a 2x2 grid
grid.arrange(plot_age, plot_genhlth, plot_education, plot_income, nrow = 2, ncol = 2)

## combine data:

#Education

education_labels <- c("never_or_kinder",
                      "elementary",                      
                      "high_school",
                      "high_school_graduate",
                      "college", "college_graduate")

for (i in 2:length(education_labels)) {
  edu_range <- education_labels[i]
  df[[edu_range]] <- ifelse(df$Education %in% c(i), 1, 0)}

df$Education <- factor(df$Education, levels = 1:6, labels = education_labels)



## Age(_AGEG5YR as codebook):
age_labels <- c("18_24", "25_29", "30_34", "35_39", "40_44", "45_49",
                "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_older")


for (i in 2:length(age_labels)) {
  age_range <- age_labels[i]
  df[[paste0("A", age_range)]] <- ifelse(df$Age %in% c(i), 1, 0)
}


df$Age <- factor(df$Age, levels = 1:13, labels = age_labels)


#Income
income_labels <- c("I10K","I15K","I20K",
                   "I25K","I35K","I50K",
                   "I75K","I75K_more")

for (i in 2:length(income_labels)) {
  income_range <- income_labels[i]
  df[[income_range]] <- ifelse(df$Income %in% c(i), 1, 0)
}

df$Income <- factor(df$Income, levels = 1:8, labels = income_labels)



summary(df)




save(list = "df",
     file = "data/data_full.RData")


write.csv(df, file = "data/data_full.csv", row.names = FALSE)
