Sys.setenv(LANG = "en")
options(scipen = 5)

# install.packages("BaylorEdPsych")
# install.packages("htmltools")
# install.packages("LogisticDx")
# install.packages("logistf")

library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf") #Firth's bias reduction method
#install.packages("ResourceSelection")
library(ResourceSelection)


load("data/data_full.RData")

# probit model estimation
probit_model <- glm(Diabetes ~ 		HighBP + HighChol + BMI + Smoker
                +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
                +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
                +	PhysHlth + Sex
                +  elementary + high_school_graduate + college + college_graduate
                + A35_39 + A40_44 + A45_49 + A50_54
                +	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
                + I20K + I25K +	I35K + I50K + I75K + I75K_more
                , data=df, 
                family=binomial(link="probit"))
summary(probit_model)

# logit model estimation
logit_model <- glm(Diabetes ~ 		HighBP + HighChol + BMI + Smoker
               +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
               +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
               +	PhysHlth + Sex
               +  elementary + high_school_graduate + college + college_graduate
               + A35_39 + A40_44 + A45_49 + A50_54
               +	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
               + I20K + I25K +	I35K + I50K + I75K + I75K_more
               , data=df, family=binomial(link = "logit"))
summary(logit_model)

cat("AIC and BIC for logit model:", AIC(logit_model),",", BIC(logit_model), "\n")
cat("AIC and BIC for probit model:", AIC(probit_model),",", BIC(probit_model), "\n")
cat(" for logit model:", BIC(logit_model), "\n")
cat("BIC for probit model:", BIC(probit_model), "\n")

# Joint insignificance of all variables test
null_probit = glm(Diabetes~1, data=df[,1:19], family=binomial(link="probit"))
lrtest(probit_model, null_probit)


#--> conclusion to paper: 
#When comparing these two models, the function ratio statistic (Chisq) is 46337 with a very high significance level (p-value < 2.2e-16). 
#This shows that adding independent variables to the model is meaningful and model 2 explains the data better than model 1.

#This can be interpreted as the independent variables added to the model have a significant effect on the dependent variable 
#and increase the explanatory power of the model.

# marginal effects for the average observation
(meff = probitmfx(formula=Diabetes ~ 		HighBP + HighChol + BMI + Smoker
                +	Stroke + HeartDiseaseorAttack + PhysActivity + Veggies
                +	HvyAlcoholConsump + AnyHealthcare + GenHlth + MentHlth
                +	PhysHlth + Sex
                +  elementary + high_school_graduate + college + college_graduate
                + A35_39 + A40_44 + A45_49 + A50_54
                +	A55_59 + A60_64 + A65_69 + A70_74 + A75_79 +	A80_older 
                + I20K + I25K +	I35K + I50K + I75K + I75K_more
                , data=df, , atmean=TRUE))

# ------
# R-squared statistics
library(DescTools)
PseudoR2(probit_model,c("McFadden","Tjur","McKelveyZavoina","VeallZimmermann","Nagelkerke"))


# Linktest
source("functions/linktest.R")
linktest_result = linktest(probit_model)
summary(linktest_result)


# Perform the Hosmer-Lemeshow test

#hoslem <- hoslem.test(observed = df$Diabetes,
#                      fitted = fitted(probit_model),
#                      g = 10) 
#summary(hoslem)

#gof.results = gof(probit_model)
#gof.results$gof
#------------------------------------------------



