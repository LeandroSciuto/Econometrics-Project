# Take home exam Econometria

rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

library(dplyr)  
library(tidyverse)
library(readr)
library(data.table)

#install.packages("questionr")
library(questionr)  # for odds.ratios
## For plotting results of regression.
#install.packages("sjPlot")
library(sjPlot)     
#install.packages("sjmisc")
library(sjmisc)     
#install.packages("effects")
library(effects)    # for probability output and plots




# Logistic regression
#install.packages("MASS")  # Install the MASS package
library(MASS)             # Load the MASS package


### Reading of the dataset
# Disclaimer: it's named df2 and not df because it was the second model I built 
# and as it became the final one, I decided not to change the names out of lazyness. I apologize.
df2 <- read.csv("lfp.csv")
str(df2)


# Correlation analysis
library(DataExplorer)
plot_correlation(df2)

df2 <- df2[,-c(1,3,5,6,9,10,11,12,13,15,16,17,18,25)] # delete of the IDs variable 
                                                      # with too many NAs and high correlation 
                                                      # and redundacies

# Categorical variables
df2$SEX <- as.factor(df2$SEX)
df2$LFP <- as.factor(df2$LFP)
df2$STACIV <- as.factor(df2$STACIV)
df2$ETA <- as.numeric(df2$ETA)

full_model2 <- glm(LFP ~ ., data = df2, family = binomial)
summary(full_model2)


step.model2 <- full_model2 %>% stepAIC(trace = FALSE)
summary(step.model2)


tab_model(step.model2, p.style = "numeric_stars", transform = NULL)

# Final model without redundancies
final_step_model <- glm(formula = LFP ~ NCOMP + SEX + STACIV + STUDIO + PERC + PERL + 
                          NPERL + AREA5 + ACOM4C, family = binomial, data = df2)

summary(final_step_model)

tab_model(final_step_model, p.style = "numeric_stars", transform = NULL)

allEffects(final_step_model) %>% summary()

plot(allEffects(final_step_model))


plot_correlation(df2)



## Split dataset in male and female
male_data <- df2 %>%
  filter(SEX == 1)

female_data <- df2 %>%
  filter(SEX == 2)

## Delete of the variable SEX in both datasets
male_data <- male_data[,-c(2)]

female_data <- female_data[,-c(2)]


### Male glm
male_model <- glm(LFP ~ ., data = male_data, family = binomial)
summary(male_model)

step.male_model <- male_model %>% stepAIC(trace = FALSE)
summary(step.male_model)

tab_model(step.male_model, p.style = "numeric_stars", transform = NULL)

### Female glm
female_model <- glm(LFP ~ ., data = female_data, family = binomial)
summary(female_model)

step.female_model <- female_model %>% stepAIC(trace = FALSE)
summary(step.female_model)

tab_model(step.female_model, p.style = "numeric_stars", transform = NULL)

## Interaction between SEX and ETA

interaction_model <- glm(formula = LFP ~ . + ETA*SEX + STACIV*SEX + STUDIO*SEX + PERC*SEX
                         + PERL*SEX + AREA5*SEX + ACOM4C*SEX, 
                         family = binomial, data = df2)

tab_model(interaction_model, p.style = "numeric_stars", transform = NULL)

summary(interaction_model)

## Stepwise interaction model
step.interaction_model <- interaction_model %>% stepAIC(trace = FALSE)

tab_model(step.interaction_model, p.style = "numeric_stars", transform = NULL)

summary(step.interaction_model)











