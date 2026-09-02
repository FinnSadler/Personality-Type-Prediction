#Load libraries
library(tidyverse) #For data cleaning and manipulation
library(stargazer) #For regression tables
library(naniar) #To visualise missing values
library(mice) #For imputation
library(pscl) #For pseudo R2

set.seed(1234) #For reproducibility

#Load the dataset
IntroExtro <- read.csv("D:\\Datasets\\IntrovertExtrovertDatasetTrain.csv")
tibble(IntroExtro)

#Find NAs
unique(IntroExtro$Stage_fear)
unique(IntroExtro$Drained_after_socializing)

#Convert blank values to NAs
IntroExtro$Stage_fear <- case_when(IntroExtro$Stage_fear == "" ~ NA,
                                   TRUE ~ IntroExtro$Stage_fear)
IntroExtro$Drained_after_socializing <- case_when(IntroExtro$Drained_after_socializing == "" ~ NA,
                                                  TRUE ~ IntroExtro$Drained_after_socializing)

vis_miss(IntroExtro)
colSums(is.na(IntroExtro))

#Convert to binary variables factors for glm() and mice
IntroExtro$Stage_fear <- as.factor(IntroExtro$Stage_fear)
IntroExtro$Drained_after_socializing <- as.factor(IntroExtro$Drained_after_socializing)

#Check if missingness is random or patterned
models <- list() #Create list to store regression models

for (i in colnames(IntroExtro)) { #Loop through columns in the data
  if (sum(is.na(IntroExtro[[i]])) > 0) { #Check if there are NAs in the given column
    models[[i]] <- glm(formula = as.formula(paste('is.na(', i, ') ~ .')), family = "binomial", data = IntroExtro) #If NAs are present, build a regression model to determine if the NAs are predictable
  } 
}

for (i in models) { #Loop through models
  print(summary(i)) #Print summaries to check p-values
}

#Impute missing values
ImputedIntroExtro <- mice(IntroExtro, m = 5) #Use multiple imputation by chained equations (MICE) regression to impute the data
CompletedIntroExtro <- complete(ImputedIntroExtro, 1) #Convert the MICE object into a data frame


#Inspect feature distribution
plotter <- function(i){ #Define a function that build plots for each variable
  if (is.numeric(i) == TRUE){ #Check if the variable is numeric
    ggplot(data = CompletedIntroExtro, mapping = aes(x = .data[[i]])) + geom_histogram(aes(fill = Personality)) #If it is numeric, build a histogram
  } else if (is.character(i) == TRUE){ #Check if the variable is a character
    ggplot(data = CompletedIntroExtro, mapping = aes(x = .data[[i]])) + geom_bar(aes(fill = Personality)) #If it is a character, build a bar plot
  }
}

plots <- list() #Create a list to store the plots that plotter() builds

for (i in colnames(CompletedIntroExtro)) { #Loop through the columns
  plots[[i]] <- plotter(i) #Run plotter() on the given varsiable and save the plot to the list so it can be retrieved
}

#Inspect some plots
plots$Social_event_attendance
plots$Stage_fear

#Read in test data from Kaggle
KaggleTest <- read.csv("C:\\Users\\shark\\Downloads\\IntrovertExtrovertDatasetTest.csv")

KaggleTest$Stage_fear <- case_when(KaggleTest$Stage_fear == "" ~ NA,
                                   TRUE ~ KaggleTest$Stage_fear)
KaggleTest$Drained_after_socializing <- case_when(KaggleTest$Drained_after_socializing == "" ~ NA,
                                                  TRUE ~ KaggleTest$Drained_after_socializing)

#Convert to factors so mice can use logistic regression to impute
KaggleTest$Stage_fear <- as.factor(KaggleTest$Stage_fear)
KaggleTest$Drained_after_socializing <- as.factor(KaggleTest$Drained_after_socializing)

#Impute missing values
ImputedKaggleTest <- mice(KaggleTest, m = 5)
CompletedKaggleTest <- complete(ImputedKaggleTest, 1)

#Split training and testing data
CompletedIntroExtro$Personality <- case_when(CompletedIntroExtro$Personality == "Introvert" ~ 0,
                               CompletedIntroExtro$Personality == "Extrovert" ~ 1) #Convert target variable to numeric so LR can parse i
Train <- sample_frac(CompletedIntroExtro, 0.7) #Dedicate 70% of the data for training and 30% for testing
Test <- anti_join(CompletedIntroExtro, Train, by = "id") 

#Fit the model
BaselineGLM <- glm(formula = Personality ~ . - id, family = "binomial", data = Train)

#Produce a regression table with Pseudo R^2 to assess fit
stargazer(
  BaselineGLM,
  type = "html",
  out = "BaselineGLM.html",
  add.lines = list(c(
    "McFadden pseudo R²",
    round(pR2(BaselineGLM)["McFadden"], 3))
  ))

#Validate the model
TestProbs <- predict(BaselineGLM, newdata = Test, type = "response")
TestPreds <- ifelse(TestProbs > 0.5, 1, 0)

#Accuracy score
mean(TestPreds == Test$Personality)

#Predict personality
CompletedKaggleTest$PersonalityProbs <- predict(BaselineGLM, newdata = CompletedKaggleTest, type = "response")
CompletedKaggleTest$Personality <- ifelse(CompletedKaggleTest$PersonalityProbs > 0.6, 1, 0) #Assign a threshold at 0.5, where below the threshold indicates an introvert, and above it an extrovert

#Rename binary labels
CompletedKaggleTest$Personality <- case_when(CompletedKaggleTest$Personality == 0 ~ "Introvert",
                               CompletedKaggleTest$Personality == 1 ~ "Extrovert")

#Save predictions
CompletedKaggleTest <- subset(CompletedKaggleTest, select = -c(PersonalityProbs, Drained_after_socializing, Friends_circle_size, Stage_fear, Going_outside, Time_spent_Alone, Post_frequency, Social_event_attendance)) #Remove excess columns for Kaggle submission
write.csv(CompletedKaggleTest, 'IntExtBaselineGLM.csv', row.names = FALSE)
#0.974089 accuracy score

