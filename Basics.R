###.                     0_ Preprosess and Modelling for prediction                             ###
###                                    Zihua Lai                                               ##
##                                 WED, 19March, 2025                                         ###

# load the necessary libraries -----------------
library(grid)
library(Matrix)
library(survival)
library(survey)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)
library(corrplot)
library(caTools)
library(xgboost)
library(lightgbm)


# load the data --------------------
asg<- load("data/asgpolm8.rdata")

## Load the PV data -----------------
PVs <- c("ASMMAT01", "ASMMAT02", "ASMMAT03", "ASMMAT04", "ASMMAT05")
PV_data <- asgpolm8%>%select(all_of(PVs))

# show 5 PVs statistics --------------------
## if there is NA ---------------
colSums(is.na(PV_data)) ## There is no NA for any plausible values 

## Mean, Median, minimum and maximum ----------------
stats <- data.frame(
  Min = apply(PV_data, 2, min, na.rm = TRUE),
  Max = apply(PV_data, 2, max, na.rm = TRUE),
  Mean = apply(PV_data, 2, mean, na.rm = TRUE),
  Median = apply(PV_data, 2, median, na.rm = TRUE)
)

print(stats)

# write.csv(stats, "statistics_of_PVs.csv", row.names = TRUE)

# create the asg file ------------------------------
## z scores of each PV -----------
PV_z_scores <- as.data.frame(apply(PV_data, 2, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

# Deal with the outliers with min-max methods --------------------
# Define function to replace outliers with min-max capping
cap_outliers <- function(z_values, raw_values) {
  threshold <- 3
  min_value <- min(raw_values[z_values > -threshold & z_values < threshold], na.rm = TRUE)
  max_value <- max(raw_values[z_values > -threshold & z_values < threshold], na.rm = TRUE)
  capped_values <- ifelse(z_values > threshold, max_value, 
                          ifelse(z_values < -threshold, min_value, raw_values))
  return(capped_values)
}

# Apply the capping function to each column
PV_capped <- PV_z_scores %>%
  mutate(across(starts_with("Z_"), 
                ~ cap_outliers(., PV_data_clean[[gsub("Z_", "", cur_column())]]), 
                .names = "Capped_{.col}"))

##  Aggregate the 5 plausible values to Maths variable -------------------------
# Step 1: Identify Replicate Weights
replicate_weights <- grep("^JKREP", names(asgpolm8), value = TRUE)

# Step 2: Convert Replicate Weights to a Matrix
rep_weights_matrix <- as.matrix(asgpolm8[, replicate_weights])

# Step 3: Define Survey Design
survey_design <- svrepdesign(
  weights = ~TOTWGT,  # Main weight column
  repweights = rep_weights_matrix,  # Replicate weights matrix
  type = "JK1",  # Jackknife replicate method
  scale = 1,  # Default scale
  rscales = rep(1, length(replicate_weights)),  # Default replicate scales
  data = asgpolm8
)

# Step 4: Compute the Plausible Value Mean (PV_WeightedMean)
asgpolm8 <- asgpolm8 %>%
  mutate(across(starts_with("ASMMAT"), ~ as.numeric(haven::zap_labels(.))))

asgpol <- asgpolm8 %>%
  rowwise() %>%
  mutate(Math = mean(c(ASMMAT01, ASMMAT02, ASMMAT03, ASMMAT04, ASMMAT05), na.rm = TRUE)) %>%
  ungroup()

# Step 3: Update Survey Design with New "Math" Variable
survey_design <- update(survey_design, Math = asgpol$Math)
asgpol$Math

# asg -------------------------------
columns = c("IDSCHOOL", "IDCLASS", "IDSTUD", "ASDGSSB", "ASDGSLM",  "ASDGSCM", "ASDGHRL", "Math")
asg <- asgpol%>%select(all_of(columns))

# save the asg file 
# write.csv(asg, "data/asg.csv")

summary(asg)
# there are 17NA in ASDGSSB (school belonging; 1 is high, 3 is low)
# 17NA in ASDGSLM(student like mathmatics, 3, don't like)
# 17NA in ASDGSCM(student confidence in mathamatics, 3, not confident) 
# 2 NA in ASDGHRL (home resources in learning, 3, few resources)

##  Deal with NA in asg --------------------------------
asg <- asg %>% mutate(
  ASDGSSB = ifelse(is.na(ASDGSSB), 3, ASDGSSB), 
  ASDGSLM = ifelse(is.na(ASDGSLM), 3, ASDGSLM),
  ASDGSCM = ifelse(is.na(ASDGSCM), 3, ASDGSCM), 
  ASDGHRL = ifelse(is.na(ASDGHRL), 3, ASDGHRL)
)

##  calcualte the correlations -------------------------------
corr_variables <- c("ASDGSSB", "ASDGSLM",  "ASDGSCM", "ASDGHRL")
corr_matrix <- asg %>% select(all_of(corr_variables))
correlation <- cor(corr_matrix)
corrplot(correlation)
print(correlation)  # No variables have correlaiton over 0.5 , so it is okay for using 

# ash ------------------------------------
ash <- load("data/ashpolm8.rdata")
columns <- c("ASBH11", "ASDHPSP", "ASDHEDUP", "ASDHAPS", "ASBH04AB", "ASDHSES", "ASDHOCCP", "IDSTUD")
ash <- ashpolm8 %>% select(all_of(columns))
summary(ash)
ash <- ash %>% mutate(ASDHEDUP = replace_na(ASDHEDUP, 6))
ash <- ash %>% mutate(ASDHAPS = replace_na(ASDHAPS, 1))
ash <- ash %>% mutate(ASDHPSP = replace_na(ASDHPSP, 3))
ash <- ash %>% mutate(ASBH11 = replace_na(ASBH11, 1))
ash <- ash %>% mutate(ASBH04AB = replace_na(ASBH04AB, 2)) # 2 is didn't attend prliminary school
ash <- ash %>% mutate(ASDHSES = replace_na(ASDHSES, 3)) # 3 is lowwer
ash <- ash%>% mutate(ASDHOCCP = replace_na(ASDHOCCP, 6)) # 6 is never worked for pay

data <- inner_join(asg, ash, by = "IDSTUD")
corr_variables <- c("ASBH11", "ASDHPSP", "ASDHEDUP", "ASDHAPS", "ASBH04AB", "ASDHSES", "ASDHOCCP")
corr_matrix <- data %>% select(all_of(corr_variables))
correlation <- cor(corr_matrix)
print(correlation)
# drop ASDHSES, ASDHOCCP, ASBH04AB
data <- data %>% select(-ASDHSES, -ASDHOCCP, -ASBH04AB)

# atg --------------------------
## Merge the table ----------------------------------
atg <- load("data/atgpolm8.rdata")
ast <- load("data/astpolm8.rdata")
atg_ast <- inner_join(atgpolm8,astpolm8, by = "IDTEALIN")
columns = c("IDSTUD", "ATDGEAS", 
            "ATDGTJS", 
            "ATDMHW",
            "ATBG01",
            "ATBG02", 
            "ATBG03")
atg_ast<- atg_ast %>% select(all_of(columns))
atg <- inner_join(atg_ast, asg, by = c("IDSTUD"))
corr_variables <- c( "ATDGEAS", 
                     "ATDGTJS", 
                     "ATDMHW",
                     "ATBG01",
                     "ATBG02", 
                     "ATBG03")
corr_matrix <- atg %>% select(all_of(corr_variables))
summary(corr_matrix)

##  Dealing with NA----------------------------
corr_matrix <- corr_matrix %>% mutate(
  ATDGEAS = replace_na(ATDGEAS, 3),
  ATDGTJS = replace_na(ATDGTJS, 3),
  ATBG02 = replace_na(ATBG02, 1), 
  ATBG03 = replace_na(ATBG03, 1)
)

corr_matrix$ATDMHW[is.na(corr_matrix$ATDMHW)] <- mean(corr_matrix$ATDMHW, na.rm = TRUE)
corr_matrix$ATBG01[is.na(corr_matrix$ATBG01)] <- mean(corr_matrix$ATBG01, na.rm = TRUE)
summary(corr_matrix)
correlation <- cor(corr_matrix)
correlation 
atg <- atg %>% select(-IDCLASS, -IDSCHOOL, - ASDGSSB, -ASDGSLM, -ASDGSCM, -ASDGHRL,-Math)
data <- inner_join(data, atg, by = "IDSTUD")



# acg -----------------
acg <- load("data/acgpolm8.rdata")
variables <- c("IDSCHOOL",
              "ACDGEAS",
               "ACDGDAS",
               "ACDGLNS",
               "ACDGTIHY",
               "ACDGSBC")
acg <- acgpolm8%>% select(all_of(variables))
data <- inner_join(acg, data, by = "IDSCHOOL")
data <- data %>% select(-IDSTUD, -IDCLASS, -IDSCHOOL)
data <- na.omit(data)

# Machine Learning ---------------------------
## Divide X and Y------------
y <- data$Math
X <- data %>% select(-Math)
## Divide traning and testing data ----------------
# Set seed for reproducibility
set.seed(123)
# Split: 80% training, 20% testing
split <- sample.split(y, SplitRatio = 0.8)

# Create training and test sets
train_data <- subset(data, split == TRUE)
summary(train_data)

train_data <- na.omit(train_data)
test_data <- subset(data, split == FALSE)

X_train <- train_data[, !(names(train_data) == "Math")]
Y_train <- train_data$Math

X_test <- test_data[, !(names(test_data) == "Math")]
Y_test <- test_data$Math

# Algorithms to predict ------------------------------
# use recursive elimination to find the best features 
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
results_rfe <- rfe(X_train, Y_train, sizes = c(5, 10, 15, 20), rfeControl = control)

# check the best group
print(results_rfe)
top_vars_rfe <- predictors(results_rfe)

# Modelling with these vars
X_train_top <- X_train[, top_vars_rfe]
X_test_top <- X_test[, top_vars_rfe]
# Subset data using top variables
X_train_top <- X_train[, top_vars]
X_test_top <- X_test[, top_vars]

## Train multiple models and compare-----------------
# Install required packages
# install.packages(c("caret", "e1071"))
library(caret)
library(e1071)

# Set training control
train_control <- trainControl(method = "cv", number = 5)

# Combine predictors and response for caret training
train_df <- cbind(X_train_top, Math = Y_train)
test_df <- cbind(X_test_top, Math = Y_test)

# Linear Regression 
lm_model <- train(Math ~ ., data = train_df, method = "lm", trControl = train_control)
lm_model
# Random Forest 
rf_model <- train(Math ~ ., data = train_df, method = "rf", trControl = train_control)
rf_model
# Support vector machine 
svm_model <- train(Math ~ ., data = train_df, method = "svmRadial", trControl = train_control)
svm_model
# XGboost Regressor 
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

xgb_model <- train(
  Math ~ ., 
  data = train_df,
  method = "xgbTree", 
  trControl = train_control,
  tuneGrid = xgb_grid
)

xgb_model$bestTune  

# LightGBM 
library(lightgbm)

# Convert data to matrix
X_train_matrix <- data.matrix(X_train_top)
X_test_matrix <- data.matrix(X_test_top)
y_train_vector <- as.numeric(Y_train)
y_test_vector <- as.numeric(Y_test)

# Create LightGBM dataset
dtrain_lgb <- lgb.Dataset(data = X_train_matrix, label = y_train_vector)

# Set LightGBM parameters
params <- list(
  objective = "regression",
  metric = "rmse",
  verbosity = -1
)

# Train model
set.seed(123)
lgb_model <- lgb.train(
  params = params,
  data = dtrain_lgb,
  nrounds = 100
)

# Predict on test set
pred_lgb <- predict(lgb_model, X_test_matrix)

# Model Evaluation on the test results -------------------
# Predict with caret models
pred_lm <- predict(lm_model, newdata = test_df)
pred_rf <- predict(rf_model, newdata = test_df)
pred_svm <- predict(svm_model, newdata = test_df)
pred_xgb <- predict(xgb_model, newdata = test_df)

# Define RMSE and R² functions
rmse <- function(pred, actual) sqrt(mean((pred - actual)^2))
r2 <- function(pred, actual) cor(pred, actual)^2

# Compare models
results_all <- data.frame(
  Model = c("Linear Regression", "Random Forest", "SVM", "XGBoost", "LightGBM"),
  RMSE = c(
    rmse(pred_lm, Y_test),
    rmse(pred_rf, Y_test),
    rmse(pred_svm, Y_test),
    rmse(pred_xgb, Y_test),
    rmse(pred_lgb, Y_test)
  ),
  R2 = c(
    r2(pred_lm, Y_test),
    r2(pred_rf, Y_test),
    r2(pred_svm, Y_test),
    r2(pred_xgb, Y_test),
    r2(pred_lgb, Y_test)
  )
)

print(results_all)

# Predict the students performance is high, medium, low ------------------
##  Categorize Y_train into 'Low', 'Medium', 'High'-------------------
quantiles <- quantile(Y_train, probs = c(0.33, 0.66))
Y_train_cat <- cut(Y_train,
                   breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
                   labels = c("Low", "Medium", "High"))

X_train_selected <- X_train[, top_vars_rfe]

## Train a classification model -------------
# Set up cross-validation
control <- trainControl(method = "cv", number = 5)

# Train a random forest classifier
model_rf <- train(x = X_train_selected, 
                  y = Y_train_cat, 
                  method = "rf", 
                  trControl = control)

## Evaluate the model ----------
predictions <- predict(model_rf, X_train_selected)
confusionMatrix(predictions, Y_train_cat)

## Apply to the test set --------------
X_test_selected <- X_test[, top_vars_rfe]
pred_test <- predict(model_rf, X_test_selected)

# Only keep these 5 vars
selected_vars <- c("ASDGSCM", "ASDGHRL", "ASDHEDUP", "ASBH11", "ACDGTIHY")
X_train_selected <- X_train[, selected_vars]
Y_train_cat <- cut(Y_train, quantile(Y_train, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE,
                   labels = c("Low", "Medium", "High"))
Y_train_cat <- factor(Y_train_cat)

control <- trainControl(method = "cv", number = 5)
model_rf <- train(x = X_train_selected, y = Y_train_cat, method = "rf", trControl = control)

# save 
top_vars_rfe <- selected_vars
save(model_rf, top_vars_rfe, file = "trained_model.RData")