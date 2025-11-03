library(readxl)
library(dplyr)
library(MASS)
library(car)
library(nnet)
library(glmnet)
library(mgcv)
library(arm)
library(pROC)
# des tree
library(rpart)
library(rpart.plot)
# random forest
library(randomForest)
library(caret)



#####################################################################
#                     Data Load and Clean
#####################################################################

df <- read_excel("/Users/teahthies/Desktop/DONE/Models/Modeling/cleanpitch.xlsx", sheet = 1)
nrow(df) #6952
colnames(df) # 27 columns
#--------------------------------------------------------------------
# Convert Columns
#--------------------------------------------------------------------
# Convert to numeric
df$Inning <- as.numeric(df$Inning)
# Convert to factors (categorical)
df$Pitch <- as.factor(df$Pitch)
df$Bat_hand <- as.factor(df$Bat_hand)
df$code <- as.factor(df$code)
df$Pitch_Result <- as.factor(df$Pitch_Result)
df$home_vis <- as.factor(df$home_vis)
df$hit_type <- as.factor(df$hit_type)
df$Strike <- as.factor(df$Strike)
df$Ball <- as.factor(df$Ball)
df$pitch_count <- as.factor(df$pitch_count)
# Convert to logical (binary)
df$swing <- as.logical(df$swing)
df$outcome <- as.factor(df$outcome)

df <- df %>%
  mutate(
    Pitch = if_else(Pitch == "B", "C", Pitch),
    Pitch = if_else(Pitch == "4", "F", Pitch),
    Location = if_else(Location == "1f", "1", Location),
    num_outs = if_else(num_outs == 4, 2, num_outs),
    num_outs = if_else(num_outs == 3, 2, num_outs),
    num_baserunners = if_else(num_baserunners == -1, 0, num_baserunners),
    num_baserunners = if_else(num_baserunners == 5, 3, num_baserunners),
    num_baserunners = if_else(num_baserunners == 4, 3, num_baserunners),
    Pitcher = if_else(Pitcher == "G.", "Gonzales", Pitcher),
    Pitcher = if_else(Pitcher == "Frutos", "Frutoz", Pitcher),
    Strike = if_else(Strike == "2", "1", Strike)
  )

df$Location <- as.factor(df$Location)
df$Strike <- as.logical(df$Strike)
df$Ball <- as.logical(df$Ball)
df$pitch_count <- as.numeric(df$pitch_count)



#####################################################################
#               Check Balance and Split Test and Train
#####################################################################

#--------------------------------------------------------------------
# Question 1: Strikeouts vs what pitch count and location?
#--------------------------------------------------------------------
# Prepare Data
df <- subset(df, bat_event_fl == TRUE) # only get bat_event_fl
nrow(df)
# make a sub_fl column (this is walk flag column)
df2 <- df %>%
  mutate(sub_fl = ifelse(code %in% c(23), 1, 0))
unique(df2$sub_fl)
nrow(df2) #4039
df2$sub_fl <- as.logical(df2$sub_fl)

# .......................................................................
### new split 
library(dplyr)

set.seed(42)  # for reproducibility

#----------------------------
# 1) Split into Train/Test
#----------------------------
########## conference split
train_df_raw <- df2 %>%
  filter(
    year %in% c(2023, 2024) |
      (year == 2025 & game_type == "non-conference")
  )

test_df <- df2 %>%
  filter(year == 2025 & game_type == "conference")

########## last 20% split
# total number of rows
# n <- nrow(df2)
# 
# # index cutoff for 80%
# cutoff <- floor(0.80 * n)
# 
# # split: first 80% = train, last 20% = test
# train_df_raw <- df2[1:cutoff, ]
# test_df      <- df2[(cutoff + 1):n, ]

# sanity check
nrow(train_df_raw)   # should be ~80% of total
nrow(test_df)        # should be ~20% of total


# (optional) check class balance pre-balance
table(train_df_raw$sub_fl)
prop.table(table(train_df_raw$sub_fl))
table(test_df$sub_fl)
prop.table(table(test_df$sub_fl))

#-----------------------------------------
# 2) Balance ONLY the training data
#    (random oversampling of minority)
#-----------------------------------------
df_0 <- train_df_raw %>% filter(sub_fl == 0)
df_1 <- train_df_raw %>% filter(sub_fl == 1)

if (nrow(df_0) >= nrow(df_1)) {
  df_1_os <- df_1 %>% sample_n(nrow(df_0), replace = TRUE)
  train_df <- bind_rows(df_0, df_1_os)
} else {
  df_0_os <- df_0 %>% sample_n(nrow(df_1), replace = TRUE)
  train_df <- bind_rows(df_0_os, df_1)
}

# shuffle rows (optional, but nice before modeling)
train_df <- train_df %>% slice_sample(prop = 1)

# sanity checks
nrow(train_df)
prop.table(table(train_df$sub_fl))
# .......................................................................

# convert to 0,1 instead of true and false
train_df$sub_fl <- factor(as.integer(train_df$sub_fl), levels = c(0,1))
test_df$sub_fl <- factor(as.integer(test_df$sub_fl), levels = c(0,1))




#-----------------------------------------------------------------
# Split RHH and LHH for reduced models 
#-----------------------------------------------------------------
# RHH train
df_right_sub_train <- train_df %>% filter(Bat_hand == "R")
df_right_sub_train <- subset(df_right_sub_train, select = -Bat_hand)

# LHH train
df_left_sub_train  <- train_df %>% filter(Bat_hand == "L")
df_left_sub_train <- subset(df_left_sub_train, select = -Bat_hand)

# RHH test
df_right_sub_test <- test_df %>% filter(Bat_hand == "R")
df_right_sub_test <- subset(df_right_sub_test, select = -Bat_hand)

# LHH test
df_left_sub_test  <- test_df %>% filter(Bat_hand == "L")
df_left_sub_test <- subset(df_left_sub_test, select = -Bat_hand)



#--------------------------------------
# RHH 
#--------------------------------------
# clean data for model 
cols_used <- c("strike_count", "sub_fl", "Inning", "Pitch", "Location", "line_up_pos", 
               "OPP_score", "num_baserunners", "ball_count", "p_throws", "LBSU_score",
               "num_outs", "home_vis", "p_throws")
sapply(df_right_sub_train[cols_used], function(x) sum(is.na(x)))
sapply(df_right_sub_test[cols_used], function(x) sum(is.na(x)))

# get rid of NA's in data we will use 
df_clean_train <- df_right_sub_train[ , cols_used] 
df_clean_train <- na.omit(df_clean_train) 
df_clean_test <- df_right_sub_test[ , cols_used]  
df_clean_test <- na.omit(df_clean_test)

# Pitch Model
rf_model <- randomForest(
  as.factor(sub_fl) ~ Pitch+Location+
    line_up_pos+
    LBSU_score+
    OPP_score+
    num_outs+
    num_baserunners+
    ball_count+
    strike_count+
    Inning+
    home_vis+
    p_throws, 
  data = df_clean_train,
  ntree = 700,
  mtry = 3,
  nodesize = 10,
  classwt = c("0" = 1, "1" = 5),
  importance = TRUE
)
print(rf_model)# better at predicting SOs, not non-SOs
# OOB estimate of  error rate: 9.06% (lower the better)
# Variable importance
varImp <- importance(rf_model)
print(varImp[order(varImp[, "MeanDecreaseAccuracy"], decreasing = TRUE), ])
# MeanDecreaseAccuracy: Drop in model accuracy when 
#         this variable is shuffled — higher = more important
# MeanDecreaseGini: How much the variable contributes 
#         to node purity in the trees — higher = more splits

# visualize importance
varImpPlot(rf_model, type = 1)  # type = 1 shows MeanDecreaseAccuracy

#-----------------------------
# Train model predictions
#-----------------------------
# get values from print(rf_model)
TN <- 1716
FN <- 24
FP <- 0
TP <- 2412

# Metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)                     # also known as sensitivity
f1_score <- 2 * precision * recall / (precision + recall)
specificity <- TN / (TN + FP)

# AUC
test_probs <- predict(rf_model, newdata = df_clean_train, type = "prob")
roc_obj <- roc(df_clean_train$sub_fl, test_probs[, 1])
plot(roc_obj)

# show metrics
round(accuracy, 2)     # 1
round(precision, 2)    # 1
round(recall, 2)       # 0.99
round(f1_score, 2)      # 1
round(specificity, 2)   # 1
round(auc(roc_obj), 2)    # 1


#-----------------------------
# test model predictions
#-----------------------------
test_probs <- predict(rf_model, newdata = df_clean_test, type = "prob")
nrow(test_probs)
nrow(df_clean_test)
predicted_classes <- ifelse(test_probs[, "1"] >= 0.5, TRUE, FALSE)
confusion <- table(Predicted = predicted_classes, Actual = df_clean_test$sub_fl)
print(confusion)

TN <- confusion[1, 1]
FP <- confusion[1, 2]
FN <- confusion[2, 1]
TP <- confusion[2, 2]

# Metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)                     # also known as sensitivity
f1_score <- 2 * precision * recall / (precision + recall)
specificity <- TN / (TN + FP)

# AUC
roc_obj <- roc(df_clean_test$sub_fl, test_probs[, 1])
plot(roc_obj)

# show metrics
round(accuracy, 2)     # 1
round(precision, 2)    # 1
round(recall, 2)       # 1
round(f1_score, 2)      # 1
round(specificity, 2)   # 1
round(auc(roc_obj), 2)    # 1




#--------------------------------------
# LHH 
#--------------------------------------
# clean data for model 
sapply(df_left_sub_train[cols_used], function(x) sum(is.na(x)))
sapply(df_left_sub_test[cols_used], function(x) sum(is.na(x)))

# get rid of NA's in data we will use 
df_clean_train <- df_left_sub_train[ , cols_used] 
df_clean_train <- na.omit(df_clean_train) 
df_clean_test <- df_left_sub_test[ , cols_used]  
df_clean_test <- na.omit(df_clean_test)

# Pitch Model
rf_model <- randomForest(
  as.factor(sub_fl) ~ Pitch+Location+
    line_up_pos+
    LBSU_score+
    OPP_score+
    num_outs+
    num_baserunners+
    ball_count+
    strike_count+
    Inning+
    home_vis+
    p_throws, 
  data = df_clean_train,
  ntree = 700,
  mtry = 2,
  nodesize = 10,
  classwt = c("0" = 1, "1" = 5),
  importance = TRUE
)
print(rf_model)# better at predicting SOs, not non-SOs
# OOB estimate of  error rate: 10.93% (lower the better)
# Variable importance
varImp <- importance(rf_model)
print(varImp[order(varImp[, "MeanDecreaseAccuracy"], decreasing = TRUE), ])
# MeanDecreaseAccuracy: Drop in model accuracy when 
#         this variable is shuffled — higher = more important
# MeanDecreaseGini: How much the variable contributes 
#         to node purity in the trees — higher = more splits

# visualize importance
varImpPlot(rf_model, type = 1)  # type = 1 shows MeanDecreaseAccuracy

#-----------------------------
# Train model predictions
#-----------------------------
# get values from print(rf_model)
TN <- 951
FN <- 4
FP <- 0
TP <- 496

# Metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)                     # also known as sensitivity
f1_score <- 2 * precision * recall / (precision + recall)
specificity <- TN / (TN + FP)

# AUC
test_probs <- predict(rf_model, newdata = df_clean_train, type = "prob")
roc_obj <- roc(df_clean_train$sub_fl, test_probs[, 1])
plot(roc_obj)

# show metrics
round(accuracy, 2)     # 1
round(precision, 2)    # 1
round(recall, 2)       # 1
round(f1_score, 2)      # 1
round(specificity, 2)   # 1
round(auc(roc_obj), 2)    # 1


#-----------------------------
# test model predictions
#-----------------------------
test_probs <- predict(rf_model, newdata = df_clean_test, type = "prob")
nrow(test_probs)
nrow(df_clean_test)
predicted_classes <- ifelse(test_probs[, "1"] >= 0.5, TRUE, FALSE)
confusion <- table(Predicted = predicted_classes, Actual = df_clean_test$sub_fl)
print(confusion)

TN <- confusion[1, 1]
FP <- confusion[1, 2]
FN <- confusion[2, 1]
TP <- confusion[2, 2]

# Metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)                     # also known as sensitivity
f1_score <- 2 * precision * recall / (precision + recall)
specificity <- TN / (TN + FP)

# AUC
roc_obj <- roc(df_clean_test$sub_fl, test_probs[, 1])
plot(roc_obj)

# show metrics
round(accuracy, 2)     # 1
round(precision, 2)    # 1
round(recall, 2)       # 1
round(f1_score, 2)      # 1
round(specificity, 2)   # 1
round(auc(roc_obj), 2)    # 1
