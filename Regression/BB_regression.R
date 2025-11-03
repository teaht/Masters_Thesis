library(readxl)
library(dplyr)
library(MASS)
library(car)
library(nnet)
library(glmnet)
library(mgcv)
library(arm)
library(pROC)
#robust regression
library(robustbase)

#####################################################################
#                     Data Load and Clean
#####################################################################

df <- read_excel("/Users/teahthies/Desktop/Modeling/cleanpitch.xlsx", sheet = 1)
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
# Prepare Data
df <- subset(df, bat_event_fl == TRUE) # only get bat_event_fl
nrow(df)
# make a sub_fl column (this is walk flag column)
df <- df %>%
  mutate(sub_fl = ifelse(code == 18, 1, 0))
unique(df$sub_fl)
df2 <- df %>% filter(ball_count == 3)
nrow(df2)
df2$sub_fl <- as.logical(df2$sub_fl)

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
# 
# ########## last 20% split
# # total number of rows
n <- nrow(df2)

# # index cutoff for 80%
# cutoff <- floor(0.80 * n)
# 
# # split: first 80% = train, last 20% = test
# train_df_raw <- df2[1:cutoff, ]
# test_df      <- df2[(cutoff + 1):n, ]
# 
# # sanity check
# nrow(train_df_raw)   # should be ~80% of total
# nrow(test_df)        # should be ~20% of total


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

nrow(train_df)
nrow(test_df)



#--------------------------------------------------------------------
# LHH RHH split
#--------------------------------------------------------------------
# RHH train
df_right_sub_train <- train_df %>% filter(Bat_hand == "R")
df_right_sub_train <- subset(df_right_sub_train, select = -Bat_hand)
 # convert to factor if it's not already
# Change reference for Pitch
df_right_sub_train$Pitch <- factor(df_right_sub_train$Pitch)
df_right_sub_train$Pitch <- relevel(df_right_sub_train$Pitch, ref = "F")  
# Change reference for Location
df_right_sub_train$Location <- relevel(df_right_sub_train$Location, ref = "5") 

# LHH train
df_left_sub_train  <- train_df %>% filter(Bat_hand == "L")
df_left_sub_train <- subset(df_left_sub_train, select = -Bat_hand)
# Change reference for Pitch
df_left_sub_train$Pitch <- factor(df_left_sub_train$Pitch)
df_left_sub_train$Pitch <- relevel(df_left_sub_train$Pitch, ref = "F") 
# Change reference for Location
df_left_sub_train$Location <- relevel(df_left_sub_train$Location, ref = "5") 

# RHH test
df_right_sub_test <- test_df %>% filter(Bat_hand == "R")
df_right_sub_test <- subset(df_right_sub_test, select = -Bat_hand)
# Change reference for Pitch
df_right_sub_test$Pitch <- factor(df_right_sub_test$Pitch)
df_right_sub_test$Pitch <- relevel(df_right_sub_test$Pitch, ref = "F")  
# Change reference for Location
df_right_sub_test$Location <- relevel(df_right_sub_test$Location, ref = "5") 

# LHH test
df_left_sub_test  <- test_df %>% filter(Bat_hand == "L")
df_left_sub_test <- subset(df_left_sub_test, select = -Bat_hand)
# Change reference for Pitch
df_left_sub_test$Pitch <- factor(df_left_sub_test$Pitch)
df_left_sub_test$Pitch <- relevel(df_left_sub_test$Pitch, ref = "F")
# Change reference for Location
df_left_sub_test$Location <- relevel(df_left_sub_test$Location, ref = "5") 




#--------------------------------------------------------------------
#  Model: RHH and LHH subset
#--------------------------------------------------------------------

#--------------------------------------
# RHH - find best model with AIC
#--------------------------------------
## BIG BASE MODEL: include all variable that do not give away outcome variable
base_model_RHH <- glm(sub_fl ~ Pitch+Location+line_up_pos+
                        LBSU_score+OPP_score+num_outs+num_baserunners+
                        # ball_count+
                        strike_count+
                        Plate_Appearance+
                        home_vis+
                        p_throws,
                      data = df_right_sub_train, family = binomial)
summary(base_model_RHH) # AIC 383


# took out pitcher because train and test would not work
# reduced model
model_RHH <- glm(sub_fl ~ Pitch+Location+
                #   line_up_pos+
                #  LBSU_score+
                 #   OPP_score+
                   num_outs+
                   num_baserunners+
                  #  ball_count+
                   strike_count+
                   Plate_Appearance,
                  # home_vis+
                  # p_throws, 
                      data = df_right_sub_train, family = binomial)
summary(model_RHH) # AIC 378

# reduced model: Pitch only 
pitch_model_RHH <- glm(sub_fl ~ Pitch+
                         #   line_up_pos+
                         #  LBSU_score+
                         #   OPP_score+
                         num_outs+
                         num_baserunners+
                         #  ball_count+
                         strike_count+
                         Plate_Appearance,
                       # home_vis+
                       # p_throws, 
                      data = df_right_sub_train, family = binomial)
summary(pitch_model_RHH) # AIC 436

# reduced model, location only 
loc_model_RHH <- glm(sub_fl ~ Location+
                       #   line_up_pos+
                       #  LBSU_score+
                       #   OPP_score+
                       num_outs+
                       num_baserunners+
                       #  ball_count+
                       strike_count+
                       Plate_Appearance,
                     # home_vis+
                     # p_throws,
                      data = df_right_sub_train, family = binomial)
summary(loc_model_RHH) # AIC 385

# reduced model, Pitch*Location 
int_model_RHH <- glm(sub_fl ~ Pitch*Location+
                       #   line_up_pos+
                       #  LBSU_score+
                       #   OPP_score+
                       num_outs+
                       num_baserunners+
                       #  ball_count+
                       strike_count+
                       Plate_Appearance,
                     # home_vis+
                     # p_throws,
                     data = df_right_sub_train, family = binomial)
summary(int_model_RHH) # AIC 391




#--------------------------------------------------------------------------------
# MODEL USED
#--------------------------------------------------------------------------------
summary(model_RHH) # AIC 378

#------------------------------
# Test - Train - reduced model
#------------------------------

# RHH
# Predict probabilities on the train set
# this is to compare to test set later
test_probs <- predict(model_RHH, newdata = df_right_sub_train, type = "response")
test_pred <- ifelse(test_probs > 0.5, 1, 0)
#confusion matrix
confusion <- table(Predicted = test_pred, Actual = df_right_sub_train$sub_fl)
confusion

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
roc_obj <- roc(df_right_sub_train$sub_fl, test_probs)
plot(roc_obj)

# show metrics
round(accuracy, 2)     # 
round(precision, 2)     # 
round(recall, 2)        # 
round(f1_score, 2)      # 
round(specificity, 2)    # 
round(auc(roc_obj), 2)    # 

#------------------------------------------
# Assumptions (good)
#------------------------------------------

#---------------------
# Test - Test Data
#---------------------

# Predict probabilities on the test set
test_probs <- predict(model_RHH, newdata = df_right_sub_test, type = "response")
test_pred <- ifelse(test_probs > 0.5, 1, 0)
 
#confusion matrix
confusion <- table(Predicted = test_pred, Actual = df_right_sub_test$sub_fl)
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
roc_obj <- roc(df_right_sub_test$sub_fl, test_probs)
plot(roc_obj)

# show metrics
round(accuracy, 2)  # 0.66
round(precision, 2)  # 0.78
round(recall, 2)     # 0.50
round(f1_score, 2)    # 0.61
round(specificity, 2)   # 0.84
round(auc(roc_obj), 2)    # 0.75














#--------------------------------------
# LHH - find best model with AIC
#--------------------------------------
## BIG BASE MODEL: include all variable that do not give away outcome variable
base_model_LHH <- glm(sub_fl ~ Pitch+Location+line_up_pos+
                        LBSU_score+OPP_score+num_outs+num_baserunners+
                        # ball_count+
                        strike_count+
                        Plate_Appearance+
                        home_vis+
                        p_throws,
                      data = df_left_sub_train, family = binomial)
summary(base_model_LHH) # AIC 247


# took out pitcher becasue train and test would not work
# reduced model
model_LHH <- glm(sub_fl ~ Pitch+Location+
                  # line_up_pos+
                  #LBSU_score+
                     OPP_score+
                  #num_outs+
                  # num_baserunners+
                   # ball_count+
                   strike_count+
                   Plate_Appearance+
                   home_vis+
                   p_throws, 
                 data = df_left_sub_train, family = binomial)
summary(model_LHH) # AIC 243

# reduced model: Pitch only 
pitch_model_LHH <- glm(sub_fl ~ Pitch+
                         # line_up_pos+
                         #LBSU_score+
                         OPP_score+
                         #num_outs+
                         # num_baserunners+
                         # ball_count+
                         strike_count+
                         Plate_Appearance+
                         home_vis+
                         p_throws, 
                       data = df_left_sub_train, family = binomial)
summary(pitch_model_LHH) # AIC 282

# reduced model, location only 
loc_model_LHH <- glm(sub_fl ~ Location+
                       # line_up_pos+
                       #LBSU_score+
                       OPP_score+
                       #num_outs+
                       # num_baserunners+
                       # ball_count+
                       strike_count+
                       Plate_Appearance+
                       home_vis+
                       p_throws, 
                     data = df_left_sub_train, family = binomial)
summary(loc_model_LHH) # AIC 246

# # reduced model, Pitch*Location 
int_model_LHH <- glm(sub_fl ~ Pitch*Location+
                       # line_up_pos+
                       #LBSU_score+
                       OPP_score+
                       #num_outs+
                       # num_baserunners+
                       # ball_count+
                       strike_count+
                       Plate_Appearance+
                       home_vis+
                       p_throws,
                     data = df_left_sub_train, family = binomial)
summary(int_model_LHH) # AIC DNE




#--------------------------------------------------------------------------------
# MODEL USED
#--------------------------------------------------------------------------------
summary(model_LHH) # AIC 243

#----------------
# Test - Train
#----------------

# LHH
# Predict probabilities on the train set
# this is to comapre to test set later
test_probs <- predict(model_LHH, newdata = df_left_sub_train, type = "response")
test_pred <- ifelse(test_probs > 0.5, 1, 0)

#confusion matrix
confusion <- table(Predicted = test_pred, Actual = df_left_sub_train$sub_fl)
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
roc_obj <- roc(df_left_sub_train$sub_fl, test_probs)
plot(roc_obj)

# show metrics
round(accuracy, 2)     # 0.75
round(precision, 2)     # 0.76
round(recall, 2)        # 0.80
round(f1_score, 2)      # 0.78
round(specificity, 2)    # 0.69
round(auc(roc_obj), 2)    # 0.84

#------------------------------------------
# Assumptions (good)
#------------------------------------------

#---------------------
# Test - Test Data
#---------------------

# Predict probabilities on the test set
test_probs <- predict(model_LHH, newdata = df_left_sub_test, type = "response")
test_pred <- ifelse(test_probs > 0.5, 1, 0)

#confusion matrix
confusion <- table(Predicted = test_pred, Actual = df_left_sub_test$sub_fl)
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
roc_obj <- roc(df_left_sub_test$sub_fl, test_probs)
plot(roc_obj)

# show metrics
round(accuracy, 2)  # 0.77
round(precision, 2)  # 0.60
round(recall, 2)     # 0.75
round(f1_score, 2)    # 0.67
round(specificity, 2)   # 0.78
round(auc(roc_obj), 2)    # 0.66



