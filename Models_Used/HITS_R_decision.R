# ============================ PACKAGES ===================================
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





# ============================ LOAD DATA ===================================
df <- read_excel("/Users/teahthies/Desktop/DONE/Models/Modeling/cleanpitch.xlsx", sheet = 1)
nrow(df) #6952
colnames(df) # 27 columns
#---------------------------------------------------------------------------
# Convert Columns
#---------------------------------------------------------------------------
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

# Fix spelling errors
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

#---------------------------------------------------------------------------
# check
#---------------------------------------------------------------------------
df <- subset(df, bat_event_fl == TRUE)
nrow(df) #4039







# ============================ Train/Test ===============================
library(dplyr)
set.seed(42)

#---------------------------------------------------------------------------
# last 20% into test set
#---------------------------------------------------------------------------
n <- nrow(df)

# index cutoff for 80%
cutoff <- floor(0.80 * n)

# split: first 80% = train, last 20% = test
train_df <- df[1:cutoff, ]
test_df <- df[(cutoff + 1):n, ]

#---------------------------------------------------------------------------
# check
#---------------------------------------------------------------------------
nrow(train_df)  # 3231 
nrow(test_df)   #808





# ============================ OUTCOME VARIABLE ===============================
# make a binary outcome column
train_df <- train_df %>%
  mutate(sub_fl = ifelse(code %in% c(20, 21, 22, 23), 1, 0))
unique(train_df$sub_fl)
train_df <- train_df %>% filter(Pitch != "F")
train_df <- train_df %>% filter(Bat_hand == "R")
train_df$sub_fl <- as.logical(train_df$sub_fl)

test_df <- test_df %>%
  mutate(sub_fl = ifelse(code %in% c(20, 21, 22, 23), 1, 0))
unique(test_df$sub_fl)
test_df <- test_df %>% filter(Pitch != "F")
test_df <- test_df %>% filter(Bat_hand == "R")
test_df$sub_fl <- as.logical(test_df$sub_fl)

nrow(train_df)
nrow(test_df)   #404



#---------------------------------------------------------------------------
# Balance ONLY the training data (random oversampling of minority)
#---------------------------------------------------------------------------
df_0 <- train_df %>% filter(sub_fl == 0)
df_1 <- train_df %>% filter(sub_fl == 1)

if (nrow(df_0) >= nrow(df_1)) {
  df_1_os <- df_1 %>% sample_n(nrow(df_0), replace = TRUE)
  train_df <- bind_rows(df_0, df_1_os)
} else {
  df_0_os <- df_0 %>% sample_n(nrow(df_1), replace = TRUE)
  train_df <- bind_rows(df_0_os, df_1)
}

# shuffle rows
train_df <- train_df %>% slice_sample(prop = 1)

# sanity checks
nrow(train_df)
prop.table(table(train_df$sub_fl))

nrow(train_df)
nrow(test_df)   # 404



train_df <- subset(train_df, select = -Bat_hand)
test_df <- subset(test_df, select = -Bat_hand)



# ============================ RHH Model ===============================
# clean data for model 
cols_used <- c("strike_count", "sub_fl", "Inning", "Pitch", "Location", "line_up_pos", 
               "OPP_score", "num_baserunners", "ball_count", "p_throws", "LBSU_score",
               "num_outs", "home_vis")
sapply(train_df[cols_used], function(x) sum(is.na(x)))
sapply(test_df[cols_used], function(x) sum(is.na(x)))

# get rid of NA's in data we will use 
df_clean_train <- train_df[ , cols_used] 
df_clean_train <- na.omit(df_clean_train) 
df_clean_test <- test_df[ , cols_used]  
df_clean_test <- na.omit(df_clean_test)

nrow(df_clean_train)

base_model <- rpart(sub_fl ~ Pitch+Location+
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
                    method = "class",
                    control = rpart.control(cp = 0.02))

# Plot tree
rpart.plot(base_model, type = 3, extra = 104, fallen.leaves = TRUE)

# -----------------------------
# POSITIVE CLASS COLUMN: detect correctly
# -----------------------------
get_positive_col <- function(prob_mat, y_true) {
  # If logical labels, use "TRUE"
  if (is.logical(y_true)) {
    if ("TRUE" %in% colnames(prob_mat)) return("TRUE")
  }
  # If numeric 0/1, use "1"
  if (is.numeric(y_true)) {
    if ("1" %in% colnames(prob_mat)) return("1")
  }
  # If factor, try common positive names; else use last level as positive
  if (is.factor(y_true)) {
    common_pos <- c("1","TRUE","True","Yes","YES","Y","pos","Positive","Strike","true","T")
    hit <- intersect(colnames(prob_mat), common_pos)
    if (length(hit) > 0) return(hit[1])
    # fallback: last level is positive
    last_lev <- tail(levels(y_true), 1)
    # prefer matching column name if present
    if (last_lev %in% colnames(prob_mat)) return(last_lev)
  }
  # Ultimate fallback: take the last column
  tail(colnames(prob_mat), 1)
}

# -----------------------------
# Helper: metrics for a threshold
# -----------------------------
metrics_from_threshold <- function(y_true, p_hat, thr = 0.5) {
  # Convert to logical positive indicator
  if (is.factor(y_true)) {
    pos_level <- tail(levels(y_true), 1)  # assumes positive is last level if factor
    y_true <- y_true == pos_level
  }
  y_pred <- p_hat >= thr
  
  TP <- sum(y_pred &  y_true)
  TN <- sum(!y_pred & !y_true)
  FP <- sum(y_pred & !y_true)
  FN <- sum(!y_pred &  y_true)
  
  precision   <- ifelse((TP+FP)==0, 0, TP/(TP+FP))
  recall      <- ifelse((TP+FN)==0, 0, TP/(TP+FN))
  f1          <- ifelse((precision+recall)==0, 0, 2*precision*recall/(precision+recall))
  accuracy    <- (TP+TN)/length(y_true)
  specificity <- ifelse((TN+FP)==0, 0, TN/(TN+FP))
  
  c(accuracy=accuracy, precision=precision, recall=recall,
    f1=f1, specificity=specificity,
    TP=TP, TN=TN, FP=FP, FN=FN, prop_pos_pred=mean(y_pred))
}

# -----------------------------
# TRAIN predictions with correct positive column
# -----------------------------
train_probs_mat <- predict(base_model, newdata = df_clean_train, type = "prob")
pos_col <- get_positive_col(train_probs_mat, df_clean_train$sub_fl)
train_prob <- train_probs_mat[, pos_col]

# Quick sanity check: show column chosen and its range
cat(sprintf("Positive class column chosen: %s\n", pos_col))
cat(sprintf("Train prob summary (pos): min=%.3f median=%.3f max=%.3f\n",
            min(train_prob), median(train_prob), max(train_prob)))

# -----------------------------
# Threshold tuning to maximize F1 (prevent degenerate thresholds)
# -----------------------------
grid <- seq(0.10, 0.90, by = 0.01)

valid_thr <- sapply(grid, function(t) {
  pr <- train_prob >= t
  prop_TRUE <- mean(pr)
  prop_TRUE >= 0.05 && prop_TRUE <= 0.95   # keep both classes in predictions
})
grid2 <- grid[valid_thr]
if (length(grid2) == 0) {
  # If everything was filtered, relax bounds slightly
  valid_thr <- sapply(grid, function(t) {
    pr <- train_prob >= t
    prop_TRUE <- mean(pr)
    prop_TRUE >= 0.02 && prop_TRUE <= 0.98
  })
  grid2 <- grid[valid_thr]
  if (length(grid2) == 0) grid2 <- grid  # ultimate fallback
}

train_f1s <- sapply(grid2, function(t) {
  metrics_from_threshold(df_clean_train$sub_fl, train_prob, thr = t)["f1"]
})
best_idx <- which(train_f1s == max(train_f1s, na.rm = TRUE))
best_thr <- grid2[best_idx[which.min(abs(grid2[best_idx] - 0.50))]]  # tie-break toward 0.5

cat(sprintf("Chosen threshold to maximize F1 on TRAIN: %.2f\n", best_thr))

# TRAIN metrics at best_thr
m_train <- metrics_from_threshold(df_clean_train$sub_fl, train_prob, thr = best_thr)
print(round(m_train[c("accuracy","precision","recall","f1","specificity","prop_pos_pred")], 3))

# AUC (TRAIN) using the SAME positive column
roc_obj_train <- roc(df_clean_train$sub_fl, train_prob)
cat(sprintf("AUC (TRAIN): %.3f\n", auc(roc_obj_train)))

# -----------------------------
# TEST predictions with same column and threshold
# -----------------------------
test_probs_mat <- predict(base_model, newdata = df_clean_test, type = "prob")

# Use the SAME column name if present; otherwise, re-detect
pos_col_test <- if (pos_col %in% colnames(test_probs_mat)) pos_col else get_positive_col(test_probs_mat, df_clean_test$sub_fl)
if (pos_col_test != pos_col) {
  cat(sprintf("Note: test prob column differs; using %s\n", pos_col_test))
}
test_prob <- test_probs_mat[, pos_col_test]

m_test <- metrics_from_threshold(df_clean_test$sub_fl, test_prob, thr = best_thr)

# -----------------------------
# Build and show confusion matrix
# -----------------------------
actual <- if (is.factor(df_clean_test$sub_fl)) {
  as.logical(df_clean_test$sub_fl == tail(levels(df_clean_test$sub_fl), 1))
} else if (is.numeric(df_clean_test$sub_fl)) {
  as.logical(df_clean_test$sub_fl == 1)
} else {
  as.logical(df_clean_test$sub_fl)
}

predicted <- test_prob >= best_thr


print(nrow(df_clean_test))
confusion_matrix <- table(Predicted = predicted, Actual = actual)
print(confusion_matrix)

print(round(m_test[c("accuracy","precision","recall","f1","specificity","prop_pos_pred")], 3))

roc_obj_test <- roc(df_clean_test$sub_fl, test_prob)
cat(sprintf("AUC (TEST): %.3f\n", auc(roc_obj_test)))


{
  cat("\nTEST size:\n")
  print(nrow(df_clean_test))
  cat("\nTEST confusion matrix:\n")
  print(confusion_matrix)
  cat("\nTEST metrics:\n")
  print(round(m_test[c("accuracy","precision","recall","f1","specificity","prop_pos_pred")], 2))
  cat(sprintf("AUC (TEST): %.3f\n", auc(roc_obj_test)))
}










#======================================================
# PR curves (Plotly) + PR-AUC (PRROC) — robust & printed
#======================================================
# install.packages(c("PRROC","plotly","htmlwidgets"))  # if needed
library(PRROC)
library(plotly)

make_pr_plot <- function(y_true, y_score, model_name = "Model", split = "TEST") {
  # Ensure score is numeric vector
  y_score <- as.numeric(y_score)
  
  # ---------- Align lengths ----------
  if (!is.null(names(y_score)) && !is.null(names(y_true))) {
    common_ids <- intersect(names(y_true), names(y_score))
    if (length(common_ids) > 0L) {
      y_true  <- y_true [common_ids]
      y_score <- y_score[common_ids]
    } else {
      n <- min(length(y_true), length(y_score))
      if (length(y_true) != length(y_score)) {
        message(sprintf("[%s] No common names; trimmed to n=%d.", split, n))
      }
      y_true  <- y_true[seq_len(n)]
      y_score <- y_score[seq_len(n)]
    }
  } else {
    n <- min(length(y_true), length(y_score))
    if (length(y_true) != length(y_score)) {
      message(sprintf("[%s] Trimmed to n=%d (y_true=%d, y_score=%d).",
                      split, n, length(y_true), length(y_score)))
    }
    y_true  <- y_true[seq_len(n)]
    y_score <- y_score[seq_len(n)]
  }
  
  # ---------- Normalize labels to logical (TRUE = positive) ----------
  if (is.factor(y_true)) {
    y_true <- y_true == tail(levels(y_true), 1)
  } else if (is.numeric(y_true)) {
    y_true <- y_true == 1
  } else {
    y_true <- as.logical(y_true)
  }
  
  # ---------- Drop NA/Inf ----------
  keep <- !is.na(y_true) & !is.na(y_score) & is.finite(y_score)
  if (any(!keep)) message(sprintf("[%s] Dropped %d rows with NA/Inf.", split, sum(!keep)))
  y_true  <- y_true[keep]
  y_score <- y_score[keep]
  
  base <- mean(y_true)
  n_pos <- sum(y_true); n_neg <- sum(!y_true)
  
  # ---------- If single-class after filtering, draw baseline ----------
  if (length(y_true) == 0L || is.na(base) || n_pos == 0L || n_neg == 0L) {
    pr_df <- data.frame(recall = c(0, 1), precision = c(ifelse(is.na(base), 0, base), ifelse(is.na(base), 0, base)))
    fig <- plot_ly(pr_df, x = ~recall, y = ~precision,
                   type = "scatter", mode = "lines",
                   name = paste0(model_name, " (PR-AUC = NA)")) |>
      layout(title = paste(model_name, split, "Precision–Recall"),
             xaxis = list(title = "Recall", range = c(0, 1), tick0 = 0, dtick = 0.1, zeroline = FALSE),
             yaxis = list(title = "Precision", range = c(0, 1), tick0 = 0, dtick = 0.1, zeroline = FALSE),
             legend = list(orientation = "h"),
             margin = list(l = 60, r = 20, t = 60, b = 60)) |>
      add_lines(x = c(0, 1), y = c(ifelse(is.na(base), 0, base), ifelse(is.na(base), 0, base)),
                name = sprintf("Prevalence = %.2f", ifelse(is.na(base), 0, base)),
                line = list(dash = "dash"))
    return(list(fig = fig, pr_auc = NA_real_))
  }
  
  # ---------- PR curve + AUC ----------
  sc_pos <- y_score[y_true]
  sc_neg <- y_score[!y_true]
  pr     <- PRROC::pr.curve(scores.class0 = sc_pos,
                            scores.class1 = sc_neg,
                            curve = TRUE)
  pr_auc <- pr$auc.integral
  
  pr_df <- data.frame(recall = pr$curve[, 1],
                      precision = pr$curve[, 2],
                      threshold = pr$curve[, 3])
  
  fig <- plot_ly(pr_df, x = ~recall, y = ~precision,
                 type = "scatter", mode = "lines",
                 name = paste0(model_name, sprintf(" (PR-AUC = %.3f)", pr_auc))) |>
    add_lines(x = c(0, 1), y = c(base, base),
              name = sprintf("Prevalence = %.2f", base),
              line = list(dash = "dash")) |>
    layout(title = paste(model_name, split, "Precision–Recall"),
           xaxis = list(title = "Recall", range = c(0, 1), tick0 = 0, dtick = 0.1, zeroline = FALSE),
           yaxis = list(title = "Precision", range = c(0, 1), tick0 = 0, dtick = 0.1, zeroline = FALSE),
           legend = list(orientation = "h"),
           margin = list(l = 60, r = 20, t = 60, b = 60))
  
  list(fig = fig, pr_auc = pr_auc)
}

# --- TRAIN PR curve & PR-AUC ---
pr_train <- make_pr_plot(df_clean_train$sub_fl, train_prob,
                         model_name = "RHH Final (rpart)", split = "TRAIN")
print(pr_train$fig)  # IMPORTANT when running via source()/Rterm
cat(sprintf("PR-AUC (TRAIN): %.3f\n", pr_train$pr_auc))

# --- TEST PR curve & PR-AUC ---
pr_test <- make_pr_plot(df_clean_test$sub_fl, test_prob,
                        model_name = "RHH Final (rpart)", split = "TEST")
print(pr_test$fig)   # IMPORTANT when running via source()/Rterm
cat(sprintf("PR-AUC (TEST): %.3f\n", pr_test$pr_auc))
