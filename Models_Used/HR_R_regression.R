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
  mutate(sub_fl = ifelse(code %in% c(23), 1, 0))
unique(train_df$sub_fl)
train_df <- train_df %>% filter(Pitch != "F")
train_df <- train_df %>% filter(Bat_hand == "R")
train_df$sub_fl <- as.logical(train_df$sub_fl)

test_df <- test_df %>%
  mutate(sub_fl = ifelse(code %in% c(23), 1, 0))
unique(test_df$sub_fl)
test_df <- test_df %>% filter(Pitch != "F")
test_df <- test_df %>% filter(Bat_hand == "R")
test_df$sub_fl <- as.logical(test_df$sub_fl)

nrow(train_df) 
nrow(test_df)   # 404

# ============================ Balance Train ===============================
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

nrow(train_df)  # 3092
nrow(test_df)   # 404


# ============================ Reference Vaiable ===============================
train_df$Location <- relevel(train_df$Location, ref = "5")  
test_df$Location <- relevel(test_df$Location, ref = "5") 



# ============================ MODELS ===============================
## BASE MODEL: include all variables
base <- glm(sub_fl ~ Pitch+Location+
              line_up_pos+
              LBSU_score+
              OPP_score+
              num_outs+
              num_baserunners+
              ball_count+
              strike_count+
              Plate_Appearance+
              home_vis+
              p_throws,
            data = train_df, family = binomial)
summary(base) # AIC 3091

## Reduced Model: Lowest AIC
model <- glm(sub_fl ~ Pitch+Location+
               line_up_pos+
              # LBSU_score+
               OPP_score+
              # num_outs+
               num_baserunners+
               ball_count+
               strike_count+
               Plate_Appearance+
               home_vis+
               p_throws,
             data = train_df, family = binomial)
summary(model) # AIC 3090

# Interaction Model: Pitch*Location 
int_model <- glm(sub_fl ~ Pitch+Location+
                   line_up_pos+
                   # LBSU_score+
                   OPP_score+
                   # num_outs+
                   num_baserunners+
                   ball_count+
                   #strike_count+
                   Plate_Appearance+
                   home_vis,
                 data = train_df, family = binomial)
summary(int_model) # AIC 2615

#--------------------------------------------------------------------------------
# Best AIC Used
#--------------------------------------------------------------------------------
summary(int_model) # AIC 2615



# ============================================================
# F1 tuning + train/test metrics, confusion matrices, and AUC
# ============================================================

# ============================ F1 Tuning ===============================

tune_f1_from_fitted_glm <- function(mod, test_df = NULL,
                                    grid = seq(0.01, 0.99, by = 0.01),
                                    guard = c(0.05, 0.95)) {
  stopifnot(inherits(mod, "glm"), family(mod)$family == "binomial")
  
  # ---- derive target/predictors from model formula ----
  fml  <- formula(mod)
  vars <- all.vars(fml)
  target <- vars[1]
  preds  <- setdiff(vars, target)
  
  # ---- training frame used at fit time ----
  mf_train <- model.frame(mod)
  y_train  <- mf_train[[target]]
  
  # ---- helpers ----
  to_logical_pos <- function(y) {
    if (is.factor(y)) {
      y == tail(levels(y), 1)      # glm() type="response" -> Pr(last level)
    } else if (is.logical(y)) y else if (is.numeric(y)) y == 1 else
      stop(sprintf("Target '%s' must be factor/logical/0-1 numeric.", target))
  }
  
  metrics_at <- function(y_true_pos, p, thr) {
    pred <- p >= thr
    TP <- sum(pred &  y_true_pos); TN <- sum(!pred & !y_true_pos)
    FP <- sum(pred & !y_true_pos); FN <- sum(!pred &  y_true_pos)
    precision <- ifelse(TP+FP==0, 0, TP/(TP+FP))
    recall    <- ifelse(TP+FN==0, 0, TP/(TP+FN))
    f1        <- ifelse(precision+recall==0, 0, 2*precision*recall/(precision+recall))
    acc       <- (TP+TN)/length(y_true_pos)
    specificity <- ifelse(TN+FP==0, 0, TN/(TN+FP))
    list(
      f1=f1, precision=precision, recall=recall, accuracy=acc,
      specificity=specificity, prop_pos_pred=mean(pred),
      TP=TP, TN=TN, FP=FP, FN=FN,
      pred=pred
    )
  }
  
  align_factor_levels <- function(mod, dat) {
    if (!is.null(mod$xlevels)) {
      for (v in names(mod$xlevels)) {
        if (v %in% names(dat)) {
          if (!is.factor(dat[[v]])) dat[[v]] <- as.factor(dat[[v]])
          trn_lvls <- mod$xlevels[[v]]
          dat[[v]] <- factor(dat[[v]], levels = union(levels(dat[[v]]), trn_lvls))
        }
      }
    }
    dat
  }
  
  # ---- TRAIN: probs & F1-opt threshold ----
  p_train_raw <- predict(mod, type = "response")
  keep_tr <- is.finite(p_train_raw) & !is.na(y_train)
  p_train <- p_train_raw[keep_tr]
  y_tr_pos <- to_logical_pos(y_train[keep_tr])
  
  valid_grid <- grid[vapply(grid, function(t) {
    pp <- mean(p_train >= t); pp >= guard[1] && pp <= guard[2]
  }, logical(1))]
  if (!length(valid_grid)) valid_grid <- grid
  
  f1s <- vapply(valid_grid, function(t) metrics_at(y_tr_pos, p_train, t)$f1, numeric(1))
  best_idx <- which(f1s == max(f1s, na.rm = TRUE))
  threshold <- valid_grid[ best_idx[ which.min(abs(valid_grid[best_idx] - 0.50)) ] ]
  
  # ---- TRAIN metrics + confusion matrix ----
  mtr <- metrics_at(y_tr_pos, p_train, threshold)
  train_metrics <- unlist(mtr[c("accuracy","precision","recall","f1","specificity","prop_pos_pred")])
  train_cm <- table(
    Predicted = factor(mtr$pred, levels = c(FALSE, TRUE), labels = c("Pred 0","Pred 1")),
    Actual    = factor(y_tr_pos,  levels = c(FALSE, TRUE), labels = c("Act 0","Act 1"))
  )
  
  # ---- TEST ----
  test_metrics <- NULL
  test_cm <- NULL
  auc_val <- NULL
  
  if (!is.null(test_df)) {
    missing <- setdiff(preds, names(test_df))
    if (length(missing))
      stop(sprintf("Missing predictors in test_df: %s", paste(missing, collapse = ", ")))
    if (!target %in% names(test_df))
      stop(sprintf("Target '%s' missing in test_df.", target))
    
    test_al <- align_factor_levels(mod, test_df)
    p_te_raw <- predict(mod, newdata = test_al, type = "response")
    keep_te  <- is.finite(p_te_raw) & !is.na(test_al[[target]])
    p_te     <- p_te_raw[keep_te]
    y_te_pos <- to_logical_pos(test_al[[target]][keep_te])
    
    mte <- metrics_at(y_te_pos, p_te, threshold)
    test_metrics <- unlist(mte[c("accuracy","precision","recall","f1","specificity","prop_pos_pred")])
    test_cm <- table(
      Predicted = factor(mte$pred, levels = c(FALSE, TRUE), labels = c("Pred 0","Pred 1")),
      Actual    = factor(y_te_pos, levels = c(FALSE, TRUE), labels = c("Act 0","Act 1"))
    )
    
    # ---- TEST AUC ----
    if (!requireNamespace("pROC", quietly = TRUE)) {
      warning("Package 'pROC' not installed. Install with install.packages('pROC') for AUC.")
      auc_val <- NA_real_
    } else {
      roc_obj <- pROC::roc(response = y_te_pos, predictor = p_te)
      auc_val <- as.numeric(pROC::auc(roc_obj))
    }
  }
  
  # ---- Return everything ----
  list(
    threshold = threshold,
    train_metrics = train_metrics,
    train_confusion_matrix = train_cm,
    test_metrics = test_metrics,
    test_confusion_matrix = test_cm,
    test_auc = auc_val
  )
}

# ============================ Testing Results ===============================
res <- tune_f1_from_fitted_glm(int_model, test_df = test_df)

# ---- OUTPUT ----
cat(sprintf("\nChosen threshold: %.2f\n", res$threshold))

cat("\nTRAIN metrics:\n")
print(round(res$train_metrics, 2))
cat("\nTRAIN confusion matrix:\n")
print(res$train_confusion_matrix)

if (!is.null(res$test_metrics)) {
  cat("\nTEST size:\n")
  print(nrow(test_df))
  cat("\nTEST confusion matrix:\n")
  print(res$test_confusion_matrix)
  cat("\nTEST metrics:\n")
  print(round(res$test_metrics, 2))
  cat(sprintf("\nTEST AUC: %.2f\n", res$test_auc))
} else {
  cat("\n(No test_df provided, so TEST results are unavailable.)\n")
}











# --- PR AUC + PR curve (robust replacement) -----------------------------------
library(PRROC)
library(plotly)

stopifnot(is.logical(train_df$sub_fl), is.logical(test_df$sub_fl))

# Force TEST factor levels to exactly the model's training levels (no union)
.align_levels <- function(mod, dat) {
  if (!is.null(mod$xlevels)) {
    for (v in names(mod$xlevels)) {
      if (v %in% names(dat)) {
        x <- as.character(dat[[v]])
        trn_lvls <- mod$xlevels[[v]]
        # Unseen levels become NA; this is intentional so we can drop them later
        dat[[v]] <- factor(x, levels = trn_lvls)
      }
    }
  }
  dat
}

# One-shot helper: predict probs, build PR curve, compute PR-AUC, plot
pr_auc_glm <- function(mod, data, split = "TEST", model_name = "Model") {
  # y_true
  y <- data$sub_fl
  stopifnot(is.logical(y))
  
  # predict with NA for rows that have unseen levels
  p <- suppressWarnings(predict(mod, newdata = data, type = "response"))
  
  # diagnostics
  cat(sprintf("\n[%s] rows: %d | NA preds: %d | pos: %d | neg: %d\n",
              split, nrow(data), sum(!is.finite(p) | is.na(p)),
              sum(y, na.rm = TRUE), sum(!y, na.rm = TRUE)))
  
  # keep only rows with valid y and p
  keep <- is.finite(p) & !is.na(p) & !is.na(y)
  dropped <- sum(!keep)
  if (dropped > 0) cat(sprintf("[%s] Dropped %d rows (NA/Inf y or p, or unseen levels)\n",
                               split, dropped))
  
  y <- y[keep]; p <- p[keep]
  if (length(y) == 0L) {
    message(sprintf("[%s] No usable rows after filtering.", split))
    return(NULL)
  }
  
  n_pos <- sum(y); n_neg <- sum(!y)
  cat(sprintf("[%s] After filter -> pos: %d | neg: %d | kept: %d\n",
              split, n_pos, n_neg, length(y)))
  
  if (n_pos == 0 || n_neg == 0) {
    message(sprintf("[%s] Only one class present after filtering; PR-AUC undefined.", split))
    return(NULL)
  }
  
  sc_pos <- p[y]
  sc_neg <- p[!y]
  pr_obj <- PRROC::pr.curve(scores.class0 = sc_pos, scores.class1 = sc_neg, curve = TRUE)
  pr_auc <- as.numeric(pr_obj$auc.integral)
  base   <- mean(y)
  
  pr_df <- data.frame(
    recall    = pr_obj$curve[,1],
    precision = pr_obj$curve[,2],
    threshold = pr_obj$curve[,3]
  )
  
  fig <- plot_ly(pr_df, x = ~recall, y = ~precision,
                 type = "scatter", mode = "lines",
                 name = paste0(model_name, sprintf(" (PR-AUC = %.3f)", pr_auc))) |>
    add_lines(x = c(0, 1), y = c(base, base),
              name = sprintf("Prevalence = %.2f", base),
              line = list(dash = "dash")) |>
    layout(title = paste(model_name, split, "Precision–Recall"),
           xaxis = list(title = "Recall", range = c(0,1)),
           yaxis = list(title = "Precision", range = c(0,1)),
           legend = list(orientation = "h"))
  
  list(fig = fig, pr_auc = pr_auc)
}

# ---------- TRAIN ----------
glm_train_frame <- model.frame(int_model)
y_train_glm     <- glm_train_frame$sub_fl
p_train_glm     <- predict(int_model, type = "response")
cat("GLM train prob summary:\n"); print(summary(p_train_glm))

pr_train_glm <- pr_auc_glm(int_model, glm_train_frame, split = "TRAIN",
                           model_name = "RHH Final (glm int_model)")
if (!is.null(pr_train_glm)) {
  pr_train_glm$fig
  cat(sprintf("PR-AUC (TRAIN, glm): %.3f\n", pr_train_glm$pr_auc))
}

# ---------- TEST (align factor levels to model) ----------
test_df_glm <- .align_levels(int_model, test_df)
p_test_glm  <- suppressWarnings(predict(int_model, newdata = test_df_glm, type = "response"))
cat("GLM test  prob summary:\n"); print(summary(p_test_glm))

pr_test_glm <- pr_auc_glm(int_model, test_df_glm, split = "TEST",
                          model_name = "RHH Final (glm int_model)")
if (!is.null(pr_test_glm)) {
  print(pr_test_glm$fig)   # <- required in scripts / inside if()
  cat(sprintf("PR-AUC (TEST, glm): %.3f\n", pr_test_glm$pr_auc))
}
