

# -------------------------------PREPARATION-------------------------------------
# -------------------------------------------------------------------------------

# Clear workspace
rm(list = ls(all = TRUE))

# Set working directory
setwd("F:/Hohenheim/SS 25-26/Statistical learning/forecast")

# packages we will use in the course
pac <- c("glmnet","RRF","pROC","ranger","caret","moments", "sandwich", "lmtest", "car", 
         "plm", "ivreg", "dynlm", "forecast", "urca", "texreg","dplyr")

# install and/or load packages
checkpac <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
  }
  require(x, character.only = TRUE)
}

# check if packages are install yet
suppressWarnings(sapply(pac, checkpac))

#Load train, test set and submission set.
train <-read.csv(file="amex_train.csv", header = TRUE, sep=",")
test <-read.csv(file="amex_validation.csv", header = TRUE, sep = ",")
submission <- read.csv(file = "amex_submission.csv", header = TRUE, sep=",")
# -------------------------------------------------------------------------------
# ---------------------------END OF PREPARATION----------------------------------


# ----------------------------DATA WRANGLING-------------------------------------
# -------------------------------------------------------------------------------

# Remove character-type and no variation variables from 'train'
counts<-table(train$D_63)
counts # 'XL','XM' and 'XZ' have very few observation 
low_cats <- names(sort(counts, decreasing = FALSE))[1:4]
train$D_63_2 <- ifelse(train$D_63 %in% low_cats, "Other", as.character(train$D_63))
train$D_63_2 <- as.factor(train$D_63_2)
train$D_63_2CO <- as.integer(train$D_63_2=="CO")
train$D_63_2CR <- as.integer(train$D_63_2=="CR")
train$D_63_2Other <- as.integer(train$D_63_2=="Other")
train$D_63_2 <-NULL
train$D_63 <-NULL
train$D_64 <-NULL
train$D_66 <-NULL   #no variation
train$S_2 <-NULL #time
train$X <-NULL
train$ID <-NULL

# Normalizing the train set using Min-Max Scaling 
df<-as.data.frame(train)
cols_scale <-1:ncol(df)
process_subset <- preProcess(df[, cols_scale], method = c("range"))
scaled_subset <- predict(process_subset, df[, cols_scale])
train_scaled <- df
train_scaled[, cols_scale] <- scaled_subset

# Remove character-type and no variation variables from 'test'
counts <- table(test$D_63)
counts
low_cats <- names(sort(counts, decreasing = FALSE))[1:4]
test$D_63_2 <- ifelse(test$D_63 %in% low_cats, "Other", as.character(test$D_63))
test$D_63_2 <- as.factor(test$D_63_2)
test$D_63_2CO <- as.integer(test$D_63_2=="CO")
test$D_63_2CR <- as.integer(test$D_63_2=="CR")
test$D_63_2Other <- as.integer(test$D_63_2=="Other")
test$D_63_2 <-NULL
test$D_63 <- NULL
test$D_64 <- NULL
test$D_66 <- NULL  # no variation
test$S_2 <-NULL
test$X <-NULL
test$ID <-NULL

# Normalizing the test set using Min-Max Scaling 
df_test <- as.data.frame(test)
cols_scale_test <- 1:ncol(df_test)
process_subset <- preProcess(df_test[, cols_scale_test], method = c("range"))
scaled_subset <- predict(process_subset, df_test[, cols_scale_test])
test_scaled <- df_test
test_scaled[, cols_scale_test] <- scaled_subset

# Ensure target is a factor with levels "0" and "1"
df<-train_scaled
df$target <- as.factor(df$target)

# Stratified hold-out: 85% for CV/tuning, 15% for final validation
set.seed(123)
holdout_idx <- createDataPartition(df$target, p = 0.15, list = FALSE)
validation_set <- df[holdout_idx, ]
train_tune      <- df[-holdout_idx, ]

# Check ratios:
cat("Overall default ratio:", mean(df$target == "1"), "\n")
cat("Train-tune default ratio:", mean(train_tune$target == "1"), "\n")
cat("Validation default ratio:", mean(validation_set$target == "1"), "\n")
# -------------------------------------------------------------------------------
# -------------------------END OF DATA WRANGLING---------------------------------


# ----------------------------BASELINE MODEL-------------------------------------
# -------------------------------------------------------------------------------

# 1. NAIVE LOGIT ESTIMATION------------------------------------------------------ 
# Create class_weights.
tab <- table(train_tune$target)
w0 <- 1 #Choose 1 as baseline. 
w1 <- as.numeric(tab["0"]) / as.numeric(tab["1"])
class_weights_vec <- ifelse(as.numeric(train_tune$target) == 1, w1, w0)

# Estimate logit using all features.
log_mod <-glm(target~., data=train_tune, weights = class_weights_vec,family = binomial)
probs <- predict(log_mod, newdata = validation_set, type = "response")

# Draw ROC Curve and extract the AUC.
roc_cu<-roc(response = validation_set$target,
            predictor = probs,
            levels = c("0", "1"))
plot(roc_cu, main="ROC Curve", col="#1c61b6")
cat("Validation-set ROC AUC:", as.numeric(auc(roc_cu)), "\n")

# 2. LOGIT and Lasso (weighted, randomly choose Lambda)--------------------------
# Convert factors to dummies (glmnet needs numeric input)
x_mat <- model.matrix(target ~ ., data = train_tune)[, -1]  # Remove intercept
y_vec <- as.numeric(as.character(train_tune$target))        # Must be numeric 0/1 for glmnet

# LASSO for feature selection
set.seed(42)
lasso <- glmnet(x = x_mat, y = y_vec, standardize = FALSE, weights = class_weights_vec, 
                family = "binomial",alpha = 1) # I already standardized the data above.

# Get selected features.
coef_lasso <- coef(lasso, s = lasso$lambda[41]) # choose randomly.
selected_vars <- rownames(coef_lasso)[which(coef_lasso != 0)]
selected_vars <- selected_vars[!selected_vars %in% "(Intercept)"]
selected_vars
# Refit the logit model with the subset train_tune data.
fml_lasso<-reformulate(selected_vars, response = "target")
log_mod_lasso<-glm(fml_lasso, data=train_tune,family = binomial)
probs_lasso <- predict(log_mod_lasso, newdata=validation_set, type="response")

# Draw ROC Curve and extract the AUC.
roc_lasso <-roc(response = validation_set$target,
                predictor = probs_lasso,
                levels = c("0", "1"))
plot(roc_lasso, main="ROC Curve", col="#1c61b6")
cat("Validation-set ROC AUC:", as.numeric(auc(roc_lasso)), "\n")

# 3. Using Random Forest (weighted) with default setting-------------------------
# Create class_weights.
class_weights <- c("0" = w0, "1" = w1) 

# Use a naive RF with default setting in R. 
rf_naive <- ranger(
  formula = target ~ .,
  data = train_tune,
  probability = TRUE,
  class.weights = class_weights,
  num.trees = 500,
  splitrule = "gini"
)

# Make estimation on validation_set
probs_rf_naive <- predict(rf_naive, data = validation_set)
pred_rf_naive <-probs_rf_naive$predictions[, "1"]

#Draw the ROC and extract AUC
roc_rf_naive <- roc(response=validation_set$target, 
                    predictor= pred_rf_naive,
                    levels = c("0", "1"))
plot(roc_rf_naive, main="ROC Curve", col="#1c61b6")
cat("Validation-set ROC AUC:", as.numeric(auc(roc_rf_naive)), "\n")

# 4. Summary---------------------------------------------------------------------
# Create a data frame that contains all the AUC of the 3 models.
# Surprisingly, logit with lasso  gives the best AUC.
name_vec <-c("AUC from naive logit model (weighted)", 
             "AUC from logit with lasso (weighted)", 
             "AUC from Random Forest (weighted) with default parameters")
num_vec <-c(as.numeric(auc(roc_cu)), 
            as.numeric(auc(roc_lasso)), 
            as.numeric(auc(roc_rf_naive)))
AUC_Baseline <-data.frame(
  name  = name_vec,
  value = num_vec,
  stringsAsFactors = FALSE
)
AUC_Baseline
# -------------------------------------------------------------------------------
# -----------------------------END OF BASELINE-----------------------------------




# --------------------------HYPERPARAMETERS TUNING-------------------------------
# -------------------------------------------------------------------------------

# 1. Logit + LASSO with Cross Validation to choose the optimal Lambda------------
# Convert factors to dummies (glmnet needs numeric input)
x_mat <- model.matrix(target ~.-1, data = train_tune) # Remove intercept
y_vec <- as.numeric(as.character(train_tune$target))  # Must be numeric 0/1 for glmnet

# Create weight for cv.glmnet
tab <- table(y_vec)  # names "0" and "1"
N0 <- as.numeric(tab["0"])
N1 <- as.numeric(tab["1"])
w0 <- 1
w1 <- N0 / N1
weights <- ifelse(y_vec == 1, w1, w0)
n_obs <- nrow(train_tune)

# Stratified folds for cv.glmnet
set.seed(123)
folds_list <- createFolds(train_tune$target, k = 5, returnTrain = FALSE)
foldid <- integer(n_obs)
for (i in seq_along(folds_list)) {
  foldid[folds_list[[i]]] <- i
}

# 1.2 LASSO for feature selection------------------------------------------------
set.seed(42)
cv_lasso <- cv.glmnet(x = x_mat, 
                      y = y_vec, 
                      family = "binomial", 
                      alpha = 1,
                      weights = weights,
                      foldid = foldid,
                      type.measure = "auc",
                      nfolds = 5)

# Get selected features
coef_lasso_cv <- coef(cv_lasso, s = "lambda.min")
selected_vars_cv <- rownames(coef_lasso_cv)[which(coef_lasso_cv != 0)]
selected_vars_cv <- selected_vars_cv[!selected_vars_cv %in% "(Intercept)"]

# 1.3 Refit the logit model------------------------------------------------------
fml_lasso_cv <- reformulate(selected_vars_cv, response = "target")
log_mod_lasso_cv<-glm(target~., data=train_tune,family = binomial)
probs_lasso_cv <- predict(log_mod_lasso_cv, newdata=validation_set, type="response")

# Draw ROC Curve and extract the AUC.
roc_lasso_cv <-roc(response = validation_set$target,
                predictor = probs_lasso_cv,
                levels = c("0", "1"))
plot(roc_lasso_cv, main="ROC Curve", col="#1c61b6")
cat("Validation-set ROC AUC:", as.numeric(auc(roc_lasso_cv)), "\n")

# 2. Random Forest (weighted) with Cross Validation for parameters tuning--------
# 2.1 Hyperparameters grid setup ------------------------------------------------
# Running this on my laptop tool around 10 mins.
# Number of predictors p = total columns minus 1 target column:
p <- ncol(train_tune) - 1  

# Random Forest tuning: mtry based on p, plus min.node.size
mtry_vals <- unique(floor(c(sqrt(p), p/2))) 
tune_grid <- expand.grid(
  mtry = mtry_vals,
  splitrule = "gini",
  min.node.size = c(1, 5, 10),
  stringsAsFactors = FALSE
)

# 2.2 Create Stratified folds for CV---------------------------------------------
set.seed(123)
k <- 5
folds_train_idx <- createFolds(train_tune$target, k = k, returnTrain = TRUE)

# Sanity check: ensure each validation fold has at least one positive
for (i in seq_len(k)) {
  tr_idx <- folds_train_idx[[i]]
  tr <- train_tune[tr_idx, ]
  va <- train_tune[-tr_idx, ]
  cat(sprintf(
    "Fold %d: train positives = %d/%d (%.4f); val positives = %d/%d (%.4f)\n",
    i,
    sum(tr$target == "1"), nrow(tr), mean(tr$target == "1"),
    sum(va$target == "1"), nrow(va), mean(va$target == "1")
  ))
}

# 2.3 Cross-validation loop -----------------------------------------------------
# Create a list to store results
results <- tune_grid
results$mean_auc <- NA_real_
results$sd_auc   <- NA_real_

# CV loop
for (j in seq_len(nrow(tune_grid))) {
  params <- tune_grid[j, ]
  fold_aucs <- numeric(k)
  
  for (i in seq_len(k)) {
    # Extract training and validation subsets for fold i
    tr_idx <- folds_train_idx[[i]]
    train_data <- train_tune[tr_idx, ]
    val_data   <- train_tune[-tr_idx, ]
    
    # Compute class weights on train_data
    tab <- table(train_data$target)
    w0 <- 1
    w1 <- as.numeric(tab["0"]) / as.numeric(tab["1"])
    class_weights <- c("0" = w0, "1" = w1)
    
    # Fit weighted Random Forest on train_data
    rf <- ranger(
      formula = target ~ .,
      data = train_data,
      probability = TRUE,
      class.weights = class_weights,
      num.trees = 100,                   
      mtry = params$mtry,
      min.node.size = params$min.node.size,
      splitrule = params$splitrule
    )
    
    # Predict on validation fold
    pred <- predict(rf, data = val_data)
    probs <- pred$predictions[, "1"]   # probability of positive class
    
    # Compute ROC AUC on this fold
    roc_obj <- roc(response = val_data$target,
                   predictor = probs,
                   levels = c("0", "1"))
    fold_aucs[i] <- as.numeric(auc(roc_obj))
  } # end of 5-fold CV for 1 param setting. We have 6 settings.
  
# Aggregate CV results for this hyperparameter setting
results$mean_auc[j] <- mean(fold_aucs)
results$sd_auc[j]   <- sd(fold_aucs)
cat(sprintf(
  "Setting %d/%d: mtry=%d, min.node.size=%d -> mean AUC=%.4f ± %.4f\n",
  j, nrow(tune_grid),
  params$mtry, params$min.node.size,
  results$mean_auc[j], results$sd_auc[j]
 ))
} # End of 6 param settings

# 2.4 Select best params---------------------------------------------------------
best_idx <- which.max(results$mean_auc)
best_params <- results[best_idx, ]
cat("Best hyperparameters:\n")
print(best_params)

# 2.5 Train on full train_tune (17k rows) and evaluate on validation_set---------
# Compute class weights on entire train_tune
tab_tt <- table(train_tune$target)
cw_tt <- c("0" = 1, "1" = as.numeric(tab_tt["0"]) / as.numeric(tab_tt["1"]))
           
# Fit Random Forest on train_tune with best hyperparams, more trees
final_tt <- ranger(
             formula = target ~ .,
             data = train_tune,
             probability = TRUE,
             class.weights = cw_tt,
             num.trees = 500,                       
             mtry = best_params$mtry,
             min.node.size = best_params$min.node.size,
             splitrule = best_params$splitrule
           )
           
# Evaluate on hold-out validation_set
pred_val <- predict(final_tt, data = validation_set)$predictions[, "1"]
roc_rf_cv <- roc(response = validation_set$target,
                          predictor = pred_val,
                          levels = c("0", "1"))
plot(roc_rf_cv, main="ROC Curve", col="#1c61b6")
cat("Validation-set ROC AUC:", as.numeric(auc(roc_rf_cv)), "\n")

# 3. Random Forest (weighted) with Cross Validation for parameters tuning--------
# 3.1 Hyperparameters grid setup ------------------------------------------------
# Running this on my laptop tool around 8 mins.
p <- length(selected_vars_cv)  #choose the features selected by lasso+cv.

# Random Forest tuning: mtry based on p, plus min.node.size
mtry_vals <- unique(floor(c(sqrt(p), p/2))) 
tune_grid <- expand.grid(
  mtry = mtry_vals,
  splitrule = "gini",
  min.node.size = c(1, 5, 10),
  stringsAsFactors = FALSE
)

# 3.2 Create Stratified folds for CV---------------------------------------------
set.seed(123)
k <- 5
folds_train_idx <- createFolds(train_tune$target, k = k, returnTrain = TRUE)

# Sanity check: ensure each validation fold has at least one positive
for (i in seq_len(k)) {
  tr_idx <- folds_train_idx[[i]]
  tr <- train_tune[tr_idx, ]
  va <- train_tune[-tr_idx, ]
  cat(sprintf(
    "Fold %d: train positives = %d/%d (%.4f); val positives = %d/%d (%.4f)\n",
    i,
    sum(tr$target == "1"), nrow(tr), mean(tr$target == "1"),
    sum(va$target == "1"), nrow(va), mean(va$target == "1")
  ))
}

# 3.3 Cross-validation loop ----------------------------------------------------
  # Create a list to store results
  results <- tune_grid
  results$mean_auc <- NA_real_
  results$sd_auc   <- NA_real_
  
  # CV loop
  for (j in seq_len(nrow(tune_grid))) {
    params <- tune_grid[j, ]
    fold_aucs <- numeric(k)
    
    for (i in seq_len(k)) {
      # Extract training and validation subsets for fold i
      tr_idx <- folds_train_idx[[i]]
      train_data <- train_tune[tr_idx, ]
      val_data   <- train_tune[-tr_idx, ]
      
      # Compute class weights on train_data
      tab <- table(train_data$target)
      w0 <- 1
      w1 <- as.numeric(tab["0"]) / as.numeric(tab["1"])
      class_weights <- c("0" = w0, "1" = w1)
      
      # Fit weighted Random Forest on train_data
      rf <- ranger(
        formula = fml_lasso_cv,
        data = train_data,
        probability = TRUE,
        class.weights = class_weights,
        num.trees = 100,                   
        mtry = params$mtry,
        min.node.size = params$min.node.size,
        splitrule = params$splitrule
      )
      
      # Predict on validation fold
      pred <- predict(rf, data = val_data)
      probs <- pred$predictions[, "1"]   # probability of positive class
      
      # Compute ROC AUC on this fold
      roc_obj <- roc(response = val_data$target,
                     predictor = probs,
                     levels = c("0", "1"))
      fold_aucs[i] <- as.numeric(auc(roc_obj))
    } # end of 5-fold CV for 1 param setting. We have 6 settings.

    # Aggregate CV results for this hyperparameter setting
    results$mean_auc[j] <- mean(fold_aucs)
    results$sd_auc[j]   <- sd(fold_aucs)
    cat(sprintf(
      "Setting %d/%d: mtry=%d, min.node.size=%d -> mean AUC=%.4f ± %.4f\n",
      j, nrow(tune_grid),
      params$mtry, params$min.node.size,
      results$mean_auc[j], results$sd_auc[j]
    ))
  } # End of 6 param settings
  
# 3.4 Select best params---------------------------------------------------------
  
  best_idx <- which.max(results$mean_auc)
  best_params <- results[best_idx, ]
  cat("Best hyperparameters:\n")
  print(best_params)
  
# 3.5 Train on full train_tune (17k rows) and evaluate on validation_set---------
  
  # Compute class weights on entire train_tune
  tab_tt <- table(train_tune$target)
  cw_tt <- c("0" = 1, "1" = as.numeric(tab_tt["0"]) / as.numeric(tab_tt["1"]))
  
  # Fit Random Forest on train_tune with best hyperparams, more trees
  final_tt <- ranger(
    formula = fml_lasso_cv,
    data = train_tune,
    probability = TRUE,
    class.weights = cw_tt,
    num.trees = 500,                       
    mtry = best_params$mtry,
    min.node.size = best_params$min.node.size,
    splitrule = best_params$splitrule
  )
  
  # Evaluate on hold-out validation_set
  pred_val <- predict(final_tt, data = validation_set)$predictions[, "1"]
  roc_rf_lasso_cv <- roc(response = validation_set$target,
                 predictor = pred_val,
                 levels = c("0", "1"))
  plot(roc_rf_lasso_cv, main="ROC Curve", col="#1c61b6")
  cat("Validation-set ROC AUC:", as.numeric(auc(roc_rf_lasso_cv)), "\n")
  
# 3.6 Compare AUC across 6 specifications           
roc_list <- list(
  "Naive logit"           = roc_cu,
  "Naive logit + LASSO"      = roc_lasso,
  "Naive Random Forest"         = roc_rf_naive,
  "Logit + LASSO (CV)"         = roc_lasso_cv,
  "Random Forest (CV)" = roc_rf_cv,
  "Random Forest + Lasso (CV)"    = roc_rf_lasso_cv
)

auc_values <- sapply(roc_list, function(x) as.numeric(auc(x)))

results_df <- data.frame(
  Model = names(auc_values),
  AUC   = auc_values,
  stringsAsFactors = FALSE
)
results_df <- results_df[order(-results_df$AUC), ]
rownames(results_df) <- NULL
print(results_df) # Choose RF+LASSO (CV) because it gives the highest AUC

# 3.7 Final fit with the full training set (20k rows) and make predictions-------
# Choose RF+LASSO (CV) because it gives the highest AUC.
# Compute class weights on full train_scaled.
tab_full <- table(df$target)
cw_full <- c("0" = 1, "1" = as.numeric(tab_full["0"]) / as.numeric(tab_full["1"]))

# Fit final model on train_scaled.
final_model <- ranger(
  formula = fml_lasso_cv,
  data = df,
  probability = TRUE,
  class.weights = cw_full,
  num.trees = 500,
  mtry = best_params$mtry,
  min.node.size = best_params$min.node.size,
  splitrule = best_params$splitrule
)

# Make prediction on test_scaled and append the probability of "1" to the file.
pred_val <- predict(final_model, data = test_scaled)$predictions[, "1"] 
#predict will ignore the features that are not included in final_model.
studentid <- "1098729"
submission_df <- data.frame(
  studentid     = studentid,
  ID            = submission$ID,
  PD = pred_val,
  stringsAsFactors = FALSE
)
head(submission_df)
write.csv(submission_df, "amex_submission_forecasted.csv")
# -------------------------------------------------------------------------------
# ------------------------------END OF TUNING------------------------------------

