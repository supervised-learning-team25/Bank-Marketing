# Function for model selection

k_fold_mod <- function(data, target_col, model_formula, k = 10) {
  
  set.seed(123)
  folds <- createFolds(data[[target_col]], k = k, list = TRUE, returnTrain = FALSE)
  
  acc_best_vec     <- numeric(k)
  f1_best_vec      <- numeric(k)
  auc_vec          <- numeric(k)
  aic_vec          <- numeric(k)
  acc_thresh_vec   <- numeric(k)
  f1_thresh_vec    <- numeric(k)
  sensitivity_vec  <- numeric(k)
  specificity_vec  <- numeric(k)
  
  for (i in 1:k) {
    test_idx <- folds[[i]]
    train_fold <- data[-test_idx, ]
    test_fold  <- data[test_idx, ]
    
    # Fit model
    fitted_model <- glm(model_formula, data = train_fold, family = binomial)
    pred_probs_train <- predict(fitted_model, newdata = train_fold, type = "response")
    actual_train <- train_fold[[target_col]]
    
    # Threshold search
    thresholds <- seq(0.01, 0.99, by = 0.01)
    acc_scores <- numeric(length(thresholds))
    f1_scores  <- numeric(length(thresholds))
    
    for (j in seq_along(thresholds)) {
      threshold <- thresholds[j]
      preds <- ifelse(pred_probs_train > threshold, 1, 0)
      acc_scores[j] <- mean(preds == actual_train)
      
      cm <- table(Predicted = preds, Actual = actual_train)
      precision <- ifelse("1" %in% rownames(cm) && "1" %in% colnames(cm),
                          cm["1", "1"] / sum(cm["1", ]), 0)
      recall <- ifelse("1" %in% rownames(cm) && "1" %in% colnames(cm),
                       cm["1", "1"] / sum(cm[, "1"]), 0)
      f1 <- ifelse((precision + recall) > 0,
                   2 * (precision * recall) / (precision + recall), 0)
      f1_scores[j] <- f1
    }
    
    # Best thresholds
    best_acc_threshold <- thresholds[which.max(acc_scores)]
    best_f1_threshold  <- thresholds[which.max(f1_scores)]
    acc_thresh_vec[i] <- best_acc_threshold
    f1_thresh_vec[i]  <- best_f1_threshold
    
    # Test predictions
    pred_probs_test <- predict(fitted_model, newdata = test_fold, type = "response")
    actual_test <- test_fold[[target_col]]
    
    pred_acc <- ifelse(pred_probs_test > best_acc_threshold, 1, 0)
    pred_f1  <- ifelse(pred_probs_test > best_f1_threshold, 1, 0)
    acc_best_vec[i] <- mean(pred_acc == actual_test)
    
    # Confusion matrix for F1 threshold
    cm <- table(Predicted = pred_f1, Actual = actual_test)
    
    tp <- ifelse("1" %in% rownames(cm) && "1" %in% colnames(cm), cm["1", "1"], 0)
    tn <- ifelse("0" %in% rownames(cm) && "0" %in% colnames(cm), cm["0", "0"], 0)
    fp <- ifelse("1" %in% rownames(cm) && "0" %in% colnames(cm), cm["1", "0"], 0)
    fn <- ifelse("0" %in% rownames(cm) && "1" %in% colnames(cm), cm["0", "1"], 0)
    
    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall    <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    
    f1_best_vec[i] <- ifelse((precision + recall) > 0,
                             2 * (precision * recall) / (precision + recall), 0)
    
    sensitivity_vec[i] <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
    specificity_vec[i] <- ifelse((tn + fp) > 0, tn / (tn + fp), NA)
    
    auc_vec[i] <- tryCatch({
      roc_obj <- roc(actual_test, pred_probs_test)
      as.numeric(auc(roc_obj))
    }, error = function(e) NA)
    
    aic_vec[i] <- AIC(fitted_model)
  }
  
  return(list(
    Accuracy_at_best_threshold = paste0(round(mean(acc_best_vec, na.rm = TRUE), 4),
                                        " (threshold = ", round(mean(acc_thresh_vec, na.rm = TRUE), 2), ")"),
    F1_at_best_threshold       = paste0(round(mean(f1_best_vec, na.rm = TRUE), 4),
                                        " (threshold = ", round(mean(f1_thresh_vec, na.rm = TRUE), 2), ")"),
    Sensitivity                = round(mean(sensitivity_vec, na.rm = TRUE), 4),
    Specificity                = round(mean(specificity_vec, na.rm = TRUE), 4),
    AUC                        = round(mean(auc_vec, na.rm = TRUE), 4),
    AIC                        = round(mean(aic_vec, na.rm = TRUE), 2)
  ))
}

evaluate_threshold <- function(probs, target, threshold) {
  pred <- ifelse(probs > threshold, 1, 0)
  cm <- table(Predicted = pred, Actual = target)
  
  tp <- ifelse("1" %in% rownames(cm) && "1" %in% colnames(cm), cm["1", "1"], 0)
  tn <- ifelse("0" %in% rownames(cm) && "0" %in% colnames(cm), cm["0", "0"], 0)
  fp <- ifelse("1" %in% rownames(cm) && "0" %in% colnames(cm), cm["1", "0"], 0)
  fn <- ifelse("0" %in% rownames(cm) && "1" %in% colnames(cm), cm["0", "1"], 0)
  
  accuracy    <- (tp + tn) / (tp + tn + fp + fn)
  precision   <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall      <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), 0)
  f1          <- ifelse((precision + recall) > 0,
                        2 * (precision * recall) / (precision + recall), 0)
  
  return(list(
    Threshold   = threshold,
    Accuracy    = round(accuracy, 4),
    F1          = round(f1, 4),
    Sensitivity = round(recall, 4),
    Specificity = round(specificity, 4)
  ))
}