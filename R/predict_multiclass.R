#' Predict cancer type (6 classes)
#'
#' @param data A data.frame with rows as samples and columns as gene expression features (ENSG IDs).
#'             Must contain all 202 required features.
#' @param model An optional pre-loaded xgboost model. If NULL, loads from package extdata.
#'
#' @return Factor vector with levels Breast, CRC, GBM, Hepatobiliary, Lung, Pancreas
#' @export
#'
#' @examples
#' \dontrun{
#' pred <- predict_cancer_type(cancer_samples)
#' }
predict_cancer_type <- function(data, model = NULL) {
  required <- multiclass_features
  if (!all(required %in% colnames(data))) {
    missing <- setdiff(required, colnames(data))
    stop("Missing required features: ", paste(head(missing, 10), collapse = ", "), 
         if (length(missing) > 10) " ..." else "")
  }
  
  data <- as.matrix(data[, required, drop = FALSE])
  
  if (is.null(model)) {
    model_path <- system.file("extdata", "xgb_multiclass_model.rds", package = "PlateletCancerClassifier")
    if (model_path == "") stop("Model file not found. Place xgb_multiclass_model.rds in inst/extdata/")
    model <- readRDS(model_path)
  }
  
  prob <- predict(model, newdata = data, reshape = TRUE)
  pred_class <- apply(prob, 1, which.max)
  factor(multiclass_levels[pred_class], levels = multiclass_levels)
}

multiclass_levels <- c("Breast", "CRC", "GBM", "Hepatobiliary", "Lung", "Pancreas")

