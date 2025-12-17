#' Predict cancer status (healthy vs cancer)
#'
#' @param data A data.frame with rows as samples and columns as gene expression features (ENSG IDs).
#'             Must contain all 84 required features.
#' @param model An optional pre-loaded randomForest model. If NULL, loads from package extdata.
#'
#' @return Factor vector with levels "healthy" and "cancer"
#' @export
#'
#' @examples
#' \dontrun{
#' pred <- predict_cancer_status(new_samples)
#' }
predict_cancer_status <- function(data, model = NULL) {
  required <- binary_features
  if (!all(required %in% colnames(data))) {
    missing <- setdiff(required, colnames(data))
    stop("Missing required features: ", paste(head(missing, 10), collapse = ", "), 
         if (length(missing) > 10) " ..." else "")
  }
  
  data <- data[, required, drop = FALSE]
  
  if (is.null(model)) {
    model_path <- system.file("extdata", "rf_binary_model.rds", package = "PlateletCancerClassifier")
    if (model_path == "") stop("Model file not found. Place rf_binary_model.rds in inst/extdata/")
    model <- readRDS(model_path)
  }
  
  factor(predict(model, newdata = data, type = "response"), levels = c("healthy", "cancer"))
}

