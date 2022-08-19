#' @title DecisionTreeClassifier
#'
#' @description Wrapper R6 Class of rpart::rpart function that can be used for LESSRegressor and LESSClassifier
#'
#' @param min_samples_split The minimum number of observations that must exist in a node in order for a split to be attempted (defaults to 2).
#' @param min_samples_leaf The minimum number of observations in any terminal (leaf) node (defaults to 1).
#' @param cp Complexity Parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
#' This means that the overall R-squared must increase by cp at each step. The main role of this parameter is
#' to save computing time by pruning off splits that are obviously not worthwhile. (defaults to 0.001)
#' @param max_depth The maximum depth of any node of the final tree, with the root node counted as depth 0.
#' Values greater than 30 will give nonsense results on 32-bit machines.
#'
#' @return R6 Class of DecisionTreeClassifier
#' @importFrom rpart rpart
#' @seealso [rpart::rpart()]
#' @export
DecisionTreeClassifier <- R6::R6Class(classname = "DecisionTreeClassifier",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "classifier",
                                       model = NULL,
                                       min_samples_split = NULL,
                                       min_samples_leaf = NULL,
                                       cp = NULL,
                                       max_depth = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of DecisionTreeClassifier
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' dt <- DecisionTreeClassifier$new(min_samples_split = 10)
                                       #' dt <- DecisionTreeClassifier$new(min_samples_leaf = 6, cp = 0.01)
                                       initialize = function(min_samples_split = 2, min_samples_leaf = 1, cp = 0.001, max_depth = 30){
                                         private$min_samples_split = min_samples_split
                                         private$min_samples_leaf = min_samples_leaf
                                         private$cp = cp
                                         private$max_depth = max_depth
                                       },
                                       #' @description Builds a decision tree regressor from the training set (X, y).
                                       #'
                                       #' @param X 2D matrix or dataframe that includes predictors
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                       #'
                                       #' @return Fitted R6 Class of DecisionTreeClassifier
                                       #'
                                       #' @examples
                                       #' data(iris)
                                       #' split_list <- train_test_split(iris, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' dt$fit(X_train, y_train)
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         private$model <- rpart::rpart(y ~ ., data = df, method = "class", minsplit = private$min_samples_split,
                                                                       minbucket = private$min_samples_leaf, cp = private$cp, maxdepth = private$max_depth)
                                         private$isFitted <- TRUE
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return The predict values.
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' dt$fit(X_train, y_train)
                                       #' preds <- dt$predict(X_test)
                                       #'
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' preds <- dt$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- DecisionTreeClassifier$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(caret::confusionMatrix(data=preds, reference = factor(y_test)))
                                       predict = function(X0) {
                                         check_is_fitted(self)
                                         data <- prepareXset(X0)
                                         y_pred <- predict(private$model, data, type = "class")
                                         return(y_pred)
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     ))

#' @title Support Vector Classification
#'
#' @description Wrapper R6 Class of e1071::svm function that can be used for LESSRegressor and LESSClassifier
#'
#' @return R6 Class of SVC
#' @seealso [e1071::svm()]
#' @importFrom e1071 svm
#' @export
SVC <- R6::R6Class(classname = "SVC",
                   inherit = SklearnEstimator,
                   private = list(
                     estimator_type = "classifier",
                     model = NULL
                   ),
                   public = list(
                     #' @description Fit the SVM model from the training set (X, y).
                     #'
                     #' @param X 2D matrix or dataframe that includes predictors
                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                     #'
                     #' @return Fitted R6 Class of SVC
                     #'
                     #' @examples
                     #' data(iris)
                     #' split_list <- train_test_split(iris, test_size =  0.3)
                     #' X_train <- split_list[[1]]
                     #' X_test <- split_list[[2]]
                     #' y_train <- split_list[[3]]
                     #' y_test <- split_list[[4]]
                     #'
                     #' svc <- SVC$new()
                     #' svc$fit(X_train, y_train)
                     fit = function(X, y){
                       df <- prepareDataset(X, y)
                       df$y <- as.factor(df$y)
                       private$model <- e1071::svm(y ~ ., data = df, type = 'C-classification')
                       private$isFitted <- TRUE
                       invisible(self)
                     },
                     #' @description Predict regression value for X0.
                     #'
                     #' @param X0 2D matrix or dataframe that includes predictors
                     #'
                     #' @return The predict values.
                     #'
                     #' @examples
                     #' svc <- SVC$new()
                     #' svc$fit(X_train, y_train)
                     #' preds <- svc$predict(X_test)
                     #'
                     #' svc <- SVC$new()
                     #' preds <- svc$fit(X_train, y_train)$predict(X_test)
                     #'
                     #' preds <- SVC$new()$fit(X_train, y_train)$predict(X_test)
                     #' print(caret::confusionMatrix(data=preds, reference = factor(y_test)))
                     predict = function(X0){
                       check_is_fitted(self)
                       data <- prepareXset(X0)
                       y_pred <- predict(private$model, data)
                       return(y_pred)
                     },
                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                     get_estimator_type = function() {
                       return(private$estimator_type)
                     }
                   ))

