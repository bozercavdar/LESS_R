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
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
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
                                       #' @return Factor of the predict classes.
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
                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
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
                     #' @return Factor of the predict classes.
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

#' @title RandomForestClassifier
#'
#' @description Wrapper R6 Class of randomForest::randomForest function that can be used for LESSRegressor and LESSClassifier
#'
#' @param n_estimators Number of trees to grow. This should not be set to too small a number,
#' to ensure that every input row gets predicted at least a few times (defaults to 100).
#' @param random_state Seed number to be used for fixing the randomness (default to NULL).
#' @param min_samples_leaf Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown
#' (and thus take less time) (defaults to 1)
#' @param max_leaf_nodes Maximum number of terminal nodes trees in the forest can have.
#' If not given, trees are grown to the maximum possible (subject to limits by nodesize).
#' If set larger than maximum possible, a warning is issued. (defaults to NULL)
#'
#' @return R6 Class of RandomForestClassifier
#' @importFrom randomForest randomForest
#' @seealso [randomForest::randomForest()]
#' @export
RandomForestClassifier <- R6::R6Class(classname = "RandomForestClassifier",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "classifier",
                                       model = NULL,
                                       n_estimators = NULL,
                                       random_state = NULL,
                                       min_samples_leaf = NULL,
                                       max_leaf_nodes = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of RandomForestClassifier
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestClassifier$new()
                                       #' rf <- RandomForestClassifier$new(n_estimators = 500)
                                       #' rf <- RandomForestClassifier$new(n_estimators = 500, random_state = 100)
                                       initialize = function(n_estimators = 100, random_state = NULL, min_samples_leaf = 1,
                                                             max_leaf_nodes = NULL){
                                         private$n_estimators = n_estimators
                                         private$random_state = random_state
                                         private$min_samples_leaf = min_samples_leaf
                                         private$max_leaf_nodes = max_leaf_nodes
                                       },
                                       #' @description Builds a random forest regressor from the training set (X, y).
                                       #'
                                       #' @param X 2D matrix or dataframe that includes predictors
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
                                       #'
                                       #' @return Fitted R6 Class of RandomForestClassifier
                                       #'
                                       #' @examples
                                       #' data(iris)
                                       #' split_list <- train_test_split(iris, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' rf <- RandomForestClassifier$new()
                                       #' rf$fit(X_train, y_train)
                                       fit = function(X, y){
                                         df <- prepareDataset(X, y)
                                         df$y <- as.factor(df$y)
                                         set.seed(private$random_state)
                                         private$model <- randomForest::randomForest(y ~ ., data = df, ntree = private$n_estimators,
                                                                                     nodesize = private$min_samples_leaf,
                                                                                     maxnodes = private$max_leaf_nodes,
                                                                                     importance = TRUE,
                                                                                     proximity = TRUE)
                                         private$isFitted <- TRUE
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return Factor of the predict classes.
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestClassifier$new()
                                       #' rf$fit(X_train, y_train)
                                       #' preds <- rf$predict(X_test)
                                       #'
                                       #' rf <- RandomForestClassifier$new()
                                       #' preds <- rf$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- RandomForestClassifier$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(caret::confusionMatrix(data=preds, reference = factor(y_test)))
                                       predict = function(X0){
                                         check_is_fitted(self)
                                         data <- prepareXset(X0)
                                         predict(private$model, data)
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     ))

#' @title KNeighborsClassifier
#'
#' @description Wrapper R6 Class of caret::knnreg function that can be used for LESSRegressor and LESSClassifier
#'
#' @param k Number of neighbors considered (defaults to 5).
#'
#' @return R6 Class of KNeighborsClassifier
#' @seealso [caret::knn3()]
#' @importFrom caret knn3
#' @export
KNeighborsClassifier <- R6::R6Class(classname = "KNeighborsClassifier",
                                   inherit = SklearnEstimator,
                                   private = list(
                                     estimator_type = "classifier",
                                     model = NULL,
                                     k = NULL
                                   ),
                                   public = list(
                                     #' @description Creates a new instance of R6 Class of KNeighborsClassifier
                                     #'
                                     #' @examples
                                     #' knc <- KNeighborsClassifier$new()
                                     #' knc <- KNeighborsClassifier$new(k = 5)
                                     initialize = function(k = 5){
                                       private$k = k
                                     },
                                     #' @description Fit the k-nearest neighbors regressor from the training set (X, y).
                                     #'
                                     #' @param X 2D matrix or dataframe that includes predictors
                                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
                                     #'
                                     #' @return Fitted R6 Class of KNeighborsClassifier
                                     #'
                                     #' @examples
                                     #' data(iris)
                                     #' split_list <- train_test_split(iris, test_size =  0.3)
                                     #' X_train <- split_list[[1]]
                                     #' X_test <- split_list[[2]]
                                     #' y_train <- split_list[[3]]
                                     #' y_test <- split_list[[4]]
                                     #'
                                     #' knc <- KNeighborsClassifier$new()
                                     #' knc$fit(X_train, y_train)
                                     fit = function(X, y){
                                       df <- prepareDataset(X, y)
                                       df$y <- as.factor(df$y)
                                       private$model <- caret::knn3(y ~ ., data = df, k=private$k)
                                       private$isFitted <- TRUE
                                       invisible(self)
                                     },
                                     #' @description Predict regression value for X0.
                                     #'
                                     #' @param X0 2D matrix or dataframe that includes predictors
                                     #'
                                     #' @return A vector of the predicted classes.
                                     #'
                                     #' @examples
                                     #' knc <- KNeighborsClassifier$new()
                                     #' knc$fit(X_train, y_train)
                                     #' preds <- knc$predict(X_test)
                                     #'
                                     #' knc <- KNeighborsClassifier$new()
                                     #' preds <- knc$fit(X_train, y_train)$predict(X_test)
                                     #'
                                     #' preds <- KNeighborsClassifier$new()$fit(X_train, y_train)$predict(X_test)
                                     #' print(caret::confusionMatrix(data=factor(preds), reference = factor(y_test)))
                                     predict = function(X0){
                                       check_is_fitted(self)
                                       data <- prepareXset(X0)
                                       predict(private$model, data, type = "class")
                                     },
                                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                     get_estimator_type = function() {
                                       return(private$estimator_type)
                                     }
                                   ))

