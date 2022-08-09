####################
# HELPER CLASSES
####################
#' @title BaseEstimator
#'
#' @description A dummy base R6 class that provides get_all_fields, get_attributes and set_random_state functionalities for estimators
#'
#' @return R6 Class of BaseEstimator
BaseEstimator <- R6::R6Class(classname = "BaseEstimator",
                             private = list(
                               priv_fields = function(){
                                 classList <- class(self)[-length(class(self))]
                                 classNum <- length(classList)
                                 fieldList <- list()
                                 for (i in 1:classNum) {
                                   priv_fields <- get(class(self)[i])$private_fields
                                   fieldList <- append(fieldList, priv_fields)
                                 }
                                 return(names(fieldList))
                               },
                               public_fields = function(){
                                 classList <- class(self)[-length(class(self))]
                                 classNum <- length(classList)
                                 fieldList <- list()
                                 for (i in 1:classNum) {
                                   public_fields <- get(class(self)[i])$public_fields
                                   fieldList <- append(fieldList, public_fields)
                                 }
                                 return(names(fieldList))
                               }
                             ),
                             public = list(
                               #' @description Auxiliary function returning the name of all private and public fields of the self class
                               get_all_fields = function(){
                                 return(append(private$priv_fields(), private$public_fields()))
                               },
                               #' @description Auxiliary function returning the name and values of all private and public fields of the self class
                               get_attributes = function(){
                                 priv_values <- purrr::map(private$priv_fields(), ~.subset2(private, .x))
                                 public_values <- purrr::map(private$public_fields(), ~.subset2(self, .x))
                                 names(priv_values) <- private$priv_fields()
                                 names(public_values) <- private$public_fields()
                                 return(append(priv_values, public_values))
                               },
                               #' @description Auxiliary function that sets random state attribute of the self class
                               #'
                               #' @param random_state seed number to be set as random state
                               #' @return self
                               set_random_state = function(random_state){
                                 private$random_state <- random_state
                                 invisible(self)
                               }
                             ))
#' @title SklearnEstimator
#'
#' @description A dummy base R6 class that includes fit, predict functions for estimators
#'
#' @return R6 Class of SklearnEstimator
SklearnEstimator <- R6::R6Class(classname = "SklearnEstimator",
                                inherit = BaseEstimator,
                                private = list(
                                  type = "estimator"
                                ),
                                public = list(
                                  #' @description Dummy fit function
                                  fit = function() {
                                    stop("Needs to implement fit(X, y)")
                                  },
                                  #' @description Dummy predict function
                                  predict = function(){
                                    stop("Needs to implement predict(X, y)")
                                    invisible(self)
                                  },
                                  #' @description Auxiliary function returning the type of the class e.g 'estimator'
                                  get_type = function(){
                                    return(private$type)
                                  }
                                )
                                )

LocalModel <- R6::R6Class(classname = "LocalModel",
                      public = list(
                        estimator = NULL,
                        center = NULL,
                        initialize = function(estimator = NULL, center = NULL) {
                          self$estimator <- estimator
                          self$center <- center
                        }
                      ))

Replication <- R6::R6Class(classname = "Replication",
                       public = list(
                         sc_object = NULL,
                         global_estimator = NULL,
                         local_estimators = NULL,
                         initialize = function(sc_object = NULL, global_estimator = NULL, local_estimators = NULL) {
                           self$sc_object <- sc_object
                           self$global_estimator <- global_estimator
                           self$local_estimators <- local_estimators
                         }
                       ))

#' @title LinearRegression
#'
#' @description Wrapper R6 Class of stats::lm function that can be used for LESSRegressor and LESSClassifier
#'
#' @return R6 Class of LinearRegression
#' @importFrom stats lm
#' @seealso [stats::lm()]
#' @export
LinearRegression <- R6::R6Class(classname = "LinearRegression",
                                inherit = SklearnEstimator,
                                private = list(
                                  estimator_type = "regressor",
                                  model = NULL
                                ),
                                public = list(
                                  #' @description Fits a linear model (y ~ X)
                                  #'
                                  #' @param X 2D matrix or dataframe that includes predictors
                                  #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                  #'
                                  #' @return Fitted R6 Class of LinearRegression
                                  #'
                                  #' @examples
                                  #' data(abalone)
                                  #' split_list <- train_test_split(abalone, test_size =  0.3)
                                  #' X_train <- split_list[[1]]
                                  #' X_test <- split_list[[2]]
                                  #' y_train <- split_list[[3]]
                                  #' y_test <- split_list[[4]]
                                  #'
                                  #' lr <- LinearRegression$new()
                                  #' lr$fit(X_train, y_train)
                                  fit = function(X, y) {
                                    df <- prepareDataset(X, y)
                                    private$model <- lm(y ~ ., data = df)
                                    # if(length(private$model$coefficients) > private$model$rank){
                                    #   print("rank deficient fit. number of parameters are more than the observations")
                                    #   print(private$model$rank)
                                    # }
                                    invisible(self)
                                  },
                                  #' @description Predict regression value for X.
                                  #'
                                  #' @param X0 2D matrix or dataframe that includes predictors
                                  #'
                                  #' @return The predict values.
                                  #'
                                  #' @examples
                                  #' lr <- LinearRegression$new()
                                  #' lr$fit(X_train, y_train)
                                  #' preds <- lr$predict(X_test)
                                  #'
                                  #' lr <- LinearRegression$new()
                                  #' preds <- lr$fit(X_train, y_train)$predict(X_test)
                                  #'
                                  #' preds <- LinearRegression$new()$fit(X_train, y_train)$predict(X_test)
                                  #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                  predict = function(X0) {
                                    data <- prepareXset(X0)
                                    suppressWarnings(predict(private$model, newdata = data))
                                  },
                                  #' @description Auxiliary function returning the summary of the fitted model
                                  printModel = function() {
                                    summary(private$model)
                                  },
                                  #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                  get_estimator_type = function() {
                                    return(private$estimator_type)
                                  }
                                )
                                )

#' @title DecisionTreeRegressor
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
#' @return R6 Class of DecisionTreeRegressor
#' @importFrom rpart rpart
#' @seealso [rpart::rpart()]
#' @export
DecisionTreeRegressor <- R6::R6Class(classname = "DecisionTreeRegressor",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "regressor",
                                       model = NULL,
                                       min_samples_split = NULL,
                                       min_samples_leaf = NULL,
                                       cp = NULL,
                                       max_depth = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of DecisionTreeRegressor
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt <- DecisionTreeRegressor$new(min_samples_split = 10)
                                       #' dt <- DecisionTreeRegressor$new(min_samples_leaf = 6, cp = 0.01)
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
                                       #' @return Fitted R6 Class of DecisionTreeRegressor
                                       #'
                                       #' @examples
                                       #' data(abalone)
                                       #' split_list <- train_test_split(abalone, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt$fit(X_train, y_train)
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         private$model <- rpart::rpart(y ~ ., method = "anova", data = df,
                                                                       control = rpart::rpart.control(minsplit = private$min_samples_split,
                                                                                                      minbucket = private$min_samples_leaf,
                                                                                                      cp = private$cp, maxdepth = private$max_depth))
                                         # rpart.plot::rpart.plot(private$model)
                                         # summary(private$model)
                                         invisible(self)
                                         # print("model: ")

                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return The predict values.
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt$fit(X_train, y_train)
                                       #' preds <- dt$predict(X_test)
                                       #'
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' preds <- dt$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- DecisionTreeRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                       predict = function(X0) {
                                         data <- prepareXset(X0)
                                         predict(private$model, data, method = "anova")
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     )
                                     )

#' @title DecisionTreeRegressor
#'
#' @description Wrapper R6 Class of party::ctree function that can be used for LESSRegressor and LESSClassifier
#'
#' @return R6 Class of DecisionTreeRegressor
#' @importFrom party ctree
#' @seealso [party::ctree()]
#' @export
DecisionTreeRegressor2 <- R6::R6Class(classname = "DecisionTreeRegressor2",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "regressor",
                                       model = NULL
                                     ),
                                     public = list(
                                       #' @description Builds a decision tree regressor from the training set (X, y).
                                       #'
                                       #' @param X 2D matrix or dataframe that includes predictors
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                       #'
                                       #' @return Fitted R6 Class of DecisionTreeRegressor
                                       #'
                                       #' @examples
                                       #' data(abalone)
                                       #' split_list <- train_test_split(abalone, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt$fit(X_train, y_train)
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         private$model <- party::ctree(
                                           y ~ .,
                                           data = df,
                                           controls = party::ctree_control(minsplit = 2, minbucket = 1))
                                         # plot(private$model)
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return The predict values.
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt$fit(X_train, y_train)
                                       #' preds <- dt$predict(X_test)
                                       #'
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' preds <- dt$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- DecisionTreeRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                       predict = function(X0) {
                                         data <- prepareXset(X0)
                                         predict(private$model, data)
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     )
                                     )

#' @title RandomForestRegressor
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
#' @return R6 Class of RandomForestRegressor
#' @importFrom randomForest randomForest
#' @seealso [randomForest::randomForest()]
#' @export
RandomForestRegressor <- R6::R6Class(classname = "RandomForestRegressor",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "regressor",
                                       model = NULL,
                                       n_estimators = NULL,
                                       random_state = NULL,
                                       min_samples_leaf = NULL,
                                       max_leaf_nodes = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of RandomForestRegressor
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestRegressor$new()
                                       #' rf <- RandomForestRegressor$new(n_estimators = 500)
                                       #' rf <- RandomForestRegressor$new(n_estimators = 500, random_state = 100)
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
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                       #'
                                       #' @return Fitted R6 Class of RandomForestRegressor
                                       #'
                                       #' @examples
                                       #' data(abalone)
                                       #' split_list <- train_test_split(abalone, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' rf <- RandomForestRegressor$new()
                                       #' rf$fit(X_train, y_train)
                                       fit = function(X, y){
                                         df <- prepareDataset(X, y)
                                         set.seed(private$random_state)
                                         private$model <- randomForest::randomForest(y ~ ., data = df, type = "regression", ntree = private$n_estimators,
                                                                                     nodesize = private$min_samples_leaf,
                                                                                     maxnodes = private$max_leaf_nodes)
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return The predict values.
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestRegressor$new()
                                       #' rf$fit(X_train, y_train)
                                       #' preds <- rf$predict(X_test)
                                       #'
                                       #' rf <- RandomForestRegressor$new()
                                       #' preds <- rf$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- RandomForestRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                       predict = function(X0){
                                         data <- prepareXset(X0)
                                         predict(private$model, data)
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     ))

#' @title KNeighborsRegressor
#'
#' @description Wrapper R6 Class of caret::knnreg function that can be used for LESSRegressor and LESSClassifier
#'
#' @param k Number of neighbors considered (defaults to 5).
#'
#' @return R6 Class of KNeighborsRegressor
#' @seealso [caret::knnreg()]
#' @importFrom caret knnreg
#' @export
KNeighborsRegressor <- R6::R6Class(classname = "KNeighborsRegressor",
                                   inherit = SklearnEstimator,
                                   private = list(
                                     estimator_type = "regressor",
                                     model = NULL,
                                     k = NULL
                                   ),
                                   public = list(
                                     #' @description Creates a new instance of R6 Class of KNeighborsRegressor
                                     #'
                                     #' @examples
                                     #' knr <- KNeighborsRegressor$new()
                                     #' knr <- KNeighborsRegressor$new(k = 5)
                                     initialize = function(k = 5){
                                       private$k = k
                                     },
                                     #' @description Fit the k-nearest neighbors regressor from the training set (X, y).
                                     #'
                                     #' @param X 2D matrix or dataframe that includes predictors
                                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                     #'
                                     #' @return Fitted R6 Class of KNeighborsRegressor
                                     #'
                                     #' @examples
                                     #' data(abalone)
                                     #' split_list <- train_test_split(abalone, test_size =  0.3)
                                     #' X_train <- split_list[[1]]
                                     #' X_test <- split_list[[2]]
                                     #' y_train <- split_list[[3]]
                                     #' y_test <- split_list[[4]]
                                     #'
                                     #' knr <- KNeighborsRegressor$new()
                                     #' knr$fit(X_train, y_train)
                                     fit = function(X, y){
                                       df <- prepareDataset(X, y)
                                       private$model <- caret::knnreg(y ~ ., data = df, k=private$k)
                                       invisible(self)
                                     },
                                     #' @description Predict regression value for X0.
                                     #'
                                     #' @param X0 2D matrix or dataframe that includes predictors
                                     #'
                                     #' @return The predict values.
                                     #'
                                     #' @examples
                                     #' knr <- KNeighborsRegressor$new()
                                     #' knr$fit(X_train, y_train)
                                     #' preds <- knr$predict(X_test)
                                     #'
                                     #' knr <- KNeighborsRegressor$new()
                                     #' preds <- knr$fit(X_train, y_train)$predict(X_test)
                                     #'
                                     #' preds <- KNeighborsRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                     #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                     predict = function(X0){
                                       data <- prepareXset(X0)
                                       predict(private$model, data)
                                     },
                                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                     get_estimator_type = function() {
                                       return(private$estimator_type)
                                     }
                                   ))

#' @title Support Vector Regression
#'
#' @description Wrapper R6 Class of e1071::svm function that can be used for LESSRegressor and LESSClassifier
#'
#' @return R6 Class of SVR
#' @seealso [e1071::svm()]
#' @importFrom e1071 svm
#' @export
SVR <- R6::R6Class(classname = "SVR",
                   inherit = SklearnEstimator,
                   private = list(
                     estimator_type = "regressor",
                     model = NULL
                   ),
                   public = list(
                     #' @description Fit the SVM model from the training set (X, y).
                     #'
                     #' @param X 2D matrix or dataframe that includes predictors
                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                     #'
                     #' @return Fitted R6 Class of SVR
                     #'
                     #' @examples
                     #' data(abalone)
                     #' split_list <- train_test_split(abalone, test_size =  0.3)
                     #' X_train <- split_list[[1]]
                     #' X_test <- split_list[[2]]
                     #' y_train <- split_list[[3]]
                     #' y_test <- split_list[[4]]
                     #'
                     #' svr <- SVR$new()
                     #' svr$fit(X_train, y_train)
                     fit = function(X, y){
                       df <- prepareDataset(X, y)
                       private$model <- e1071::svm(y ~ ., data = df)
                       invisible(self)
                     },
                     #' @description Predict regression value for X0.
                     #'
                     #' @param X0 2D matrix or dataframe that includes predictors
                     #'
                     #' @return The predict values.
                     #'
                     #' @examples
                     #' svr <- SVR$new()
                     #' svr$fit(X_train, y_train)
                     #' preds <- svr$predict(X_test)
                     #'
                     #' svr <- SVR$new()
                     #' preds <- svr$fit(X_train, y_train)$predict(X_test)
                     #'
                     #' preds <- SVR$new()$fit(X_train, y_train)$predict(X_test)
                     #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                     predict = function(X0){
                       data <- prepareXset(X0)
                       predict(private$model, data)
                     },
                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                     get_estimator_type = function() {
                       return(private$estimator_type)
                     }
                   ))

StandardScaler <- R6::R6Class(classname = "StandardScaler",
                              private = list(
                                mean = NULL,
                                stdev = NULL
                              ),
                              public = list(
                                fit = function(X) {
                                  # standart deviation function for internal use
                                  # the default stdev() function of R, divides by length(ln)-1
                                  standart_dev <- function(ln) {
                                    sqrt(sum((ln - mean(ln)) ^ 2 / length(ln)))
                                  }
                                  # assign mean and standart deviation parameters
                                  private$stdev <- apply(X, 2, standart_dev)
                                  private$mean <- colMeans(X)
                                  invisible(self)
                                },
                                transform = function(X) {
                                  # append mean and standart deviation values to the X matrix
                                  merged <- rbind(private$mean, private$stdev, X)
                                  # standardize each value by the corresponding mean and stdev values
                                  # using z = (x - u) / s formula
                                  merged <- apply(merged, 2, function(x) (x - x[1]) / x[2] )
                                  #return the standardized version of original X matrix, extract the mean and stdev rows (first 2 cols)
                                  if(nrow(merged) > 3){
                                    return(as.matrix(merged[3:nrow(merged),]))
                                  }else if(nrow(merged) == 3){
                                    return(t(matrix(merged[3:nrow(merged),])))
                                  }
                                },
                                fit_transform = function(X) {
                                  self$fit(X)$transform(X)
                                },
                                print = function() {
                                  cat("Mean: ", private$mean, "\n")
                                  cat("Standart Deviation: ", private$stdev, "\n")
                                }
                              ))

RandomGenerator <- R6::R6Class(classname = "RandomGenerator",
                               private = list(
                                 random_state = NULL,
                                 index = NULL
                               ),
                               public = list(
                                 initialize = function(random_state){
                                   private$random_state = random_state
                                   private$index = 1
                                 },
                                 choice = function(range, size){
                                   # range: sampling takes place from 1:range
                                   # size: a non-negative integer giving the number of items to choose
                                   set.seed(private$random_state)
                                   permutation <- sample(range)

                                   # this part helps if the index go beyond range.
                                   if((private$index + size - 1) > range){
                                     set.seed(private$random_state)
                                     permutation <- c(permutation, sample(range, size=(private$index + size - 1 - range), replace = TRUE))
                                   }

                                   result <- permutation[private$index:(private$index+size-1)]
                                   private$index <- private$index + size
                                   return(result)
                                 },
                                 integers = function(range, size = 1) {
                                   set.seed(private$random_state)
                                   permutation <- sample.int(range)

                                   # this part helps if the index go beyond range.
                                   if((private$index + size - 1) > range){
                                     set.seed(private$random_state)
                                     permutation <- c(permutation, sample.int(range, size=(private$index + size - 1 - range), replace = TRUE))
                                   }

                                   result <- permutation[private$index:(private$index+size-1)]
                                   private$index <- private$index + size
                                   return(result)
                                 },
                                 get_random_state = function(){
                                   return(private$random_state)
                                 }
                               ))

LESSWarn <- R6::R6Class(classname = "LESSWarn",
                        public = list(
                          initialize = function(msg = "", flag = TRUE){
                            if(flag){
                              warning(msg)
                            }
                          }
                        ))

#' @title KDTree - Nearest Neighbor Search
#'
#' @description Wrapper R6 Class of RANN::nn2 function that can be used for LESSRegressor and LESSClassifier
#'
#' @param X An \strong{M x d} data.frame or matrix, where each of the \strong{M} rows is a point or a (column) vector (where \strong{d=1}).
#'
#' @return R6 Class of KDTree
#' @importFrom RANN nn2
#' @seealso [RANN::nn2()]
#' @export
KDTree <- R6::R6Class(classname = "KDTree",
                      private = list(
                        X = NULL
                      ),
                      public = list(
                        #' @description Creates a new instance of R6 Class of KDTree
                        #'
                        #' @examples
                        #' data(abalone)
                        #' kdt <- KDTree$new(abalone)
                        initialize = function(X = NULL) {
                          private$X = X
                        },
                        #' @description Finds the p number of near neighbours for each point in an input/output dataset. The advantage of the kd-tree is that it runs in O(M log M) time.
                        #'
                        #' @param query_X A set of \strong{N x d} points that will be queried against \code{X}. \strong{d}, the number of columns, must be the same as \code{X}.
                        #' If missing, defaults to  \code{X}.
                        #' @param k The maximum number of nearest neighbours to compute (deafults to 1).
                        #'
                        #' @return A \code{list} of length 2 with elements:\tabular{ll}{
                        #'    \code{nn.idx} \tab A \strong{N x k} integer matrix returning the near neighbour indices. \cr
                        #'    \tab \cr
                        #'    \code{nn.dists} \tab A \strong{N x k} matrix returning the near neighbour Euclidean distances \cr
                        #' }
                        #'
                        #' @examples
                        #' data(abalone)
                        #' kdt <- KDTree$new(abalone)
                        #' res <- kdt$query(abalone[1:3,], k=2)
                        #' print(res)
                        query = function(query_X = private$X, k=1){
                          # query the tree for the k nearest neighbors
                          query <- as.matrix(query_X)
                          RANN::nn2(data = private$X, query = query, k = k)
                        }
                      ))

#' @title KMeans Clustering
#'
#' @description Wrapper R6 Class of stats::kmeans function that can be used for LESSRegressor and LESSClassifier
#'
#' @param n_clusters the number of clusters. A random set of (distinct) rows in X is chosen as the initial centres (default to 8)
#' @param n_init how many random sets should be chosen? (default to 10)
#' @param max_iter the maximum number of iterations allowed (default to 300).
#' @param random_state seed number to be used for fixing the randomness (default to NULL).
#'
#' @return R6 Class of KMeans
#' @importFrom stats kmeans
#' @seealso [stats::kmeans()]
#' @export
KMeans <- R6::R6Class(classname = "KMeans",
                      inherit = BaseEstimator,
                      private = list(
                        model = NULL,
                        n_clusters = NULL,
                        n_init = NULL,
                        max_iter = NULL,
                        cluster_centers = NULL,
                        labels = NULL,
                        random_state = NULL
                      ),
                      public = list(
                        #' @description Creates a new instance of R6 Class of KMeans
                        #'
                        #' @examples
                        #' km <- KMeans$new()
                        #' km <- KMeans$new(n_clusters = 10)
                        #' km <- KMeans$new(n_clusters = 10, random_state = 100)
                        initialize = function(n_clusters = 8, n_init = 10, max_iter = 300, random_state = NULL){
                          private$n_clusters = n_clusters
                          private$n_init = n_init
                          private$max_iter = max_iter
                          private$random_state = random_state
                        },
                        #' @description Perform k-means clustering on a data matrix.
                        #'
                        #' @param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
                        #'
                        #' @return Fitted R6 class of KMeans() that has 'cluster_centers' and 'labels' attributes
                        #'
                        #' @examples
                        #' data(abalone)
                        #' km <- KMeans$new()
                        #' km$fit(abalone)
                        fit = function(X){
                          set.seed(private$random_state)
                          private$model <- kmeans(X, centers = private$n_clusters, iter.max = private$max_iter, nstart = private$n_init)
                          private$cluster_centers <- private$model$centers
                          private$labels <- private$model$cluster
                          invisible(self)
                        },
                        #' @description Auxiliary function returning the cluster centers
                        #' @examples
                        #' print(km$get_cluster_centers())
                        get_cluster_centers = function(){
                          return(private$cluster_centers)
                        },
                        #' @description Auxiliary function returning a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
                        #' @examples
                        #' print(km$get_labels())
                        get_labels = function(){
                          return(private$labels)
                        }
                      ))

#' @title Hierarchical Clustering
#'
#' @description Wrapper R6 Class of stats::hclust function that can be used for LESSRegressor and LESSClassifier
#'
#' @param linkage the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of
#' "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC)
#' (defaults to ward.D2).
#' @param n_clusters the number of clusters (defaults to 8).
#'
#' @return R6 Class of HierarchicalClustering
#' @importFrom stats hclust
#' @seealso [stats::hclust()]
#' @export
HierarchicalClustering <- R6::R6Class(classname = "HierarchicalClustering",
                                      inherit = BaseEstimator,
                                      private = list(
                                        model = NULL,
                                        n_clusters = NULL,
                                        cluster_centers = NULL,
                                        linkage = NULL,
                                        labels = NULL
                                      ),
                                      public = list(
                                        #' @description Creates a new instance of R6 Class of HierarchicalClustering
                                        #'
                                        #' @examples
                                        #' hc <- HierarchicalClustering$new()
                                        #' hc <- HierarchicalClustering$new(n_clusters = 10)
                                        #' hc <- HierarchicalClustering$new(n_clusters = 10, linkage = "complete")
                                        initialize = function(linkage = "ward.D2", n_clusters = 8){
                                          private$linkage <- linkage
                                          private$n_clusters <- n_clusters
                                        },
                                        #' @description Perform hierarchical clustering on a data matrix.
                                        #'
                                        #' @param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
                                        #'
                                        #' @return Fitted R6 class of HierarchicalClustering() that has 'labels' attribute
                                        #'
                                        #' @examples
                                        #' data(abalone)
                                        #' hc <- HierarchicalClustering$new()
                                        #' hc$fit(abalone)
                                        fit = function(X){
                                          private$model <- stats::hclust(dist(X), method = private$linkage)
                                          private$labels <- unname(cutree(private$model, k = private$n_clusters))
                                          invisible(self)
                                        },
                                        #' @description Auxiliary function returning the cluster centers
                                        #' @examples
                                        #' print(hc$get_cluster_centers())
                                        get_cluster_centers = function(){
                                          return(private$cluster_centers)
                                        },
                                        #' @description Auxiliary function returning a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
                                        #' @examples
                                        #' print(hc$get_labels())
                                        get_labels = function(){
                                          return(private$labels)
                                        }
                                      ))


####################
# HELPER FUNCTIONS
####################

#' @title RBF kernel - L2 norm
#'
#' @description The default distance function in LESS.
#'
#' @param data Data that includes points in shape of \strong{(M x d)}
#' @param center A constant point in shape of \strong{(1 x d)}
#' @param coeff Coefficient value for RBF kernel
#'
#' @return A numeric vector containing the Radial basis function kernel distance between each point in \strong{data} and \strong{center}.
#' @export
#'
#' @examples
#' data <- matrix(1:12, nrow=3)
#' center <- c(2, 7, 1, 3)
#' distances <- rbf(data, center)
#' print(distances)
rbf <- function(data, center, coeff=0.01){
  dataDiff <- sweep(data, 2, center) #extract center from all rows of data e.g. (data-center)
  normRows <- wordspace::rowNorms(dataDiff, method = "euclidean", p=2) #take l2 norms of each row
  exp(-coeff * normRows)
}

#' @title Laplacian kernel - L1 norm
#'
#' @description An alternative distance function that can be used in LESS.
#'
#' @param data Data that includes points in shape of \strong{(M x d)}
#' @param center A constant point in shape of \strong{(1 x d)}
#' @param coeff Coefficient value for Laplacian kernel
#'
#' @return A numeric vector containing the laplacian kernel distance between each point in \strong{data} and \strong{center}.
#' @export
#'
#' @examples
#' data <- matrix(1:12, nrow=3)
#' center <- c(2, 7, 1, 3)
#' distances <- laplacian(data, center)
#' print(distances)
laplacian <- function(data, center, coeff=0.01){
  dataDiff <- sweep(data, 2, center) #extract center from all rows of data e.g. (data-center)
  normRows <- wordspace::rowNorms(dataDiff, method = "manhattan", p=1) #take l1 norms of each row
  exp(-coeff * normRows)
}

# takes X and y datasets and merges them into a dataframe with column names
prepareDataset = function(X, y) {
  merged_data <- cbind(y, X)
  df <- as.data.frame(merged_data)
  colX <- list()
  if(!is.vector(merged_data[,-1])){
    ncolumns <- ncol(merged_data[,-1])
  }else{
    if(length(y) == 1){ # this means X includes 1 row
      ncolumns <- length(merged_data[,-1])
    }else{ # this means x has only 1 feature
      ncolumns <- 1
    }
  }
  for(i in 1:ncolumns){
    colX <- append(colX, paste(c("X", i), collapse = "_"))
  }
  column_names <- append(list("y"), colX)
  colnames(df) <- column_names
  df
}

# takes X dataset and convert it into a dataframe with column names
prepareXset = function(X) {
  df <- as.data.frame(X)
  colX <- list()
  if(!is.vector(X)){
    ncolumns <- ncol(X)
  }else{
    ncolumns <- 1
  }
  for(i in 1:ncolumns){
    colX <- append(colX, paste(c("X", i), collapse = "_"))
  }
  colnames(df) <- colX
  df
}

# checks if the input estimator's type is regressor
is_regressor = function(estimator) {
  if(is.null(estimator)){
    return(FALSE)
  }else{
    return(estimator$get_estimator_type() == "regressor")
  }
}

# checks if the input estimator's type is classifier
is_classifier = function(estimator) {
  if(is.null(estimator)){
    return(FALSE)
  }else{
    return(estimator$get_estimator_type() == "classifier")
  }
}

# returns the class name of the input object
getClassName = function(obj) {
  class(obj)[1]
}

#' @title Dataset splitting
#'
#' @description Split dataframes or matrices into random train and test subsets. Takes the column at the \strong{y_index} of \strong{data} as response variable \strong{(y)}
#' and the rest as the independent variables \strong{(X)}
#'
#' @param data Dataset that is going to be split
#' @param test_size Represents the proportion of the dataset to include in the test split.
#' Should be between 0.0 and 1.0 (defaults to 0.3)
#' @param random_state Controls the shuffling applied to the data before applying the split.
#' Pass an int for reproducible output across multiple function calls (defaults to NULL)
#' @param y_index Corresponding column index of the response variable \strong{y} (defaults to last column of \strong{data})
#'
#' @return A \code{list} of length 4 with elements:\tabular{ll}{
#'    \code{X_train} \tab Training input variables  \cr
#'    \tab \cr
#'    \code{X_test} \tab Test input variables \cr
#'    \tab \cr
#'    \code{y_train} \tab Training response variables   \cr
#'    \tab \cr
#'    \code{y_test} \tab Test response variables \cr
#' }
#' @export
#'
#' @examples
#' data(abalone)
#' split_list <- train_test_split(abalone, test_size =  0.3)
#' X_train <- split_list[[1]]
#' X_test <- split_list[[2]]
#' y_train <- split_list[[3]]
#' y_test <- split_list[[4]]
#'
#' print(head(X_train))
#' print(head(X_test))
#' print(head(y_train))
#' print(head(y_test))
train_test_split = function(data, test_size=0.3, random_state=NULL, y_index = ncol(data)){
  if(!is.null(test_size)) {
    if(test_size <= 0.0 | test_size >= 1.0){
      stop("\tParameter test_size should be in the interval (0, 1).")
    }
  }
  if(y_index%%1!=0){
    stop("\tParameter y_index should be an integer")
  }
  if(y_index < 1 | y_index > ncol(data)) {
    stop("\tParameter y_index should be in the interval [1, ncol(data)]")
  }

  set.seed(random_state)
  sample <- sample.int(n = nrow(data), size = floor((1-test_size)*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]

  X_train <- as.matrix(train[,-y_index])
  X_test <- as.matrix(test[,-y_index])
  y_train <- as.matrix(train[,y_index])
  y_test <- as.matrix(test[,y_index])
  return(list(X_train = X_train, X_test = X_test, y_train = y_train, y_test = y_test))
}

# checks if the given estimator is fitted
check_is_fitted = function(estimator){
  if(is.null(estimator$get_type())){
    stop("\tGiven estimator is not an estimator instance.")
  }else if(estimator$get_type() != "estimator"){
    stop("\tGiven estimator is not an estimator instance.")
  }

  if(is.null(estimator$get_isFitted())){
    is_fitted <- FALSE
  }else{
    is_fitted <- estimator$get_isFitted()
  }

  if(!is_fitted){
    stop("\tThis estimator instance is not fitted yet.\n\tCall 'fit' with appropriate arguments before using this estimator.")
  }
}

# Input validation on a matrix.
# The input is checked to be a non-empty 2D matrix or dataframe containing only finite values.
check_matrix = function(matrix){
  is.scalar <- function(x) is.atomic(x) && length(x) == 1L
  matrix_name <- deparse(substitute(matrix))
  if(is.scalar(matrix) & !is.matrix(matrix) & !is.data.frame(matrix)){
    stop(sprintf("\tThe input '%s' is expected to be a 2D matrix or dataframe, got a scalar instead.
                 \tYour data must be (n,1) dimensional if your data has a single feature or
                 \t(1, n) dimensional  if it contains a single sample.", matrix_name))
  }else if(is.null(dim(matrix))){
    stop(sprintf("\tThe input '%s' is expected to be a 2D matrix or dataframe, got a 1D vector instead.
                 \tYour data must be (n,1) dimensional if your data has a single feature or
                 \t(1, n) matrix if it contains a single sample.", matrix_name))
  }else if(!is.matrix(matrix) & !is.data.frame(matrix)){
    stop(sprintf("\tThe input '%s' is expected to be a 2D matrix or dataframe, got a %s", matrix_name, class(matrix)))
  }

  dirty_indices <- apply(matrix, 2, function(x) is.na(x) | is.infinite(x) | is.nan(x))
  is_dirty <- Reduce('|', dirty_indices)
  if(is_dirty){
    stop("\t Values in X cannot be infinite, NaN or NA")
  }

  if(!is.numeric(as.matrix(matrix))){
    stop(sprintf("\tThe input '%s' is expected to be a numeric", matrix_name))
  }
}

# Isolated part of check_X_y dedicated to y validation
check_y = function(y) {
  is.scalar <- function(x) is.atomic(x) && length(x) == 1L
  y_name <- deparse(substitute(y))
  if(is.scalar(y) & !is.matrix(y) & !is.data.frame(y)){
    stop(sprintf("\tThe input '%s' is expected to be a 1D vector or (n,1) dimensional matrix/dataframe, got a scalar instead.", y_name))
  }else if(!is.matrix(y) & !is.data.frame(y) & !is.vector(y)){
    stop(sprintf("\tThe input '%s' is expected to be a 1D vector or (n,1) dimensional matrix/dataframe, got a %s", y_name, class(y)))
  }else if(is.matrix(y) | is.data.frame(y)){
    if(ncol(y)>1){
      stop(sprintf("\tThe input '%s' is expected to be a 1D vector or (n,1) dimensional matrix/dataframe, got a (n,%s) dimensional matrix/dataframe", y_name, ncol(y)))
    }
  }

  dirty_indices <- apply(as.matrix(y), 2, function(x) is.na(x) | is.infinite(x) | is.nan(x))
  is_dirty <- Reduce('|', dirty_indices)
  if(is_dirty){
    stop("\t Values in y cannot be infinite, NaN or NA")
  }

  if(!is.numeric(as.matrix(y))){
    stop(sprintf("\tThe input '%s' is expected to be a numeric", y_name))
  }

  return(as.matrix(y))
}

# Checks X and y for consistent length, enforces X to be 2D and y 1D.
# X is checked to be non-empty and containing only finite values.
# Standard input checks are also applied to y, such as checking that y
# does not have nan or inf targets.
check_X_y = function(X, y){
  check_matrix(X)
  y <- check_y(y)
  if(nrow(X) != nrow(y)){
    stop(sprintf("Found input variables with inconsistent numbers of samples:\n\tX: %s\n\ty: %s", nrow(X), nrow(y)))
  }
  return(list(X, y))
}


synthetic_sine_curve = function() {
  xvals <- seq(-10,10,length.out=201)[-201]
  # plot(xvals, 10*sin(xvals), type = "l", col="red", ylab="",yaxt="n", xlab="",xaxt="n")
  # par(new=TRUE)

  X <- rep(0, 200)
  y <- rep(0, 200)
  for(i in 1:200){
    xran <- -10 + 20*runif(1)
    X[i] <- xran
    y[i] <- 10*sin(xran) + 2.5*rnorm(1)
  }
  # plot(X, y, pch = 19, col="blue",  ylab="",yaxt="n", xlab="",xaxt="n")

  return(list(X, y))
}

comparison_plot = function(X, y, model_list){
  xlb <- floor(min(X)-1)
  xub <- floor(max(X)+1)
  xvals <- seq(xlb, xub, by=0.1)
  color_list <- c("blue", "green", "red", "black", "brown", "purple", "orange", "seagreen2")
  color_index <- 1
  par(mfrow=c(length(model_list)/2+1, 2))
  plot(X, y, main = "True", pch = 19, col=color_list[color_index], ylab="",yaxt="n", xlab="",xaxt="n")
  # par(new=TRUE)
  for (model in model_list) {
    color_index <- color_index + 1
    y_pred <- model$fit(X, y)$predict(prepareXset(xvals))
    plot(xvals, y_pred, main=getClassName(model), type="l", lwd = 2, pch = 19, col=color_list[color_index],  ylab="",yaxt="n", xlab="",xaxt="n")
  }
}


###################

#' @title LESSBase
#'
#' @description The base class for LESSRegressor and LESSClassifier
#'
#' @param isFitted Flag to check whether LESS is fitted
#' @param replications List to store the replications
#' @param scobject Scaling object used for normalization (less::StandardScaler)
#'
#' @return R6 class of LESSBase
LESSBase <- R6::R6Class(classname = "LESSBase",
                      inherit = SklearnEstimator,
                      private = list(
                        isFitted = FALSE,
                        replications = NULL,
                        scobject = NULL,
                        set_local_attributes = function() {
                          if(is.null(private$local_estimator)){
                            stop("\tLESS does not work without a local estimator.")
                          }

                          if(is_classifier(private$local_estimator)){
                            LESSWarn$new("\tLESS might work with local classifiers.\n\tHowever, we recommend using regressors as the local estimators.",
                                         private$warnings)
                          }

                          if(getClassName(self) == "LESSRegressor" & is_classifier(private$global_estimator)){
                            LESSWarn$new("\tLESSRegressor might work with a global classifier.\n\tHowever, we recommend using a regressor as the global estimator.",
                                         private$warnings)
                          }

                          if(getClassName(self) == "LESSClassifier" & is_regressor(private$global_estimator)){
                            LESSWarn$new("\tLESSClassifier might work with a global regressor.\n\tHowever, we recommend using a classifier as the global estimator.",
                                         private$warnings)
                          }

                          if(!is.null(private$val_size)) {
                            if(private$val_size <= 0.0 | private$val_size >= 1.0){
                              stop("\tParameter val_size should be in the interval (0, 1).")
                            }
                          }

                          if(!is.null(private$frac)) {
                            if(private$frac <= 0.0 | private$frac > 1.0){
                              stop("\tParameter frac should be in the interval (0, 1).")
                            }
                          }

                          if(private$n_replications < 1){
                            stop("\tThe number of replications should be greater than or equal to one.")
                          }

                          if(length(private$cluster_method) != 0){ #length of NULL is zero. if it is not a null environment(class), length is not zero
                            if(!is.null(private$frac) | !is.null(private$n_neighbors) | !is.null(private$n_subsets)){
                              LESSWarn$new("\tParameter cluster_method overrides parameters frac, n_neighbors and n_subsets.\n\tProceeding with clustering...",
                                           private$warnings)
                              private$frac <- NULL
                              private$n_neighbors <- NULL
                            }

                            # Different numbers of subsets may be generated by the clustering method
                            private$n_subsets <- list()

                            if('n_clusters' %in% private$cluster_method$get_all_fields()){
                              if(private$cluster_method$get_attributes()$n_cluster == 1){
                                LESSWarn$new("\tThere is only one cluster, so the global estimator is set to NULL.",
                                             private$warnings)
                                private$global_estimator <- NULL
                                private$d_normalize <- TRUE
                                # If there is also no validation step, then there is
                                # no randomness. So, no need for replications.
                                if(is.null(private$val_size)){
                                  LESSWarn$new("\tSince validation set is not used, there is no randomness.\n\tThus, the number of replications is set to one.",
                                               private$warnings)
                                  private$n_replications <- 1
                                }
                              }
                            }
                          }else if(is.null(private$frac) &
                                   is.null(private$n_neighbors) &
                                   is.null(private$n_subsets)){
                            private$frac <- 0.05
                          }
                        },

                        check_input = function(len_X) {
                          if(length(private$cluster_method) == 0){
                            if(!is.null(private$frac)){
                              private$n_neighbors <- as.integer(ceiling(private$frac * len_X))
                              private$n_subsets <- as.integer(len_X/private$n_neighbors)
                            }

                            if(is.null(private$n_subsets)){
                              private$n_subsets <- as.integer(len_X/private$n_neighbors)
                            }

                            if(is.null(private$n_neighbors)){
                              private$n_neighbors <- as.integer(len_X/private$n_subsets)
                            }

                            if(private$n_neighbors > len_X){
                              LESSWarn$new("\tThe number of neighbors is larger than the number of samples. \n\tSetting number of subsets to one.",
                                           private$warnings)
                              private$n_neighbors <- len_X
                              private$n_subsets <- 1
                            }

                            if(private$n_subsets > len_X){
                              LESSWarn$new("\tThe number of subsets is larger than the number of samples. \n\tSetting number of neighbors to one.",
                                           private$warnings)
                              private$n_neighbors <- 1
                              private$n_subsets <- len_X
                            }

                            if(private$n_subsets == 1){
                              LESSWarn$new("\tThere is only one subset, so the global estimator is set to NULL",
                                           private$warnings)
                              private$global_estimator <- NULL
                              private$d_normalize <- TRUE
                              # If there is also no validation step, then there is
                              # no randomness. So, no need for replications.
                              if(is.null(private$val_size)){
                                LESSWarn$new("\tSince validation set is not used, there is no randomness. \n\tThus, the number of replications is set to one.",
                                             private$warnings)
                                private$n_replications <- 1
                              }
                            }
                          }
                        },

                        fitnoval = function(X, y) {
                          # Fit function: All data is used with the global estimator (no validation)
                          # Tree method is used (no clustering)
                          len_X <- length(y)
                          private$check_input(len_X)
                          tree <- private$tree_method(X)
                          private$replications <- list()
                          for (i in 1:private$n_replications) {
                            sample_indices <- private$rng$choice(range = len_X, size = private$n_subsets)
                            nearest_neighbors <- tree$query(X[sample_indices,], private$n_neighbors)
                            neighbor_indices_list <- nearest_neighbors[[1]]

                            local_models <- list()
                            dists <- matrix(0, len_X, private$n_subsets)
                            predicts <- matrix(0, len_X, private$n_subsets)

                            for (i in 1:nrow(neighbor_indices_list)) {
                              Xneighbors <- as.matrix(X[neighbor_indices_list[i, ],])
                              yneighbors <- as.matrix(y[neighbor_indices_list[i, ]])

                              # Centroid is used as the center of the local sample set
                              local_center <- colMeans(Xneighbors)

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$local_estimator$get_all_fields())) {
                                # set random state to an integer from rng
                                private$local_estimator$set_random_state(private$rng$integers(32767))
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[,i] <- local_model$predict(X)
                              if(is.null(c(private$distance_function))) {
                                dists[,i] <- rbf(X, local_center, 1.0/(private$n_subsets ^ 2.0))
                              }else {
                                dists[,i] <- private$distance_function(X, local_center)
                              }
                            }

                            # Normalize the distances from samples to the local subsets
                            if(private$d_normalize) {
                              denom <- rowSums(dists)
                              denom[denom < 1e-08] <- 1e-08
                              dists <- t(t(dists)/denom)
                            }

                            Z <- dists * predicts
                            scobject <- StandardScaler$new()
                            if(private$scaling){
                              Z <- scobject$fit_transform(Z)
                            }

                            # if(Reduce('|', is.null(private$global_estimator)))
                            if(length(private$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$global_estimator$get_all_fields())){
                                private$global_estimator$set_random_state(private$rng$integers(32767))
                                global_model <- private$global_estimator$fit(Z, y)$clone()
                              }else{
                                global_model <- private$global_estimator$fit(Z, y)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            private$replications <- append(private$replications, Replication$new(local_estimators = local_models,
                                                                                                 sc_object = scobject,
                                                                                                 global_estimator = global_model))
                          }

                          invisible(self)
                        },

                        fitval = function(X, y) {
                          # Fit function: (val_size x data) is used for the global estimator (validation)
                          # Tree method is used (no clustering)

                          private$replications <- list()
                          for (i in 1:private$n_replications) {
                            #Split for global estimation
                            split_list <- train_test_split(cbind(X, y), test_size =  private$val_size,
                                                           random_state = private$rng$integers(32767))
                            X_train <- split_list[[1]]
                            X_val <- split_list[[2]]
                            y_train <- split_list[[3]]
                            y_val <- split_list[[4]]

                            len_X_val <- length(y_val)
                            len_X_train <- length(y_train)
                            # Check the validity of the input
                            if(i == 1){
                              private$check_input(len_X_train)
                            }

                            # A nearest neighbor tree is grown for querying
                            tree <- private$tree_method(X_train)

                            # Select n_subsets many samples to construct the local sample sets
                            sample_indices <- private$rng$choice(range = len_X_train, size = private$n_subsets)
                            # Construct the local sample sets
                            nearest_neighbors <- tree$query(X[sample_indices,], private$n_neighbors)
                            neighbor_indices_list <- nearest_neighbors[[1]]

                            local_models <- list()
                            dists <- matrix(0, len_X_val, private$n_subsets)
                            predicts <- matrix(0, len_X_val, private$n_subsets)

                            for (i in 1:nrow(neighbor_indices_list)) {
                              Xneighbors <- as.matrix(X_train[neighbor_indices_list[i, ],])
                              yneighbors <- as.matrix(y_train[neighbor_indices_list[i, ]])

                              # Centroid is used as the center of the local sample set
                              local_center <- colMeans(Xneighbors)

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$local_estimator$get_all_fields())) {
                                # set random state to an integer from rng
                                private$local_estimator$set_random_state(private$rng$integers(32767))
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[,i] <- local_model$predict(X_val)
                              if(is.null(c(private$distance_function))) {
                                dists[,i] <- rbf(X_val, local_center, 1.0/(private$n_subsets ^ 2.0))
                              }else {
                                dists[,i] <- private$distance_function(X, local_center)
                              }
                            }

                            # Normalize the distances from samples to the local subsets
                            if(private$d_normalize) {
                              denom <- rowSums(dists)
                              denom[denom < 1e-08] <- 1e-08
                              dists <- t(t(dists)/denom)
                            }

                            Z <- dists * predicts
                            scobject <- StandardScaler$new()
                            if(private$scaling){
                              Z <- scobject$fit_transform(Z)
                            }

                            # if(Reduce('|', is.null(private$global_estimator)))
                            if(length(private$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$global_estimator$get_all_fields())){
                                private$global_estimator$set_random_state(private$rng$integers(32767))
                                global_model <- private$global_estimator$fit(Z, y_val)$clone()
                              }else{
                                global_model <- private$global_estimator$fit(Z, y_val)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            private$replications <- append(private$replications, Replication$new(local_estimators = local_models,
                                                                                                 sc_object = scobject,
                                                                                                 global_estimator = global_model))
                          }
                          invisible(self)
                        },

                        fitnovalc = function(X, y){
                          # Fit function: All data is used for the global estimator (no validation)
                          # Clustering is used (no tree method)

                          len_X <- length(y)
                          # Check the validity of the input
                          private$check_input(len_X)

                          # if the cluster method does not have parameter named 'random_state'
                          if(!('random_state' %in% (private$cluster_method$get_all_fields()))){
                            LESSWarn$new("\tClustering method is not random,
                            \tso there is no need for replications unless validaton set is used.
                            \tThe number of replications is set to one.", private$warnings)
                            private$n_replications <- 1
                          }

                          if(private$n_replications == 1){
                            cluster_fit <- private$cluster_method$fit(X)
                          }

                          private$replications <- list()
                          for (i in 1:private$n_replications) {

                            if(private$n_replications > 1){
                              cluster_fit <- private$cluster_method$
                                set_random_state(private$rng$integers(32767))$
                                fit(X)
                            }

                            unique_labels <- unique(cluster_fit$get_labels())
                            # Some clustering methods may find less number of clusters than requested 'n_clusters'
                            private$n_subsets <- append(private$n_subsets, length(unique_labels))
                            n_subsets <- private$n_subsets[[i]]

                            local_models <- list()
                            dists <- matrix(0, len_X, n_subsets)
                            predicts <- matrix(0, len_X, n_subsets)

                            if(!is.null(cluster_fit$get_cluster_centers())){
                              use_cluster_centers <- TRUE
                            }else{
                              use_cluster_centers <- FALSE
                            }

                            for (cluster_indx in 1:length(unique_labels)) {
                              neighbor_indices <- cluster_fit$get_labels() == unique_labels[[cluster_indx]]
                              Xneighbors <- as.matrix(X[neighbor_indices, ])
                              yneighbors <- as.matrix(y[neighbor_indices])
                              if(nrow(yneighbors) == 1){
                                # if there is only one sample in a group,
                                # prevent Xneighbors being a (n,1) dimensional matrix
                                Xneighbors <- t(Xneighbors)
                              }

                              # Centroid is used as the center of the local sample set
                              if(use_cluster_centers){
                                local_center <- cluster_fit$get_cluster_centers()[cluster_indx, ]
                              }else{
                                local_center <- colMeans(Xneighbors)
                              }

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$local_estimator$get_all_fields())) {
                                # set random state to an integer from rng
                                private$local_estimator$set_random_state(private$rng$integers(32767))
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[, cluster_indx] <- local_model$predict(X)
                              if(is.null(c(private$distance_function))) {
                                dists[, cluster_indx] <- rbf(X, local_center, 1.0/(n_subsets ^ 2.0))
                              }else {
                                dists[, cluster_indx] <- private$distance_function(X, local_center)
                              }
                            }

                            # Normalize the distances from samples to the local subsets
                            if(private$d_normalize) {
                              denom <- rowSums(dists)
                              denom[denom < 1e-08] <- 1e-08
                              dists <- t(t(dists)/denom)
                            }

                            Z <- dists * predicts
                            scobject <- StandardScaler$new()
                            if(private$scaling){
                              Z <- scobject$fit_transform(Z)
                            }

                            # if(Reduce('|', is.null(private$global_estimator)))
                            if(length(private$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$global_estimator$get_all_fields())){
                                private$global_estimator$set_random_state(private$rng$integers(32767))
                                global_model <- private$global_estimator$fit(Z, y)$clone()
                              }else{
                                global_model <- private$global_estimator$fit(Z, y)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            private$replications <- append(private$replications, Replication$new(local_estimators = local_models,
                                                                                                 sc_object = scobject,
                                                                                                 global_estimator = global_model))
                          }

                          invisible(self)
                        },

                        fitvalc = function(X, y){
                          # Fit function: (val_size x data) is used for the global estimator (validation)
                          # Clustering is used (no tree method)

                          private$replications <- list()
                          for (i in 1:private$n_replications){
                            # Split for global estimation
                            split_list <- train_test_split(cbind(X, y), test_size =  private$val_size,
                                                           random_state = private$rng$integers(32767))
                            X_train <- split_list[[1]]
                            X_val <- split_list[[2]]
                            y_train <- split_list[[3]]
                            y_val <- split_list[[4]]

                            len_X_val <- length(y_val)
                            len_X_train <- length(y_train)
                            # Check the validity of the input
                            if(i == 1){
                              private$check_input(len_X_train)
                            }

                            if('random_state' %in% (private$cluster_method$get_all_fields())){
                              cluster_fit <- private$cluster_method$
                                set_random_state(private$rng$integers(32767))$
                                fit(X_train)
                            }else{
                              cluster_fit <- private$cluster_method$fit(X_train)
                            }

                            if(i == 1){
                              if(!is.null(cluster_fit$get_cluster_centers())){
                                use_cluster_centers <- TRUE
                              }else{
                                use_cluster_centers <- FALSE
                              }
                            }

                            unique_labels <- unique(cluster_fit$get_labels())
                            # Some clustering methods may find less number of clusters than requested 'n_clusters'
                            private$n_subsets <- append(private$n_subsets, length(unique_labels))
                            n_subsets <- private$n_subsets[[i]]

                            local_models <- list()
                            dists <- matrix(0, len_X_val, n_subsets)
                            predicts <- matrix(0, len_X_val, n_subsets)

                            for (cluster_indx in 1:length(unique_labels)){
                              neighbor_indices <- cluster_fit$get_labels() == unique_labels[[cluster_indx]]
                              Xneighbors <- as.matrix(X_train[neighbor_indices, ])
                              yneighbors <- as.matrix(y_train[neighbor_indices])
                              if(nrow(yneighbors) == 1){
                                # if there is only one sample in a group,
                                # prevent Xneighbors being a (n,1) dimensional matrix
                                Xneighbors <- t(Xneighbors)
                              }

                              # Centroid is used as the center of the local sample set
                              if(use_cluster_centers){
                                local_center <- cluster_fit$get_cluster_centers()[cluster_indx, ]
                              }else{
                                local_center <- colMeans(Xneighbors)
                              }

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$local_estimator$get_all_fields())) {
                                # set random state to an integer from rng
                                private$local_estimator$set_random_state(private$rng$integers(32767))
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- private$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[, cluster_indx] <- local_model$predict(X_val)
                              if(is.null(c(private$distance_function))) {
                                dists[, cluster_indx] <- rbf(X_val, local_center, 1.0/(n_subsets ^ 2.0))
                              }else {
                                dists[, cluster_indx] <- private$distance_function(X_val, local_center)
                              }
                            }

                            # Normalize the distances from samples to the local subsets
                            if(private$d_normalize) {
                              denom <- rowSums(dists)
                              denom[denom < 1e-08] <- 1e-08
                              dists <- t(t(dists)/denom)
                            }

                            Z <- dists * predicts
                            scobject <- StandardScaler$new()
                            if(private$scaling){
                              Z <- scobject$fit_transform(Z)
                            }

                            # if(Reduce('|', is.null(private$global_estimator)))
                            if(length(private$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (private$global_estimator$get_all_fields())){
                                private$global_estimator$set_random_state(private$rng$integers(32767))
                                global_model <- private$global_estimator$fit(Z, y_val)$clone()
                              }else{
                                global_model <- private$global_estimator$fit(Z, y_val)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            private$replications <- append(private$replications, Replication$new(local_estimators = local_models,
                                                                                                 sc_object = scobject,
                                                                                                 global_estimator = global_model))
                          }

                          invisible(self)
                        }
                      ),
                      public = list(
                        #' @description Creates a new instance of R6 Class of LESSBase
                        initialize = function(replications = NULL, scobject = NULL, isFitted = FALSE) {
                          private$replications = replications
                          private$scobject = scobject
                          private$isFitted = isFitted
                        },

                        #' @description Prints detailed information of the class
                        print = function() {
                          n_subsets <- unlist(private$n_subsets)
                          cat("Number of subsets: ", n_subsets, "\n")
                          cat("Number of samples in each subset: ", private$n_neighbors, "\n")
                        },

                        #' @description  Auxiliary function returning the number of subsets
                        get_n_subsets = function(){
                          return(private$n_subsets)
                        },

                        #' @description Auxiliary function returning the number of neighbors
                        get_n_neighbors = function(){
                          return(private$n_neighbors)
                        },

                        #' @description Auxiliary function returning the percentage of samples used to set the number of neighbors
                        get_frac = function(){
                          return(private$frac)
                        },

                        #' @description Auxiliary function returning the number of replications
                        get_n_replications = function(){
                          return(private$n_replications)
                        },

                        #' @description Auxiliary function returning the flag for normalization
                        get_d_normalize = function(){
                          return(private$d_normalize)
                        },

                        #' @description Auxiliary function returning the flag for scaling
                        get_scaling = function(){
                          return(private$scaling)
                        },

                        #' @description Auxiliary function returning the validation set size
                        get_val_size = function(){
                          return(private$val_size)
                        },

                        #' @description Auxiliary function returning the random seed
                        get_random_state = function(){
                          return(private$random_state)
                        },

                        #' @description Auxiliary function returning the isFitted flag
                        get_isFitted = function(){
                          return(private$isFitted)
                        }
                      )
                    )

#' @title  LESSRegressor
#'
#' @description Regressor for Learning with Subset Selection (LESS)
#'
#' @param frac fraction of total samples used for the number of neighbors (default is 0.05)
#' @param n_neighbors number of neighbors (default is NULL)
#' @param n_subsets number of subsets (default is NULL)
#' @param n_replications number of replications (default is 20)
#' @param d_normalize distance normalization (default is TRUE)
#' @param val_size percentage of samples used for validation (default is NULL - no validation)
#' @param random_state initialization of the random seed (default is NULL)
#' @param tree_method method used for constructing the nearest neighbor tree, e.g., less::KDTree (default)
#' @param cluster_method method used for clustering the subsets, e.g., less::KMeans (default is NULL)
#' @param local_estimator estimator for the local models (default is less::LinearRegression)
#' @param global_estimator estimator for the global model (default is less::DecisionTreeRegressor)
#' @param distance_function distance function evaluating the distance from a subset to a sample,
#' e.g., df(subset, sample) which returns a vector of distances (default is RBF(subset, sample, 1.0/n_subsets^2))
#' @param scaling flag to normalize the input data (default is TRUE)
#' @param warnings flag to turn on (TRUE) or off (FALSE) the warnings (default is TRUE)
#'
#' @return R6 class of LESSRegressor
#' @seealso [LESSBase]
#' @export
LESSRegressor <- R6::R6Class(classname = "LESSRegressor",
                             inherit = LESSBase,
                             private = list(
                               frac = NULL,
                               n_neighbors = NULL,
                               n_subsets = NULL,
                               n_replications = NULL,
                               d_normalize = NULL,
                               val_size = NULL,
                               random_state = NULL,
                               tree_method = NULL,
                               cluster_method = NULL,
                               local_estimator = NULL,
                               global_estimator = NULL,
                               distance_function = NULL,
                               scaling = NULL,
                               warnings = NULL,
                               rng = NULL
                             ),
                             public = list(
                               #' @description Creates a new instance of R6 Class of LESSRegressor
                               #'
                               #' @examples
                               #' lessRegressor <- LESSRegressor$new()
                               #' lessRegressor <- LESSRegressor$new(val_size = 0.3)
                               #' lessRegressor <- LESSRegressor$new(cluster_method = less::KMeans$new())
                               #' lessRegressor <- LESSRegressor$new(val_size = 0.3, cluster_method = less::KMeans$new())
                               initialize = function(frac = NULL, n_neighbors = NULL, n_subsets = NULL, n_replications = 20, d_normalize = TRUE, val_size = NULL,
                                                     random_state = NULL, tree_method = function(X) KDTree$new(X), cluster_method = NULL,
                                                     local_estimator = LinearRegression$new(), global_estimator = DecisionTreeRegressor$new(), distance_function = NULL,
                                                     scaling = TRUE, warnings = TRUE) {
                                 private$frac = frac
                                 private$n_replications = n_replications
                                 private$random_state = random_state
                                 private$n_subsets = n_subsets
                                 private$n_neighbors = n_neighbors
                                 private$local_estimator = local_estimator
                                 private$d_normalize = d_normalize
                                 private$global_estimator = global_estimator
                                 private$scaling = scaling
                                 private$cluster_method = cluster_method
                                 private$distance_function = distance_function
                                 private$rng = RandomGenerator$new(random_state = private$random_state)
                                 private$warnings = warnings
                                 private$val_size = val_size
                                 private$tree_method = tree_method
                               },

                               #' @description
                               #' Dummy fit function that calls the proper method according to validation and clustering parameters
                               #' Options are:
                               #' - Default fitting (no validation set, no clustering)
                               #' - Fitting with validation set (no clustering)
                               #' - Fitting with clustering (no) validation set)
                               #' - Fitting with validation set and clustering
                               #'
                               #' @param X 2D matrix or dataframe that includes predictors
                               #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                               #'
                               #' @return Fitted R6 Class of LESSRegressor
                               #'
                               #' @examples
                               #' data(abalone)
                               #' split_list <- train_test_split(abalone, test_size =  0.3)
                               #' X_train <- split_list[[1]]
                               #' X_test <- split_list[[2]]
                               #' y_train <- split_list[[3]]
                               #' y_test <- split_list[[4]]
                               #'
                               #' lessRegressor <- LESSRegressor$new()
                               #' lessRegressor$fit(X_train, y_train)
                               fit = function(X, y){

                                 # Check that X and y have correct shape
                                 X_y_list <- check_X_y(X, y)
                                 X <- X_y_list[[1]]
                                 y <- X_y_list[[2]]
                                 private$set_local_attributes()

                                 if(private$scaling){
                                   private$scobject <- StandardScaler$new()
                                   X <- private$scobject$fit_transform(X)
                                 }

                                 if(!is.null(private$val_size)){
                                   # Validation set is used for global estimation
                                   if(length(private$cluster_method) == 0){
                                     private$fitval(X, y)
                                   }
                                   else{
                                     private$fitvalc(X, y)
                                   }
                                 }
                                 else{
                                   # Validation set is not used for global estimation
                                   if(length(private$cluster_method) == 0){
                                     private$fitnoval(X, y)
                                   }
                                   else{
                                     private$fitnovalc(X, y)
                                   }
                                 }
                                 private$isFitted <- TRUE
                                 invisible(self)
                               },

                               #' @description
                               #' Predictions are evaluated for the test samples in X0
                               #'
                               #' @param X0 2D matrix or dataframe that includes predictors
                               #'
                               #' @return Predicted values of the given predictors
                               #'
                               #' @examples
                               #' lessRegressor <- LESSRegressor$new()
                               #' lessRegressor$fit(X_train, y_train)
                               #' preds <- lessRegressor$predict(X_test)
                               #'
                               #' lessRegressor <- LESSRegressor$new()
                               #' preds <- lessRegressor$fit(X_train, y_train)$predict(X_test)
                               #'
                               #' preds <- LESSRegressor$new()$fit(X_train, y_train)$predict(X_test)
                               #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                               predict = function(X0) {

                                 check_is_fitted(self)
                                 # Input validation
                                 check_matrix(X0)

                                 if(private$scaling){
                                   X0 <- private$scobject$transform(X0)
                                 }

                                 len_X0 <- nrow(X0)
                                 yhat <- matrix(0, len_X0, 1)

                                 for (i in 1:private$n_replications) {
                                   # Get the fitted global and local estimators
                                   global_model <- private$replications[[i]]$global_estimator
                                   local_models <- private$replications[[i]]$local_estimators

                                   if(length(private$cluster_method) == 0){
                                     n_subsets <- private$n_subsets
                                   }else{
                                     n_subsets <- private$n_subsets[[i]]
                                   }

                                   dists <- matrix(0, len_X0, n_subsets)
                                   predicts <- matrix(0, len_X0, n_subsets)
                                   for(j in 1:n_subsets){
                                     local_center <- local_models[[j]]$center
                                     local_model <- local_models[[j]]$estimator
                                     predicts[, j] <- local_model$predict(X0)

                                     if(is.null(c(private$distance_function))) {
                                       dists[, j] <- rbf(X0, local_center, 1.0/(n_subsets ^ 2.0))
                                     }else {
                                       dists[, j] <- private$distance_function(X0, local_center)
                                     }
                                   }

                                   # Normalize the distances from samples to the local subsets
                                   if(private$d_normalize) {
                                     denom <- rowSums(dists)
                                     denom[denom < 1e-08] <- 1e-08
                                     dists <- t(t(dists)/denom)
                                   }

                                   Z0 <- dists * predicts
                                   if(private$scaling){
                                     Z0 <- private$replications[[i]]$sc_object$transform(Z0)
                                   }

                                   if(length(global_model) != 0){
                                     yhat <- yhat + global_model$predict(Z0)
                                   }else{
                                     yhat <- yhat + rowSums(Z0)
                                   }
                                 }

                                 yhat <- yhat/private$n_replications

                                 return(yhat)
                               }
                             ))

###########################

sine_data_list <- synthetic_sine_curve()
X_sine <- sine_data_list[[1]]
y_sine <- sine_data_list[[2]]
synthetic_sine_data <- cbind(X_sine, y_sine)

#########################

test <- function(data = abalone) {
  # UNCOMMENT THIS CODE BLOCK TO PROFILE THE CODE AND SEE A PERFORMANCE ANALYSIS OF THE CODE
  # profvis::profvis({
  #
  # })

  # Now Selecting 70% of data as sample from total 'n' rows of the data
  split_list <- train_test_split(data, test_size =  0.3, random_state = 1)
  X_train <- split_list[[1]]
  X_test <- split_list[[2]]
  y_train <- split_list[[3]]
  y_test <- split_list[[4]]

  # y_index = 1
  # X_train <- as.matrix(data[1:400,-y_index])
  # y_train <- data[1:400,y_index]
  # X_test <- as.matrix(data[401:nrow(data),-y_index])
  # y_test <- data[401:nrow(data),y_index]

  # model <- SVR$new()
  # preds <- model$fit(X_train, y_train)$predict(X_test)
  # print(head(matrix(c(y_test, preds), ncol = 2)))
  # mape <- MLmetrics::MAPE(preds, y_test)
  # cat("MAPE: ", mape, "\n")

  hc <- KMeans$new()
  hc$fit(X_train)
  hc$set_random_state(5)

  # cat("Total number of training samples: ", nrow(X_train), "\n")
  # LESS <- LESSRegressor$new(cluster_method = HierarchicalClustering$new())
  # preds <- LESS$fit(X_train, y_train)$predict(X_test)
  # print(LESS)
  # print(head(matrix(c(y_test, preds), ncol = 2)))
  # mape <- MLmetrics::MAPE(preds, y_test)
  # cat("MAPE: ", mape, "\n")
  #
  # cat("Total number of training samples: ", nrow(X_train), "\n")
  # LESS <- LESSRegressor$new()
  # preds <- LESS$fit(X_train, y_train)$predict(X_test)
  # print(LESS)
  # print(head(matrix(c(y_test, preds), ncol = 2)))
  # mape <- MLmetrics::MAPE(preds, y_test)
  # cat("MAPE: ", mape, "\n")

}

comparison = function(dataset = superconduct){
  data_list <- list("abalone"= abalone, "machine" = machine, "insurance" = insurance,
                    "forestFires" = forestFires, "concrete" = concrete)
  model_list <- c(LESSRegressor$new(),
                  LESSRegressor$new(global_estimator = DecisionTreeRegressor2$new()),
                  DecisionTreeRegressor$new(),
                  DecisionTreeRegressor2$new(),
                  LinearRegression$new(),
                  KNeighborsRegressor$new(),
                  RandomForestRegressor$new())
  model_name_list <- c("LESS-rpart", "LESS-party", "DT-rpart", "DT-party", "LR", "KNN", "RF")
  # comparison_plot(X_train, y_train, model_list)

  for(d in 1:length(data_list)){
    data <- data_list[[d]]
    print(names(data_list)[d])
    for(i in 1:length(model_list)){
      mse_matrix <- matrix(0, 5, 1)
      mape_matrix <- matrix(0, 5, 1)
      for (a in 1:5) {
        split_list <- train_test_split(data, test_size =  0.3)
        X_train <- split_list[[1]]
        X_test <- split_list[[2]]
        y_train <- split_list[[3]]
        y_test <- split_list[[4]]
        preds <- model_list[[i]]$fit(X_train, y_train)$predict(X_test)
        mse_matrix[a,1] <- MLmetrics::MSE(preds, y_test)
        mape_matrix[a,1] <- MLmetrics::MAPE(preds, y_test)
      }
      cat(model_name_list[[i]], "MSE:", colMeans(mse_matrix), "\n")
      cat(model_name_list[[i]], "MAPE:", colMeans(mape_matrix), "\n")
    }
  }

}
