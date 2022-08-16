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

getMode <- function(v) {
  tab <- table(v)[which.max(table(v))]
  mode <- as.integer(names(tab))
  count <- unname(tab)
  return(c(mode, count))
}

#' @title Synthetic Sine Curve
#'
#' @description A simple function to generate n_samples from sine curve in the range (-10, 10) with some amplitude.
#' The function returns the dataset (X, y), and plots the function (curve) along with the dataset (circles)
#'
#' @export
synthetic_sine_curve = function(n_samples=200) {
  xvals <- seq(-10,10,length.out=n_samples+1)[-(n_samples+1)]

  X <- rep(0, n_samples)
  y <- rep(0, n_samples)
  for(i in 1:n_samples){
    xran <- -10 + 20*runif(1)
    X[i] <- xran
    y[i] <- 10*sin(xran) + 2.5*rnorm(1)
  }
  dev.new(width=960, height=540, unit = "px", noRStudioGD = TRUE)
  plot(xvals, 10*sin(xvals), type = "l", col="red", ylab="",yaxt="n", xlab="",xaxt="n")
  par(new=TRUE)
  plot(X, y, pch = 19, col="blue",  ylab="",yaxt="n", xlab="",xaxt="n")
  par(new=FALSE)

  return(list(X, y))
}

#' @title Comparsion Plot
#'
#' @description Plots the fitted functions obtained with various regressors (using their default values) on the
#' one-dimensional dataset (X, y).
#'
#' @export
comparison_plot = function(X, y, model_list){
  xlb <- floor(min(X)-1)
  xub <- floor(max(X)+1)
  xvals <- seq(xlb, xub, by=0.1)
  color_list <- c("blue", "green", "red", "black", "brown", "purple", "orange", "seagreen2", "pink")
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

#' @title Get Functions
#'
#' @description Prints the available regressors, clustering methods, tree functions and helper functions within LESS package.
#'
#' @export
#'
#' @examples
#' get_functions()
get_functions = function(){
  regressor_list <- c("DecisionTreeRegressor", "KNeighborsRegressor", "LESSRegressor", "LinearRegression", "RandomForestRegressor", "SVR")
  clustering_list <- c("HierarchicalClustering", "KMeans")
  tree_list <- c("CoverTree", "KDTree")
  helper_function_list <- c("laplacian", "rbf", "train_test_split")
  cat("Regressors within LESS Package:", "\n")
  for(reg in regressor_list){
    cat("-", reg, "\n")
  }
  cat("\n")
  cat("Clustering Methods within LESS Package:", "\n")
  for(clust in clustering_list){
    cat("-", clust, "\n")
  }
  cat("\n")
  cat("Tree Functions within LESS Package:", "\n")
  for(tree in tree_list){
    cat("-", tree, "\n")
  }
  cat("\n")
  cat("Helper functions within LESS Package:", "\n")
  for(help in helper_function_list){
    cat("-", help, "\n")
  }
}
