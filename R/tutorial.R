#' @importFrom profvis profvis
testFunc <- function(data = abalone) {
  # UNCOMMENT THIS CODE BLOCK TO PROFILE THE CODE AND SEE A PERFORMANCE ANALYSIS OF THE CODE
  # profvis::profvis({
  #
  # })

  data <- iris
  # data <- read.csv("C:/Users/ozerc/Desktop/data/algeria-fire-cls1.csv", sep = ",", header = TRUE)
  # data[,-ncol(data)] <- sapply(data[,-ncol(data)], as.numeric)
  # data <- data[,-c(2)]
  # data <- data[,1:30]

  # split_list <- train_test_split(data, test_size =  0.3, y_index = 1)
  split_list <- train_test_split(data, test_size =  0.3)
  X_train <- split_list[[1]]
  X_test <- split_list[[2]]
  y_train <- split_list[[3]]
  y_test <- split_list[[4]]


  # print(k_fold_cv(data = data, random_state = 2, model = LESSClassifier$new(), k = 3))

  test_timing(2, X_train, y_train)

  # str <- LESSClassifier$new()
  # preds <- str$fit(X_train, y_train)$predict(X_test)
  # example <- caret::confusionMatrix(data=factor(preds), reference = factor(y_test))
  # print(example$overall["Accuracy"])

  # profvis::profvis({
  #   str <- LESSClassifier$new(multiclass = "ovr")
  #   preds <- str$fit(X_train, y_train)$predict(X_test)
  #   example <- caret::confusionMatrix(data=factor(preds), reference = factor(y_test))
  #   print(example)
  # })

  # str <- LESSClassifier$new(multiclass = "ovr")
  # # str <- KNeighborsClassifier$new()
  # preds <- str$fit(X_train, y_train)$predict(X_test)
  # example <- caret::confusionMatrix(data=factor(preds), reference = factor(y_test))
  # print(example)

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

  # cat("Total number of training samples: ", nrow(X_train), "\n")
  # LESS <- LESSRegressor$new(random_state = 1, tree_method = function(data) CoverTree$new(X=data))
  # preds <- LESS$fit(X_train, y_train)$predict(X_test)
  # print(LESS)
  # print(head(matrix(c(y_test, preds), ncol = 2)))
  # mape <- MLmetrics::MAPE(preds, y_test)
  # cat("MAPE: ", mape, "\n")
  #
  # cat("Total number of training samples: ", nrow(X_train), "\n")
  # LESS <- LESSRegressor$new(random_state = 1, global_estimator = LESSRegressor$new())
  # preds <- LESS$fit(X_train, y_train)$predict(X_test)
  # print(LESS)
  # print(head(matrix(c(y_test, preds), ncol = 2)))
  # mape <- MLmetrics::MAPE(preds, y_test)
  # cat("MAPE: ", mape, "\n")

  # labels <- c(6,5,4,6,5,5,4,4,5,5)
  # testdata <- concrete[1:10,-3]
  # testdata[,ncol(testdata)] <- labels
  # X <- as.matrix(testdata[,-ncol(testdata)])
  # y <- as.matrix(testdata[,ncol(testdata)])

}

#' @title Synthetic Sine Curve
#'
#' @description A simple function to generate n_samples from sine curve in the range (-10, 10) with some amplitude.
#' The function returns the dataset (X, y), and plots the function (curve) along with the dataset (circles)
#'
#' @param n_samples Number of data points to be generated
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
#' @param X Predictors
#' @param y Response variables
#' @param model_list List of models to be compared
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

speed_test_regressors = function(X, y){
  regressor_names <- c("LESS", "DT", "LR", "KNN", "RF", "SVR")

  regressor_list <- c(LESSRegressor$new(),
                      DecisionTreeRegressor$new(),
                      LinearRegression$new(),
                      KNeighborsRegressor$new(),
                      RandomForestRegressor$new(),
                      SVR$new())

  all_timings = list()
  for(i in 1:length(regressor_list)){
    start <- Sys.time()
    regressor_list[[i]]$fit(X, y)
    end <- Sys.time()
    all_timings[regressor_names[[i]]] <- (end-start)
  }
  return(all_timings)
}

speed_test_classifiers = function(X, y){
  classifier_names <- c("LESS", "DT", "KNN", "RF", "SVC")

  classifier_list <- c(LESSClassifier$new(),
                       DecisionTreeClassifier$new(),
                       KNeighborsClassifier$new(),
                       RandomForestClassifier$new(),
                       SVC$new())

  all_timings = list()
  for(i in 1:length(classifier_list)){
    start <- Sys.time()
    classifier_list[[i]]$fit(X, y)
    end <- Sys.time()
    all_timings[classifier_names[[i]]] <- (end-start)
  }
  return(all_timings)
}

#' @title Compare Fitting Time
#'
#' @description Plots a histogram chart which shows the fitting time obtained from various regressors/classifiers (using their default values) on the
#' given dataset (X, y).
#'
#' @param type 1 to compare regressors, 2 for comparing classifiers
#' @param X Predictors
#' @param y Response variables
#'
#' @export
test_timing = function(type = 1, X, y){
 if(type == 1){
   timings <- speed_test_regressors(X, y)
   barplot(unlist(timings),
           main = "Fitting Time of Different Regressors",
           xlab = "Regressors",
           ylab = "Time Elapsed in log(sec.)",
           col = "darkred",
           log = "y",
           names.arg = names(timings))
 }else if(type == 2){
   timings <- speed_test_classifiers(X, y)
   barplot(unlist(timings),
           main = "Fitting Time of Different Classifiers",
           xlab = "Classifiers",
           ylab = "Time Elapsed in log(sec.)",
           col = "darkred",
           log = "y",
           names.arg = names(timings))
 }
}


