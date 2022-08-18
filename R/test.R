testFunc <- function(data = abalone) {
  # UNCOMMENT THIS CODE BLOCK TO PROFILE THE CODE AND SEE A PERFORMANCE ANALYSIS OF THE CODE
  # profvis::profvis({
  #
  # })

  data <- iris
  # data <- read.csv("C:/Users/ozerc/Desktop/data/balance-scale-cls.data", sep = ",", header = FALSE)
  # data <- data[,-c(2)]
  # data <- data[,1:30]

  # split_list <- train_test_split(data, test_size =  0.3, y_index = 1)
  split_list <- train_test_split(data, test_size =  0.3)
  X_train <- split_list[[1]]
  X_test <- split_list[[2]]
  y_train <- split_list[[3]]
  y_test <- split_list[[4]]

  # profvis::profvis({
  #   str <- LESSClassifier$new(multiclass = "ovo")
  #   preds <- str$fit(X_train, y_train)$predict(X_test)
  #   example <- caret::confusionMatrix(data=factor(preds), reference = factor(y_test))
  #   print(example)
  # })

  str <- LESSClassifier$new(multiclass = "ovr")
  # str <- DecisionTreeClassifier$new()
  preds <- str$fit(X_train, y_train)$predict(X_test)
  print(preds)
  example <- caret::confusionMatrix(data=factor(preds), reference = factor(y_test))
  print(example)

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
  # LESS <- LESSRegressor$new(random_state = 1)
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

comparison = function(dataset = synthetic_sine_data){
  # split_list <- train_test_split(dataset, test_size =  0.3)
  # X_train <- split_list[[1]]
  # X_test <- split_list[[2]]
  # y_train <- split_list[[3]]
  # y_test <- split_list[[4]]

  data_list <- list("abalone"= abalone, "machine" = machine, "insurance" = insurance,
                    "forestFires" = forestFires, "concrete" = concrete)
  model_list <- c(LESSRegressor$new(),
                  LESSRegressor$new(global_estimator = DecisionTreeRegressor2$new()),
                  DecisionTreeRegressor$new(),
                  DecisionTreeRegressor2$new(),
                  LinearRegression$new(),
                  KNeighborsRegressor$new(),
                  RandomForestRegressor$new(),
                  SVR$new())
  model_name_list <- c("LESS-rpart", "LESS-party", "DT-rpart", "DT-party", "LR", "KNN", "RF", "SVR")
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
