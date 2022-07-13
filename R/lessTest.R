SklearnEstimator <- R6::R6Class(classname = "SklearnEstimator",
                                public = list(
                                  random_state = NULL,
                                  initialize = function(random_state = NA) {
                                    self$random_state = random_state
                                  },
                                  fit = function() {
                                    #maybe check for
                                    print("dummy fit function")
                                    invisible(self)
                                  },
                                  predict = function(){
                                    print("dummy predict function")
                                    invisible(self)
                                  },
                                  public_fields = function(){
                                    classList <- class(self)[-length(class(self))]
                                    classNum <- length(classList)
                                    fieldList <- list()
                                    for (i in 1:classNum) {
                                      fields <- get(class(self)[i])$public_fields
                                      fieldList <- append(fieldList, fields)
                                    }
                                    return(names(fieldList))
                                  },

                                  get_attributes = function(){
                                    values <- purrr::map(self$public_fields(), ~.subset2(self, .x))
                                    names(values) <- self$public_fields()
                                    return(values)
                                  }
                                )
                                )

LocalModel <- R6::R6Class(classname = "LocalModel",
                      public = list(
                        estimator = NULL,
                        center = NULL,
                        initialize = function(estimator = NA, center = NA) {
                          self$estimator <- estimator
                          self$center <- center
                        }
                      ))

Replication <- R6::R6Class(classname = "Replication",
                       public = list(
                         sc_object = NULL,
                         global_estimator = NULL,
                         local_estimators = NULL,
                         initialize = function(sc_object = NA, global_estimator = NA, local_estimators = NA) {
                           self$sc_object <- sc_object #"StandardScaler"
                           self$global_estimator <- global_estimator #"SklearnEstimator"
                           self$local_estimators <- local_estimators #List[LocalModel]
                         }
                       ))

LinearRegression <- R6::R6Class(classname = "LinearRegression",
                                inherit = SklearnEstimator,
                                public = list(
                                  model = NULL,
                                  fit = function(X, y) {
                                    df <- prepareDataset(X, y)
                                    self$model <- lm(y ~ ., data = df)
                                    invisible(self)
                                  },
                                  predict = function(X) {
                                    data <- prepareXset(X)
                                    predict(self$model, newdata = data)
                                  },
                                  printModel = function() {
                                    summary(self$model)
                                  }
                                )
                                )

DecisionTreeRegressor <- R6::R6Class(classname = "DecisionTreeRegressor",
                                     inherit = SklearnEstimator,
                                     public = list(
                                       model = NULL,
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         self$model <- rpart::rpart(y ~ ., method = "anova", data = df)
                                         invisible(self)
                                         # print("model: ")
                                         # summary(model)
                                         # rpart.plot::rpart.plot(model)
                                       },
                                       predict = function(X) {
                                         data <- prepareXset(X)
                                         predict(self$model, data, method = "anova")
                                       }
                                     )
                                     )
StandardScaler <- R6::R6Class(classname = "StandardScaler",
                              public = list(
                                mean = NULL,
                                stdev = NULL,
                                fit = function(X) {
                                  # standart deviation function for internal use
                                  # the default stdev() function of R, divides by length(ln)-1
                                  standart_dev <- function(ln) {
                                    sqrt(sum((ln - mean(ln)) ^ 2 / length(ln)))
                                  }
                                  # assign mean and standart deviation parameters
                                  self$stdev <- apply(X, 2, standart_dev)
                                  self$mean <- colMeans(X)
                                  invisible(self)
                                },
                                transform = function(X) {
                                  # append mean and standart deviation values to the X matrix
                                  merged <- rbind(self$mean, self$stdev, X)
                                  # standardize each value by the corresponding mean and stdev values
                                  # using z = (x - u) / s formula
                                  merged <- apply(merged, 2, function(x) (x - x[1]) / x[2] )
                                  #return the standardized version of original X matrix, extract the mean and stdev rows (first 2 cols)
                                  return(merged[3:nrow(merged),])
                                },
                                fit_transform = function(X) {
                                  self$fit(X)$transform(X)
                                },
                                print = function() {
                                  cat("Mean: ", self$mean, "\n")
                                  cat("Standart Deviation: ", self$stdev, "\n")
                                }
                              ))

####################

#' RBF kernel - L2 norm
#' This is is used as the default distance function in LESS
rbf <- function(data, center, coeff=0.01){
  # data <- matrix(data)
  distFunction <- function(point1, point2) {
    exp(-coeff * norm(point1 - point2, type = "2"))
  }
  apply(data, 1, distFunction, center)
}

# Standardization function instead of StandardScaler
standardize = function(x){
  standart_dev <- function(ln) {
    sqrt(sum((ln - mean(ln)) ^ 2 / length(ln)))
  }
  z <- (x - mean(x)) / standart_dev(x)
  return( z)
}

# takes X and y datasets and merges them into a dataframe with column names
prepareDataset = function(X, y) {
  merged_data <- cbind(y, X)
  df <- as.data.frame(merged_data)
  colX <- list()
  for(i in 1:ncol(X)){
    colX <- append(colX, paste(c("X", i), collapse = "_"))
  }
  column_names = append(list("y"), colX)
  colnames(df) <- column_names
  df
}

# takes X dataset and convert it into a dataframe with column names
prepareXset = function(X) {
  df <- as.data.frame(X)
  colX <- list()
  for(i in 1:ncol(X)){
    colX <- append(colX, paste(c("X", i), collapse = "_"))
  }
  colnames(df) <- colX
  df
}
###################

xData=c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
yData=c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

####################

LESSBase <- R6::R6Class(classname = "LESSBase",
                      inherit = SklearnEstimator,
                      public = list(
                        isFitted = FALSE,
                        replications = NULL,
                        scobject = NULL,

                        # fix replication amount
                        initialize = function(replications = NA, scobject = NA, isFitted = FALSE) {
                          self$replications = replications
                          self$scobject = scobject
                          self$isFitted = isFitted
                        },
                        set_local_attributes = function() {
                          print("to be done")
                        },

                        fitnoval = function(X, y) {
                          #' Fit function: All data is used with the global estimator (no validation)
                          #' Tree method is used (no clustering)
                          len_X <- length(y)
                          #FIXME check_input
                          self$replications <- list()
                          for (i in 1:self$n_replications) {
                            # set.seed(self$random_state) # set seed each time so
                            sample_indices <- sample(len_X, size = self$n_subsets)
                            nearest_neighbors <- RANN::nn2(data = X, query = X[sample_indices,], k = self$n_neighbors)
                            neighbor_indices_list <- nearest_neighbors[[1]]

                            local_models <- list() # List[LocalModel]
                            dists <- matrix(0, len_X, self$n_subsets)
                            predicts <- matrix(0, len_X, self$n_subsets)

                            for (i in 1:nrow(neighbor_indices_list)) {
                              Xneighbors <- as.matrix(X[neighbor_indices_list[i, ],])
                              yneighbors <- as.matrix(y[neighbor_indices_list[i, ]])

                              # Centroid is used as the center of the local sample set
                              local_center = colMeans(Xneighbors)

                              local_model <- NULL
                              #if random_state is set
                              if(!is.na(self$local_estimator$get_attributes()$random_state)) {
                                # FIXME
                                # self$local_estimator$random_state <-
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              # predicts[,i] <- self$local_estimator$predict(X)
                              predicts[,i] <- local_model$predict(X)
                              if(is.na(self$distance_function)) {
                                dists[,i] <- rbf(X, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }else {
                                # FIXME add distance function instead of rbf function
                                dists[,i] <- rbf(X, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }
                            }


                            if(self$d_normalize) {
                              denom <- rowSums(dists)
                              denom[denom < 1e-08] <- 1e-08
                              dists <- t(t(dists)/denom)
                            }

                            Z <- dists * predicts
                            scobject <- StandardScaler$new()
                            if(self$scaling){
                              Z <- scobject$fit_transform(Z)
                            }

                            global_model <- NULL
                            # if(Reduce('|', is.na(self$global_estimator)))
                            if(length(self$global_estimator) != 0){ #for a null environment, the length is 0
                              if(!is.na(self$global_estimator$get_attributes()$random_state)){
                                # FIXME add random state
                                global_model <- self$global_estimator$fit(Z, y)$clone()
                              }else{
                                global_model <- self$global_estimator$fit(Z, y)$clone()
                              }
                            }
                            self$replications <- append(self$replications, Replication$new(local_estimators = local_models,
                                                                                           sc_object = scobject,
                                                                                           global_estimator = global_model))
                          }

                          invisible(self)
                        }
                      )
                    )


LESSRegressor <- R6::R6Class(classname = "LESSRegressor",
                             inherit = LESSBase,
                             public = list(
                               n_replications = NULL,
                               random_state = NULL,
                               n_subsets = NULL,
                               n_neighbors = NULL,
                               local_estimator = NULL,
                               d_normalize = NULL,
                               global_estimator = NULL,
                               scaling = NULL,
                               cluster_method = NULL,
                               distance_function = NULL,
                               initialize = function(n_replications = 5, random_state = NA, n_subsets = 2, n_neighbors = 5,
                                                     local_estimator = NA, d_normalize = TRUE, global_estimator = NA, scaling = TRUE,
                                                     cluster_method = NA, distance_function = NA) {
                                 self$n_replications = n_replications
                                 self$random_state = random_state
                                 self$n_subsets = n_subsets
                                 self$n_neighbors = n_neighbors
                                 self$local_estimator = local_estimator
                                 self$d_normalize = d_normalize
                                 self$global_estimator = global_estimator
                                 self$scaling = scaling
                                 self$cluster_method = cluster_method
                                 self$distance_function = distance_function
                               },
                               fit = function(X, y){
                                 # FIXME check operations

                                 if(self$scaling){
                                   self$scobject <- StandardScaler$new()
                                   X <- self$scobject$fit_transform(X)
                                 }
                                 self$fitnoval(X, y)
                                 self$isFitted = TRUE
                                 invisible(self)
                               },
                               predict = function(X0) {
                                 if(self$scaling){
                                   X0 = self$scobject$fit_transform(X0)
                                 }

                                 len_X0 <- NULL
                                 if(is.matrix(X0) | is.data.frame(X0)){
                                   len_X0 <- nrow(X0)
                                 }else{
                                   print("nah")
                                 }

                                 yhat <- matrix(0, len_X0, 1)
                                 for (i in 1:self$n_replications) {
                                   global_model <- self$replications[[i]]$global_estimator
                                   local_models <- self$replications[[i]]$local_estimators

                                   n_subsets <- NULL
                                   if(is.na(self$cluster_method)){
                                     n_subsets <- self$n_subsets
                                   }else{
                                     n_subsets <- self$n_subsets[[i]]
                                   }

                                   dists <- matrix(0, len_X0, n_subsets)
                                   predicts <- matrix(0, len_X0, n_subsets)
                                   for(j in 1:n_subsets){
                                     # Get the fitted global and local estimators
                                     local_center <- local_models[[j]]$center
                                     local_model <- local_models[[j]]$estimator
                                     predicts[,j] <- local_model$predict(X0)

                                     if(is.na(self$distance_function)) {
                                       dists[, j] <- rbf(X0, local_center, 1.0/(n_subsets ^ 2.0))
                                     }else {
                                       # FIXME add distance function instead of rbf function
                                       dists[, j] <- rbf(X0, local_center, 1.0/(n_subsets ^ 2.0))
                                     }
                                   }

                                   # Normalize the distances from samples to the local subsets
                                   if(self$d_normalize) {
                                     denom <- rowSums(dists)
                                     denom[denom < 1e-08] <- 1e-08
                                     dists <- t(t(dists)/denom)
                                   }

                                   Z0 <- dists * predicts
                                   if(self$scaling){
                                     Z0 <- self$replications[[i]]$sc_object$transform(Z0)
                                   }

                                   if(length(global_model) != 0){
                                     yhat <- yhat + global_model$predict(Z0)
                                   }else{
                                     yhat <- yhat + rowSums(Z0)
                                   }
                                 }

                                 yhat <- yhat/self$n_replications
                                 return(yhat)
                               }
                             ))


#' Apply linear regression
#'
#' @param x vector of x values, default c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
#' @param y vector of y values, default c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
#'
#' @return Prints the relation between y and x
#' @export
#'
#' @examples linReg()
linReg <- function() {
  abalone <- read.csv(file='datasets/abalone.csv', header = FALSE)
  xvals <- abalone[,-ncol(abalone)]
  yval <- abalone[,ncol(abalone)]
  LESS <- LESSRegressor$new(n_replications = 50, n_neighbors = 209, n_subsets=19, local_estimator = LinearRegression$new(), global_estimator = LinearRegression$new())
  preds <- LESS$fit(xvals, yval)$predict(xvals)
  data <- data.frame(actual = yval, pred = preds)
  mse <- mean((data$actual - data$pred)^2)
  print(mse)


}
