SklearnEstimator <- R6::R6Class(classname = "SklearnEstimator",
                                public = list(
                                  random_state = NULL,
                                  initialize = function(random_state = NA) {
                                    self$random_state = random_state
                                  },
                                  printsk = function() {
                                    cat("sklearn: ", self$isFitted, "\n")
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
                                  fit = function(X, y) {
                                    df <- prepareDataset(X, y)
                                    model <- lm(y ~ ., data = df)
                                    model
                                  },
                                  predict = function(X, model) {
                                    data <- prepareXset(X)
                                    predict(model, newdata = data)
                                  }
                                )
                                )

DecisionTreeRegressor <- R6::R6Class(classname = "DecisionTreeRegressor",
                                     inherit = SklearnEstimator,
                                     public = list(
                                       fit = function(X, y) {
                                         merged_data <- cbind(y, X)
                                         df <- as.data.frame(merged_data)
                                         model <- rpart::rpart(y ~ ., method = "anova", data = df)
                                         # print("model: ")
                                         # summary(model)
                                         # rpart.plot::rpart.plot(model)
                                       },
                                       predict = function(X, model) {
                                         data <- data.frame(X)
                                         predict(model, data, method = "anova")
                                       }
                                     )
                                     )

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

                          len_X <- NULL
                          if(is.matrix(X) | is.data.frame(X)){
                            len_X <- nrow(X)
                          }else{
                            print("nah")
                          }
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
                              # cat(i, "th subset", "x neighbors: ", Xneighbors, "\n")
                              # cat(i, "th subset", "y neighbors: ", yneighbors, "\n")

                              # Centroid is used as the center of the local sample set
                              local_center = colMeans(Xneighbors)

                              local_model <- NULL
                              #if random_state is set
                              if(!is.na(self$local_estimator$get_attributes()$random_state)) {
                                # FIXME
                                # self$local_estimator$random_state <-
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)
                              }else{
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[,i] <- self$local_estimator$predict(X, local_model)

                              if(is.null(self$distance_function)) {
                                dists[,i] = rbf(X, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }else {
                                # FIXME add distance function instead of rbf function
                                dists[,i] = rbf(X, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }
                            }

                            if(self$d_normalize) {
                              denom <- rowSums(dists)
                              # print("denom: ")
                              denom[denom < 1e-08] <- 1e-08
                              # print(denom)
                              # print("dists before norm: ")
                              # print(dists)
                              dists <- t(t(dists)/denom)
                              # print("dists after norm: ")
                              # print(dists)
                            }

                            Z <- dists * predicts
                            if(self$scaling){
                              Z <- apply(Z, 2, standardize)
                            }

                            global_model <- NULL
                            # if(Reduce('|', is.na(self$global_estimator)))
                            if(length(self$global_estimator) != 0){ #for a null environment, the length is 0
                              if(!is.na(self$global_estimator$get_attributes()$random_state)){
                                # FIXME add random state
                                global_model <- self$global_estimator$fit(Z, y)
                              }else{
                                global_model <- self$global_estimator$fit(Z, y)
                              }
                            }
                            #ADD scobject to the replication ?
                            self$replications <- append(self$replications, Replication$new(local_estimators = local_models,
                                                                                         global_estimator = global_model))

                          }
                          invisible(self)
                        }
                      )
                    )


LESSRegressor <- R6::R6Class(classname = "LESSRegressor",
                             inherit = LESSBase,
                             public = list(
                               n_replications = 50,
                               random_state = NULL,
                               n_subsets = NULL,
                               n_neighbors = NULL,
                               local_estimator = NULL,
                               d_normalize = NULL,
                               global_estimator = NULL,
                               scaling = NULL,
                               initialize = function(n_replications = 5, random_state = NA, n_subsets = 2, n_neighbors = 5,
                                                     local_estimator = NA, d_normalize = TRUE, global_estimator = NA, scaling = TRUE) {
                                 self$n_replications = n_replications
                                 self$random_state = random_state
                                 self$n_subsets = n_subsets
                                 self$n_neighbors = n_neighbors
                                 self$local_estimator = local_estimator
                                 self$d_normalize = d_normalize
                                 self$global_estimator = global_estimator
                                 self$scaling = scaling
                               },
                               fit = function(X, y){
                                 self$fitnoval(X, y)
                                 self$isFitted = TRUE
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
linReg <- function(x=c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131), y=c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)) {
  abalone <- read.csv(file='datasets/abalone.csv', header = FALSE)
  xvals <- abalone[,-ncol(abalone)]
  yval <- abalone[,ncol(abalone)]
  LESS <- LESSRegressor$new(n_neighbors = 209, n_subsets=19, random_state = 5, local_estimator = LinearRegression$new(), global_estimator = DecisionTreeRegressor$new())
  LESS$fit(xvals, yval)
}
