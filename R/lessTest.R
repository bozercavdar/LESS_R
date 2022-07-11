SklearnEstimator <- R6::R6Class(classname = "SklearnEstimator",
                                public = list(
                                  printsk = function() {
                                    cat("sklearn: ", self$isFitted, "\n")
                                  },
                                  fit = function() {
                                    #maybe check for
                                    print("dummy fit function")
                                  },
                                  predict = function(){
                                    print("dummy predict function")
                                  },
                                  public_fields = function(){
                                    return(names(get(class(self)[1])$public_fields))
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
                                    model <- lm(y ~ as.vector(X))
                                    model
                                  },
                                  predict = function(X, model) {
                                    data <- data.frame(X)
                                    predict(model, data)
                                  }
                                )
                                )

####################

#' RBF kernel - L2 norm
#' This is is used as the default distance function in LESS
rbf <- function(data, center, coeff=0.01){
  distFunction <- function(point1, point2) {
    exp(-coeff * norm(point1 - point2, type = "2"))
  }
  mapply(distFunction, data, center)
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
                        n_replications = 50,
                        random_state = NULL,
                        n_subsets = NULL,
                        n_neighbors = NULL,
                        local_estimator = NULL,
                        # fix replication amount
                        initialize = function(replications = NA, scobject = NA, isFitted = FALSE, n_replications = 5,
                                              random_state = NA, n_subsets = 2, n_neighbors = 5, local_estimator = NA) {
                          self$replications = replications
                          self$scobject = scobject
                          self$isFitted = isFitted
                          self$n_replications = n_replications
                          self$random_state = random_state
                          self$n_subsets = n_subsets
                          self$n_neighbors = n_neighbors
                          self$local_estimator = local_estimator

                        },
                        set_local_attributes = function() {
                          print("to be done")
                        },

                        fitnoval = function(X, y) {
                          #' Fit function: All data is used with the global estimator (no validation)
                          #' Tree method is used (no clustering)

                          len_X <- length(X)
                          #FIXME check_input
                          self$replications <- c()
                          for (i in 1:self$n_replications) {
                            # set.seed(self$random_state) # set seed each time so
                            sample_indices <- sample(len_X, size = self$n_subsets)
                            cat("sample indices: ", sample_indices, "\n")
                            nearest_neighbors <- RANN::nn2(X, X[sample_indices], k = self$n_neighbors)
                            neighbor_indices_list <- nearest_neighbors[[1]]
                            print(neighbor_indices_list)

                            local_models <- list() # List[LocalModel]
                            dists <- matrix(0, len_X, self$n_subsets)
                            predicts <- matrix(0, len_X, self$n_subsets)

                            for (i in 1:nrow(neighbor_indices_list)) {
                              Xneighbors <- as.matrix(X[neighbor_indices_list[i, ]])
                              yneighbors <- as.matrix(y[neighbor_indices_list[i, ]])
                              # cat(i, "th subset", "x neighbors: ", Xneighbors, "\n")
                              # cat(i, "th subset", "y neighbors: ", yneighbors, "\n")

                              # Centroid is used as the center of the local sample set
                              local_center = colMeans(Xneighbors)

                              local_model <- NULL
                              #if random_state is set
                              if(!is.na(self$get_attributes()$random_state)) {
                                # FIXME
                                # self$local_estimator$random_state <-
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)
                              }else{
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)
                              }
                              local_models <- list(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[,i] <- self$local_estimator$predict(X, local_model)

                              if(is.null(self$distance_function)) {
                                dists[,i] = rbf(X, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }else {
                                # FIXME add distance function
                                dists[,i] = rbf(X, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }

                            }
                          }

                        }
                      )
                    )


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
  LESS <- LESSBase$new(random_state = 5, local_estimator = LinearRegression$new())
  LESS$fitnoval(x, y)
}
