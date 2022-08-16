OneVsRestClassifier <- R6::R6Class(classname = "OneVsRestClassifier",
                                   private = list(
                                     estimator = NULL,
                                     uniqc = NULL,
                                     class_len = NULL,
                                     estimator_list = NULL
                                   ),
                                   public = list(
                                     initialize = function(estimator = NULL){
                                       private$estimator = estimator
                                       private$estimator_list = list()
                                     },
                                     fit = function(X, y){
                                       private$uniqc <- sort(unique(y))
                                       private$class_len <- length(private$uniqc)
                                       class_matrix <- matrix(0, private$class_len, length(y))
                                       for(i in 1:private$class_len){
                                         class_matrix[i,y==private$uniqc[i]] <- 1
                                         private$estimator_list <- append(private$estimator_list, private$estimator$fit(X, class_matrix[i,])$clone())
                                       }
                                       invisible(self)
                                     },
                                     predict = function(X0){
                                       data <- prepareXset(X0)
                                       class_probs <- matrix(0, private$class_len, nrow(data))
                                       for(i in 1:private$class_len){
                                         probs <- private$estimator_list[[i]]$predict_proba(data)
                                         class_probs[i,] <- probs[,2]
                                       }
                                       class_preds <- private$uniqc[max.col(t(class_probs))]
                                       return(class_preds)
                                     }
                                   ))

OneVsOneClassifier <- R6::R6Class(classname = "OneVsOneClassifier",
                                  private = list(
                                    estimator = NULL,
                                    uniqc = NULL,
                                    class_len = NULL,
                                    estimator_list = NULL,
                                    combinations = NULL
                                  ),
                                  public = list(
                                    initialize = function(estimator = NULL){
                                      private$estimator = estimator
                                      private$estimator_list = list()
                                    },
                                    fit = function(X, y){
                                      private$uniqc <- sort(unique(y))
                                      private$class_len <- length(private$uniqc)
                                      private$combinations = combn(private$uniqc, 2)
                                      class_matrix <- matrix(NA, ncol(private$combinations), length(y))
                                      for(i in 1:ncol(private$combinations)){
                                        class_matrix[i,y==private$combinations[1,i]] <- 0 # convert the first class to 0
                                        class_matrix[i,y==private$combinations[2,i]] <- 1 # convert the second class to 1
                                        X_train <- as.matrix(X[-which(is.na(class_matrix[i,])),]) # omit the data points that belongs to other classes
                                        y_train <- as.matrix(na.omit(class_matrix[i,])) # omit the data points that belongs to other classes
                                        private$estimator_list <- append(private$estimator_list,
                                                                         private$estimator$fit(X_train, y_train)$clone())
                                      }
                                      invisible(self)
                                    },
                                    predict = function(X0){
                                      data <- prepareXset(X0)
                                      win_counts <- matrix(0, private$class_len, nrow(data))
                                      class_probs <- matrix(0, private$class_len, nrow(data))
                                      for(i in 1:ncol(private$combinations)){
                                        probs <- private$estimator_list[[i]]$predict_proba(data) # Probability estimates of each data point
                                        classes <- private$combinations[,i] # two classes that is being compared
                                        pred_index <- max.col(probs) # index of the class that has the higher probability for each data point
                                        pred <- classes[pred_index] # predictions for each data point
                                        for(c in 1:nrow(data)){
                                          # for each data point, increase the count the of winner class
                                          win_counts[private$uniqc == pred[c],c] <-  win_counts[private$uniqc == pred[c],c] + 1
                                        }
                                      }
                                      # the class with the highest number of wins is the prediction
                                      class_preds <- private$uniqc[max.col(t(win_counts))]
                                      return(class_preds)
                                    }
                                  ))

OutputCodeClassifier <- R6::R6Class(classname = "OutputCodeClassifier",
                                    private = list(
                                      estimator = NULL,
                                      uniqc = NULL,
                                      class_len = NULL,
                                      estimator_list = NULL,
                                      code_size = NULL,
                                      code_book = NULL,
                                      random_state = NULL
                                    ),
                                    public = list(
                                      initialize = function(estimator = NULL, code_size = 1.5, random_state = NULL){
                                        private$estimator = estimator
                                        private$estimator_list = list()
                                        private$code_size = code_size
                                        private$random_state = random_state
                                      },
                                      fit = function(X, y){
                                        if(private$code_size <= 0){
                                          stop(sprintf("code_size should be greater than 0, got %s", private$code_size))
                                        }

                                        #FIXME check if the estimator has decision_func or predict_p
                                        private$uniqc <- sort(unique(y))
                                        private$class_len <- length(private$uniqc)

                                        if(private$class_len == 0){
                                          stop("OutputCodeClassifier can not be fit when no class is present.")
                                        }

                                        code_size <- as.integer(private$class_len * private$code_size)

                                        set.seed(private$random_state)
                                        # create a code book
                                        private$code_book <- matrix(runif(private$class_len * code_size), nrow = private$class_len)
                                        private$code_book[private$code_book > 0.5] = 1
                                        # this part is implemented directly for those estimators with predict_proba() function
                                        private$code_book[private$code_book != 1] = 0

                                        classes_index <- setNames(1:private$class_len, private$uniqc) # named vector with classnames and their indexes
                                        Y <- matrix(0, length(y), code_size)
                                        for (i in 1:length(y)) {
                                          # for each data point (label), take the corresponding code_book row
                                          Y[i,] <- private$code_book[classes_index[y[i]],]
                                        }

                                        for(i in 1:code_size){
                                          if(length(unique(Y[,i])) == 1){
                                            private$estimator_list <- append(private$estimator_list, ConstantPredictor$new()$fit(X, Y[,i])$clone())
                                          }else{
                                            private$estimator_list <- append(private$estimator_list,
                                                                             private$estimator$fit(X, Y[,i])$clone())
                                          }
                                        }
                                        invisible(self)
                                      },
                                      predict = function(X0){
                                        data <- prepareXset(X0)
                                        class_probs <- matrix(0, length(private$estimator_list), nrow(data))
                                        for(i in 1:length(private$estimator_list)){
                                          # take the positive class probability using each fitted estimator
                                          probs <- private$estimator_list[[i]]$predict_proba(data)
                                          class_probs[i,] <- probs[,2]
                                        }
                                        # calculate the distance matrix
                                        distances <- pracma::distmat(t(class_probs), private$code_book)
                                        # take the index of class which has the minimum distance
                                        class_preds <- apply(distances, 1, which.min)

                                        return(private$uniqc[class_preds])
                                      }
                                    ))

LESSBinaryClassifier <- R6::R6Class(classname = "LESSBinaryClassifier",
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
                                      rng = NULL,
                                      yorg = NULL
                                    ),
                                    public = list(
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
                                        private$yorg = NULL
                                      },
                                      fit = function(X, y){
                                        # Check that X and y have correct shape
                                        X_y_list <- check_X_y(X, y)
                                        X <- X_y_list[[1]]
                                        y <- X_y_list[[2]]

                                        # Original labels
                                        private$yorg <- sort(unique(y))

                                        if(length(private$yorg) != 2){
                                          stop("LESSBinaryClassifier works only with two labels. Please try LESSClassifier.")
                                        }

                                        # Convert to binary labels
                                        ymin1 <- y == private$yorg[1]
                                        ypls1 <- y == private$yorg[2]
                                        y[ymin1] <- -1
                                        y[ypls1] <- 1

                                        private$set_local_attributes()

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
                                      predict_proba = function(X0){
                                        check_is_fitted(self)
                                        # Input validation
                                        check_matrix(X0)

                                        len_X0 <- nrow(X0)
                                        yhat <- matrix(0, len_X0, private$n_replications)
                                        predprobs <- matrix(0, len_X0, 2)
                                        for(i in 1:private$n_replications){
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
                                              dists[, j] <- rbf(as.matrix(X0), local_center, 1.0/(n_subsets ^ 2.0))
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
                                            yhat[,i] <- as.integer(global_model$predict(Z0))
                                            # Convert to 0-1
                                            yhat[,i] <- (yhat[,i] + 1)/2

                                          }else{
                                            rowSum <- rowSums(Z0)
                                            yhat[rowSum < 0, i] = 0
                                            yhat[rowSum >= 0, i] = 1
                                          }
                                        }

                                        mode_matrix <- t(apply(yhat, 1, getMode))
                                        yhat <- mode_matrix[,1]
                                        cnt <- mode_matrix[,2]
                                        yhat0 <- yhat == 0
                                        yhat1 <- yhat == 1
                                        predprobs[yhat0, 1] <- cnt[yhat0]
                                        predprobs[yhat0, 2] <- private$n_replications - cnt[yhat0]
                                        predprobs[yhat1, 2] <- cnt[yhat1]
                                        predprobs[yhat1, 1] <- private$n_replications - cnt[yhat1]

                                        predprobs <- predprobs / private$n_replications

                                        return(predprobs)
                                      }
                                    ))

LESSClassifier <- R6::R6Class(classname = "LESSClassifier",
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
                                rng = NULL,
                                multiclass = NULL,
                                bclassifier = NULL,
                                strategy = NULL,
                                set_strategy = function(n_classes){
                                  if(n_classes == 2){
                                    private$strategy <- OneVsRestClassifier$new(estimator = LESSBinaryClassifier$new())
                                  }else if(private$multiclass == "ovr"){
                                    private$strategy <- OneVsRestClassifier$new(estimator = LESSBinaryClassifier$new())
                                  }else if(private$multiclass == "ovo"){
                                    private$strategy <- OneVsOneClassifier$new(estimator = LESSBinaryClassifier$new())
                                  }else if(private$multiclass == "occ"){
                                    private$strategy <- OutputCodeClassifier$new(estimator = LESSBinaryClassifier$new())
                                  }else{
                                    private$strategy <- OneVsRestClassifier$new(estimator = LESSBinaryClassifier$new())
                                    LESSWarn$new("LESSClassifier works only with one of the following options:
                                                  (1) 'ovr' : OneVsRestClassifier (default),
                                                  (2) 'ovo' : OneVsOneClassifier,
                                                  (3) 'occ' : OutputCodeClassifier,
                                                  Switching to 'ovr' ...", private$warnings)
                                  }
                                }
                              ),
                              public = list(
                                initialize = function(frac = NULL, n_neighbors = NULL, n_subsets = NULL, n_replications = 20, d_normalize = TRUE, val_size = NULL,
                                                      random_state = NULL, tree_method = function(X) KDTree$new(X), cluster_method = NULL,
                                                      local_estimator = LinearRegression$new(), global_estimator = DecisionTreeRegressor$new(), distance_function = NULL,
                                                      scaling = TRUE, warnings = TRUE, multiclass = "ovr") {
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
                                  private$multiclass = multiclass
                                  private$bclassifier = LESSBinaryClassifier$new(frac = private$frac, n_neighbors = private$n_neighbors, n_subsets = private$n_subsets,
                                                                                 n_replications = private$n_replications, d_normalize = private$d_normalize,
                                                                                 val_size = private$val_size, random_state = private$random_state, tree_method = private$tree_method,
                                                                                 cluster_method = private$cluster_method, local_estimator = private$local_estimator,
                                                                                 global_estimator = private$local_estimator, distance_function = private$distance_function,
                                                                                 scaling = private$scaling, warnings = private$warnings)
                                },
                                fit = function(X, y){
                                  if(private$scaling){
                                    private$scobject <- StandardScaler$new()
                                    X <- private$scobject$fit_transform(X)
                                  }

                                  n_classes <- length(unique(y))
                                  private$set_strategy(n_classes)
                                  private$strategy$fit(X, y)
                                  #private$update_params()
                                  private$isFitted <- TRUE
                                  invisible(self)
                                },
                                predict = function(X0){
                                  if(private$scaling){
                                    X0 <- private$scobject$transform(X0)
                                  }
                                  private$strategy$predict(X0)
                                }
                              )
)
