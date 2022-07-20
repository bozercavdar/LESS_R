####################
# HELPER CLASSES
####################
BaseEstimator <- R6::R6Class(classname = "BaseEstimator",
                             public = list(
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
                               },
                               set_random_state = function(random_state){
                                 self$random_state = random_state
                                 invisible(self)
                               }
                             ))

SklearnEstimator <- R6::R6Class(classname = "SklearnEstimator",
                                inherit = BaseEstimator,
                                public = list(
                                  fit = function() {
                                    #maybe check for
                                    print("dummy fit function")
                                    invisible(self)
                                  },
                                  predict = function(){
                                    print("dummy predict function")
                                    invisible(self)
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
                                  estimator_type = "regressor",
                                  model = NULL,
                                  fit = function(X, y) {
                                    df <- prepareDataset(X, y)
                                    self$model <- lm(y ~ ., data = df)
                                    # if(length(self$model$coefficients) > self$model$rank){
                                    #   print("rank deficient fit. number of parameters are more than the observations")
                                    #   print(self$model$rank)
                                    # }
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

# Decision Tree wrapper class with using rpart package
# DecisionTreeRegressor <- R6::R6Class(classname = "DecisionTreeRegressor",
#                                      inherit = SklearnEstimator,
#                                      public = list(
#                                        estimator_type = "regressor",
#                                        model = NULL,
#                                        fit = function(X, y) {
#                                          df <- prepareDataset(X, y)
#                                          self$model <- rpart::rpart(y ~ ., method = "anova", data = df, control = rpart::rpart.control(minsplit = 2, minbucket = 1))
#                                          rpart.plot::rpart.plot(self$model)
#                                          invisible(self)
#                                          # print("model: ")
#                                          # summary(model)
#
#                                        },
#                                        predict = function(X) {
#                                          data <- prepareXset(X)
#                                          predict(self$model, data, method = "anova")
#                                        }
#                                      )
#                                      )

# Decision Tree wrapper class with using party package
DecisionTreeRegressor <- R6::R6Class(classname = "DecisionTreeRegressor",
                                     inherit = SklearnEstimator,
                                     public = list(
                                       estimator_type = "regressor",
                                       model = NULL,
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         self$model <- party::ctree(
                                           y ~ .,
                                           data = df,
                                           controls = party::ctree_control(minsplit = 2, minbucket = 1))
                                         # plot(self$model)
                                         invisible(self)
                                       },
                                       predict = function(X) {
                                         data <- prepareXset(X)
                                         predict(self$model, data)
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

RandomGenerator <- R6::R6Class(classname = "RandomGenerator",
                               public = list(
                                 random_state = NULL,
                                 index = NULL,
                                 initialize = function(random_state){
                                   self$random_state = random_state
                                   self$index = 1
                                 },
                                 choice = function(range, size){
                                   # range: sampling takes place from 1:range
                                   # size: a non-negative integer giving the number of items to choose
                                   set.seed(self$random_state)
                                   permutation <- sample(range)

                                   # this part helps if the index go beyond range.
                                   if((self$index + size - 1) > range){
                                     set.seed(self$random_state)
                                     permutation <- c(permutation, sample(range, size=(self$index + size - 1 - range), replace = TRUE))
                                   }

                                   result <- permutation[self$index:(self$index+size-1)]
                                   self$index <- self$index + size
                                   return(result)
                                 },
                                 integers = function(range, size = 1) {
                                   set.seed(self$random_state)
                                   permutation <- sample.int(range)

                                   # this part helps if the index go beyond range.
                                   if((self$index + size - 1) > range){
                                     set.seed(self$random_state)
                                     permutation <- c(permutation, sample.int(range, size=(self$index + size - 1 - range), replace = TRUE))
                                   }

                                   result <- permutation[self$index:(self$index+size-1)]
                                   self$index <- self$index + size
                                   return(result)
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

KDTree <- R6::R6Class(classname = "KDTree",
                      public = list(
                        X = NULL,
                        initialize = function(X = NA) {
                          self$X = X
                        },
                        query = function(query_X, k=1){
                          # query the tree for the k nearest neighbors
                          RANN::nn2(data = self$X, query = query_X, k = k)
                        }
                      ))

KMeans <- R6::R6Class(classname = "KMeans",
                      inherit = BaseEstimator,
                      public = list(
                        model = NULL,
                        n_clusters = NULL,
                        n_init = NULL,
                        max_iter = NULL,
                        cluster_centers = NULL,
                        labels = NULL,
                        random_state = NULL,
                        initialize = function(n_clusters = 8, n_init = 10, max_iter = 300, random_state = NULL){
                          self$n_clusters = n_clusters
                          self$n_init = n_init
                          self$max_iter = max_iter
                          self$random_state = random_state
                        },
                        fit = function(X){
                          set.seed(self$random_state)
                          self$model <- kmeans(X, centers = self$n_clusters, iter.max = self$max_iter, nstart = self$n_init)
                          self$cluster_centers <- self$model$centers
                          self$labels <- self$model$cluster
                          invisible(self)
                        }
                      ))

####################
# HELPER FUNCTIONS
####################

#' RBF kernel - L2 norm
#' This is is used as the default distance function in LESS
rbf <- function(data, center, coeff=0.01){
  dataDiff <- sweep(data, 2, center) #extract center from all rows of data
  normRows = wordspace::rowNorms(dataDiff, method = "euclidean", p=2) #take l2 norms of each row
  exp(-coeff * normRows)
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

# checks if the input estimator's type is regressor
is_regressor = function(estimator) {
  estimator$estimator_type == "regressor"
}

# checks if the input estimator's type is classifier
is_classifier = function(estimator) {
  estimator$estimator_type == "classifier"
}

# returns the class name of the input object
getClassName = function(obj) {
  class(obj)[1]
}

train_test_split = function(data, test_size=0.3, random_state=NULL){
  print(random_state)
  set.seed(random_state)
  sample <- sample.int(n = nrow(data), size = floor(.7*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]

  X_train <- train[,-ncol(train)]
  y_train <- train[,ncol(train)]
  X_test <- test[,-ncol(test)]
  y_test <- test[,ncol(test)]
  return(list(X_train, X_test, y_train, y_test))
}
###################

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
                          if(is.null(self$local_estimator)){
                            stop("LESS does not work without a local estimator.")
                          }

                          if(is_classifier(self$local_estimator)){
                            LESSWarn$new("LESS might work with local classifiers.\n--However, we recommend using regressors as the local estimators.",
                                         self$warnings)
                          }

                          if(getClassName(self) == "LESSRegressor" & is_classifier(self$global_estimator)){
                            LESSWarn$new("LESSRegressor might work with a global classifier.\n--However, we recommend using a regressor as the global estimator.",
                                         self$warnings)
                          }

                          if(getClassName(self) == "LESSClassifier" & is_regressor(self$global_estimator)){
                            LESSWarn$new("LESSClassifier might work with a global regressor.\n--However, we recommend using a classifier as the global estimator.",
                                         self$warnings)
                          }

                          if(!is.na(self$val_size)) {
                            if(self$val_size <= 0.0 | self$val_size >= 1.0){
                              stop("Parameter val_size should be in the interval (0, 1).")
                            }
                          }

                          if(!is.na(self$frac)) {
                            if(self$frac <= 0.0 | self$frac >= 1.0){
                              stop("Parameter frac should be in the interval (0, 1).")
                            }
                          }

                          if(self$n_replications < 1){
                            stop("The number of replications should greater than equal to one.")
                          }

                          if(length(self$cluster_method) != 0){ #length of NULL is zero. if it is not a null environment(class), length is not zero
                            # FIXME
                            # to be implemented


                          }else if(is.na(self$frac) &
                                   is.na(self$n_neighbors) &
                                   is.na(self$n_subsets)){
                            self$frac = 0.05
                          }
                        },

                        check_input = function(len_X) {
                          if(length(self$cluster_method) == 0){
                            if(!is.na(self$frac)){
                              self$n_neighbors <- as.integer(ceiling(self$frac * len_X))
                              self$n_subsets <- as.integer(len_X/self$n_neighbors)
                            }

                            if(is.na(self$n_subsets)){
                              self$n_subsets <- as.integer(len_X/self$n_neighbors)
                            }

                            if(is.na(self$n_neighbors)){
                              self$neighbors <- as.integer(len_X/self$n_subsets)
                            }

                            if(self$n_neighbors > len_X){
                              LESSWarn$new("The number of neighbors is larger than the number of samples. \n--Setting number of subsets to one.",
                                           self$warnings)
                              self$n_neighbors <- len_X
                              self$n_subsets <- 1
                            }

                            if(self$n_subsets > len_X){
                              LESSWarn$new("The number of subsets is larger than the number of samples. \n--Setting number of neighbors to one.",
                                           self$warnings)
                              self$n_neighbors <- 1
                              self$n_subsets <- len_X
                            }

                            if(self$n_subsets == 1){
                              LESSWarn$new("There is only one subset, so the global estimator is set to NULL",
                                           self$warnings)
                              self$global_estimator <- NULL
                              self$d_normalize <- TRUE
                              # If there is also no validation step, then there is
                              # no randomness. So, no need for replications.
                              if(is.na(self$val_size)){
                                LESSWarn$new("Since validation set is not used, there is no randomness. \n--Thus, the number of replications is set to one.",
                                             self$warnings)
                                self$n_replications = 1
                              }
                            }
                          }
                          else{
                            self$n_subsets <- list()
                          }
                        },

                        fitnoval = function(X, y) {
                          #' Fit function: All data is used with the global estimator (no validation)
                          #' Tree method is used (no clustering)
                          len_X <- length(y)
                          self$check_input(len_X)
                          tree <- self$tree_method(X)
                          self$replications <- list()
                          for (i in 1:self$n_replications) {
                            sample_indices <- self$rng$choice(range = len_X, size = self$n_subsets)
                            nearest_neighbors <- tree$query(X[sample_indices,], self$n_neighbors)
                            neighbor_indices_list <- nearest_neighbors[[1]]

                            local_models <- list()
                            dists <- matrix(0, len_X, self$n_subsets)
                            predicts <- matrix(0, len_X, self$n_subsets)

                            for (i in 1:nrow(neighbor_indices_list)) {
                              Xneighbors <- as.matrix(X[neighbor_indices_list[i, ],])
                              yneighbors <- as.matrix(y[neighbor_indices_list[i, ]])

                              # Centroid is used as the center of the local sample set
                              local_center = colMeans(Xneighbors)

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (self$local_estimator$public_fields())) {
                                # set random state to an integer from rng
                                self$local_estimator$set_random_state(self$rng$integers(32767))
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

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

                            # if(Reduce('|', is.na(self$global_estimator)))
                            if(length(self$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (self$global_estimator$public_fields())){
                                self$global_estimator$set_random_state(self$rng$integers(32767))
                                global_model <- self$global_estimator$fit(Z, y)$clone()
                              }else{
                                global_model <- self$global_estimator$fit(Z, y)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            self$replications <- append(self$replications, Replication$new(local_estimators = local_models,
                                                                                           sc_object = scobject,
                                                                                           global_estimator = global_model))
                          }

                          invisible(self)
                        },

                        fitval = function(X, y) {
                          # Fit function: (val_size x data) is used for the global estimator (validation)
                          # Tree method is used (no clustering)

                          self$replications <- list()
                          for (i in 1:self$n_replications) {
                            #Split for global estimation
                            split_list <- train_test_split(cbind(X, y), test_size =  self$val_size,
                                                           random_state = self$rng$integers(32767))
                            X_train <- split_list[[1]]
                            X_val <- split_list[[2]]
                            y_train <- split_list[[3]]
                            y_val <- split_list[[4]]

                            len_X_val <- length(y_val)
                            len_X_train <- length(y_train)
                            # Check the validity of the input
                            if(i == 1){
                              self$check_input(len_X_train)
                            }

                            # A nearest neighbor tree is grown for querying
                            tree <- self$tree_method(X_train)

                            # Select n_subsets many samples to construct the local sample sets
                            sample_indices <- self$rng$choice(range = len_X_train, size = self$n_subsets)
                            # Construct the local sample sets
                            nearest_neighbors <- tree$query(X[sample_indices,], self$n_neighbors)
                            neighbor_indices_list <- nearest_neighbors[[1]]

                            local_models <- list()
                            dists <- matrix(0, len_X_val, self$n_subsets)
                            predicts <- matrix(0, len_X_val, self$n_subsets)

                            for (i in 1:nrow(neighbor_indices_list)) {
                              Xneighbors <- as.matrix(X_train[neighbor_indices_list[i, ],])
                              yneighbors <- as.matrix(y_train[neighbor_indices_list[i, ]])

                              # Centroid is used as the center of the local sample set
                              local_center = colMeans(Xneighbors)

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (self$local_estimator$public_fields())) {
                                # set random state to an integer from rng
                                self$local_estimator$set_random_state(self$rng$integers(32767))
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[,i] <- local_model$predict(X_val)
                              if(is.na(self$distance_function)) {
                                dists[,i] <- rbf(X_val, local_center, 1.0/(self$n_subsets ^ 2.0))
                              }else {
                                # FIXME add distance function instead of rbf function
                                dists[,i] <- rbf(X_val, local_center, 1.0/(self$n_subsets ^ 2.0))
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

                            # if(Reduce('|', is.na(self$global_estimator)))
                            if(length(self$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (self$global_estimator$public_fields())){
                                self$global_estimator$set_random_state(self$rng$integers(32767))
                                global_model <- self$global_estimator$fit(Z, y_val)$clone()
                              }else{
                                global_model <- self$global_estimator$fit(Z, y_val)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            self$replications <- append(self$replications, Replication$new(local_estimators = local_models,
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
                          self$check_input(len_X)

                          #FIXME make it same as the original code
                          if(!is.null(self$cluster_method$get_attributes()$random_state)){
                            LESSWarn$new("Clustering method is not random,
                            so there is no need for replications unless validaton set is used.
                            The number of replications is set to one.", self$warnings)
                            self$n_replications <- 1
                          }

                          if(self$n_replications == 1){
                            cluster_fit <- self$cluster_method$fit(X)
                          }

                          self$replications <- list()
                          for (i in 1:self$n_replications) {
                            if(self$n_replications > 1){
                              cluster_fit <- self$cluster_method$set_random_state(self$rng$integers(32767))$fit(X)
                              # print(length(cluster_fit$labels))
                            }
                            unique_labels <- unique(cluster_fit$labels)
                            # Some clustering methods may find less number of clusters than requested 'n_clusters'
                            self$n_subsets <- append(self$n_subsets, length(unique_labels))
                            n_subsets <- self$n_subsets[[i]]

                            local_models <- list()
                            dists <- matrix(0, len_X, n_subsets)
                            predicts <- matrix(0, len_X, n_subsets)

                            if(!is.null(cluster_fit$cluster_centers)){
                              use_cluster_centers = TRUE
                            }else{
                              use_cluster_centers = FALSE
                            }

                            for (cluster_indx in 1:length(unique_labels)) {
                              neighbor_indices <- cluster_fit$labels == unique_labels[[cluster_indx]]
                              Xneighbors <- as.matrix(X[neighbor_indices, ])
                              yneighbors <- as.matrix(y[neighbor_indices])
                              if(nrow(yneighbors) == 1){
                                # if there is only one sample in a group,
                                # prevent Xneighbors being a (n,1) dimensional matrix
                                Xneighbors <- t(Xneighbors)
                              }

                              # Centroid is used as the center of the local sample set
                              if(use_cluster_centers){
                                local_center <- cluster_fit$cluster_centers[cluster_indx, ]
                              }else{
                                local_center <- colMeans(Xneighbors)
                              }

                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (self$local_estimator$public_fields())) {
                                # set random state to an integer from rng
                                self$local_estimator$set_random_state(self$rng$integers(32767))
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }else{
                                local_model <- self$local_estimator$fit(Xneighbors, yneighbors)$clone()
                              }
                              local_models <- append(local_models, LocalModel$new(estimator = local_model, center = local_center))

                              predicts[, cluster_indx] <- local_model$predict(X)
                              if(is.na(self$distance_function)) {
                                dists[, cluster_indx] <- rbf(X, local_center, 1.0/(n_subsets ^ 2.0))
                              }else {
                                # FIXME add distance function instead of rbf function
                                dists[, cluster_indx] <- rbf(X, local_center, 1.0/(n_subsets ^ 2.0))
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

                            # if(Reduce('|', is.na(self$global_estimator)))
                            if(length(self$global_estimator) != 0){ #for a null environment, the length is 0
                              #if random_state is one of the estimator's parameter
                              if('random_state' %in% (self$global_estimator$public_fields())){
                                self$global_estimator$set_random_state(self$rng$integers(32767))
                                global_model <- self$global_estimator$fit(Z, y)$clone()
                              }else{
                                global_model <- self$global_estimator$fit(Z, y)$clone()
                              }
                            }
                            else{
                              global_model <- NULL
                            }
                            self$replications <- append(self$replications, Replication$new(local_estimators = local_models,
                                                                                           sc_object = scobject,
                                                                                           global_estimator = global_model))
                          }

                          invisible(self)
                        },

                        print = function() {
                          n_subsets <- unlist(self$n_subsets)
                          cat("Number of subsets: ", n_subsets, "\n")
                          cat("Number of samples in each subset: ", self$n_neighbors, "\n")
                        }
                      )
                    )


LESSRegressor <- R6::R6Class(classname = "LESSRegressor",
                             inherit = LESSBase,
                             public = list(
                               frac = NULL,
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
                               rng = NULL,
                               warnings = NULL,
                               val_size = NULL,
                               tree_method = NULL,
                               initialize = function(frac = 0.05, n_replications = 20, random_state = NULL, n_subsets = NA, n_neighbors = NA,
                                                     local_estimator = LinearRegression$new(), d_normalize = TRUE, global_estimator = DecisionTreeRegressor$new(), scaling = TRUE,
                                                     cluster_method = NULL, distance_function = NA, warnings = TRUE, val_size = NA, tree_method = function(X) KDTree$new(X)) {
                                 self$frac = frac
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
                                 self$rng = RandomGenerator$new(random_state = self$random_state)
                                 self$warnings = warnings
                                 self$val_size = val_size
                                 self$tree_method = tree_method
                               },
                               fit = function(X, y){
                                 # FIXME check_X_y()
                                 self$set_local_attributes()

                                 if(self$scaling){
                                   self$scobject <- StandardScaler$new()
                                   X <- self$scobject$fit_transform(X)
                                 }

                                 if(!is.na(self$val_size)){
                                   # Validation set is not used for global estimation
                                   if(length(self$cluster_method) == 0){
                                     self$fitval(X, y)
                                   }
                                   else{
                                     self$fitvalc(X, y)
                                   }
                                 }
                                 else{
                                   # Validation set  not used for global estimation
                                   if(length(self$cluster_method) == 0){
                                     self$fitnoval(X, y)
                                   }
                                   else{
                                     self$fitnovalc(X, y)
                                   }
                                 }
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
                                   if(length(self$cluster_method) == 0){
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


#' Apply LESS regression
#'
#' @export
#'
#' @examples lessReg()
lessReg <- function() {
  # UNCOMMENT THIS CODE BLOCK TO PROFILE THE CODE AND SEE A PERFORMANCE ANALYSIS OF THE CODE
  # profvis::profvis({
  #   abalone <- read.csv(file='datasets/abalone.csv', header = FALSE)
  #
  #   # Now Selecting 70% of data as sample from total 'n' rows of the data
  #   sample <- sample.int(n = nrow(abalone), size = floor(.7*nrow(abalone)), replace = F)
  #   train <- abalone[sample, ]
  #   test  <- abalone[-sample, ]
  #
  #   X_train <- train[,-ncol(train)]
  #   y_train <- train[,ncol(train)]
  #   X_test <- test[,-ncol(test)]
  #   y_test <- test[,ncol(test)]
  #
  #   # xvals <- abalone[,-ncol(abalone)]
  #   # yval <- abalone[,ncol(abalone)]
  #   LESS <- LESSRegressor$new()
  #   preds <- LESS$fit(X_train, y_train)$predict(X_test)
  #
  #   # print(head(matrix(c(y_test, preds), ncol = 2)))
  #   mape <- MLmetrics::MAPE(preds, y_test)
  #   print(mape)
  # })

  data <- read.csv(file='datasets/abalone.csv', header = FALSE)

  # Now Selecting 70% of data as sample from total 'n' rows of the data
  set.seed(1)
  sample <- sample.int(n = nrow(data), size = floor(.7*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]

  X_train <- train[,-ncol(train)]
  y_train <- train[,ncol(train)]
  X_test <- test[,-ncol(test)]
  y_test <- test[,ncol(test)]

  X <- data[, -ncol(data)]
  y <- data[, ncol(data)]

  # X_train <- train[,-1]
  # y_train <- train[,1]
  # X_test <- test[,-1]
  # y_test <- test[,1]

  cat("Total number of training samples: ", nrow(X_train), "\n")
  LESS <- LESSRegressor$new(val_size = 0.3, random_state = 100)
  preds <- LESS$fit(X_train, y_train)$predict(X_test)
  print(LESS)
  print(head(matrix(c(y_test, preds), ncol = 2)))
  mape <- MLmetrics::MAPE(preds, y_test)
  print(mape)


  #UNCOMMENT THIS CODE BLOCK TO SEE ERROR COMPARISON BETWEEN DIFFERENT ESTIMATORS
  # models <- list(LESSRegressor$new(),
  #                LinearRegression$new(),
  #                DecisionTreeRegressor$new())
  # for(model in models){
  #   preds <- model$fit(X_train, y_train)$predict(X_test)
  #   mape <- MLmetrics::MSE(preds, y_test)
  #   cat(getClassName(model), " MSE: ", mape, "\n")
  # }

}
