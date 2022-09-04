#' @title Superconduct Dataset
#'
#' @description 21623 x 82. Reference: \url{https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data}
#'
#' @importFrom utils read.csv
#'
#' @export
data.superconduct = function(){
  data <- read.csv(sprintf("%s/datasets/superconduct.csv", getwd()), sep = ",", header = FALSE)
  return(data)
}

#' @title Insurance Dataset
#'
#' @description 5822 x 39. Reference: \url{https://archive.ics.uci.edu/ml/datasets/Insurance+Company+Benchmark+%28COIL+2000%29}
#'
#' @importFrom utils read.csv
#'
#' @export
data.insurance = function(){
  data <- read.csv(sprintf("%s/datasets/insurance.csv", getwd()), sep = ",", header = FALSE)
  return(data)
}

#' @title Algerian Forest Fire Dataset
#'
#' @description 243 x 11. Reference: \url{https://archive.ics.uci.edu/ml/datasets/Algerian+Forest+Fires+Dataset++}
#'
#' @importFrom utils read.csv
#'
#' @export
data.algerianForestFire = function(){
  data <- read.csv(sprintf("%s/datasets/algeria-fire-cls.csv", getwd()), sep = ",", header = TRUE)
  return(data)
}
