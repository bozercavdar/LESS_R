#' @title Superconduct Dataset
#'
#' @description The dataset contains data on 21263 superconductors and their relevant features.
#'
#' @importFrom utils read.csv
#'
#' @export
data.superconduct = function(){
  id <- "1rU3LlYpxSORlcKoPIniaxEdTyhfGWurN"
  data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), sep = ",", header = FALSE)
  return(data)
}
