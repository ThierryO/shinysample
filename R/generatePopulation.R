#' Create a random population
#' @param n the size of the population
#' @param dimension A matrix with the proportion for the size of the rectangles
#' @param continuous.mean A vector of population means
#' @param continuous.var A variance covariance matrix of the population means
#' @param categorical.proportion A vector with proportions
#' @export
#' @importFrom mvtnorm rmvnorm
generatePopulation <- function(
  n = 99, 
  dimension = matrix(rpois(25, lambda = 10), ncol = 5),
  continuous.mean = c(5, 10, 7), 
  continuous.var = 2 * diag(length(continuous.mean)),
  categorical.proportion = c(1/6, 1/3, 1/2)
){
  ##################
  # Checking input #
  ##################
  if(!class(n) %in% c("numeric", "integer")){
    stop("n must be a number")
  }
  if(length(n) != 1){
    stop("n must be a single value")
  }
  if(n %% 1 > 0){
    stop("n must be integer")
  }
  if(n < 1){
    stop("n must be strict positive")
  }
  if(class(dimension) != "matrix"){
    stop("dimension must be a matrix")
  }
  if(min(dim(dimension)) == 0){
    stop("dimension must have at least one row and one column")
  }
  if(!class(dimension[1, 1]) %in% c("numeric", "integer")){
    stop("dimension must be numeric")
  }
  if(min(dimension) < 0){
    stop("all proportions in dimension must be positive")
  }
  if(max(dimension) <= 0){
    stop("at least one proportion in dimension must be strict positive")
  }
  if(!class(continuous.mean) %in% c("numeric", "integer")){
    stop("continuous.mean must be a numeric vector")
  }
  if(length(continuous.mean) < 2){
    stop("continuous.mean must have length >= 2")
  }
  
  if(class(continuous.var) != "matrix"){
    stop("continuous.var must be a matrix")
  }
  if(!class(continuous.var[1, 1]) %in% c("numeric", "integer")){
    stop("continuous.var must be a numeric matrix")
  }
  if(any(dim(continuous.var) != length(continuous.mean))){
    stop("continuous.var must have the same number of columns and rows as the number of elements in continuous.mean")
  }
  
  if(!class(categorical.proportion) %in% c("numeric", "integer")){
    stop("categorical.proportion must be a numeric vector")
  }
  if(length(categorical.proportion) < 1){
    stop("categorical.proportion must have a least one proportion")
  }
  if(any(categorical.proportion <= 0)){
    stop("All elements of categorical.proportion must be strict positive")
  }
  ######################
  # End checking input #
  ######################
  
  dimension.df <- data.frame(
    Height = rep(seq_len(nrow(dimension)), ncol(dimension)),
    Width = rep(seq_len(ncol(dimension)), each = nrow(dimension)),
    Propotion = as.vector(dimension)
  )
  selection <- sample(nrow(dimension.df), size = n, prob = dimension.df$Proportion, replace = TRUE)
  population <- dimension.df[selection, c("Width", "Height")]
  continuous <- rmvnorm(n, mean = continuous.mean, sigma = continuous.var)
  colnames(continuous) <- paste0("X", seq_len(ncol(continuous)))
  population <- cbind(population, continuous)
  categorical <- sample(
    LETTERS[seq_along(categorical.proportion)], 
    size = n, 
    prob = categorical.proportion, 
    replace = TRUE
  )
  population$Categorical <- factor(
    categorical, 
    levels = LETTERS[seq_along(categorical.proportion)]
  )
  population$ID <- seq_len(nrow(population))
  population$Selected <- FALSE
  return(population)
}
