#' Display the population
#' @export
#' @param population A data.frame with the population as the output of \code{\link{generatePopulation}}
#' @param rows the number of rows
#' @param cols the number of columns
#' @importFrom plyr ddply
#' @importFrom ggplot2 ggplot aes_string geom_polygon coord_fixed scale_size_discrete theme element_blank
plotPopulation <- function(population, rows, cols){
  ##################
  # Checking input #
  ##################
  if(!missing(cols)){
    if(!class(cols) %in% c("numeric", "integer")){
      stop("cols must be a number")
    }
    if(length(cols) != 1){
      stop("cols must be a single number")
    }
    if(cols %% 1 > 0){
      stop("cols must be integer")
    }
    if(cols < 1){
      stop("cols must be strict positive")
    }
  }
  
  if(!missing(rows)){
    if(!class(rows) %in% c("numeric", "integer")){
      stop("rows must be a number")
    }
    if(length(rows) != 1){
      stop("rows must be a single number")
    }
    if(rows %% 1 > 0){
      stop("rows must be integer")
    }
    if(rows < 1){
      stop("rows must be strict positive")
    }
  }
  
  if(class(population) != "data.frame"){
    stop("population must be a data.frame")
  }
  if(nrow(population) == 0){
    stop("empty population")
  }
  if(!all(c("Width", "Height", "Categorical", "ID", "Selected") %in% colnames(population))){
    stop("population must contain at least the columns Width, Height, Categorical, Selected and ID")
  }
  if(!class(population$Width) %in% c("numeric", "integer")){
    stop("population$Width must be numeric")
  }
  if(!class(population$Height) %in% c("numeric", "integer")){
    stop("population$Height must be numeric")
  }
  if(length(unique(population$ID)) != nrow(population)){
    stop("population$ID must contain unique values")
  }
  if(class(population$Categorical) != "factor"){
    population$Categorical <- factor(population$Categorical)
    warning("population$Categorical converted to a factor")
  }
  if(class(population$Selected) != "logical"){
    stop("population$Selected must be logical")
  }
  if(any(is.na(population[, c("Width", "Height", "Categorical", "ID", "Selected")]))){
    stop("the columns Width, Height, Categorical, Selected and ID from population should not contain missing values")
  }
  ######################
  # End checking input #
  ######################
 if(missing(rows)){
    if(missing(cols)){
      rows <- cols <- ceiling(sqrt(nrow(population)))
    } else {
      rows <- ceiling(nrow(population) / cols)
    }
  } else {
    if(missing(cols)){
      cols <- ceiling(nrow(population) / rows)
    }
  }
 if(rows * cols < nrow(population)){
   stop("number of rows and cols not adequat to cover the entire population")
 }
  population$Row <- head(rep(seq_len(rows) - 1, each = cols), nrow(population))
  population$Column <- head(rep(seq_len(cols), rows), nrow(population))
  unit <- 3 * c(max(population$Width), max(population$Height))
  population$Xc <- unit[1] * population$Column
  population$Yc <- unit[2] * population$Row
  polygon <- ddply(population, c("ID", "Categorical", "Selected"), function(x){
    with(x,
      cbind(
        Width = Xc + c(1, 1, -1, -1, 1) * Width,
        Height = Yc + c(1, -1, -1, 1, 1) * Height
      )
    )
  })
  ggplot(polygon, aes_string(x = "Width", y = "Height", group = "ID", fill = "Categorical", size = "Selected")) + 
    geom_polygon(show_guide = FALSE, colour = "black") + 
    coord_fixed() + 
    scale_size_discrete(range = c(.1, 1.5)) + 
    theme(
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      panel.background = element_blank()
    )
}
