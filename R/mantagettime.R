# Roxygen Comments mantagettime
#' mantaLs and mantaFind callback
#' 
#' @param line  R structured directory line
#'
mantagettime<- function(line) {
    return(line$mtime) 
}

