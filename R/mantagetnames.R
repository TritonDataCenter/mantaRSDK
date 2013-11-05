# Roxygen Comments mantagetnames
#' mantaLs and mantaFind callback
#'
#' @param line  R structured directory line
#'
mantagetnames <- function(line) { 
    if (!is.na(charmatch("name", names(line)))) {
      return(line$name)
    }
    if (!is.na(charmatch("id", names(line)))) {
      return(line$id)
    }
}
