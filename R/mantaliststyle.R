# Roxygen Comments mantaliststyle
#' mantaLs and mantaFind callback
#' 
#' @param line  R structured directory line
#'
mantaliststyle <- function(line) {
    if (!is.na(charmatch("name", names(line)))) {
      return(paste(line$name, sep="") )
    }
    if (!is.na(charmatch("id", names(line)))) { 
      return(paste(line$id,  sep="") )
    }
}
