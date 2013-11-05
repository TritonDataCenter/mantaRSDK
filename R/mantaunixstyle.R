# Roxygen Comments mantaunixstyle.R
#' mantaLs and mantaFind callback - Unix like listings
#' mimic of ln -o but some of this is static
#' 
#' @param line  R structured directory line
#' 
mantaunixstyle <- function(line) { 
    time_now <- as.POSIXlt(Sys.time(), "UTC")
    # more than 6 months, show year instad of time
    if ( as.numeric(as.difftime(182.5, units = 'days')) > as.numeric(difftime(time_now, line$mtime, units = 'days')) ) {
      time <- format(line$mtime, "%d %b %H:%m")
    } else {
      time <- format(line$mtime, "%d %b  %Y")
    }
    if (line$type == "directory")  {
      dn <- "d"
    } else {
      dn <- "-"
    }
    permissions <- paste(dn,"rwxr-xr-x",sep="")
    size <- sprintf("%14s",line$size)
    links <- "1"
    owner <- manta_globals$manta_user
    if (!is.na(charmatch("name", names(line)))) {
      name <- line$name
    }
    if (!is.na(charmatch("id", names(line)))) {
      name <- line$id
    }

    return( paste(permissions, links, owner, size, time, name, sep=" ") )
}
