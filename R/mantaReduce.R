# Roxygen Comments mantaReduce
#' Constructor for R format Manta Job for Reduce Unix task.
#'
#' Helper function to construct R structure describing a Reduce task. To be used
#' to satisfy the \code{...} argument of \code{mantaJob.setup} and specify 
#' the Unix command line task, any initialization tasks, 
#' an array of Manta filesystem \code{asset} files,
#' and the memory/disk size to be used for the compute instance on Manta.
#'
#' On Manta, a Reduce task phase executes a generic UNIX command
#' when specified in \code{mantaJob.launch}, or from the output
#' pipelined from a previous step. Use \code{mantaReduce} to run a job that has no Manta
#' object input data.
#'
#' The \code{exec} argument must be a valid generic UNIX command line, not an R function.
#' The \code{exec} argument may call executables or runtime language scripts that are
#' hosted on Manta and specified as \code{assets}.  The \code{init} parameter
#' command is called before the
#' \code{exec} argument and is not passed input. The init argument may be used, for example
#' to extract scripts from an \code{asset} on Manta saved as a \code{tar} object.
#'
#' Note that you do not have to specify the input for a Reduce task for \code{mantaJob.launch}, 
#' the service pipes the output of the previous Map task phase as input to the Reduce task.
#' Note also tha the piped input for a Reduce task may arrive in any order, no sorting
#' is done by the service to the pipe between Map and Reduce tasks.
#'
#' @inheritParams mantaMap  
#'
#' @param reducers integer. Number of reducers to use from 1 to 1024. Use with caution. 
#'
#' @return Returns an R list for consumption by \code{mantaJob.setup}
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' # Example - Map/Reduce Unix Word Count
#' job <- mantaJob.setup("word count",
#'          mantaMap("wc"),
#'          mantaReduce("awk '{ l += $1; w += $2; c += $3 } END { print l, w, c }'"))
#'}
#'
#'
#' @export
mantaReduce <-
function(exec, init, assets, reducers, memory, disk) {
#asssets - use assets = mantaLs.paths(items = o, grepfor = "[.]sh") to make a list of all the *.sh files in the current directory for an asset list

    # If this is the first export function called in the library
    if (manta_globals$manta_initialized == FALSE) {
      mantaInitialize(useEnv = TRUE)
    }
  
   if ( missing(exec) && missing(init) ) {
     msg <- ("mantaReduce - no exec or init shell command to execute")
     bunyanLog.error(msg)
     stop(msg)
   }

   if (! missing(reducers)) {
     if ( (reducers < 1) || (reducers > 1024) ) {
       msg <- paste("mantaReduce - Number of reducers provided: ", reducers, " cannot be < 1 or > 1024\n", sep="")
       bunyanLog.error(msg)
       stop(msg)
     }
   }

   if (! missing(memory) ) {
     memoryvalues <- c(128, 256, 512, 1024, 2048, 4096, 8192, 16384)
     if (length(which(memoryvalues == memory)) != 1) {
       msg <- paste("mantaReduce - memory specified as: ", memory, "\nMust be one of: ", memoryvalues," (MB)\n",sep="") 
       bunyanLog.error(msg)
       stop(msg)
     }
   }

   if (! missing(disk) ) {
     diskvalues <- c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
     if (length(which(diskvalues == disk)) != 1) {
       msg <- paste("mantaReduce - disk specified as: ", disk, "\nMust be one of: ", diskvalues ," (GB)\n",sep="")
       bunyanLog.error(msg)
       stop(msg) 
     }
   }

   phase_r <- c('type' = "reduce")
   if (! missing(init) ) {
    phase_r <- c('init' = init)
   }
   if (! missing(exec) ) {
    phase_r <- c(phase_r, 'exec' = exec) 
   }
   if (! missing(assets) ) {
    phase_r <- c(phase_r, 'assets' = list(assets)) 
   } 
   if (! missing(memory) ) {
    phase_r <- c(phase_r, 'memory' = memory)
   }
   if (! missing(disk) ) {
    phase_r <- c(phase_r, 'disk' = disk)
   }
  return(phase_r)
}
