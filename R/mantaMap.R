# Roxygen Comments mantaMap
#' Constructor for R format Manta Job for Map Unix task.
#'
#' Helper function to construct R structure describing a Map task. To be used
#' to satisfy the \code{...} argument of \code{\link{mantaJob.setup}} 
#' and specify the Unix command line task, any initialization tasks, 
#' an array of Manta \code{asset} objects,
#' and the memory/disk size to be used for the compute instance on Manta.
#'
#' On Manta, a Map task phase executes a generic UNIX command given some input 
#' Manta object list which is specified in \code{mantaJob.launch}, which distributes 
#' the jobs to compute instances local to the Manta object location.
#' The \code{exec} argument must be a valid generic UNIX command line, not an R function.
#' The \code{exec} argument may call executables or runtime language scripts that are
#' hosted on Manta and specified as \code{assets}.  The \code{init} argument is called before the 
#' exec argument and is not passed input. The init argument may be used, for example
#' to extract scripts from an \code{asset} on Manta saved as a \code{tar} file.
#'
#' @param exec character required. The Unix shell command to be executed in the Map task
#' operating on the input Manta objects specified when the job is launched.
#' \code{exec} may be any valid Unix shell command capable of running on the Manta compute node 
#' at execution time. Use the Node.js command \code{mlogin} to test out commands. 
#' Pipelines and shell syntax escaping and substitution are all supported. 
#' You can also execute your own programs stored as Manta objects by including them with the 
#' \code{assets} parameter and referencing them from the exec command from the
#' \code{/assets} folder.\cr
#' See:\cr 
#' \code{http://apidocs.joyent.com/manta/jobs-reference.html}\cr 
#' for more details.
#'
#' @param init character optional. A Unix shell command executed prior to the \code{exec} command.
#' Used to run initialization steps on the Manta compute node prior to task execution. 
#' \code{init} can also execute programs stored as Manta objects mounted as POSIX read-only
#' files mounted at \code{/assets}. For example it can unpack a \code{tar} \code{asset} before
#' running \code{exec}. 
#'
#' @param assets array of character, optional. Specify Manta objects that are to
#' be accessed by the compute node at job runtime. Include shell scripts, installation steps
#' configuration steps, custom executables compiled for SmartOS, or \code{tar} files 
#' as you require here.
#' At job runtime, each node will provide the specified Manta objects as POSIX files at 
#' the \code{/assets} directory for read-only access from your \code{exec} or \code{init} 
#' shell commands. 
#' For example a Manta object listed as an asset that lives at \code{ ~~/stor/data.tgz} 
#' will be found by your script on the Manta compute node as a 
#' mounted read-only POSIX file at \code{/assets/~~/stor/data.tgz} where
#' \code{~~} is your Manta username.
#' 
#' @param memory integer optional. Amount of memory requested for Manta 
#' compute node instance. 
#' \code{128, 256, 512, 1024, 2048, 8192,} or \code{16384} are valid values in MB. 
#' Default is \code{1024} MB.
#'
#' @param disk integer optional. Amount of temporary working disk (not Manta storage space) to be used 
#' by the compute node when executing the task. Valid values are:
#' \code{2, 4, 8, 16, 32, 64, 128, 256, 512,} or \code{1024} GB. 
#' Default is \code{8} GB. Writeable disk on each compute node is found 
#' at the \code{/var/tmp} directory during \code{init} or \code{exec} job runtime. 
#' To save data from this space onto permanent Manta storage, use the Node.js command 
#' \code{mput} in your exec script to upload the files from \code{/var/tmp} onto Manta storage.
#' 
#' @return Returns an R list describing a Map task phase for consumption by \code{mantaJob.setup} 
#' 
#' @keywords Manta, manta
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
#' @export
mantaMap <- 
function(exec, init, assets, memory, disk) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  } 
 
   if ( missing(exec) && missing(init) ) {
     msg <- ("mantaMap - no exec or init shell command to execute")
     bunyanLog.error(msg)
     stop(msg)
   }

   if (! missing(memory) ) {
     memoryvalues <- c(128, 256, 512, 1024, 2048, 4096, 8192, 16384)
     if (length(which(memoryvalues == memory)) != 1) {
       msg <- paste("mantaMap - memory specified as: ", memory, "\nMust be one of: ", memoryvalues," (MB)\n",sep="") 
       bunyanLog.error(msg)
       stop(msg)
     }
   }

   if (! missing(disk) ) {
     diskvalues <- c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
     if (length(which(diskvalues == disk)) != 1) {
       msg <- paste("mantaMap - disk specified as: ", disk, "\nMust be one of: ", diskvalues ," (GB)\n",sep="")
       bunyanLog.error(msg)
       stop(msg) 
     }
   }

   phase_m <- c('type' = "map")
   if (! missing(init) ) {
    phase_m <- c('init' = init)
   }
   if (! missing(exec) ) {
    phase_m <- c(phase_m, 'exec' = exec) 
   }
   if (! missing(assets) ) {
    phase_m <- c(phase_m, 'assets' = list(assets)) 
   } 
   if (! missing(memory) ) {
    phase_m <- c(phase_m, 'memory' = memory)
   }
   if (! missing(disk) ) {
    phase_m <- c(phase_m, 'disk' = disk)
   }
  return(phase_m)
}
