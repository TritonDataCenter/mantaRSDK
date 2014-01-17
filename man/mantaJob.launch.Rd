\name{mantaJob.launch}
\alias{mantaJob.launch}
\title{The interface from which compute Jobs are launched.}
\usage{
  mantaJob.launch(inputs, job, batchsize = 500,
    watch = TRUE, sleep = 15, watchtimeout = 600,
    silent = FALSE, verbose = FALSE)
}
\arguments{
  \item{inputs}{vector of character optional. List of
  \code{inputs} as a vector of character, each containing
  valid paths to Manta objects as the intended job input
  files. You may use output from \code{mantaFind} or
  \code{mantaLs.paths} here. If you have no inputs, your
  initial Job task must be a \code{mantaReduce} step.}

  \item{job}{required. The R job structure as created with
  \code{\link{mantaJob.setup}} and Map and Reduce job tasks
  as defined therein by one or more \code{\link{mantaMap}}
  and/or \code{\link{mantaReduce}} steps. More information
  and parameters are explained in the help for these three
  functions.}

  \item{batchsize}{numeric. Maximum number of input object
  paths to upload in one batch to the running job. This
  function sends \code{inputs} in batches until they are
  all sent. Default is \code{500}.}

  \item{watch}{logical. When \code{TRUE} calls
  \code{mantaJob.done} in polling mode, after job is
  initiated, sleeping for for \code{sleep} seconds up to
  the duration of the \code{watchtimeout} value in seconds.
  This causes the function to wait until the job is done to
  return, or until timed out. Timeout does not imply job
  success or failure.}

  \item{sleep}{integer. Number of seconds to wait between
  status requests in polling mode when \code{watch} is
  \code{TRUE}. Default is 15 seconds.}

  \item{watchtimeout}{integer. Number of seconds after
  which polling ends. Passed to \code{\link{mantaJob.done}}
  when \code{watch} is set to \code{TRUE}. Default is 10
  minutes (600 seconds).. If \code{watchtimeout} is
  exceeded, it means the job is still running or queued on
  Manta. \code{mantaJob.done(poll = TRUE)} or
  \code{mantaJob.status} can be called afterward for more
  monitoring.}

  \item{silent}{logical. Supress console messages, does not
  affect verbose setting.}

  \item{verbose}{logical optional. Passed to \code{RCURL}
  to reveal \code{HTTP} communication.}
}
\value{
  Returns a Manta \code{status} JSON structure. The Manta
  Job identifier is the "id": field - like this
  \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}. This value
  is the \code{jobid} parameter to be used used by other
  \code{mantaJob} functions for inputs, status, errors and
  outputs as Job lookup key. This key can also be used by
  Node.js Manta command-line \code{mjob} commands.
}
\description{
  Submits R format Manta Job specification, runs Job, sends
  \code{inputs} if specified, closes \code{inputs}, polls
  Job status, returns Job status JSON.
}
\details{
  Job is created by \code{\link{mantaJob.setup}} and tasks
  as defined therein by \code{\link{mantaMap}}, and/or
  \code{\link{mantaReduce}} functions. Note that Manta
  tasks are UNIX shell commands, not native R commands.
}
\examples{
\dontrun{
## Example - Map/Reduce Unix Word Count

## Part 1.
## Job to download all of Shakespeare's plays to your account
plays <-
c("1kinghenryiv.txt", "1kinghenryvi.txt", "2kinghenryiv.txt",
"2kinghenryvi.txt", "3kinghenryvi.txt", "allswellthatendswell.txt",
"antonyandcleopatra.txt", "asyoulikeit.txt", "comedyoferrors.txt",
"coriolanus.txt", "cymbeline.txt", "hamlet.txt", "juliuscaesar.txt",
"kinghenryv.txt", "kinghenryviii.txt", "kingjohn.txt", "kinglear.txt",
"kingrichardii.txt", "kingrichardiii.txt", "loverscomplaint.txt",
"loveslabourslost.txt", "macbeth.txt", "measureforemeasure.txt",
"merchantofvenice.txt", "merrywivesofwindsor.txt",
"midsummersnightsdream.txt",
"muchadoaboutnothing.txt", "othello.txt", "periclesprinceoftyre.txt",
"rapeoflucrece.txt", "romeoandjuliet.txt", "sonnets.txt",
"tamingoftheshrew.txt",
"tempest.txt", "timonofathens.txt", "titusandronicus.txt",
"troilusandcressida.txt",
"twelfthnight.txt", "twogentlemenofverona.txt", "various.txt",
"venusandadonis.txt", "winterstale.txt")

file <- file("plays_list.txt", "wb")
# Important: This forces Windows to use /n instead of /r/n on write()
write(plays, file)
close(file)
rm(file)

mantaSetwd.stor()
mantaPut("plays_list.txt")

inputlist <- mantaLs.paths(grepfor = "plays_list.txt")

mantaMkdir("shakespeare")
mantaSetwd("shakespeare")
fileslocation <-
"https://us-east.manta.joyent.com/cwvhogue/public/shakespeare/"
destination <- mantaGetwd()
mantaJob.setup("Get Plays",
 mantaMap(paste("xargs -I {} sh -c 'curl -ksL ",
                 fileslocation,
                "{} | mput ",
                destination,
                "/{}'",
                sep=""))) -> moveplays

## Launch the first job to download the plays:
mantaJob.launch(inputlist, moveplays)

## See if they arrived.
mantaLs()
mantaLs.n()
mantaLs.du()

## Copy all the plays to your local drive?
# mantaGet(mantaFind())

## Speedread all of Shakespeare?
# mantaCat(mantaFind())

## Part 2.
## Map/Reduce Count all the words with wc and awk

inputs <- mantaFind()
job <- mantaJob.setup(
            name = "Word Count",
            mantaMap("wc"),
            mantaReduce("awk '{ l += $1; w += $2; c += $3 } END { print l, w, c }'")
        )

mantaJob.launch(inputs, job) -> status
## Getting Job Results:
## These functions find the last Job run if no jobid provided.
# mantaJob.status()  ## check to see if job is complete, as JSON information
# mantaJob.done()    ## returns logical job done (TRUE/FALSE)
# mantaJob.inputs()  ## returns list of inputs
mantaJob.outputs() ## retrieve list of paths to Manta output objects
# mantaJob.errors()  ## retrieve JSON formatted job error information
mantaJob.outputs.cat()   ## Print job output (text files) to console
# mantaJob.errors.stderr() ## Print any stderr messages to console
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.cancel}},
  \code{\link{mantaJob.done}},
  \code{\link{mantaJob.errors}},
  \code{\link{mantaJob.errors.stderr}},
  \code{\link{mantaJob.failures}},
  \code{\link{mantaJob.inputs}},
  \code{\link{mantaJob.outputs}},
  \code{\link{mantaJob.outputs.cat}},
  \code{\link{mantaJob.setup}},
  \code{\link{mantaJob.status}}, \code{\link{mantaJobs}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}, \code{\link{mantaMap}},
  \code{\link{mantaReduce}}
}
\keyword{Manta,}
\keyword{manta}

