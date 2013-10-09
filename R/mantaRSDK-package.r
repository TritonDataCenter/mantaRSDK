#' mantaRSDK
#'
#' Joyent Manta Storage Service R Software Development Kit
#'  
#'  UNDER DEVELOPMENT
#'
#'  R functions to transmit/receive native R data and 
#'  files to the Manta Storage Service.
#' 
#'  Manta jobs can compute on these with MapReduce and
#'  R in the cloud. Core functions communicate via
#'  the Manta REST API using RCurl. Optional interactive
#'  Manta system R access via Node.js.
#'
#'  To use this library you require a Joyent Manta account
#'  http://www.joyent.com
#'  
#'  Configuration requires 3 environment variables
#'  MANTA_USER
#'  MANTA_KEY
#'  MANTA_URL
#'  and your private SSL key as registered with Joyent 
#'
#'
#' ...
#' 
#' @references http://apidocs.joyent.com/manta/
#' @import PKI digest RCurl RJSONIO 
#' @name mantaRSDK
#' @docType package
NULL
