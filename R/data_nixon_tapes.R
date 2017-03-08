#' Nixon Recordings
#'
#' Watergate trial conversations between Nixon and close staffers which were 
#' recorded.
#'
#' @details
#' Corpus:
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item author. The speaker/writer of the text element
#'   \item text. The text variable
#'   \item order. Order of conversation within recordings
#' }
#' 
#' Meta:
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item date. The date the conversation was recorded
#'   \item location. The location the conversation was recorded
#'   \item minutes. The length of time of the recorded conversation
#' }
#'
#' @docType data
#' @name nixon_tapes
#' @usage data(nixon_tapes)
#' @format A list with two tibbles:
#' \code{corpus} with 8,817 rows and 4 variables &
#' \code{meta} with 31 rows and 4 variables
#' @references
#' https://www.nixonlibrary.gov/forresearchers/find/tapes/watergate/trial/transcripts.php
NULL

