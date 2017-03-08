#' Presidential Debates
#'
#' Presidential/vice presidential primary and general election debates,
#'
#' @details
#' Corpus:
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item author. The speaker/writer of the text element
#'   \item text. The text variable
#'   \item order. The order of speakers within debates
#' }
#' 
#' Meta:
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item type. The type of debate (e.g., primary, general, vice, etc.)
#'   \item institution. Institution where the debate took place
#'   \item city. City where the debate took place
#'   \item date. Date when the debate took place
#'   \item state. State where the debate took place
#'   \item state_abb. State abbreviation of where the debate took place
#'   \item latitude. Latitude of the city where the debate took place
#'   \item longitude. Longitude of the city where the debate took place
#' }
#'
#' @docType data
#' @name presidential_debates
#' @usage data(presidential_debates)
#' @format A list with two tibbles:
#' \code{corpus} with 226,900 rows and 4 variables &
#' \code{meta} with 138 rows and 9 variables
#' @references
#' http://www.presidency.ucsb.edu
NULL

