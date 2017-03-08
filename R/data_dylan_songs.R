#' Bob Dylan Songs
#'
#' A collection of lyrics to Bob Dylan's songs.
#'
#' @details
#' Corpus:
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item author. The speaker/writer of the text element
#'   \item text. The text variable
#'   \item order. The order of the lyrics within the songs
#' }
#' 
#' Meta:
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item song. The song title
#'   \item year. Year when song was released
#'   \item release_album. Album the song was released on
#'   \item label. The label the album was with
#'   \item first_played_date. Date when album was released
#' }
#'
#' @docType data
#' @name dylan_songs
#' @usage data(dylan_songs)
#' @format A list with two tibbles:
#' \code{corpus} with 85 rows and 4 variables &
#' \code{meta} with 5 rows and 6 variables
#' @references
#' https://bobdylan.com/songs/
NULL

