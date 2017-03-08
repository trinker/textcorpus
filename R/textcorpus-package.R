#' A Community Based Text Corpus Data Store
#'
#' Contains text corpus data sets and tools for easy community additions of new data sets.
#' @docType package
#' @name textcorpus
#' @aliases textcorpus package-textcorpus
NULL


#' Descriptions of \pkg{textcorpus} Datasets
#'
#' A searchable meta dataset on the available datasets in \pkg{textcorpus}.
#'
#' @details
#' \itemize{
#'   \item id. An id the maps between the \code{corpus} and \code{meta} datasets
#'   \item author. The speaker/writer of the text element
#'   \item text. The text variable
#'   \item order. VARIABLE DESCRIPTION
#' }
#' 
#' \itemize{
#'   \item data. The name of available corpus data set
#'   \item genre. The genere of the data set (e.g., poem, literature, song, debate, transcript, etc.)
#'   \item subgenre. A subgenre of the data set (e.g., political, educational, folk, country, comedy, etc.)
#'   \item source. Where the data was obtained from
#'   \item submitted_by. Who submitted the data to \pkg{textcorpus}
#'   \item submitted_on. The date when the data was submitted
#' }
#'
#' @docType data
#' @name description
#' @usage data(description)
#' @format A tibble with 6 variables 
NULL
