% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_nixon_tapes.R
\docType{data}
\name{nixon_tapes}
\alias{nixon_tapes}
\title{Nixon Recordings}
\format{A list with two tibbles:
\code{corpus} with 8,817 rows and 4 variables &
\code{meta} with 31 rows and 4 variables}
\usage{
data(nixon_tapes)
}
\description{
Watergate trial conversations between Nixon and close staffers which were 
recorded.
}
\details{
Corpus:
\itemize{
  \item id. An id the maps between the \code{corpus} and \code{meta} datasets
  \item author. The speaker/writer of the text element
  \item text. The text variable
  \item order. Order of conversation within recordings
}

Meta:
\itemize{
  \item id. An id the maps between the \code{corpus} and \code{meta} datasets
  \item date. The date the conversation was recorded
  \item location. The location the conversation was recorded
  \item minutes. The length of time of the recorded conversation
}
}
\references{
https://www.nixonlibrary.gov/forresearchers/find/tapes/watergate/trial/transcripts.php
}
