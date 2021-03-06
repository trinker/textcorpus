% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_textcorpus_data.R
\name{add_textcorpus_data}
\alias{add_textcorpus_data}
\title{Add a Corpus Dataset to \pkg{textcorpus}}
\usage{
add_textcorpus_data(x, description, scraping.script = NULL, ...)
}
\arguments{
\item{x}{A prepared \pkg{textcorpus} dataset.  Use 
\code{\link{prepare_textcorpus_data}} to ensure data is in the correct format.}

\item{description}{A description list that contains the required meta data 
about the corpus being added.  Use \code{\link{description_template}} to
start the template.}

\item{scraping.script}{An optional path to the .R script used to generate the
dataset.}

\item{\ldots}{ignored.}
}
\description{
Add a corpus dataset to \pkg{textcorpus}.  This function carefully checks the 
format of the data to make sure it meets \pkg{textcorpus} standards.  After 
the function is run the contributor will need to:
}
\details{
\enumerate{
  \item Fill in the documentation in R/data_YOUR_CONTRIBUTED_DATASET.R
  \item Run `devtools::document()`
  \item Run `devtools::check()`
  \item Push to GitHub
}
}
\examples{
\dontrun{
library(dplyr)
library(stringi)

corp <- data_frame(
    id = stri_rand_strings(10, 10),
    author = c('sam', 'cal', 'sue', 'bob', 'sal', 'pam', 'pat', 'joe', 'arr', 'nmr'),
    text = stri_rand_lipsum(10),
    state = state.name[1:10],
    month = month.name[1:10]
)

mydat <- prepare_textcorpus_data(corp, corpus.cols = c('id', 'author', 'text', 'state')) 

desc_dat <- list(
    data = "mydat",
    genre = 'poem',
    subgenre = 'haiku',
    source = "https://url/to/nowhere.com",
    submitted_by = "Tyler Rinker",
    submitted_on = Sys.Date()
)

# add_textcorpus_data(mydat, desc_dat)
}
}
