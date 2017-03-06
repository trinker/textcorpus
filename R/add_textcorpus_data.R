# pax::new_data(
#
#     presidential_debates_2012
#
# metadata <- dplyr::data_frame(
#     data = c('presidential_debates_2012'),
#     genre = c('debate'),
#     subgenre = c('political'),
#     source = c('http://www.presidency.ucsb.edu'),
#     submitted_by = c('tyler rinker'),
#     sbmitted_on = c(Sys.Date())
# )
#
# ## use a unit test that grabs the data sets and passes the checks

add_textcorpus_data <- function(x, description, scraping.script = NULL, ...) {

    check_add_textcorpus_data(x[['corpus']], x[['meta']])
    check_description(description)
    description <- description[desc_list]
    description[['submitted_on']] <- as.Date(description[['submitted_on']])

# add data based on pax::new_data
# add .R data file based on pax::new_data
# add description data
# message to check the .R file

}

#' Title
#'
#' Description
#'
#' @param copy2clip logical.  If \code{TRUE} the template is copied to the
#' clipboard.
#' @rdname description_template
#' @export
#' @examples
#' description_template(FALSE)
description_template <- function(copy2clip = TRUE) {
    out <- paste0("list(\n", paste(paste0("    ", desc_list, " = \"\""), collapse = ",\n"), "\n)\n")
    out <- gsub('\"\"\n)', 'Sys.Date()\n)', out)

    if (copy2clip) clipr::write_clip(out)
    cat(out)
}



# x <- list(
#     data = "presidential_debates",
#     genre = "debate",
#     subgenre = "political",
#     source = "http://www.presidency.ucsb.edu",
#     submitted_by = "Tyler Rinker",
#     submitted_on = Sys.Date()
# )



desc_list <- c("data", "genre", "subgenre", "source", "submitted_by", "submitted_on")




check_description <- function(x, ...){

    if(!all(names(x) %in% desc_list)) {
        stop("The following fields not found:\n\n",
            paste(paste0("  -", names(x)[!names(x) %in% desc_list]), collapse = "\n"))
    }
    x <- x[desc_list]
    if(any(is.na(x) | grepl("^\\s*$", x))) {
         stop("The following fields are missing:\n\n",
              paste(paste0("  -", names(is.na(x))[is.na(x) | grepl("^\\s*$", x)]), collapse = "\n"))
    }
    if(is.na(as.Date(x[['submitted_on']])) ) {
         stop("`submitted_on` does not appear to be coercable to a date.")
    }

    message("`description` list looks ready for inclusion.")
}


check_add_textcorpus_data <- function(x, meta, ...){

    corpus.cols <- c('id', 'author', 'text')

    check_textcorpus_data(x)
    check_textcorpus_meta_data(x, meta)

}
