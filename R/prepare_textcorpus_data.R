#' Perpare a Corpus Data Set for Inclusion in the \pkg{textcorpus} Package
#'
#' Prepare a corpus data.frame for inclusion in the \pkg{textcorpus} package.
#'
#' @param x The main corpus data set.
#' @param corpus.cols The corpus columns found in \code{x}.  At the very least
#' \code{c('id', 'author', 'text')} are required columns.  The user may supply
#' the entire data set for \code{x} and specify meta data columns to peel off
#' and make distinct or may pass a reduced number of columns to \code{x} and
#' pass the matching meta data through the \code{meta} argument.
#' @param meta The optional meta data set.
#' @param \ldots ignored.
#' @return Returns a list of a main corpus text data and meta data with a
#' matching `id` key.
#' @export
#' @examples
#' library(dplyr)
#' library(stringi)
#'
#' corp <- data_frame(
#'     id = stri_rand_strings(10, 10),
#'     author = c('sam', 'cal', 'sue', 'bob', 'sal', 'pam', 'pat', 'joe', 'arr', 'nmr'),
#'     text = stri_rand_lipsum(10),
#'     state = state.name[1:10],
#'     month = month.name[1:10]
#' )
#'
#' prepare_textcorpus_data(corp)
#' prepare_textcorpus_data(corp, corpus.cols = c('id', 'author', 'text', 'state'))
prepare_textcorpus_data <- function(x, corpus.cols = c('id', 'author', 'text'),
    meta = NULL, ...){

    message("Checking data...\n")
    check_textcorpus_data(x)

    if (!is.null(meta)){

        check_textcorpus_meta_data(x, meta)
        corpus <- x

    } else {
        message("Preparing the `meta` data...")
        if (!all(c('id', 'author', 'text') %in% corpus.cols)) stop("`corpus.cols` must include `c('id', 'author', 'text')`")
        req <- c('id', 'author', 'text')
        corpus_vars <- c(req, corpus.cols[!corpus.cols %in% req])
        meta_vars <- c('id', colnames(x)[!colnames(x) %in% corpus.cols])

        corpus <- dplyr::select_(x, .dots = corpus_vars)
        meta <- dplyr::distinct(dplyr::select_(x, .dots = meta_vars), .keep_all = TRUE)
        message("`meta` prepared...")
    }
    if (nrow(x) == nrow(meta)) message("`x` appears to have the same number of rows as `meta`.  Make sure this is correct!")
    message("Outputing a list with a `corpus` tibble and a `meta` tibble...\n")
    list(corpus = set_tibble(corpus), meta = set_tibble(meta))
}

#' Check if Data Meets \pkg{textcorpus} Standards
#'
#' \code{check_textcorpus_data} - Ensure a corpus data set meets
#' \pkg{textcorpus} standards.  The data is expected to contain
#' \code{c('id', 'author', 'text')} columns.  The \code{"text"} column must
#' be a character vector.  Non-ASCII characters are not allowed in any columns.
#'
#' @param x The main corpus data set.
#' @param meta The meta data set.
#' @param \ldots ignored.
#' @rdname check_textcorpus_data
#' @export
#' @examples
#' library(dplyr)
#' library(stringi)
#'
#' corp <- data_frame(
#'     id = stri_rand_strings(10, 10),
#'     author = c('sam', 'cal', 'sue', 'bob', 'sal', 'pam', 'pat', 'joe', 'arr', 'nmr'),
#'     text = stri_rand_lipsum(10),
#'     state = state.name[1:10],
#'     month = month.name[1:10]
#' )
#'
#' check_textcorpus_data(corp)
#' check_textcorpus_meta_data(corp, corp[c('id', 'state', 'month')])
check_textcorpus_data <- function(x, ...){

    if (!is.data.frame(x)) stop('`x` is not a data.frame')

    cols <- c('id', 'author', 'text')
    missing_cols <- cols[!cols %in% colnames(x)]
    if (length(missing_cols) > 0) {
        stop(paste0("The following columns are required for inclusion in the `textcorpus` package`:\n\n",
            paste(paste0(" -", missing_cols), collapse = "\n")))
    }

    if (!is.character(x[['text']])) stop("The `text` column is not of class `character`.")

    nons <- suppressWarnings(check_non_ascii(x))
    if(sum(nons[['count_non_ascii']]) > 0 ) {
        bad_eggs <- nons[['variable']][nons[['count_non_ascii']] > 0]
        stop(paste0("Non-ASCII characters found. The following column(s) contained non-ASCII characters:\n\n",
            paste(paste0(" -", bad_eggs), collapse = "\n"),
            "\n\nThe `textcorpus::check_non_ascii` function can provide more details.\n",
            "Suggested use of `textclean::replace_non_ascii` to remove bad characters.\n"
        ))
    }

    message("Looks like `x` is ready for inclusion in the `textcorpus` package!")
}

#' Check if Data Meets \pkg{textcorpus} Standards
#'
#' \code{check_textcorpus_meta_data} - Ensure a corpus meta data set meets
#' \pkg{textcorpus} standards.  The data is expected to contain
#' an 'id' column at minimum.  Other `id` level meta data are accepted as well.
#' The `id` column must match the class of `x`'s `id` column.  Non-ASCII
#' characters are not allowed in any columns.
#'
#' @export
#' @rdname check_textcorpus_data
check_textcorpus_meta_data <- function(x, meta, ...){

    if (!is.data.frame(x)) stop('`x` is not a data.frame')
    if (!is.data.frame(meta)) stop('`meta` is not a data.frame')
    if (!'id' %in% colnames(meta)) stop("`id` not found in `meta` data.  This must be included and correspond to an `id` column in `x`.")
    suppressMessages(check_textcorpus_data(x))
    x_has <- setdiff(unique(x[['id']]), unique(meta[['id']]))
    meta_has <- setdiff(unique(meta[['id']]), unique(x[['id']]))
    if (length(x_has) > 0 | length(meta_has) > 0) {
        if (length(x_has) > 0 & length(meta_has) == 0) {
            stop("`x` has `id` elements not found in `meta`")
        }
        if (length(x_has) > 0 & length(meta_has) > 0) {
            stop("both `meta` and `x` have `id` elements not found in the other")
        }
        stop("`meta` has `id` elements not found in `x`")
    }
    class_x <- class(x[['id']])
    class_meta <- class(meta[['id']])
    if (class_x != class_meta) {
        stop(paste0("`x` is of class '", class_x, "' while `meta` is of class '", class_x, "'\n",
            "x$id and meta$id must be of the same class"))
    }

    message("Looks like `meta` is ready for inclusion in the `textcorpus` package!")
}


is_non_ascii <- function(x) stringi::stri_detect_regex(x, "[^ -~]")

## convert a data.table to tibble
set_tibble <- function(x, ...){
    stopifnot(is.data.frame(x))
    class(x) <- c("tbl_df", "tbl", "data.frame")
    x
}

check_non_ascii <- function(x) {

    stopifnot(is.data.frame(x))

    isnon <- lapply(x, is_non_ascii)
    nons <- textshape::tidy_list(lapply(isnon, sum, na.rm = TRUE), 'variable', 'count_non_ascii')
    nons$location <- lapply(isnon, which)

    if(sum(nons[['count_non_ascii']]) > 0 ) warning("Non ascii characters found.  Suggest use of `textclean::replace_non_ascii`.")

    set_tibble(nons)
}




