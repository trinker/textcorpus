#' Add a Corpus Dataset to \pkg{textcorpus}
#' 
#' Add a corpus dataset to \pkg{textcorpus}.  This function carefully checks the 
#' format of the data to make sure it meets \pkg{textcorpus} standards.  After 
#' the function is run the contributor will need to:
#' 
#' \enumerate{
#'   \item Fill in the documentation in R/data_YOUR_CONTRIBUTED_DATASET.R
#'   \item Run `devtools::document()`
#'   \item Run `devtools::check()`
#'   \item Push to GitHub
#' } 
#' 
#' @param x A prepared \pkg{textcorpus} dataset.  Use 
#' \code{\link{prepare_textcorpus_data}} to ensure data is in the correct format.
#' @param description A description list that contains the required meta data 
#' about the corpus being added.  Use \code{\link{description_template}} to
#' start the template.
#' @param scraping.script An optional path to the .R script used to generate the
#' dataset.
#' @param \ldots ignored.
#' @export
#' @examples 
#' \dontrun{
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
#' mydat <- prepare_textcorpus_data(corp, corpus.cols = c('id', 'author', 'text', 'state')) 
#' 
#' desc_dat <- list(
#'     data = "mydat",
#'     genre = 'poem',
#'     subgenre = 'haiku',
#'     source = "https://url/to/nowhere.com",
#'     submitted_by = "Tyler Rinker",
#'     submitted_on = Sys.Date()
#' )
#' 
#' # add_textcorpus_data(mydat, desc_dat)
#' }
add_textcorpus_data <- function(x, description, scraping.script = NULL, ...) {

    ## make sure in correct repo and data file exists
    if (basename(getwd()) != "textcorpus") stop("The working directory must be `textcorpus`.")
    if (!file.exists('data')) stop("You don't appear to be using the forked `textcorpus` repo.\nThe `data` directory is missing.")
        
    ## Check the data and description
    check_add_textcorpus_data(x[['corpus']], x[['meta']])
    nm <- as.character(substitute(x))
    if(nm != description[['data']]) stop('Dataset name (`x`) does not match \'data\' field in the \'description\'')
    check_description(description)
    description <- description[desc_list]
    description[['submitted_on']] <- as.Date(description[['submitted_on']])

    ## check if .rda file already exists
    if (file.exists(sprintf("%s/%s.rda", 'data', nm))) {
        message(sprintf("The following data set already exists:\n%s\n", sprintf("%s/%s.rda", 'data', nm)))
        message("Should this file be over-written?")
        ans <- utils::menu(c("Yes", "No"))
        if (ans == "2") {
            stop("`add_textcorpus_data` aborted")
        } 
    }
    
    ## add .rda file to data dir    
    datenv <- new.env(FALSE)
    assign(nm, x, envir = datenv)
    save(list = nm, envir = datenv, file = sprintf("%s/%s.rda", 'data', nm), compress = TRUE)
    if (file.exists(sprintf("%s/%s.rda", 'data', nm))) {
        message(sprintf("Data has been added as '%s'...", sprintf("%s/%s.rda", 'data', nm)))
        message("Adding .R data documentation file...")        
    } else {
        stop('Data set was not added.  I\'m not sure why.')
    }
    
    ## check if .R file already exists
    if (file.exists(sprintf("%s/data_%s.R", 'R', nm))) {
        message(sprintf("The following .R documentation file already exists:\n%s\n", sprintf("%s/data_%s.R", 'R', nm)))
        message("Should this file be over-written?")
        ans <- utils::menu(c("Yes", "No"))
        if (ans == "2") {
            stop("`add_textcorpus_data` aborted")
        } 
    }    
    
    ## add .R data file based on pax::new_data
    format_r_file(x, description, file = sprintf("%s/data_%s.R", 'R', nm), nm)    
    if (file.exists(sprintf("%s/data_%s.R", 'R', nm))) {
        message(sprintf(".R data documentation file has been added as '%s'...", sprintf("%s/data_%s.R", 'R', nm)))
        message("Adding description of dataset...")        
    } else {
        warning('.R file was not added.  I\'m not sure what went wrong.')
    }
    
    
    # add description data
    add_description(description)

    if (!is.null(scraping.script)){
        scripts <-gsub("\\.[A-Za-z]+$", "", dir('inst/scraping_scripts'))
        if(description[['data']] %in% scripts) {
            message(sprintf("The following .R scraping script file already exists:\n'%s'\n", sprintf("%s/%s.R", 'inst/scraping_scripts', description[['data']])))
            message("Should this file be over-written?")
            ans <- utils::menu(c("Yes", "No"))
            if (ans == "2") {
                stop("`add_textcorpus_data` aborted")
            } 
        }
        cat(paste(readLines(scraping.script), collapse = "\n"), 
            file = sprintf('inst/scraping_scripts/%s.R', description[['data']]))
    }
    
    message(paste0("\n\nAll appears to have went well.  The next steps are:\n\n",
        sprintf("1. Fill in the documentation in %s\n", sprintf("%s/data_%s.R", 'R', nm)),
        "2. Run `devtools::document()`\n",
        "3. Run `devtools::install()`\n",
        "4. Run `devtools::check()`\n",
        "5. Push to GitHub"
        ))
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







desc_list <- c("data", "genre", "subgenre", "source", "submitted_by", "submitted_on")



#' Check a \pkg{textcorpus} \code{description}
#' 
#' Ensures a \code{description} list is up \pkg{textcorpus} standards.
#' 
#' @param x A description list that contains the required meta data 
#' about the corpus being added.  Use \code{\link{description_template}} to
#' start the template.
#' @param \ldots ignored.
#' @export
#' @rdname check_description
#' @examples 
#' desc_dat <- list(
#'     data = "mydat",
#'     genre = 'poem',
#'     subgenre = 'haiku',
#'     source = "https://url/to/nowhere.com",
#'     submitted_by = "Tyler Rinker",
#'     submitted_on = Sys.Date()
#' )
#' 
#' check_description(desc_dat)
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
    if (length(x[['data']]) > 1) {
         stop("`data` field must be of length 1.")
    }   
    if (length(x[['genre']]) > 1) {
         stop("`genre` field must be of length 1.")
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


format_r_file <- function(x, description, file, nm, ...){

    more_corpus_cols <- setdiff(colnames(x[['corpus']]), c("id", "author", "text"))
    more_meta_cols <- setdiff(colnames(x[['meta']]), c("id"))

    if (length(more_corpus_cols) > 0) {
        more_corpus_cols <- sprintf("  \\item %s. VARIABLE DESCRIPTION", more_corpus_cols)
    }

    if (length(more_meta_cols) > 0) {
        more_meta_cols <- sprintf("  \\item %s. VARIABLE DESCRIPTION", more_meta_cols)        
    }


    refs <- paste0("#' ", description[['source']])

    title_desc_corpus_desc <- c(
        "TITLE HERE\n#'\n#' DESCRIPTION HERE\n#'", "@details\n#' Corpus:", "\\itemize{",
        "  \\item id. An id the maps between the \\code{corpus} and \\code{meta} datasets",
        "  \\item author. The speaker/writer of the text element",
        "  \\item text. The text variable",
        more_corpus_cols,    
        "}"
    )
    

    meta_desc <- c(
        "\n#' Meta:\n#' \\itemize{",
        "  \\item id. An id the maps between the \\code{corpus} and \\code{meta} datasets",
        more_meta_cols,    
        "}"
    )

    back_end <- c(
        sprintf("#'\n#' @docType data\n#' @name %s\n#' @usage data(%s)", nm, nm),
        "#' @format A list with two tibbles:",
        sprintf("#' \\code{corpus} with %s rows and %s variables &", prettyNum(nrow(x[['corpus']]), big.mark=","), prettyNum(ncol(x[['corpus']]), big.mark=",")),
        sprintf("#' \\code{meta} with %s rows and %s variables", prettyNum(nrow(x[['meta']]), big.mark=","), prettyNum(ncol(x[['meta']]), big.mark=",")),
        "#' @references", refs, "NULL\n\n"
    )

    cat(paste(c(paste('#\'', c(title_desc_corpus_desc, meta_desc)), back_end), collapse = "\n"), file = file)
    
}



#' Add a \pkg{textcorpus} \code{description}
#' 
#' Adds a \code{description} list to the \pkg{textcorpus} \code{description}.
#' 
#' @param x A description list that contains the required meta data 
#' about the corpus being added.  Use \code{\link{check_description}} to
#' check the template.
#' @param description.data The \pkg{textcorpus} description dataset.
#' @param \ldots ignored.
#' @export
#' @rdname add_description
#' @examples 
#' \dontrun{
#' desc_dat <- list(
#'     data = "mydat",
#'     genre = 'poem',
#'     subgenre = 'haiku',
#'     source = "https://url/to/nowhere.com",
#'     submitted_by = "Tyler Rinker",
#'     submitted_on = Sys.Date()
#' )
#' 
#' check_description(desc_dat)
#' add_description(desc_dat)
#' }
add_description <- function(x, description.data = textcorpus::description, ...){  
    
    suppressMessages(check_description(x))
    
    if (x[['data']] %in% description.data[['data']]) {
        message(sprintf("'%s' already exists in `textcorpus` 'description' dataset...", x[['data']]))
        message("Should this file be over-written?")
        ans <- utils::menu(c("Yes", "No"))
        if (ans == "2") {
            stop("`add_textcorpus_data` aborted")
        }            
    }
    
    updated_description <- dplyr::select_(dplyr::arrange_(tidyr::nest_(dplyr::bind_rows(
        dplyr::tbl_df(description.data),
        dplyr::select_(tidyr::nest_(dplyr::tbl_df(x), 'subgenre', "subgenre"),
            "data", "genre", "subgenre", "source", "submitted_by", "submitted_on")
    ), 'subgenre', "subgenre"), 'genre', 'data'), 
        "data", "genre", "subgenre", "source", "submitted_by", "submitted_on")

    datenv <- new.env(FALSE)
    assign('description', updated_description, envir = datenv)
    save(list = 'description', envir = datenv, file = sprintf("%s/%s.rda", 'data', 'description'), compress = TRUE)
    if (file.exists(sprintf("%s/%s.rda", 'data', 'description'))) {
        message("'data/description.rda' data has been updated...")
    } else {
        stop("'data/description.rda' was not updated.  I\'m not sure why.")
    }    
    
}

