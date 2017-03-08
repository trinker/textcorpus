pacman::p_load(tidyverse, rvest, xml2, textreadr, textshape, textclean, stringi)

pdf_links <- 'https://www.nixonlibrary.gov/forresearchers/find/tapes/watergate/trial/transcripts.php' %>%
    read_html() %>%
    html_nodes(xpath = '//li/a') %>%
    html_attr('href') %>%
    {grep('forresearchers.+\\.pdf', ., value=TRUE)} %>%
    {file.path('https://www.nixonlibrary.gov', .)}

td <- tempdir()
loc <- file.path(td, 'nixon_tapes')
dir.create(loc)

lapply(pdf_links, function(x){
    out <- try(textreadr::download(x, loc))
    if (inherits(out, 'try-error')) out <- try(textreadr::download(x, loc))
    if (inherits(out, 'try-error')) out <- try(textreadr::download(x, loc))
    if (inherits(out, 'try-error')) browser()
    out
})

trans <- dir(loc, full.names = TRUE)

out <- lapply(trans, function(x) {

    dat <- try(read_pdf(x))
    first <- (which(grepl("^[A-Z]+(-|:)", trimws(dat$text)))[1])

    date <- paste(dat$text[1:(first-1)], collapse = " ") %>%
        tolower() %>%
        stringi::stri_extract_first_regex(paste0('(', tolower(paste(month.abb, collapse = "|")), ')[a-z]*\\s+\\d{0,2},?\\s+\\d{4}')) %>%
        {gsub("\\s+", " ", .)} %>% 
        tolower() %>%
        textclean::mgsub(tolower(c(month.name, month.abb)), c(month.abb, month.abb)) %>%
        {ifelse(nchar(.) == 8, gsub(" ", " 01, ", .), .)} %>%
        as.Date(format = "%b %d, %Y")
        
if (is.na(date)) browser()

     out <- try(
     trimws(dat$text)[first:(nrow(dat))] %>%
        {gsub("I--no\\.{3}", 'I...no...', .)} %>%
        split_match('^[A-Z]+(-|:)', include = TRUE, regex = TRUE) %>%
        sapply(paste, collapse = " ") %>% #substring(1, 80) %>% unname()
        {gsub("^N:", "DEAN:", .)} %>%
        {gsub("APRIL 19, 1973", " ", .)} %>%
        {data_frame(raw = .)} %>%
        extract(raw, into = c("author", "text"), regex = "(^[A-Z]+)\\s*[-:]+\\s*([^ ].+$)") %>%
        mutate(
            text = gsub("\\s+", " ", text),
            author = textclean::mgsub(author, c('PRESIDENT', 'HARDTN'), c('Nixon', 'HARDin')) %>%
                {gsub("(^.)(.+$)", "\\U\\1\\L\\2", ., perl = TRUE)} %>%
                textclean::mgsub( 
                    c('Kaumbach', 
                      'Ehplichman', 'Ehreichman', 'Ehrhlichman', 'Ehrlichyan', 'Ehrlicrman', 'Erlichman',
                      'Ehlrichman', 'Ehrlchman', 'Ehrlichaman', 'Ehrlichmm', 'Ehrltchman', 'Ehlrichman', 'Ehrlichlian',
                      'Cont', 'Prent', 'Presidemt', 'Presinent', 'Presidnet', 'Ppesident', 'Ppesident', 'Presidemt', 'Presift', 
                      'Presdent', 'Prpsident', 'Pxesident', 'Esident', 'Unidenitified', 'Haldefian', 'Haldemn', 'Haldemam', 
                       'Dealn', 'Dear', 'Deam'),                                                      
                    c('Kalmbach', rep('Ehrlichman', 7), rep('Nixon', 11), 'Unidentified', rep('Haldeman', 3), rep('Dean', 3))),

            order = seq_along(text),
            date = date
        )
    )
    if (any(c("N", "I", "Cont") %in% out$author)) browser()

   out
})

lapply(out, function(x) table(x$author))
lapply(out, function(x) x$text)

meta <- 'https://www.nixonlibrary.gov/forresearchers/find/tapes/watergate/trial/transcripts.php' %>%
    read_html() %>%
    html_nodes(xpath = '//li') %>%
    html_text() %>%
    `[`(-c(1:5)) %>%
    split_match('^Cassette Number', include = TRUE, regex = TRUE) %>%    
    lapply(function(x) {

        str <- x %>%  
            paste(collapse = " ") %>%
            textclean::mgsub('Transcript | Listen', '') %>%
            textclean::mgsub('(\n|\r)+', ' ', fixed = FALSE)

        data_frame(
            minutes = str %>%
                stringi::stri_extract_first_regex("(?<=\\()[^\\)]+(?=\\))"),
            location = str %>%
                stringi::stri_extract_first_regex("(?<=Location:).+?(?=Exhibit Number:)") %>%
                stringi::stri_replace_all_regex("^\\s*|\\s*$", ""),
            exhibit = str %>%
                stringi::stri_extract_first_regex("(?<=Exhibit Number:)\\s*Exhibit\\s+\\d{1,2}") %>%
                stringi::stri_replace_all_regex("^\\s*|\\s*$|\\s*Exhibit\\s+", "")
        )

    }) %>%
    bind_rows() %>%
    mutate(
        exhibit = sprintf("%02d", as.numeric(exhibit)),
        exhibit = ifelse(minutes == '30 minutes' & exhibit == '01', '00 - connally', exhibit),
        minutes = as.numeric(gsub('[^0-9]', '', minutes))
    ) %>%
    group_by(exhibit, location) %>%
    summarize(minutes = sum(minutes))


nixon_tapes_primed <- out %>%
    setNames(meta$exhibit) %>%
    tidy_list('id') %>%
    left_join(meta, by = c('id' = 'exhibit')) %>%
    tbl_df() %>%
    mutate(
        text = replace_non_ascii(text)   
    )




pacman::p_load_current_gh('trinker/textcorpus')
nixon_tapes <- prepare_textcorpus_data(nixon_tapes_primed, corpus.cols = c("id", "author", "text", 'order'))

desc_dat <- list(
    data = "nixon_tapes",
    genre = 'transcript',
    subgenre = 'political',
    source = "https://www.nixonlibrary.gov/forresearchers/find/tapes/watergate/trial/transcripts.php",
    submitted_by = "Tyler Rinker",
    submitted_on = Sys.Date()
)


add_textcorpus_data(nixon_tapes, desc_dat)
