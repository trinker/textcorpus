pacman::p_load(rvest, xml2, tidyverse, textshape, textreadr, stringi)

html_front <- 'https://bobdylan.com/songs/' %>%
    read_html()

links_table <- data_frame(
    
    links = html_front %>%
        html_nodes(xpath = '//span[@class="song"]/a') %>%
        html_attr('href'),
    song = html_front %>%
        html_nodes(xpath = '//span[@class="song"]/a') %>%
        html_text() %>%
        {gsub("‘", "'", .)},
    release_album = html_front %>%
        html_nodes(xpath = '//span[@class="release"]/a') %>%
        html_text(),
    first_played_date = html_front %>%
        html_nodes(xpath = '//span[@class="played link_date_center"]/a') %>%
        html_text() %>%
        trimws() %>% 
        `[`(c(TRUE, FALSE)) %>%
        as.Date(format = "%b %d, %Y")
    
) 

albumn_page <- 'http://www.allmusic.com/artist/bob-dylan-mn0000066915/discography/all' %>%
    read_html()

albums <- albumn_page %>%
    html_nodes(xpath = '//table') %>%
    html_table(header = TRUE) %>%
    `[[`(1) %>%
    setNames(c('x1', 'x2', 'Year', 'Album', 'Label', 'AllMusicRating', 'UserRatings', 'x3'))  %>%
    select(Year, Album, Label) %>%
    tbl_df() 

missings <- grep("^\\s*$", unique(links_table$release_album[!links_table$release_album %in% albums$Album]), value = TRUE, invert = TRUE)


matches <- data_frame(
    release_album = missings, 
        match = sapply(
            textclean::mgsub(
                missings, 
                c('Vol 4: Bob Dylan Live 1966', 'Volume II', 'Greatest Hits Volume 3'), 
                c('Vol. 4: The \"Royal Albert Hall\" Concert', 'Vol 2', 'Bob Dylan\'s Greatest Hits Vol. 3')),
            
                function(x){
        
                    m <- gofastr::q_dtm(c(x, albums$Album))
                    cos <- clustext::cosine_distance(m)
                    mat <- as.matrix(cos)[-1,1]
                    if (min(mat) > .3) return(NA)
                    unname(albums$Album[which.min(mat)[1]])
                    
                })
)

print(matches, n = Inf)


links_table <- links_table %>%
    left_join(matches, by = 'release_album') %>%
    mutate(release_album = ifelse(is.na(match), release_album, match)) %>%
    select(-match) %>%
    left_join(albums, by = c('release_album' = 'Album')) %>%
    mutate(id = seq_along(song)) %>%
    setNames(tolower(colnames(.)))



len <-length(links_table$links)

dylan_songs_primed <- Map(function(x, y){

cat(paste0(y, ' of ', len, '\n'));flush.console()    
    
    html_dat <- x %>%
        read_html()

    lyrics <- html_dat %>%
        html_nodes(xpath = '//div[@class="article-content lyrics"]') %>%
        as.character() %>%
        {gsub("(<br\\s*/*>)+\r*\n*", "splitheresplit", .)} %>%
        textclean::replace_html() %>%
        stringi::stri_split_fixed("splitheresplit") %>%
        unlist() %>%
        trimws() %>%
        {gsub('\t+.+?Copyright.+$', '', .)}

    data_frame(author = 'Bob Dylan', order = seq_along(lyrics), text = lyrics, links = x)

}, links_table$links, seq_along(links_table$links)) %>%
    bind_rows() %>%
    left_join(links_table, by = 'links') %>%
    select(-links) %>%
    group_by(id) %>%
    select(id, author, text, order, song, year, release_album, label, first_played_date) %>%
    mutate(
        text = textclean::replace_non_ascii(text),
        author = textclean::replace_non_ascii(author),
        song = textclean::replace_non_ascii(song) 
    ) %>%
    filter(!grepl('^\\s*$'))


pacman::p_load_current_gh('trinker/textcorpus')
x <- dylan_songs <- prepare_textcorpus_data(dylan_songs_primed, corpus.cols = c("id", "author", "text", 'order'))

desc_dat <- list(
    data = "dylan_songs",
    genre = 'song',
    subgenre = c("folk", 'rock'),
    source = "https://bobdylan.com/songs/",
    submitted_by = "Tyler Rinker",
    submitted_on = Sys.Date()
)


add_textcorpus_data(dylan_songs, desc_dat)

format_r_file
