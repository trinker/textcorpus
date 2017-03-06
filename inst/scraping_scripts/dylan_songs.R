pacman::p_load(rvest, xml2, tidyverse, textshape, textreadr, stringi)

html_front <- 'http://www.lyricsfreak.com/b/bob+dylan/' %>%
    read_html()

links_table <- html_front %>%
    html_nodes(xpath = '//table[@name="song"]') %>%
    html_table(header = TRUE) %>%
    `[[`(1) %>%
    setNames(c('song', 'time', 'stars')) %>%
    mutate(
        song = gsub("^.\\s+|\\s+\\.$", "", song) %>%
            {gsub("\\s+Lyrics$", "", .)}
    ) %>%
    tbl_df()

'http://www.lyricsfreak.com/b/bob+dylan/aint+no+more+cane_20021562.html'
'http://www.lyricsfreak.com/b/bob+dylan//b/bob+dylan/aint+no+more+cane_20021562.html'

links <- data_frame(
    links = html_front %>%
        html_nodes(xpath = '//td[@class="colfirst"]/a') %>%
        html_attr('href') %>%
        {file.path('http://www.lyricsfreak.com', .)},
    song = html_front %>%
        html_nodes(xpath = '//td[@class="colfirst"]/a') %>%
        html_text() %>%
        {gsub("^.\\s+|\\s+\\.$", "", .)} %>%
        {gsub("\\s+Lyrics$", "", .)}
) %>%
    left_join(links_table %>% select(-stars), by = 'song') %>%
    mutate(id = gsub("(^.+?_)(\\d{5,})(\\.html$)", '\\2', links))


dylan_songs <- lapply(links$links, function(x){

    html_dat <- x %>%
        read_html()

    lyrics <- html_dat %>%
        html_nodes(xpath = '//div[@class="dn"]') %>%
        as.character() %>%
        {gsub("(<br/>)+", "splitheresplit", .)} %>%
        textclean::replace_html() %>%
        stringi::stri_split_fixed("splitheresplit") %>%
        unlist() %>%
        trimws()

    data_frame(author = 'Bob Dylan', id = gsub("(^.+?_)(\\d{5,})(\\.html$)", '\\2', x), text = lyrics)

}) %>%
    bind_rows() %>%
    left_join(links %>% select(-links), by = 'id') %>%
    group_by(id) %>%
    mutate(order = seq_along(text)) %>%
    select(id, author, text, order, time) %>%
    mutate(
        text = textclean::replace_non_ascii(text),
        author = textclean::replace_non_ascii(author)
    )
