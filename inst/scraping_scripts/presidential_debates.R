pacman::p_load(rvest, xml2, tidyverse, textshape, textreadr, stringi, zipcode)
data(zipcode)


links <- 'http://www.presidency.ucsb.edu/debates.php' %>%
    read_html() %>%
    html_nodes(xpath = '//td[@class="doctext"]//a') %>%
    html_attr("href")


# lapply(presidential_debates, function(x) table(x$author) %>% sort())
# sapply(presidential_debates, nrow)

presidential_debates_primed <- lapply(links, function(x){

# x <- links[44]

    id <- gsub("(^.+=)(\\d+$)", '\\2', x)
    cat(x, "\n"); flush.console()

    html <- x %>%
        read_html()

    txt <- as.character(html_nodes(html, xpath = '//span[@class="displaytext"]'))


    if (stringi::stri_count_regex(txt, "<b>") > 20){

        text_prime <- txt %>%
            stringi::stri_split_regex('<b>(?=[A-Za-z\', ]+(</b>)?[.:])')

        text <- text_prime%>%
            unlist() %>%
            textclean::replace_html() %>%
            trimws() %>%
            stringi::stri_replace_all_regex('U\\.\\s?S\\.', 'United States') %>%
            stringi::stri_replace_all_regex(' [A-PR-S]\\.', ' ') %>%
            textclean::mgsub(c('MALE VOICE:', 'RONALD WILSON REAGAN, PRESIDENT OF THE UNITED STATES',
                  'N. REAGAN', 'R. REAGAN', 'AUDIENCE MEMBER:', 'Number two:', 'Number three:', 'MR.', 'SEN.', 'Sen.', "Audience Members:", 'SEN./MR.', "Gen. Chuck Yeager (USAF, Ret.)", "(singing)", "Florida Republican Party Chairman ", "SEN./REP./MR.", "MR.", "\n    Duncan Hunter: ", " (?)", "Representative Tom Tancredo (R-Col.)", "Representative Ron Paul (R-Texas)",
                  "Former Virginia Governor James Gilmore (R-Va.)", "Former Arkansas Governor ", "Senator Sam Brownback (R-Kan.)",
                  "Former Massachusetts Governor ", "Representative Duncan Hunter (R-Calif.):", "Former Wisconsin Governor ",
                  "Senator John McCain (R-Ariz.):", "Former New York City Mayor"),
                c('Male:', 'Ronald_REAGAN', 'Nancy_REAGAN', 'Ronald_REAGAN', 'AUDIENCE_MEMBER:', 'Number twotemporaryundolater', 'Number threetemporaryundolater', '', '', '', "Audience_Members:", '', "Chuck Yeager", "", "", "", "",
                  "<<split>>Duncan Hunter<split>", '', "Tom Tancredo", "Ron Paul", "James Gilmore", "", "Sam Brownback", "",
                  "Duncan Hunter:", "", "John McCain", ""
                  ), fixed = TRUE)  %>%
            textclean::mgsub(c("M[Rr].", "Audience Member:", ", CNN .+?:",
                  "\\(R.+:", "GARY BAUER :  Hi, Candy\\."),
                c("Mister",
                  "Audience_Member:", ":", ":", "<<split>>GARY BAUER<split>  Hi, Candy."), fixed = FALSE) %>%
            trimws() %>%
            stringi::stri_replace_all_regex('(\\(.+)(\\.)(\\))', '$1$3') %>%
            stringi::stri_replace_all_regex('(((, CNN)|(\\(R)).+?)|(, PRINCETON UNIVERSITY):', ":") %>%
            textclean::mgsub(c('J. KING', 'S. KING'), c('John_King', 'Steve_King')) %>%
            stringi::stri_replace_all_regex('(^M[Rr]?[Ss]?|^S[Ee][Nn]|^R[eE][Pp]|^P[Rr][Oo][Ff]|^G[Oo][Vv]|^R[eE][Vv]|^G[Ee][Nn]|D[Rr])(\\.)', '$1') %>%
            stringi::stri_replace_all_regex("(^[^.: ]{1,11}\\s?[^.: ]{0,11})(:|\\.)(?!$)", "$1<split>" ) %>%
            trimws() %>%
            paste(collapse = "<<split>>") %>%
            stringi::stri_split_regex("<<split>>") %>%
            unlist() %>%
            trimws() %>%
            {grep("^\\s*$", ., value=TRUE, invert=TRUE)} %>%
            stringi::stri_replace_all_regex("\n", " ") %>%
            data_frame(x=.) %>%
            extract(x, c("author", "text"), "(^.+?)<split>(.+$)") %>%
            mutate_all(funs(trimws)) %>%
            filter(!grepl("^(PARTICIPANTS|MODERATORS|candidates)$", author, ignore.case = TRUE)) %>%
            mutate(
                author = gsub("^.+\\s+", "", author),
                author = gsub("(^.)(.+$)", "\\U\\1\\L\\2", author, perl = TRUE),
                author = gsub("(_|')(.)", "\\1\\U\\2", author, perl = TRUE),
                author = gsub("Mccain", "McCain", author)
            )  %>%
            mutate(text = gsub('temporaryundolater', '.', text)) %>%
            filter(!is.na(author)) #%>% peek(Inf)
#  table(text$author)

    } else {

        text <- txt %>%
            stringi::stri_split_regex('</p><p>') %>%
            unlist() %>%
            textclean::replace_html() %>%
            trimws() %>%
            stringi::stri_replace_all_regex('U\\.\\s?S\\.', 'United States') %>%
            stringi::stri_replace_all_regex(' [A-PR-S]\\.', ' ') %>%
            textclean::mgsub(c("MALE VOICE:", "The President's.", 'Here\'s another.', "Qualifications.", "Now, leadership.", "Nuclear power:", "MR.", "THE. ", "Thank you.", "[moderated by Gwen Ifill of PBS]   ", "working.", "The rules:", "reminder:", "flip-flops:", "This side.", "Health care: ", "You bet. Senator Dole.",
                  "Thank you.", "Next thing.", 'It\'s fascinating.', 'Audience members.', 'Mr.', 'Step one:', 'St. Louis',
                  'Mr. President.', 'Now, Mr. President', 'Mr. Nitze', 'VICE PRESIDENT',  'And Mr. Vice President -- Dan', 'REP JOHN B. ',
                  'JANE BRYANT QUINN', ', EDITORIAL WRITER, THE NEW YORK TIMES  :', ', CBS NEWS/ NEWSWEEK  / WASHINGTON POST  :', ', STAFF WRITER, THE LOS ANGELES TIMES  - WASHINGTON BUREAU:',
                  ', MILITARY CORRESPONDENT, THE SUN  , BALTIMORE:', ', SYNDICATED COLUMNIST:', 'GOV RONALD REAGAN', 'CAROL LOOMIS, BOARD OF EDITORS, FORTUNE MAGAZINE  :',
                  'REP JOHN B. ANDERSON', 'RUTH J. HINERFELD, CHAIR, LEAGUE OF WOMEN VOTERS EDUCATION FUND',
                  'HOWARD K. SMITH, MODERATOR', 'MR NIXON;', "Bob L. Schieffer.", 'Republican Presidential Nominee W. Mitt Romney.', "DOROTHY S. RIDINGS:"),
                c('Male:', "The President's", 'Here\'s anothertemporaryundolater', "Qualificationstemporaryundolater", "Now, leadershiptemporaryundolater", "Nuclear powertemporaryundolater", "MISTER", "", "Thank youtemporaryundolater", "", "workingtemporaryundolater", "The rulestemporaryundolater", "remindertemporaryundolater", "flip-flopstemporaryundolater", "This sidetemporaryundolater", "Health caretemporaryundolater", "The President<split>You bet. Senator Dole<<split>>", "Thank youtemporaryundolater", "Next thingtemporaryundolater", 'It\'s fascinatingtemporaryundolater', 'Audience memberstemporaryundolater', 'Mister', 'Step one,', 'Saint Louis', 'Mister President', 'Now, Mister President', 'Mister Nitze', '', 'And Mister Vice President -- Dan', '', "QUINN", ":", ":", ":", ":", ':', 'REAGAN', 'LOOMIS:',
                  'ANDERSON', 'HINERFELD', 'HOWARD SMITH', 'MR NIXON:', "Bob Schieffer.", 'Mitt Romney.', "DOROTHY RIDINGS:")) %>%
            stringi::stri_replace_all_regex('([Nn]umber [^:]+)(:)', '$1') %>%
            stringi::stri_replace_all_regex('(\\(.+)(\\.)(\\))', '$1$3') %>%
            stringi::stri_replace_all_regex('(^M[Rr]?[Ss]?|^S[Ee][Nn]|R[eE][Pp]|P[Rr][Oo][Ff]|G[Oo][Vv]|R[eE][Vv]|G[Ee][Nn]|D[Rr])(\\.)', '$1') %>%
            stringi::stri_replace_all_regex("(^[^.: ]{1,11}\\s?[^.: ]{0,11})(:|\\.)(?!$)", "$1<split>" ) %>%
            {ifelse(grepl('<split>', .) & seq_along(.) != 1, paste0('<<split>>', .), .)} %>%
            {gsub('<<split>>Barbara<split> ', 'Barvara ', .)} %>%
            {gsub("<<split>>You bet<split>  Senator Dole\\.", "You bet.<<split>>Senator Dole<split>", .)} %>%
            paste(collapse ="") %>%
            stringi::stri_split_regex('<<split>>') %>%
            unlist() %>%
            stringi::stri_replace_all_regex("\n", " ") %>%
            data_frame(x=.) %>%
            extract(x, c("author", "text"), "(^.+)<split>(.+$)") %>%
            mutate_all(funs(trimws)) %>%
            filter(!grepl("PARTICIPANTS|MODERATORS", author)) %>%
            mutate(
                author = gsub("^.+\\s+", "", author),
                author = gsub("(^.)(.+$)", "\\U\\1\\L\\2", author, perl = TRUE),
                author = gsub("(_|')(.)", "\\1\\U\\2", author, perl = TRUE)
            ) %>%
            mutate(text = gsub('temporaryundolater', '.', text)) %>%
            filter(!is.na(author)) #%>% peek(Inf)
#  substring(text, 1, 100)
#  which(text$author == 'N')
#  text$text[83]
#  table(text$author)

    }

    text <- text %>%
        filter(!author %in% c("Participants", "Moderators", "Panelists")) %>%
        mutate(author = textclean::mgsub(author, c("Sm1th", "Cater", "Giulaini", "Mcelveen"), c("Smith", "Carter", "Giuliani", "McElveen")))

    meta <- bind_cols(
        html %>%
            html_nodes(xpath = '//span[@class="paperstitle"]') %>%
            html_text() %>%
            stringi::stri_replace_all_regex("^[^A-Za-z]+", "") %>%
            stringi::stri_replace_first_fixed(' from ', ' in ') %>%
            {ifelse(grepl("\\bat\\b", .), ., gsub('\\bin\\b', 'at - in', .))} %>%
            {ifelse(grepl("\\bat\\b", .), ., gsub('\\bon.+$', 'at - in -', .))} %>%
            {ifelse(!grepl("\\bat\\b", .), paste(., 'at - in -'), .)} %>%
            {ifelse(grepl("^(?!.*(\\bin\\b))(?=.*(\\bat\\b))", ., perl=TRUE), paste(., 'in -'), .)} %>%
            stringi::stri_replace_first_regex('at( the)? ', '<<<>>>')  %>%
            stringi::stri_replace_last_regex(', [A-Za-z ]+$', '') %>%
            data_frame(x=.) %>%
            extract(x, c("type", "institution", "city"), "(^.+)\\s*<<<>>>\\s*(.+?)\\s+in\\s+(.+$)")  %>%
            mutate_all(funs(trimws)) %>%
            mutate(city = ifelse(city == "St. Louis", "Saint Louis", city)),
        html %>%
            html_nodes(xpath = '//span[@class="docdate"]') %>%
            html_text()  %>%
            data_frame(date=.) %>%
            mutate(date = as.Date(date, format = "%B %d, %Y"))
    ) %>%
        mutate(
            institution = ifelse(institution == "-", as.character(NA), institution),
            city = ifelse(city == "-", as.character(NA), city)
        )

    meta$id <- id

    bind_cols(meta[rep(1, nrow(text)), ], text) %>%
        filter(!is.na(text))
}) %>%
    bind_rows() %>%
    left_join(zipcode %>% select(-zip) %>% group_by(city, state) %>% slice(1), by = "city") %>%
    rename(state_abb = state) %>%
    left_join(data_frame(state_abb = state.abb, state = state.name), by = 'state_abb') %>%
    group_by(id) %>%
    mutate(order = seq_along(text)) %>%
    select(id, author, text, order, type, institution, city, date, state, state_abb, latitude, longitude) %>%
    mutate(
        text = textclean::replace_non_ascii(text),
        author = textclean::replace_non_ascii(author)
    )

pacman::p_load_current_gh('trinker/textcorpus')
presidential_debates <- prepare_textcorpus_data(presidential_debates_primed)


desc_dat <- list(
    data = "presidential_debates",
    genre = "debate",
    subgenre = "political",
    source = "http://www.presidency.ucsb.edu",
    submitted_by = "Tyler Rinker",
    submitted_on = Sys.Date()
)


