# Created by Michael Cahana in early November 2018
# Matches names within a specified dataset
# To be called by master.csv

# ==============================
# Inputs
# ==============================
# df tibble
# output_file string

# ==============================
# Basic setup
# ==============================
root <- getwd()
while(basename(root) != 'name_matching') {
  root <- dirname(root)
}
source(file.path(root, 'data.R'))
source(file.path(root, 'code', 'functions', 'utils.R'))

# ==============================
# Needed libraries
# ==============================
library(tidyverse)
library(fst)
library(stringi)
library(tictoc)
library(stringdist)
library(text2vec)
library(readxl)
library(fuzzyjoin)
library(data.table)

# ==============================
# common words
# ==============================

common_words <- c('PROD', 'INC', 'CORP', 'CORPORATION', 'CO', 'COMPANY', 'LLC', 
    'ENERGY', 'OIL', 'GAS', 'O&G', 'OG', '&', 'OPERATIONS', 'PRODUCTIONS', 
    'ENGY', 'ENGINEERING', 'BOB', 'CONSULTING', 'ROYALTIES', 'EP', 
    'LM', 'CHARLES', 'CRAIG', 'RESERVES', 'ROBERT', 'SCOTT', 'STEVEN', 
    'HOLDING', 'ACQUISITION', 'CONSULTANTS', 'CONSULTANT', 'TRUST',
    'GREG', 'LE', 'DIVERSIFIED', 
    'ENERGY', 'ROYALTY', 'TEXAS', 'PETR', 'TOM', 'RANDY', 'PATRICIA', 'MARK', 
    'SERV', 'MINERAL', 'MIN', 'OPERATING', 'RESOURCES', 'LTD', 'LIMITED', 
    'WELL', 'OPERATOR', 'PRODUCTION', '', ' ', 'AND', 'THE', 'COMPANY', 'USA', 
    'PETROLEUM', 'JR', 'MANAGEMENT', 'MGMT', 'ET', 'AL', 'DRILLING', 'ETAL', 
    'CONSTRUCTION', 'ASSOC', 'PRODUCING', 'SYNDICATE', 'EXPLORATION', 'DRLG', 
    'OF', 'STATE', 'EXPL', 'PETRO', 'LAND', 'DRILLIN', 'INVESTMENTS', 
    'INVESTMENT', 'DEVELOPMENT', 'NATIONAL', 'PROPERTIES', 'SERVICES', 'LP', 
    'PIPELINE', 'MINING', 'DEV', 'GROUP', 'AMERICAN', 'REFINING', 'REFINERY', 
    'VALLEY', 'EL', 'SERVICE', 'PUMPING', 'PARTNERS', 'OILFIELD', 'BROTHERS', 
    'FUNDS', 'PET', 'COAST', 'CRUDE', 'SANDS', 'FAMILY', 'ASSOCIATES', 
    'PARTNERSHIP', 'MINERALS', 'ESTATE', 'US', 'MRS', 'MR', 'BROS', 'LEASE', 
    'SOLUTIONS', 'OPER', 'NATURAL', 'ENTERPRISES', 'ENTERPRISE', 
    'INTERNATIONAL', 'DBA', 'INCORPORATED', 'UNITED', 'STATES', 'SONS', 'SON', 
    'I', 'II', 'III', 'RES', 'CAPITAL', 'FARM', 'FARMS', 'CREEK', 'STREET', 
    'HILL', 'HOLDINGS', 'OP', 'SUPPLY', 'E&P', 'LC', 'RESOURCE', 
    'PRODUCERS', 'OPERATORS', 'UPSTREAM', 'MGMNT', 'INTERESTS', 'COAL', 
    'ROCK', 'RIVER', 'CATTLE', 'LAND', 'VENTURES', 'RIDGE', 'RUN', 'LAKE', 
    'WILLIAM', 'JOHN', 'GEORGE', 'DAVE', 'MICHAEL', 'ROBERT', 
    'RICHARD', 'JAMES', 'DAVID', 'JAMES', 'HENRY', 'JACK', 'STEPHEN', 'THOMAS', 
    'RONALD', 'LARRY', 'DONALD', 'RALPH', 'PIPE', 'FRANK', 'SALES', 'KENNETH', 
    'DON', 'RAY',  'HAROLD', 'DALE', 'MARY')

standalone_words <- c('AMERICA', 'PERMIAN', 'MARCELLUS', 'UTICA', 
    'HAYNESVILLE-BOSSIER', 'HAYNESVILLE', 'BOSSIER', 'BARNETT', 'WOODFORD', 
    'EAGLE', 'FORD', 'FAYETTEVILLE', 'NIOBRARA', 'BAKKEN', 'ANTRIM', 'CENTURY', 
    'WESTERN', 'WEST', 'NORTHERN', 'NORTH', 'SOUTH', 'SOUTHERN', 'EAST', 
    'EASTERN', 'NEW')


# words that signify a name is a company
company <-
  c('ACQUISITION', 'ENERG', 'RESOURCE', 'OG', 'O & G', 'O&G', 'EXPL', '\\sLAND', 
    'OPERAT', 'SERVICES', '\\sOIL', 'PROPERTIES', '\\sEP', 'ASSOC', 'PETR', 
    'PARTNER', 'PROD', 'NATURAL', 'GEOSOUTHERN', 'MIDLAND', 'INTEREST', 'TEXAS',
    'ROYALT', 'BASIN', 'DRLG', 'DRILL', '\\sOIL', '\\sGAS', 'DEVELOPMENT', 
    'HOLDING', 'RIVER', 'PERMIAN', 'CHALFANT', 'MGMT', 'MINERALS', 'CONTINENTAL',
    'SOUTHWESTERN', 'E & P', 'E&P', '\\sGAS', 'GAS\\s', 'OIL\\s', 'CHURCH', 
    'INVEST', 'CAPITAL', 'UNIVERSITY', '\\sLEASING', 'LTD', 'LIMITED', 
    'MINERAL', 'TITLE', 'ENTERPRIS', 'FINANC', 'REFINERY', 'GROUP', 'TRUST',
    'LP', 'L&P', 'L & P', 'CRUDE', 'PROPERTIES', '\\sPROD', 'VALLEY', 'CORP',
    'REFINERY', 'STATES', 'PIPELINE', 'MINING', 'BROKER', 'LONESTAR', '\\sCO$',
    'COMPANY', 'INSTITUTIONAL', 'INTERNATIONAL', 'FUND', 'EAGLE', 'SYSTEMS', 
    '\\sBANK', 'BANK\\s', 'FOUNDATION', 'AMERICA', 'LEASE') %>%
  paste(., collapse = '|') %>%
  regex(., ignore_case = TRUE)


# ==============================
# Helper functions
# ==============================

clean_name  <- function(name, drop_common_words=FALSE) {
    words <- 
        strsplit(name %>% str_replace_all(',', ' '), split=' ')[[1]] %>% 
        str_replace_all('\xc9', 'E') %>%
        str_replace_all('[[:punct:]]', '') %>% 
        toupper() %>% 
        str_trim() %>% 
        str_squish()
    if (drop_common_words) {
        words <- words[!words %in% common_words]
        all_drops <- words[!words %in% standalone_words]
        if (length(all_drops)>0) {
            words <- all_drops
        }
    } else {
        words <- words[!words %in% c('', ' ')]
    }
    # letters <- words[nchar(words)==1] %>% paste(collapse = '-')
    
    letters <- 
      ifelse(nchar(words)>1, '', words) %>% 
      paste(collapse = '-') %>% 
      str_replace_all('--', ' ') %>% 
      strsplit(split=' ') %>% 
      .[[1]] %>% 
      .[!nchar(.)<3] %>% 
      .[str_count(.,'-')<2 | nchar(.)>3]

    # UNDOS BAG OF WORDS
    words <- words[!nchar(words)==1] %>% append(letters) %>% 
        paste(collapse=' ')
    return(words)
}

get_words  <- function(names, drop_common_words = TRUE) {
    count <- 1
    for (name in names){ 
        words <- 
            strsplit(name, split=' ')[[1]] %>% 
            str_replace_all('\xc9', 'E') %>%
            str_replace_all('[[:punct:]]', '') %>% 
            toupper() %>% 
            str_trim() %>% 
            str_squish()
        if (drop_common_words) {
            words <- words[!words %in% common_words]
            all_drops <- words[!words %in% standalone_words]
            if (length(all_drops)>0) {
                words <- all_drops
            }
        }
        letters <- ifelse(nchar(words)>1, '', words) %>% 
            paste(collapse = '-') %>% str_replace_all('--', ' ') %>% 
            strsplit(split=' ') %>% .[[1]] %>% .[!nchar(.)<3] %>% 
            .[str_count(.,'-')<2 | nchar(.)>3]
        # UNDOS BAG OF WORDS
        words <- words[!nchar(words)==1] %>% append(letters) 

        if (count==1) {
            l <- list(words)
        } else {
            l[[count]] <- words
        }
        count <- count + 1
    }
    return(l)
}

matched_df <- function(name, matches, names, num) {
    df <- tibble(name = rep(name, length(matches)), 
        match = names[matches], shared_words = num)
    return (df)
}

get_matches <- function(words, bag, row_comparison = FALSE) {
    strings <- c()
    indices <- c()
    if (class(bag)!='character') {
        bag <- bag[[1]]
    }
    if (identical(bag, character(0))) {
    	return (indices)
    }
    for (i in 1:length(words)) {
        if (sum(bag %in% words[[i]])>0) {
            indices <- c(indices, i)
            if (i!=1 | row_comparison==TRUE) {
                string_indices <- 
                    match(bag, words[[i]])
                string_indices <- string_indices[!is.na(string_indices)]
                string <- paste('\\b', 
                    paste(words[[i]][string_indices], 
                        collapse='\\b|\\b'), 
                    '\\b', sep='')
                strings <- c(strings, string)
            }
        }
    }
    if(!row_comparison) { indices <- indices[!indices==1] }
    return (list(indices, strings))
}


# ==============================
# Human Name functions
# ==============================

clean_name  <- function(name, drop_common_words=FALSE, human = FALSE) {
    words <- 
        strsplit(name %>% str_replace_all(',', ' '), split=' ')[[1]] %>% 
        str_replace_all('\xc9', 'E') %>%
        str_replace_all('[[:punct:]]', '') %>% 
        toupper() %>% 
        str_trim() %>% 
        str_squish()
    if (drop_common_words) {
        words <- words[!words %in% common_words]
        all_drops <- words[!words %in% standalone_words]
        if (length(all_drops)>0) {
            words <- all_drops
        }
    } else {
        words <- words[!words %in% c('', ' ')]
    }
    # letters <- words[nchar(words)==1] %>% paste(collapse = '-')
    
    if(human){
        letters <- 
          ifelse(nchar(words)>1, '', words) %>% 
          paste(collapse = '') %>% 
          str_replace_all('--', ' ') %>% 
          strsplit(split=' ') %>% 
          .[[1]] %>% 
          .[str_count(.,'-')<2 | nchar(.)>3]
      
    } else {
        letters <- 
          ifelse(nchar(words)>1, '', words) %>% 
          paste(collapse = '-') %>% 
          str_replace_all('--', ' ') %>% 
          strsplit(split=' ') %>% 
          .[[1]] %>% 
          .[!nchar(.)<3] %>% 
          .[str_count(.,'-')<2 | nchar(.)>3]
    }
   
      
    # UNDOS BAG OF WORDS
    words <- words[!nchar(words)==1] %>% append(letters) %>% 
        paste(collapse=' ')
    return(words)
}


# use graph theory magic - if a intersects b and b intersects c, we want
# a, b, and c to all be in one group
int_bundles <- function(x, y){
  pairs <- 
    map2(x, y, c) %>% 
    map(as.character)

  pairs_embed <- do.call("rbind", lapply(pairs, embed, 2))
  pairs_vert <- graph.edgelist(pairs_embed, directed = F)
  pairs_id <- split(V(pairs_vert)$name, clusters(pairs_vert)$membership)

  # collapse list of ids in the same group to a dataframe 
  map2_df(pairs_id, seq_along(pairs_id), 
    function(x, y) cbind(x, y) %>% as_tibble %>% setNames(c("name", "id")))
}

# Sources for names dictionaries:
# http://www2.census.gov/topics/genealogy/2010surnames/names.zip
# https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/male.txt
# https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/female.txt
# https://github.com/carltonnorthern/nickname-and-diminutive-names-lookup/blob/master/names.csv

# function to identify last name and extract 
extract_name <- function(names){

    # download census list of surnames
    surnames <- 
      read_csv(file.path(rdir, "human_names", "surnames.csv")) %>%
      mutate_all(toupper) %>%
      mutate(IsLast = TRUE) 

    male_first <- 
      file.path(rdir, "human_names", "male_names.txt") %>%
      read_delim(delim = ' ') %>%
      select(first_name = 1) %>%
      filter(first_name != '#')

    female_first <-
      file.path(rdir, "human_names", "female_names.txt") %>%
      read_delim(delim = ' ') %>%
      select(first_name = 1) %>%
      filter(first_name != '#')

    nicknames <-
      file.path(rdir, "human_names/nicknames.xlsx") %>%
      read_excel(col_names = FALSE) %>%
      rename(std_name = 1) %>%
      gather(key, nickname, -std_name) %>%
      select(-key) %>%
      filter(!is.na(nickname)) %>%
      group_by(nickname) %>%
      filter(row_number() == 1) %>%
      ungroup %>%
      mutate_all(toupper) %>%
      filter(!(nickname %in% .$std_name))

    first_names <- 
      tibble(first_name = c(nicknames$std_name, nicknames$nickname)) %>%
      bind_rows(female_first, male_first)  %>%
      mutate_all(toupper) %>%
      unique %>%
      mutate(first_name = str_to_upper(first_name), 
        IsFirst = TRUE)

    names <- tibble(name = names)

    # split name and identify which parts are a first name and which are last
    split_name <-
      names %>%
      mutate(company = str_detect(name, company)) %>%
      mutate(name = str_to_upper(name), 
        name_split = str_split(name, ' ')) %>%
      unnest(name_split) %>%
      left_join(select(surnames, name_split = name, IsLast)) %>%
      left_join(select(first_names, name_split = first_name, IsFirst)) %>%
      replace_na(list(IsLast = FALSE, IsFirst = FALSE)) %>%
      group_by(name, IsLast, IsFirst) %>%
      arrange(name_split) %>%
      filter(row_number() == 1) %>%
      ungroup 

    # split up name based on what is categorized as
    split_name <- 
      split_name %>%
      mutate(Rank = case_when(
        IsLast & !IsFirst ~ 1, 
        IsLast & IsFirst ~ 2, 
        !IsLast & IsFirst ~ 3, 
        TRUE ~ 4)) %>%
      mutate(last_name = case_when(
            Rank == 1 ~ name_split, 
            Rank == 2 ~ name_split, 
            Rank == 3 ~ str_replace(name, name_split, ''), 
            TRUE ~ NA_character_), 
        first_name = case_when(
            Rank == 1 ~ str_replace(name, name_split, ''), 
            Rank == 2 ~ str_replace(name, name_split, ''), 
            Rank == 4 ~ name_split, 
            TRUE ~ NA_character_)) %>%
      arrange(name, Rank) %>%
      group_by(name) %>%
      filter(row_number() == 1) %>%
      ungroup

    # join with nicknames dataset to convert nicknames to first names - JK THIS IS TERRIBLE
    split_name <-
      split_name %>%
      select(-IsFirst, -IsLast) %>%
      mutate(first_name = str_replace_all(first_name, '-', ' ')) %>%
      mutate_if(is.character, funs(str_squish(str_trim(.)))) #%>%
      #left_join(rename(nicknames, first_name = nickname)) %>%
     # mutate(first_name = if_else(!is.na(std_name), std_name, first_name)) %>%
     # select(-std_name)

    # extract initials and store as a list column
    split_name <-
      split_name %>%
      mutate(initials = str_split(first_name, ' ')) %>%
      group_by(name) %>%
      mutate(initials = list(str_extract(pluck(initials, 1), '.{1}')), 
        initials = if_else(nchar(first_name) <= 2, list(first_name), initials)) %>%
      mutate(first_name = str_replace(first_name, '\\sJR$', ''), 
        last_name = str_replace(last_name, '\\sJR$', '')) %>%
      ungroup %>%
      right_join(names)

    return(split_name)
}

match_first_name <- function(df, jthreshold, cthreshold){
    clean_names <- 
      df %>% 
      select(name) %>%
      distinct() %>% 
      rowwise() %>% 
      mutate(name = clean_name(name, human = TRUE)) %>% 
      pull()

    split_names <- 
      extract_name(clean_names) %>%
      rename(clean_name = name) %>%
      bind_cols(name = df$name, .) %>%
      filter(!company) 

    # create pairs of names we want to check based on if they have the same
      # last name
      # Check if initials are a match if the first name has more than one word
      # or if it has less than 4 characters 
      # (ie we don't want to match john smith to jake smith)
    human_names <- 
      filter(split_names, !is.na(last_name)) %>%
      mutate(id = row_number()) %>%
      group_by(id) %>%
      mutate(check_initials = length(str_split(first_name, ' ')[[1]]) > 1 | 
        nchar(first_name) < 4) %>%
      ungroup 

    human_names1 <-
      human_names %>%
      setNames(paste0(colnames(.), '1')) %>%
      rename(last_name = last_name1)
   
   # full_join human names with itself
      # check if bag of words containing first initial of each word in the first
      # name matches
      # then do string dist on first names
    human_names_match <-
      human_names %>%
      full_join(human_names1) %>%
      filter(id < id1) %>%
      mutate(initials_match = map2_lgl(initials, initials1, setequal), 
        initials_match = if_else(check_initials | check_initials1, 
            initials_match, FALSE)) 

    matches <-
      human_names_match %>%
      mutate(human_jw_distance = stringdist(first_name, first_name1, 
        method = "jw", p = 0), 
      human_cosine_similarity = stringdist(first_name, first_name1, 
        method = "cosine")) %>%
      select(name, match = name1, human_jw_distance, human_cosine_similarity, 
        initials_match) %>%
      filter(human_jw_distance < jthreshold | 
        human_cosine_similarity < cthreshold | initials_match)

    return(matches)
}


# ==============================
# String Distance functions
# ==============================
# match names using stringdist methods (default Jaro-Winkler)
# matches are only those with scores that are at or below the threshold
match_names_stringdist <- function(names, clean_names, 
    method='jw', threshold=0.25) {

    df <- data.table()

    while (length(clean_names)>1) {

        c_name <- clean_names[1]
        f_name <- names[1]

        clean_names <- clean_names[-1]
        names <- names[-1]

        if (c_name=='') {
            clean_names <- clean_names[-1]
            names <- names[-1]
            next
        }

        name_table <- as.data.table(cbind(cname = c_name, cmatch = clean_names, name = f_name, match = names))
        name_table <- name_table[, jw_distance := stringdist(cname, cmatch, method = method, p = 0)]
        name_table <- name_table[jw_distance <= threshold]

        if(nrow(name_table)>0) {
            df <- rbindlist(list(df, name_table))
        }

    }

    df <- 
        as_tibble(df) %>%
        select(name, match, jw_distance)

    return(df)
}


# match names if they share a word
match_names_shared_word <- function(names, ...) {
    count <- 0
    bad_names <- c()
    words <- get_words(names, ...)
    while (length(words)>0){
        bag <- words[[1]]
        name <- names[1]
        matches <- get_matches(words, bag)
        shared_words <- as.vector(sapply(matches[[2]], str_count, '\\|'))
        matches <- matches[[1]]
        if(length(matches)>0) {
            shared_words <- shared_words + 1
            if (count==0) {
                matches_df <- matched_df(name, matches, names, shared_words)
            } else {
                matches_df <- 
                    matches_df %>% 
                    bind_rows(matched_df(name, matches, names, shared_words))
            }
            count <- count + 1
            words <- words[-1]
            names <- names[-1]
        } else {
            bad_names <- c(bad_names, names[1])
            words <- words[-1]
            names <- names[-1]
        }
    }
    return (list(matches_df, bad_names))
}

# match names using cosine similarity scores, matches only being those that
# are at or exceed the given threshold
match_names_cosine <- function(names, similarity_matrix, threshold=0.4) {
    count <- 0
    for (i in 1:dim(names)[1]) {
        name <- names %>% 
            slice(i) %>% 
            select(name) %>% 
            pull()
        matches <- c()
        cosine_sims <- c()
        indices <- order(similarity_matrix[i,], decreasing=TRUE)[2:11]
        for (index in indices) {
            match <- 
                names %>%
                slice(index) %>% 
                select(name) %>% 
                pull()
            cosine_sim <- similarity_matrix[i, index]

            matches <- c(matches, match)
            indices <- c(indices, index)
            cosine_sims <- c(cosine_sims, cosine_sim)
        }
        if (count==0){
            df <- 
                tibble(name = rep(name,10), match = matches,
                cosine_similarity = cosine_sims)
        } else {
            df <- 
                df %>% 
                bind_rows(tibble(name = rep(name,10), match = matches,
                cosine_similarity = cosine_sims))
        }
        count <- count + 1
    }
    df <- 
        df %>% 
        filter(cosine_similarity>=threshold)
    return (df)
}


# determine inverse-document frequency of word in list of names
idf <- function(names, word) {
    regex <- paste('\\b', word, '\\b', sep='')
    match_count <- 
        names %>% 
        mutate(name = str_replace_all(name, '[[:punct:]]', '')) %>% 
        filter(str_detect(name, regex)) %>% 
        dim() %>% 
        .[1]
    total_count <- dim(names)[1]
    return (log(total_count/match_count))
}

# extract maximum idf score
extract_idf <- function(word, idfs) {
    idf <- 
        idfs %>% 
        filter(str_detect(name, word)) %>% 
        pull(idf) %>% 
        max()
    return (idf)
}

match_names <- function(df, output_file, cosine_threshold =  0.4, 
    jaro_threshold = 0.15, human_cosine_threshold = .6, 
    human_jaro_threshold = .6, write_csv = TRUE) {


    #=================================
    # ----------- matching -----------
    #=================================

    df <- 
        df %>% 
        mutate(name = str_replace_all(name, '\xc9', 'E'))

    #===========
    # human first name distance
    #===========
    print("first names distance")
    tic()
    name_map_human <- match_first_name(df, human_jaro_threshold, human_cosine_threshold)
    toc()

    #===========
    # bag of words, tf-idf, cosine similarity
    #===========

    names <- 
        df %>% 
        group_by(name) %>% 
        summarize(n=n()) %>% 
        arrange(desc(n)) %>%
        filter(!is.na(name)) %>% 
        select(-n) %>%
        rowwise() %>% 
        mutate(clean_name = clean_name(name, drop_common_words=T))  

    it <- itoken(names$clean_name, progressbar = FALSE)
    v <- create_vocabulary(it) %>% prune_vocabulary()
    vectorizer = vocab_vectorizer(v)

    dtm <- create_dtm(it, vectorizer)
    tfidf <- TfIdf$new()
    dtm_tfidf <- fit_transform(dtm, tfidf)

    similarity_matrix <- sim2(x = dtm_tfidf, method = 'cosine', norm = 'l2')

    print('cosine similarity')
    tic()
    name_map_cosine_similarity <- match_names_cosine(names, similarity_matrix, 
        threshold = cosine_threshold)
    toc()

    #===========
    # shared word
    #===========

    names <- 
        df %>% 
        select(name) %>%
        filter(!is.na(name)) %>% 
        distinct() %>% 
        pull()

    print('shared word')
    tic()
    name_map_shared_word <- 
        match_names_shared_word(names) %>% .[[1]]
    toc()

    #===========
    # jaro distance
    #===========

    clean_names <- 
        df %>% 
        select(name) %>%
        filter(!is.na(name)) %>% 
        distinct() %>% 
        rowwise() %>% 
        mutate(name = clean_name(name, 
            drop_common_words=T)) %>% 
        pull()

    print('jaro distance')
    tic()
    name_map_jaro <- match_names_stringdist(names, clean_names, 
        threshold = jaro_threshold)
    toc()


    #===========
    # combine
    #=========== 
    name_map_human <-
      name_map_human %>%
      rowwise() %>%
      mutate(a1 = alpha_order(name, match, 1)) %>% 
      mutate(a2 = alpha_order(name,  match, 2)) %>% 
      select(-name, -match) %>%
      rename(name = a1, match = a2) %>% 
      unique()      

    name_map_cosine_similarity <- 
        name_map_cosine_similarity %>% 
        rowwise() %>% 
        mutate(a1 = alpha_order(name, match, 1)) %>% 
        mutate(a2 = alpha_order(name,  match, 2)) %>% 
        select(a1, a2, cosine_similarity) %>% 
        rename(name = a1, match = a2) %>% 
        unique()

    name_map_shared_word <- 
        name_map_shared_word %>% 
        rowwise() %>% 
        mutate(a1 = alpha_order(name, match, 1)) %>% 
        mutate(a2 = alpha_order(name,  match, 2)) %>% 
        select(a1, a2, shared_words) %>% 
        rename(name = a1, match = a2) %>% 
        unique()

    name_map_jaro <- 
        name_map_jaro %>% 
        rowwise() %>% 
        mutate(a1 = alpha_order(name, match, 1)) %>% 
        mutate(a2 = alpha_order(name,  match, 2)) %>% 
        select(a1, a2, jw_distance) %>% 
        rename(name = a1, match = a2) %>% 
        unique()

    master <- 
        name_map_cosine_similarity %>% 
        full_join(name_map_shared_word, by = c('name', 'match')) %>%
        full_join(name_map_jaro, by = c('name', 'match')) %>% 
        full_join(name_map_human, by = c('name', 'match')) %>%
        filter(name!=match) %>%
        ungroup

    #===========
    # save output or return dataframe
    #===========
    if (write_csv) {
        write_csv(master, output_file)
    } else {
        return (master)
    }
}
