# Created by Michael Cahana in early November 2018
# Matches names within a specified dataset
# To be called by master.csv

#===========
# inputs: 
#===========
# df tibble
# output_file string

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# needed libraries
#===========
library(tidyverse)
library(fst)
library(stringi)
library(tictoc)
library(stringdist)
library(text2vec)
library(readxl)
library(fuzzyjoin)

#===========
# common words
#===========

common_words <- c('PROD', 'INC', 'CORP', 'CORPORATION', 'CO', 'COMPANY', 'LLC', 
    'ENERGY', 'OIL', 'GAS', 'O&G', 'OG', '&', 'OPERATIONS', 'PRODUCTIONS', 
    'ENGY', 'ENGINEERING', 'BOB', 'CONSULTING', 'NORTH', 'ROYALTIES', 'EP', 
    'LM', 'CHARLES', 'CRAIG', 'RESERVES', 'ROBERT', 'SCOTT', 'STEVEN', 
    'HOLDING', 
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

#===========
# functions
#===========

clean_name  <- function(name, drop_common_words=FALSE) {
    words <- 
        strsplit(name %>% str_replace_all(',', ' '), split=' ')[[1]] %>% 
        str_replace_all('[[:punct:]]', '') %>% 
        toupper() %>% 
        str_trim() %>% 
        str_squish()
    if (drop_common_words) {
        words <- words[!words %in% common_words]
    } else {
        words <- words[!words %in% c('', ' ')]
    }
    # letters <- words[nchar(words)==1] %>% paste(collapse = '-')
    letters <- ifelse(nchar(words)>1, '', words) %>% 
        paste(collapse = '-') %>% str_replace_all('--', ' ') %>% 
        strsplit(split=' ') %>% .[[1]] %>% .[!nchar(.)<3] %>% 
        .[str_count(.,"-")<2 | nchar(.)>3]
    # UNDOS BAG OF WORDS
    words <- words[!nchar(words)==1] %>% append(letters) %>% 
        paste(collapse=' ')
    return(words)
}

get_words  <- function(names) {
    count <- 1
    for (name in names){ 
        words <- 
            strsplit(name, split=' ')[[1]] %>% 
            str_replace_all('[[:punct:]]', '') %>% 
            toupper() %>% 
            str_trim() %>% 
            str_squish()
        words <- words[!words %in% common_words]

        letters <- ifelse(nchar(words)>1, '', words) %>% 
            paste(collapse = '-') %>% str_replace_all('--', ' ') %>% 
            strsplit(split=' ') %>% .[[1]] %>% .[!nchar(.)<3] %>% 
            .[str_count(.,"-")<2 | nchar(.)>3]
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

matched_df <- function(name, matches, names) {
    df <- tibble(name = rep(name, length(matches)), 
        match = names[matches])
    return (df)
}

get_matches <- function(words, bag) {
    indices <- c()
    if (identical(bag, character(0))) {
    	return (indices)
    }
    for (i in 1:length(words)) {
        if (sum(bag %in% words[[i]])>0) {
            indices <- c(indices, i)
        }
    }
    indices <- indices[!indices==1]
    return (indices)
}

match_names_stringdist <- function(names, clean_names, 
    method='jw', threshold=0.25) {

    count <- 0
    while (length(clean_names)>0) {

        c_name <- clean_names[1]
        f_name <- names[1]

        if (c_name=='') {
            clean_names <- clean_names[-1]
            names <- names[-1]
            next
        }

        matrix <- stringdistmatrix(c_name, 
            clean_names, method=method, p=0)

        match_indices <- c()
        match_names <- c()
        dist_scores <- c()

        for (i in 1:length(matrix)) {
            if (matrix[i]<=threshold & i>1) {
                match_indices <- c(match_indices,i)
                match_names <- c(match_names, names[i])
                dist_scores <- c(dist_scores, matrix[i])
            } 
        }
        if(length(match_indices)>0) {
            if (count==0){
                df <- tibble(name = rep(f_name, length(match_indices)), 
                    match = match_names, 
                    dist_score =  dist_scores)
            } else {
                new <- tibble(name = rep(f_name, length(match_indices)),
                    match = match_names, 
                    dist_score =  dist_scores)
                df <- 
                    df %>% 
                    bind_rows(new)
            }
            count <- count + 1
        }

        clean_names <- clean_names[-1]
        names <- names[-1]
    }
    return(df)
}

match_names_shared_word <- function(names) {
    count <- 0
    bad_names <- c()
    words <- get_words(names)
    while (length(words)>0){
        bag <- words[[1]]
        name <- names[1]
        matches <- get_matches(words, bag)
        if(length(matches)>0) {
            if (count==0) {
                matches_df <- matched_df(name, matches, names)
            } else {
                matches_df <- 
                    matches_df %>% 
                    bind_rows(matched_df(name, matches, names)) 
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

alpha_order <- function(name, match, order) {
    vec <- c(name, match)
    a1 <- sort(vec)[order]
    return(a1)
}

match_names <- function(df, output_file) {

    #=================================
    # ----------- matching -----------
    #=================================

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

    it = itoken(names$clean_name, progressbar = FALSE)
    v = create_vocabulary(it) %>% prune_vocabulary()
    vectorizer = vocab_vectorizer(v)

    dtm = create_dtm(it, vectorizer)
    tfidf = TfIdf$new()
    dtm_tfidf = fit_transform(dtm, tfidf)

    similarity_matrix = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")

    print('cosine similarity')
    tic()
    name_map_cosine_similarity <- match_names_cosine(names, similarity_matrix, 
        threshold=0.3)
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
    name_map_jaro <- match_names_stringdist(names, clean_names, threshold = 0.1)
    toc()

    #===========
    # combine
    #=========== 

    name_map_cosine_similarity <- 
        name_map_cosine_similarity %>% 
        mutate(method = 'tf-idf cosine') %>% 
        rename(score = cosine_similarity)

    name_map_shared_word <- 
        name_map_shared_word %>% 
        mutate(method = 'shared word') %>% 
        mutate(score = NA)

    name_map_jaro <- 
        name_map_jaro %>% 
        mutate(method = 'jaro') %>% 
        rename(score = dist_score)

    master <- 
        name_map_cosine_similarity %>% 
        bind_rows(name_map_shared_word) %>%
        bind_rows(name_map_jaro) %>% 
        rowwise() %>% 
        mutate(a1 = alpha_order(name, match, 1)) %>% 
        mutate(a2 = alpha_order(name,  match, 2)) %>% 
        select(a1, a2, method, score) %>% 
        rename(name = a1, match = a2) %>% 
        group_by(name, match) %>% 
        filter(row_number()==1) %>%
        arrange(name) %>% 
        filter(name!=match)

    #===========
    # save output
    #===========

    write_csv(master, output_file)
}
