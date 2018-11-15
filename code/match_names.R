# Created by Michael Cahana in early November 2018
# Matches operator names within our modeled data

#===========
# inputs
# pden_desc-2017-04-30.fst
# modeled_prices.Rds
#===========

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

#===========
# data read in
#===========

desc <- read_fst(file.path(rdir, 'pden_desc-2017-04-30.fst')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        stri_pad_right(14, 0)) %>% 
        select(api_no, curr_oper_id, curr_oper_no, curr_oper_name)

modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) %>% 
    select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
    inner_join(desc, by='api_no')

cleaned_300 <- read_excel(file.path(rdir, 'names_edited.xlsx')) %>% 
    select(curr_oper_name, replacement)

#===========
# operator name selection
#===========

modeled <- 
    modeled %>% 
    left_join(cleaned_300, by='curr_oper_name') %>% 
    mutate(replacement = 
        if_else(is.na(replacement), curr_oper_name, replacement)) %>% 
    select(-curr_oper_name) %>% 
    rename(curr_oper_name = replacement)

#===========
# common words
#===========

common_words <- c('PROD', 'INC', 'CORP', 'CORPORATION', 'CO', 'COMPANY', 'LLC', 
    'ENERGY', 'OIL', 'GAS', '&', 'OPERATIONS', 'PRODUCTIONS', 'ENGY', 'ENERGY', 
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
        str_replace_all('\\.', '') %>%
        str_replace_all('-', ' ') %>%
        str_replace_all("'", '') %>%
        str_replace_all("\\)", '') %>%
        str_replace_all("\\(", '') %>%
        toupper()
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
            str_replace(',', '') %>% 
            str_replace_all('\\.', '') %>%
            str_replace_all('-', '') %>%
            str_replace_all("'", '') %>%
            str_replace_all("\\)", '') %>%
            str_replace_all("\\(", '') %>%
            toupper()
        words <- words[!words %in% common_words]
        # letters <- words[nchar(words)==1] %>% paste(collapse = '-')
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

review_df <- function(df) {
	df <- 
		df %>% 
		mutate(id = row_number()) %>% 
		mutate(response = 1)

	ns <- 0
	ys <- 0
	ms <- 0
	for (i in 1:dim(df)[1]) {
		print(paste('name', df %>% slice(i) %>% select(name), sep=': '))
		print(paste('replacement', 
			df %>% slice(i) %>% select(replacement), sep=': '))
		user_response <- 
			readline(prompt=paste('Does the replacement look reasonable?', 
				'Enter 1 if yes, 0 if no, 2 if unsure: ', sep=' ')) %>% 
			as.numeric()
		while (!(user_response %in% c(0,1,2))) {
			user_response <- 
			readline(prompt=paste('Does the replacement look reasonable?', 
				'Enter 1 if yes, 0 if no, 2 if unsure: ', sep=' ')) %>% 
			as.numeric()
		}
		df <- 
			df %>% 
			mutate(response = if_else(id==i, user_response, response))
		if (user_response==0) {
			# df <- 
			# 	df %>% 
			# 	mutate(replacement = if_else(id==i, 'NA', replacement))
			ns <- ns + 1
		} else if (user_response==1) {
			ys <- ys + 1
		} else {
			ms <- ms + 1
		}
		if (i%%10==0) {
			cat('\n')
			print(paste(dim(df)[1]-i, 'rows left', sep=' ')) 
			cat('\n')
		}
	}
	cat('**********\n')
	print(paste(ys, 'correct replacements,', ns, 
		'flagged as unreasonable,', ms, 'flagged as unsure', sep=' '))
	cat('**********\n')
	return (df)
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

match_names_shared_word <- function(names, clean_names) {
    count <- 0
    bad_names <- c()
    words <- get_words(names)
    while (length(words)>0){
        bag <- words[[1]]
        name <- names[1]
        clean_name <- clean_names[1]
        matches <- get_matches(words, bag)
        if(length(matches)>0) {
            if (count==0) {
                df <- matched_df(name, matches, names)
            } else {
                df <- 
                    df %>% 
                    bind_rows(matched_df(name, matches, names)) 
            }
            count <- count + 1
            words <- words[-1]
            names <- names[-1]
            clean_names <- clean_names[-1]
        } else {
            bad_names <- c(bad_names, names[1])
            words <- words[-1]
            names <- names[-1]
            clean_names <- clean_names[-1]
        }
    }
    return (list(df, bad_names))
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

#=================================
# ----------- matching -----------
#=================================

#===========
# bag of words, tf-idf, cosine similarity
#===========

names <- 
    modeled %>% 
    group_by(curr_oper_name) %>% 
    summarize(n=n()) %>% 
    arrange(desc(n)) %>%
    filter(!is.na(curr_oper_name)) %>% 
    mutate(id = row_number()) %>%  
    select(-n) %>% 
    rename(name = curr_oper_name) %>% 
    rowwise() %>% 
    mutate(clean_name = clean_name(name))  

it = itoken(names$clean_name, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary()
vectorizer = vocab_vectorizer(v)

dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)

similarity_matrix = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")

tic()
name_map_cosine_similarity <- match_names_cosine(names, similarity_matrix, 
    threshold=0.4)
toc()

#===========
# shared word
#===========

names <- 
    modeled %>% 
    select(curr_oper_name) %>%
    filter(!is.na(curr_oper_name)) %>% 
    distinct() %>% 
    pull()

clean_names <- 
    modeled %>% 
    select(curr_oper_name) %>%
    filter(!is.na(curr_oper_name)) %>% 
    distinct() %>% 
    rowwise() %>% 
    mutate(curr_oper_name = clean_name(curr_oper_name, 
        drop_common_words=T)) %>% 
    pull()

tic()
name_map_shared_word <- match_names_shared_word(names, clean_names) %>% .[[1]]
toc()

#===========
# jaro distance
#===========

tic()
name_map_jaro <- match_names_stringdist(names, clean_names, threshold = 0.15)
toc()

#===========
# combine
#=========== 

name_map_cosine_similarity <- 
    name_map_cosine_similarity %>% 
    mutate(method = 'tf-idf cosine')

name_map_shared_word <- 
    name_map_shared_word %>% 
    mutate(method = 'shared word')

name_map_jaro <- 
    name_map_jaro %>% 
    mutate(method = 'jaro')

master <- 
    name_map_cosine_similarity %>% 
    bind_rows(name_map_shared_word) %>%
    bind_rows(name_map_jaro) %>% 
    rowwise() %>% 
    mutate(a1 = alpha_order(name, match, 1)) %>% 
    mutate(a2 = alpha_order(name,  match, 2)) %>% 
    select(a1, a2, method) %>% 
    rename(name = a1, match = a2) %>% 
    group_by(name, match) %>% 
    filter(row_number()==1) %>%
    arrange(name)

#===========
# save output
#===========

write_csv(master, file.path(ddir, 'matches.csv'))



