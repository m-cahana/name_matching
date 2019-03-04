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

desc <- 
  read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst'), 
    columns = c("api_no", "curr_oper_id", "curr_oper_no", "curr_oper_name")) %>% 
  mutate(api_no = str_replace_all(api_no, '-', '') %>% 
  stri_pad_right(14, 0))

modeled <- 
  readRDS(file.path(rdir, 'modeled_prices.Rds')) %>% 
  select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
  inner_join(desc, by='api_no')

cleaned_300 <- 
  read_excel(file.path(rdir, 'names_edited.xlsx')) %>% 
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

# words that signify a name is a company
company <-
  c('ACQUISITION', 'ENERG', 'RESOURCE', 'OG', 'O & G', 'O&G', 'EXPL', '\\sLAND', 
    'OPERAT', 'SERVICES', '\\sOIL', 'PROPERTIES', '\\sEP', 'ASSOC', 'PETR', 
    'PARTNER', 'PROD', 'NATURAL', 'GEOSOUTHERN', 'MIDLAND', 'INTEREST', 
    'ROYALT', 'BASIN', 'DRLG', 'DRILL', '\\sOIL', '\\sGAS', 'DEVELOPMENT', 
    'HOLDING', 'RIVER', 'PERMIAN', 'CHALFANT', 'MGMT', 'MINERALS', 'CONTINENTAL',
    'SOUTHWESTERN', 'E & P', 'E&P', '\\sGAS', 'GAS\\s', 'OIL\\s', 'CHURCH', 
    'INVEST', 'CAPITAL', 'UNIVERSITY', '\\sLEASING', 'LTD', 'LIMITED', 
    'MINERAL', 'TITLE', 'ENTERPRIS', 'FINANC', 'REFINERY', 
    'LP', 'L&P', 'L & P', 'CRUDE', 'PROPERTIES', '\\sPROD', 'VALLEY', 
    'REFINERY', 'STATES', 'PIPELINE', 'MINING', 'BROKER', 'LONESTAR') %>%
  paste(., collapse = '|') %>%
  regex(., ignore_case = TRUE)

#===========
# functions
#===========

clean_name <- function(name, drop_common_words=FALSE) {
    words <- 
      name %>%
      str_replace_all(",", " ") %>%
      str_replace_all("-", " ") %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_replace_all("L L C", "") %>%
      strsplit(split = " ") %>%
      pluck(1) %>%
      toupper
       
    if (drop_common_words) {
        words <- words[!words %in% common_words]
    } else {
        words <- words[!words %in% c('', ' ')]
    }
    # letters <- words[nchar(words)==1] %>% paste(collapse = '-')
    letters <- 
      words[nchar(words) < 2] %>% 
      paste(collapse = ' ') %>% 
      str_replace_all('--', ' ') 

    # UNDOS BAG OF WORDS
    words <- 
      words[!nchar(words)==1] %>% 
      append(letters) %>% 
      paste(collapse=' ') %>%
      str_trim %>%
      str_squish
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

# function to identify last name and extract 
extract_name <- function(names){
    # download census list of surnames
    surname_url <- "http://www2.census.gov/topics/genealogy/2010surnames/names.zip"
    tf <- tempfile()
    download.file(surname_url, tf, mode = "wb")     # download archive of surname data
    files <- unzip(tf, exdir = tempdir())  # unzips and returns a vector of file names
    surnames <- 
      read_csv(files[grepl("\\.csv$", files)]) %>%
      mutate(IsLast = TRUE)

    male_first <- 
      "https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/male.txt" %>%
      read_delim(delim = " ") %>%
      select(first_name = 1) %>%
      filter(first_name != "#")

    female_first <-
    "https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/female.txt" %>%
      read_delim(delim = " ") %>%
      select(first_name = 1) %>%
      filter(first_name != "#")

    first_names <- 
      bind_rows(female_first, male_first) %>%
      unique %>%
      mutate(first_name = str_to_upper(first_name), 
        IsFirst = TRUE) 

    names <- tibble(name = names)

    # split name and identify which parts are a first name and which are last
    split_name <-
      names %>%
      filter(!str_detect(name, company)) %>%
      mutate(name = str_to_upper(name), 
        name_split = str_split(name, " ")) %>%
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
      mutate(last_name = case_when(
            IsLast & !IsFirst ~ name_split, 
            IsLast & IsFirst ~ name_split, 
            !IsLast & IsFirst ~ str_replace(name, name_split, ""), 
            TRUE ~ NA_character_), 
        first_name = case_when(
            IsLast & !IsFirst ~ str_replace(name, name_split, ""), 
            IsLast & IsFirst ~ str_replace(name, name_split, ""), 
            !IsLast & IsFirst ~ name_split, 
            TRUE ~ NA_character_)) %>%
      arrange(name, -IsLast, -IsFirst) %>%
      group_by(name) %>%
      filter(row_number() == 1) %>%
      ungroup
     

    # extract initials and store as a list column
    split_name <-
      split_name %>%
      select(-IsFirst, -IsLast) %>%
      mutate_all(funs(str_squish(str_trim(.)))) %>%
      mutate(initials = str_split(first_name, " ")) %>%
      group_by(name) %>%
      mutate(initials = list(str_extract(pluck(initials, 1), ".{1}")), 
        initials = if_else(nchar(first_name) <= 2, list(first_name), initials)) %>%
      mutate(first_name = str_replace(first_name, "\\sJR$", ""), 
        last_name = str_replace(last_name, "\\sJR$", "")) %>%
      ungroup %>%
      right_join(names)

    return(split_name)
}

match_first_name <- function(names, clean_names){
    split_names <- 
      extract_name(clean_names) %>%
      rename(clean_name = name) %>%
      bind_cols(name = names, .) 

    # filter out company names
    company_names <- 
      filter(split_names, is.na(last_name)) %>%
      select(name, clean_name)

    # create pairs of names we want to check based on if they have the same
      # last name
      # Check if initials are a match if the first name has more than one word
      # or if it has less than 4 characters 
      # (ie we don't want to match john smith to jake smith)
    human_names <- 
      filter(split_names, !is.na(last_name)) %>%
      mutate(id = row_number()) %>%
      group_by(id) %>%
      mutate(check_initials = length(str_split(first_name, " ")[[1]]) > 1 | 
        nchar(first_name) < 4) %>%
      ungroup 

    human_names1 <-
      human_names %>%
      setNames(paste0(colnames(.), "1")) %>%
      rename(last_name = last_name1)
   
   # full_join human names with itself
      # check if bag of words containing first initial of each word in the first
      # name matches
      # then do string dist on first names
    human_names_match <-
      human_names %>%
      full_join(human_names1) %>%
      filter(id < id1) %>%
      mutate(InitialMatch = map2_lgl(initials, initials1, setequal), 
        InitialMatch = if_else(check_initials | check_initials1, 
            InitialMatch, FALSE))

    return(list(human = human_names_match, company = company_names))
}

match_names_stringdist <- function(names, clean_names, 
    method='jw', threshold=0.25) {

    # first identify which names are human names vs company names
    human_matches_all <- match_first_name(names, clean_names)
    human_matches <-
      human_matches_all %>%
      pluck("human") %>%
      mutate(dist_score = stringdist(first_name, first_name1, 
            method = method, p = 0)) %>%
      filter(dist_score <= threshold | InitialMatch) %>%
      mutate(dist_score = if_else(InitialMatch, NA_real_, dist_score), 
        method = if_else(InitialMatch, "initials", 
            paste(method, "first names"))) %>%
      select(name, match = name1, dist_score, method)

    company_names <- 
      pluck(human_matches_all, "company") %>%
      filter(clean_name != "")

    # now loop through the company names
    df <- tibble()
    all_names <- tibble(name = names, clean_name = clean_names)
    i <- 0
    while (nrow(company_names) > 0) {
        i <- i + 1
        print(i)

        c_name <- company_names$clean_name[1]
        f_name <- company_names$name[1]
        all_names <- filter(all_names, name != f_name)

        if (c_name == "") {
            all_names <- filter(all_names, name != f_name)
            next
        }

        match <-
          all_names %>%
          mutate(c_name = c_name, 
            f_name = f_name, 
            dist_score = stringdist(c_name, clean_name, method = method)) %>%
          filter(dist_score < threshold) %>%
          select(name, match = f_name, dist_score)

        df <- bind_rows(df, match)

        company_names <- company_names[-1, ]
    }

    df <- 
      df %>%
      mutate(method = method) %>%
      bind_rows(human_matches)

    return(df)
}

match_names_shared_word <- function(names, clean_names) {
    # clean human names first
    # modify the get_matches function to return the match!!
    human_matches_all <- match_first_name(names, clean_names)

    human_matches <-
      human_matches_all %>%
      pluck("human") %>%
      group_by(id, id1) %>%
      mutate(bag = str_split(first_name, " "), 
        bag1 = str_split(first_name1, " ")) %>%
      mutate(shared = length(intersect(bag, bag1))) %>%
      filter(shared > 0 | InitialMatch) %>%
      mutate(method = if_else(InitialMatch, "initials", "shared word first names")) %>%
      ungroup %>% 
      select(name, match = name1, method)

    company_names <- 
      pluck(human_matches_all, "company") %>%
      filter(clean_name != "") %>%
      group_by(name) %>%
      mutate(bag = str_split(clean_name, " ")) %>%
      ungroup

    words <- 
      tibble(name = names, clean_name = clean_names) %>%
      group_by(name) %>%
      mutate(bag = str_split(clean_name, " ")) %>%
      ungroup

    df <- tibble()
    while (nrow(company_names)>0){
       bag <- company_names$bag[1]
       match <- company_names$name[1]
       words <- filter(words, name != match)
       matches <- get_matches(words$bag, bag)
       
       if(length(matches)>0) {
            df <- bind_rows(df, matched_df(match, matches, words$name))
       }

       company_names <- company_names[-1, ]
    }
    
    df <- 
      df %>%
      mutate(method = "shared word") %>%
      bind_rows(human_matches)

    return(df)
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
    mutate(curr_oper_name = clean_name(curr_oper_name, drop_common_words=T)) %>% 
    pull()

tic()
name_map_shared_word <- match_names_shared_word(names, clean_names)
toc()

# make sure they share more than one letter!!

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



