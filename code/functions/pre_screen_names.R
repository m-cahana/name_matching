# Modified by Michael Cahana in late Mar. 2019
# Verifies name matches determined echoed in address matches

#===========
# inputs: 
#===========
# name_matches tibble
# address_matches tibble
# output_file string

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))
source(file.path(root, "code", "functions", "utils.R"))

#===========
# needed libraries
#===========
library(igraph)
library(tidyverse)

#===========
# functions
#===========

source(file.path(root, "code", "functions", "random_forest_utils.R"))

create_edge <- function(name, match) {
	edge <- c(name, match)
	return (edge)
}


all_edges <- function(cluster) {
	edges <- 
		cluster %>% 
		pull(name) %>% 
		combn(2) %>% 
		t() %>% 
		as_tibble() %>% 
		setNames(c('name', 'match')) %>% 
		filter(name != match) 
	return (unlist(map2(edges$name, edges$match, create_edge)))
}

list_to_df_edge <- function(l) {
	df <- 
		l %>% 
		t() %>% 
		as_tibble() %>% 
		setNames(c('name', 'match'))
	return (df)
}


rf_predict <- function(df, train_file_path) {
	train <- read_csv(train_file_path)
	func <- 
		paste("shared_words", "cosine_similarity", 
			"jw_distance", sep = "+") %>%
		paste("keep", ., sep = "~") %>%
		as.formula()
	rf <-
		func %>%
  		regression_forest2(train)
  	df <- 
  		rf %>%
  		predict2(func, df) %>%
  		as_tibble() %>%
  		bind_cols(df, .) %>% 
  		mutate(rf_prob = predictions) %>% 
  		select(-predictions)

  	return(df)
}

calculate_distance <- function(df, max_threshold, min_threshold) {
	df <-
		df %>% 
		rowwise() %>% 
		mutate(importance_dist = case_when(
			max_n >= max_threshold & min_n < min_threshold ~ 
				min_threshold - min_n, 
			max_n < max_threshold & min_n >= min_threshold ~ 
				max_threshold - max_n, 
			max_n < max_threshold & min_n < min_threshold ~ 
				raster::pointDistance(c(max_n, min_n), 
					c(max_threshold, min_threshold), lonlat = F), 
			max_n >= max_threshold & min_n >= min_threshold ~ 
				0
			))
	return (df)
}

pre_screen_names <- function(name_matches, address_matches, lease_count, 
	output_file, human_jw_threshold = .6, human_cos_threshold = .6) {

	# flag matches where human name distance is high
	name_matches <-
	  name_matches %>% 
	  mutate(keep = if_else(human_jw_distance > human_jw_threshold &
	  	human_cosine_similarity > human_cos_threshold & is.na(initials_match), 
	  	0, as.double(NA)))

	# verify name matches that have an address match, add in lease counts, 
	# calculate closeness scores and minimum n's for each pair, adjust n's to 
	# avoid double counting 
	name_matches <- 
		name_matches %>% 
		left_join(address_matches, by = c('name','match')) %>% 
	    mutate(keep = if_else(!is.na(address), 1, keep)) %>% 
	    left_join(lease_count, by = 'name') %>% 
	    left_join(lease_count, by = c('match' = 'name')) %>%
	    rowwise() %>% 
	    mutate(min_n = min(n.x, n.y)) %>% 
	    mutate(max_n = max(n.x, n.y)) %>% 
	    mutate(sum_n = sum(n.x, n.y)) %>% 
	    mutate(actual_n.x = n.x) %>%
	    mutate(actual_n.y = n.y) %>%  
	    ungroup()  %>% 
	    mutate(n.x = ifelse(duplicated(name), 0, n.x)) %>% 
	    mutate(n.y = ifelse(duplicated(match), 0, n.y)) %>% 
	    mutate(n.y = ifelse(match %in% .$name, 0, n.y)) %>% 
	    replace_na(list(n.x = 0, n.y = 0))

	# mark match pairs we deem "important" based on their count coverage
	# note that the importance_dist variable will represent the distance 
	# an (x,y) pair is from being within both min and max thresholds 
	# (x here is max_n, and y min_n)
	count_deciles <- lease_count %>% pull %>% quantile(probs = seq(.1,1,.1))
	seventieth_percentile <- count_deciles[7]
	ninetieth_percentile <- count_deciles[9]

	name_matches <- 
		name_matches %>% 
		calculate_distance(ninetieth_percentile, seventieth_percentile) %>% 
		arrange(importance_dist)


	# determine pairs that are now verified as correct but previously were not, 
	# if relevant 
	reviewed_files <- 
		list.files(vdir, full.names = TRUE) %>% 
		.[.!=file.path(vdir, 'group_name_matches.csv')]

	if (length(reviewed_files>0)) {
		reviewed_pairs <- 
			reviewed_files %>% 
			map_df(read_csv) %>% 
			select(name, match, keep) 
		previous_non_pairs <- 
			name_matches %>% 
			inner_join(filter(reviewed_pairs, keep == 0), 
				by = c('name', 'match')) %>% 
			select(-c('keep.x', 'keep.y', 'n.x', 'n.y'))
	}

	# if we already did some human review on these matches, incorporate it, 
	# don't overwrite it
	if (file.exists(output_file)) {
		pre_existing_name_matches <- read_csv(output_file)
		name_matches <- 
			pre_existing_name_matches %>% 
			bind_rows(name_matches) %>% 
			distinct(name, match, .keep_all = T) %>% 
			arrange(importance_dist)
	}

	# if we already have some group matches (that is, verified matches from 
	# other datasets, not just this one), incorporate those matches as well
	if (file.exists(file.path(ddir, 'grouped_matches', 'all_groups.csv'))) {
		reviewed_clusters <- read_csv(file.path(ddir, 'grouped_matches', 
			'all_groups.csv'))
		edges <- unlist(map2(reviewed_clusters$name, 
			reviewed_clusters$group_name, create_edge)) 
		g <- graph(edges, directed=FALSE)
		clusters <- clusters(g)
		membership <- clusters$membership

		connected_components <- 
			enframe(c(names(membership)), name = NULL) %>% 
			rename(name = value) %>% 
			bind_cols(enframe(membership, name = NULL)) %>%
			rename(cluster = value)

		# ensure clusters are complete (all possible edges drawn) such that 
		# every match is covered
		complete_clusters <- 
			 connected_components %>% 
			 split(.$cluster) %>% 
			 sapply(all_edges) %>% 
			 unlist() %>% 
			 as.vector() %>% 
			 graph(directed = FALSE)

		current_edges <- unlist(map2(name_matches$name, name_matches$match, 
			create_edge)) 
		current_graph <- graph(current_edges, directed=FALSE)

		# find edges in name_matches that were already verified by the previous
		# grouping
		redundant_edges <- 
			intersection(current_graph, complete_clusters) %>% 
			E() %>% 
			attributes() %>% 
			.$vnames %>% 
			strsplit('\\|') %>% 
			map_df(list_to_df_edge) %>% 
			rowwise() %>% 
			mutate(a1 = alpha_order(name, match, 1)) %>% 
	        mutate(a2 = alpha_order(name,  match, 2)) %>% 
	        select(-c('name', 'match')) %>% 
	        rename(name = a1, match = a2) %>% 
	        mutate(prior_check = 1) 

		name_matches <- 
			name_matches %>% 
			left_join(redundant_edges, by = c('name', 'match')) %>% 
			mutate(keep = ifelse(is.na(prior_check), keep, prior_check)) %>% 
			select(-prior_check)

		# get list of inferred edges implied by completeness to be correct but
		# not explicitly marked with keep == 1
		inferred_matches <- 
			redundant_edges %>% 
			anti_join(filter(reviewed_pairs, keep ==1), 
				by = c('name', 'match')) %>% 
			select(-prior_check)
	}

	# verify name matches that have a low match probability according to a 
	# trained random forest (rf) model 
	if(file.exists(file.path(ddir, 'training', 'lease_match_sample.csv'))) {
		name_matches <- 
			name_matches %>% 
			rf_predict(file.path(ddir, 'training', 
				'lease_match_sample.csv')) %>% 
			mutate(keep = ifelse((rf_prob<=0.2 & is.na(keep)), 0, keep))
	}	

	# write out notification files for pairs previously marked as incorrect
	# but now known to be corrrect, and pairs inferred to be correct via cluster
	# completeness but not actually verified by humans/addresses in  prior round
	# ensure files aren't overwritten but just appended to
	if(file.exists(file.path(ddir, 
		'notifications', 'previous_non_pairs.csv'))) {
		df <- read_csv(file.path(ddir, 
			'notifications', 'previous_non_pairs.csv'))
		previous_non_pairs <- 
			df %>% 
			bind_rows(previous_non_pairs) %>% 
			distinct(name, match, .keep_all = T)
	}

	if(file.exists(file.path(ddir, 
		'notifications', 'inferred_matches.csv'))) {
		df <- read_csv(file.path(ddir, 
			'notifications', 'inferred_matches.csv'))
		inferred_matches <- 
			df %>% 
			bind_rows(inferred_matches) %>% 
			distinct(name, match, .keep_all = T)
	} 

	if (length(reviewed_files>0)) {
		if (dim(previous_non_pairs)[1]>0) {
			write_csv(previous_non_pairs, file.path(ddir, 
				'notifications', 'previous_non_pairs.csv'))
		}
	}
	if (file.exists(file.path(ddir, 'grouped_matches', 'all_groups.csv'))) {
		write_csv(inferred_matches, file.path(ddir, 
			'notifications', 'inferred_matches.csv'))
	}
	write_csv(name_matches, output_file)
}