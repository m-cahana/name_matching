# Modified by Michael Cahana in mid Feb. 2018
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

#===========
# needed libraries
#===========
library(igraph)
library(tidyverse)

#===========
# functions
#===========

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

pre_screen_names <- function(name_matches, address_matches, lease_count, 
	output_file) {
	name_matches <- 
		name_matches %>% 
		left_join(address_matches, by=c('name','match')) %>% 
	    mutate(keep = if_else(!is.na(address), 1, as.double(NA))) %>% 
	    left_join(lease_count, by='name') %>% 
	    left_join(lease_count, by=c('match' = 'name')) %>% 
	    mutate(n.x = ifelse(duplicated(name), 0, n.x)) %>% 
	    mutate(n.y = ifelse(duplicated(match), 0, n.y)) %>% 
	    mutate(n.y = ifelse(match %in% .$name, 0, n.y)) %>% 
	    mutate(n = n.x + n.y) %>% 
	    arrange(desc(n)) %>% 
	    mutate(pct_coverage = cumsum(n)/sum(n)) 

	if (file.exists(output_file)) {
		pre_existing_name_matches <- read_csv(output_file)
		name_matches <- 
			pre_existing_name_matches %>% 
			bind_rows(name_matches) %>% 
			distinct(name, match, .keep_all = T)
	}

	if (file.exists(file.path(ddir, 'grouped_matches', 'all_groups.csv'))) {
		reviewed_pairs <- read_csv(file.path(ddir, 'grouped_matches', 
			'all_groups.csv'))
		edges <- unlist(map2(reviewed_pairs$name, reviewed_pairs$group_name, 
			create_edge)) 
		g <- graph(edges, directed=FALSE)
		clusters <- clusters(g)
		membership <- clusters$membership

		connected_components <- 
			enframe(c(names(membership)), name = NULL) %>% 
			rename(name = value) %>% 
			bind_cols(enframe(membership, name = NULL)) %>%
			rename(cluster = value)

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

		redundant_edges <- 
			intersection(current_graph, complete_clusters) %>% 
			E() %>% 
			attributes() %>% 
			.$vnames %>% 
			strsplit('\\|') %>% 
			map_df(list_to_df_edge) %>% 
			mutate(prior_check = 1)

		name_matches <- 
			name_matches %>% 
			left_join(redundant_edges, by = c('name', 'match')) %>% 
			mutate(keep = if_else(is.na(prior_check), keep, prior_check)) %>% 
			select(-prior_check)
	}

	write_csv(name_matches, output_file)
}