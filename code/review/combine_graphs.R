# Created by Michael Cahana in late March 2019
# Combines graphs of previous rounds with that of current round to
# avoid redundant human review

#===========
# inputs
# contents of vdir
# new data of interest
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
library(igraph)

#===========
# read in
#===========

reviewed_pairs <- 
	list.files(vdir, full.names = TRUE) %>% 
	map_df(read_csv) %>% 
	filter(keep == 1) %>% 
	select(name, match)

test <- tibble(name = c('EOG RESOURCES', 'EG RESOURCES', 'SAMSON EXPL'),  
	match = c('EAOG RESOURCES', 'EOG RESOURCES', 'SAMON LONE STAR'), 
	keep = rep(as.double(NA), 3))

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

#===========
# compile reviewed pairs into new graph with all clusters complete 
# (all possible edges within a cluster existing)
#===========

edges <- unlist(map2(reviewed_pairs$name, reviewed_pairs$match, create_edge)) 
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

#===========
# find edges of intersection between reviewed graph and new graph
#===========

test_edges <- unlist(map2(test$name, test$match, create_edge)) 
test_g <- graph(test_edges, directed=FALSE)

redundant_edges <- 
	intersection(test_g, complete_clusters) %>% 
	E() %>% 
	attributes() %>% 
	.$vnames %>% 
	strsplit('\\|') %>% 
	map_df(list_to_df_edge) %>% 
	mutate(prior_check = 1)

#===========
# mark redundant pairs in new data with 1's
#===========

test <- 
	test %>% 
	left_join(redundant_edges, by = c('name', 'match')) %>% 
	mutate(keep = if_else(is.na(prior_check), keep, prior_check)) %>% 
	select(-prior_check)

#===========
# save new data with redudancies marked
#===========

write_csv()

