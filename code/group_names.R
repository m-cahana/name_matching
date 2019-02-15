# Modified by Michael Cahana in early Dec. 2018
# Groups operator names within our permits data

#===========
# INPUTS
# operator_name_matches_desc_permits.xlsx
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
# NEEDED LIBRARIES
#===========
library(tidyverse)
library(igraph)

#===========
# data read in
#===========

df <- read_csv(file.path(ddir, 'matches', 'address_matches.csv'))

#===========
# functions
#===========

create_edge <- function(name, match) {
	edge <- c(name, match)
}

extract_group_name <- function(no) {
	group_name <- 
		cc %>% 
		filter(cluster==no) %>% 
		select(name) %>% 
		arrange() %>% 
		slice(1) %>% 
		pull()
}

#===========
# find connected components
#===========

edges <- unlist(map2(df$name, df$match, create_edge)) 

g <- graph(edges, directed=FALSE)
# NOTE: considering connected components for now
# i.e. a set of nodes (matches) do not have to form a complete graph 
# they only need to form a connected graph
# in order to consider complete graphs, look at the cliques function
clusters <- clusters(g)
membership <- clusters$membership

cc <- 
	as_tibble(c(names(membership))) %>% 
	rename(name = value) %>% 
	bind_cols(as_tibble(membership)) %>%
	rename(cluster = value)

#===========
# create group names
#===========

# determine group name to be the first name (by alphabetical order)
# within a cluster of names 
group_names <- sapply(seq(1,clusters$no), extract_group_name)

# match group names (i.e. replacement names) to cluster ids
distinct_cc <- 
	tibble(cluster = seq(1, clusters$no, 1)) %>% 
	bind_cols(tibble(group_names)) %>% 
	rename(group_name = group_names) %>% 
	select(cluster, group_name)

# merge group names into dataframe with current names
cc <- 
	cc %>% 
	inner_join(distinct_cc, by='cluster') %>% 
	arrange(cluster, name)

#===========
# save output
#===========

saveRDS(cc, file = file.path(ddir, 'grouped_matches', 
	'grouped_address_matches.Rds'))


