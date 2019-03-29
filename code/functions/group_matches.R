# Modified by Michael Cahana in early Dec. 2018
# Groups operator names according to graph clusters
# Assigns group name based on alphabetical order

#===========
# INPUTS
# matches dataset
# output file string
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
# functions
#===========

create_edge <- function(name, match) {
	edge <- c(name, match)
	return (edge)
}

extract_group_name <- function(no, cc) {
	group_name <- 
		cc %>% 
		filter(cluster==no) %>% 
		select(name) %>% 
		arrange() %>% 
		slice(1) %>% 
		pull()
}

alpha_order <- function(name, match, order) {
    vec <- c(name, match)
    a1 <- sort(vec)[order]
    return(a1)
}

find_cluster <- function(operator_name) {
	cluster <- 
		cc %>% 
		filter(name == operator_name) %>% 
		pull(cluster) 
	return (cluster)
}

graph_cluster <- function(cluster_edges, cluster_to_plot) {
	cluster_edges <- 
		cluster_edges %>%  
		filter(cluster == cluster_to_plot)

	edges <- unlist(map2(cluster_edges$name, cluster_edges$match, create_edge)) 
	g <- graph(edges, directed = FALSE)
	plot(g, main = paste('Cluster ', cluster_to_plot, '\n', 
		clusters$csize[cluster_to_plot], ' nodes', sep = ''))
}

group_matches <- function(df, output_file) {
	#===========
	# data prep
	#===========

	df <- 
		df %>% 
		filter(keep == 1) %>% 
		select(name, match)

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
		enframe(c(names(membership)), name = NULL) %>% 
		rename(name = value) %>% 
		bind_cols(enframe(membership, name = NULL)) %>%
		rename(cluster = value)

	#===========
	# create group names
	#===========

	# determine group name to be the first name (by alphabetical order)
	# within a cluster of names 
	group_names <- sapply(seq(1,clusters$no), extract_group_name, cc)

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

	# note that for some reason write_csv outputs special characters
	# so write.csv used instead
	write.csv(cc, output_file, row.names = F) 
}


