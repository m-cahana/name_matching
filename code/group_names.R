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
library(readxl)

#===========
# data read in
#===========

df <- read_excel(file.path(dropbox, 'reviewed_data', 'matches_EKIN.xlsx')) 

#===========
# find connected components
#===========

edges <- c()
for (i in 1:dim(df)[1]) {
    keep <- df %>% slice(i) %>% select(keep)
    if (keep==1) {
        name <- df %>% slice(i) %>% select(name) %>% pull()
        match <- df %>% slice(i) %>% select(match) %>% pull()
        edges <-  c(edges, name, match)
    }
}

g <- graph(edges, directed=FALSE)
# NOTE: considering connected components for now
# i.e. a set of nodes (matches) do not have to form a complete graph 
# they only need to form a connected graph
# in order to consider complete graphs, look at the cliques function
clusters <- clusters(g)
membership <- clusters$membership

for (i in 1:length(membership)) {
	name <- membership[i] %>% names()
	cluster <- membership[i]
	sub_cc <- tibble(name = name, cluster = cluster)
	if (i>1) {
		cc <- cc %>% bind_rows(sub_cc)
	} else {
		cc <- sub_cc
	}
}

#===========
# plotting
#===========

# plot (only plot a subset, otherwise way too big)
# g <- graph(edges[1:20], directed=FALSE)
# plot(g)

#===========
# test connected components for completeness
#===========	

# get distinct clusters
distinct_cc <- tibble(cluster = seq(1, clusters$no, 1))

# get number of edges within each cluster
for (i in 1:clusters$no) {
	names <- cc %>% 
		filter(cluster==i) %>% 
		select(name) %>% 
		pull()
	edges <- c()
	# for every name within a cluster, get edges associated with it
	for (name in names) {
		new_edges <- incident(g, name)
		edges <- c(edges, new_edges)
	}
	# extract number of unique edges (non-directional graph, so x-y same as y-x)
	edges <- unique(edges)
	num_edges <- length(edges)
	if (i==1) {
		all_edges <- c(num_edges)
	} else {
		all_edges <- c(all_edges, num_edges)
	}
}

distinct_cc <- 
	distinct_cc %>% 
	bind_cols(tibble(clusters$csize)) %>% 
	bind_cols(tibble(all_edges)) %>% 
	rename(nodes = `clusters$csize`, edges = all_edges) %>% 
	mutate(edges = if_else(nodes==1, as.integer(0), edges)) %>% 
	mutate(nC2 = choose(nodes,2)) 

complete_clusters <- 
	distinct_cc %>% 
	filter(edges == nC2) %>%
	select(cluster) %>% 
	pull()

#===========
# plot specific cluster
#===========	

find_cluster <- function(name_of_interest) {
	cluster <- cc %>% 
		filter(name==name_of_interest) %>% 
		select(cluster) %>% 
		pull()
	if (length(cluster)==0) {
		return (NA)
	} else {
		return (cluster)
	}
}

graph_cluster <- function(cluster_edges, cluster_to_plot) {
	cluster_edges <- 
		cluster_edges %>%  
		filter(name_cluster == cluster_to_plot)

	edges <- c()
	for (i in 1:dim(cluster_edges[1])) {
		name <- cluster_edges %>% slice(i) %>% select(name) %>% pull()
		match <- cluster_edges %>% slice(i) %>% select(match) %>% pull()
		edges <-  c(edges, name, match)
	}
	g <- graph(edges, directed = FALSE)
	plot(g, main = paste('Cluster ', cluster_to_plot, '\n', 
		clusters$csize[cluster_to_plot], ' nodes', sep = ''))
}

cluster_edges <- 
	df %>% 
	rowwise() %>% 
	mutate(name_cluster = find_cluster(name)) %>% 
	mutate(match_cluster = find_cluster(match)) %>% 
	filter(keep==1) 

# graph_cluster(cluster_edges, 10)

#===========
# create group names
#===========

# determine group name to be the first name (by alphabetical order)
# within a cluster of names 
group_names <- c()
for (i in 1:clusters$no) {
	subsetted_names <- 
		cc %>% 
		filter(cluster==i) %>% 
		select(name)

	group_name <- 
		subsetted_names %>% 
		arrange() %>% 
		slice(1) %>% 
		pull() 

	group_names <- c(group_names, group_name)

}

# match group names (i.e. replacement names) to cluster ids
distinct_cc <- 
	distinct_cc %>% 
	bind_cols(tibble(group_names)) %>% 
	rename(group_name = group_names) %>% 
	select(cluster, group_name)

# merge group names into dataframe with current names
cc <- 
	cc %>% 
	inner_join(distinct_cc, by='cluster')

#===========
# save output
#===========

saveRDS(cc, file = file.path(ddir, 'di', 
	'grouped_EKIN_matches.Rds'))


