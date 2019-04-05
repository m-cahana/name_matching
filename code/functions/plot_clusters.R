# Created by Michael Cahana in early Apr. 2019
# Plots graphs that result from list of matches

#===========
# inputs
# df of clustered name matches
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

# given a dataframe of names and matches, pre-assigned to clusters, plots a 
# given cluster (specified by an integer)
plot_clusters <- function(df, clusters_to_plot) {
	clusters_to_plot <- c(clusters_to_plot)
	df <- 
		df %>%  
		filter(cluster %in% clusters_to_plot)

	edges <- unlist(map2(df$name, df$match, create_edge)) 
	g <- graph(edges, directed = FALSE)
	plot(g, main = paste('Cluster ', paste(clusters_to_plot, collapse = ', '), 
		'\n', dim(distinct(df, name))[1], ' nodes', sep = ''))
}

# given a dataframe of names and matches (nodes connected by an edge), plots 
# all edges that contain a given word
plot_word <- function(df, word_to_detect) {
	df <- 
		df %>%  
		filter(str_detect(match, word_to_detect))

	edges <- unlist(map2(df$name, df$match, create_edge)) 
	g <- graph(edges, directed = FALSE)
	plot(g, main = paste('Cluster(s) for word', word_to_detect, sep = ' '))
}
