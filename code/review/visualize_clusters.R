# Created by Michael Cahana in late November 2018
# Visually explore graphs that result from list of matches

#===========
# inputs
# csv of clustered matches
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

source(file.path(root, 'code', 'functions', 'plot_graphs.R'))

#===========
# data read in
#===========

df <- 
	read_csv(file.path(ddir, 'grouped_matches', 'all_groups.csv')) %>% 
	rename(match = group_name)
edges <- unlist(map2(df$name, df$match, create_edge)) 
g <- graph(edges, directed=FALSE)

#===========
# visual exploration
#===========

plot_word(df, 'CARRIZO')
plot_clusters(df, c(412,960))