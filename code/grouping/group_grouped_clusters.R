# Modified by Michael Cahana in early Apr. 2019
# Groups together clusters in grouped matches that refer to same entity

#===========
# INPUTS
# all files in \reviewed_data
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

#===========
# functions
#===========

source(file.path(root, 'code', 'functions', 'group_matches.R'))

#===========
# data read-in
#===========

df <- 
	list.files(vdir, full.names = TRUE) %>% 
	map_df(read_csv) 

output_file <- file.path(ddir, 'grouped_matches', 'all_groups.csv')

#===========
# group matches
#===========

group_matches(df, output_file)
