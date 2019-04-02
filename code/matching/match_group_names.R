# Modified by Michael Cahana in early Dec. 2018
# Matches all group names in all_groups.csv

#===========
# INPUTS
# group names
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

source(file.path(root, 'code', 'functions', 'match_names.R'))

#===========
# read in 
#===========

# gather list of group names
df <- 
	read_csv(file.path(ddir, 'grouped_matches', 'all_groups.csv')) %>% 
	count(group_name) %>% 
	rename(name = group_name)

output_file <- file.path(vdir, 'group_name_matches.csv') 

#===========
# identify potential "cluster of clusters" - multiple distinct clusters that
# belong together in one cluster
#===========

# match group names
df <- 
	df %>% 
	match_names(output_file, cosine_threshold =  0.65, write_csv = F) %>% 
	# remove pure shared word matches, add keep column 
	filter(!(is.na(cosine_similarity)) | !(is.na(jw_distance))) %>% 
	mutate(keep = NA) 

if(file.exists(output_file)) {
	existing_df <- read_csv(output_file)
	df <- 
		existing_df %>% 
		bind_rows(df) %>% 
		distinct(name, match, .keep_all = T)
}

write_csv(df, output_file)