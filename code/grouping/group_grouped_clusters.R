# Modified by Michael Cahana in early Apr. 2019
# Groups together clusters in grouped matches that refer to same entity

#===========
# INPUTS
# all_groups.csv
# group_name_matches.csv
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

df <- read_csv(file.path(ddir, 'grouped_matches', 'all_groups.csv'))
group_name_matches <- read_csv(file.path(vdir, 'group_name_matches.csv'))

output_file <- file.path(ddir, 'grouped_matches', 'standardized_names.csv')

#===========
# group clusters
#===========
# only group together clusters if cluster matches were identified
if (dim(filter(group_name_matches, keep == 1))[1]>0) {
	# group together cluster matches
	group_name_matches <-
		group_name_matches %>%
		group_matches('placeholder', write_csv = F) %>%
		rename(group_name = name, grouped_group_name = group_name) %>%
		select(-cluster)

	# rename cluster groups according to matching cluster group names
	df <-
		df %>%
		left_join(group_name_matches, by = 'group_name') %>%
		mutate(group_name = if_else(!is.na(grouped_group_name),
			grouped_group_name, group_name)) %>%
		select(-c(grouped_group_name, cluster)) %>%
		mutate(cluster = group_indices(., group_name)) %>%
		arrange(cluster, name)
}
#===========
# save output
#===========

write_csv(df, output_file)
