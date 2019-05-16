# Created by Michael Cahana in mid Feb. 2019
# Determines name matches for leases

#===========
# inputs: 
#===========
# all_leases.Rds

#===========
# needed libraries
#===========
library(tidyverse)
library(sf)

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# functions
#===========

source(file.path(root, 'code', 'functions', 'match_names.R'))

#===========
# data read in
#===========

all_leases <- 
	readRDS(file.path(rdir, 'leases', 'all_leases.Rds'))

#===========
# match names 
#===========

# all
df <- 
    all_leases %>% 
    count(grnte_al) %>% 
    rename(name = grnte_al) 

output_file <- file.path(ddir, 'matches', 'names', 'leases_name_matches.csv')

match_names(df, output_file)


