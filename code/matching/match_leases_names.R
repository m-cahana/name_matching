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

memory.limit(13000000000000)


#===========
# data read in
#===========

leases_di <-
	readRDS(file.path(rdir, 'leases', 'leases_di.Rds')) %>%
	count(grnte_al)

leases_jb <-
  readRDS(file.path(rdir, 'leases', 'leases_jb.Rds')) %>%
	mutate(Lessee = str_to_upper(Lessee)) %>%
	count(Lessee) 

#===========
# match names
#===========
# di + jb leases
df <-
    leases_di %>%
		select(name = grnte_al) %>%
		rbind(select(leases_jb, name = Lessee))

output_file <- file.path(ddir, 'matches', 'names', 'leases_name_matches.csv')

match_names(df, output_file)
