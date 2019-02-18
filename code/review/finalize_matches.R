# Created by Michael Cahana in late November 2018
# Finalizes matches data after resolving conflicts 

#===========
# inputs
# matches_EKIN.xlsx
# matches_MIRIAM.xlsx
# conflicts.xlsx
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
library(readxl)
library(openxlsx)

#===========
# data read in
#===========

ekin <- read_excel(file.path(vdir, 'matches_EKIN.xlsx')) %>% 
	select(name, match, keep) %>% 
	rename(keep_ekin = keep)

miriam <- read_excel(file.path(vdir, 'matches_MIRIAM.xlsx')) %>% 
	select(name, match, keep) %>% 
	rename(keep_miriam = keep)

conflicts <- read_excel(file.path(vdir, 'conflicts.xlsx')) %>% 
	select(name, match, keep)

#===========
# merge in conflicting data with non-conflicting
#===========

non_conflicts <- 
	ekin %>% 
	left_join(miriam, by=c('name', 'match')) %>% 
	filter(keep_ekin == keep_miriam) %>% 
	select(name, match, keep_ekin) %>% 
	rename(keep = keep_ekin)

master <- 
	non_conflicts %>% 
	bind_rows(conflicts)

#===========
# merge in conflicting data with non-conflicting
#===========

write.xlsx(master, file.path(vdir, 'finalized_matches.xlsx'))
