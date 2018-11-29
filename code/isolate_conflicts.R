# Created by Michael Cahana in late November 2018
# Finds conflicts between matches_EKIN and matches_MIRIAM

#===========
# inputs
# matches_EKIN.xlsx
# matches_MIRIAM.xlsx
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
	select(name, match, keep, notes) %>% 
	rename(keep_ekin = keep, notes_ekin = notes)

miriam <- read_excel(file.path(vdir, 'matches_MIRIAM.xlsx')) %>% 
	select(name, match, keep, notes) %>% 
	rename(keep_miriam = keep, notes_miriam = notes)

#===========
# find conflicts
#===========

conflicts <- 
	ekin %>% 
	left_join(miriam, by=c('name', 'match')) %>% 
	filter(keep_ekin != keep_miriam) %>% 
	mutate(keep = NA) %>% 
	select(name, match, keep, keep_ekin, keep_miriam, notes_ekin, notes_miriam)

#===========
# save output
#===========

write.xlsx(conflicts, file.path(vdir, 'conflicts.xlsx'))