# Created by Michael Cahana in late November 2018
# Finds conflicts between matches_EKIN and matches_MIRIAM

#===========
# inputs
# matches_EKIN.xlsx
# matches_MIRIAM.xlsx
# wells.Rds
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

wells <- readRDS(file.path(ddir, 'wells.Rds'))

#===========
# find conflicts
#===========

twos <- 
	ekin %>% 
	left_join(miriam, by=c('name', 'match')) %>% 
	filter(keep_ekin == keep_miriam) %>%  
	filter(keep_ekin==2)


conflicts <- 
	ekin %>% 
	left_join(miriam, by=c('name', 'match')) %>% 
	filter(keep_ekin != keep_miriam) %>% 
	bind_rows(twos) %>% 
	mutate(keep = NA, justification = NA) %>% 
	select(name, match, keep, justification, 
		keep_ekin, keep_miriam, notes_ekin, notes_miriam)

#===========
# find total well count
#===========

well_count <- function(name_of_interest) {
	count <- wells %>% 
		filter(curr_oper_name==name_of_interest) %>% 
		select(n) %>% 
		pull()
	return (count)
}

conflicts <- 
	conflicts %>% 
	rowwise() %>% 
	mutate(count1 = well_count(name)) %>% 
	mutate(count2 = well_count(match)) %>% 
	mutate(well_count = count1 + count2) %>% 
	select(-c(count1, count2))

#===========
# save output
#===========

write.xlsx(conflicts, file.path(vdir, 'conflicts.xlsx'))