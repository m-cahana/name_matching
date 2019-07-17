# Created by Michael Cahana in early May 2019
# Reads in lease data from raw DI csvs, cleans and outputs as RDS

#===========
# inputs:
#===========
# leases/csvs dir

#===========
# needed libraries
#===========
library(tidyverse)
library(lubridate)

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# data read in
#===========

leases_di <-
	list.files(file.path(rdir, 'leases', 'csvs'), full.names = T) %>%
	map_df(read_csv, col_types = cols(.default = "c"))

#===========
# clean
#===========

# set column names/types, remove leases before 2000
leases_di <-
	leases_di %>%
	setNames(c("state_pr", "cnty_sta", "DIBasin", "DIPlay", "DISubplay",
		"grantor", "grnte_al", "vol_pg", "rec_numb", "inst_typ", "inst_dt",
		"recd_dt", "effct_dt", "term_mos", "expir_dt", "exten", "extbonus",
		"ext_term", "bonus", "royalty", "acres", "abstract", "section",
		"twnshp", "twnshp_dir", "range", "range_dir", "block", "blm",
		"state_ls", "grantee", "grntr_ad", "grnte_ad", "maj_legal_assignee",
		"maj_legal_effct_dt", "maj_legal_assignee_interest",
		"maj_assignment_vol_pg", "max_dep", "min_dep", "latitude",
		"longitude")) %>%
	mutate(effct_dt = as.Date(effct_dt), inst_dt = as.Date(inst_dt),
		recd_dt = as.Date(recd_dt), expir_dt = as.Date(expir_dt),
		exten = as.double(exten), extbonus = as.double(extbonus),
		royalty = as.double(royalty), acres = as.double(acres),
		section = as.double(section), twnshp = as.double(twnshp),
		range = as.double(range), blm = as.double(blm),
		state_ls = as.double(state_ls), max_dep = as.double(max_dep),
		min_dep = as.double(min_dep), latitude = as.double(latitude),
		longitude = as.double(longitude)) %>%
	filter(year(expir_dt)>=2000 & year(inst_dt)>=2000 & year(recd_dt)>=2000)


#===========
# save output
#===========

saveRDS(leases_di, file = file.path(rdir, 'leases', 'leases_di.Rds'))
