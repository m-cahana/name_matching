# Created by Michael Cahana in mid Feb. 2019
# Verifies certain name matches given an address match

#===========
# inputs: 
#===========
# pden_desc-2018-09-26.fst
# modeled_prices.Rds
# names_edited.xlsx

#===========
# needed libraries
#===========
library(tidyverse)
library(fst)
library(stringi)
library(readxl)

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

source(file.path(root, 'code', 'functions', 'pre_screen_names.R'))

#===========
# data read in
#===========

desc <- read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst'), 
    columns = c('api_no', 'curr_oper_id', 'curr_oper_no', 
        'common_oper_name')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        stri_pad_right(14, 0)) 
modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) 

#===========
# verify name matches with addresses 
#===========

name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
    'modeled_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'modeled_address_matches.csv')) 
output_file <- file.path(vdir, 'modeled_matches.csv')
lease_count <- 
    modeled %>% 
    select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
    inner_join(desc, by='api_no') %>% 
    rename(name = common_oper_name) %>% 
    count(name)

pre_screen_names(name_matches, address_matches, lease_count, output_file)