# Created by Michael Cahana in mid Feb. 2018
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

source(file.path(root, 'code', 'functions', 'verify_names.R'))

#===========
# data read in
#===========

desc <- read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst'), 
    columns = c('api_no', 'curr_oper_id', 'curr_oper_no', 'curr_oper_name')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        stri_pad_right(14, 0)) 
modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) 
cleaned_300 <- read_excel(file.path(rdir, 'names_edited.xlsx')) %>% 
    select(curr_oper_name, replacement)

#===========
# verify name matches with addresses 
#===========

name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
    'modeled_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'modeled_address_matches.csv')) %>% 
    select(-method)
output_file <- file.path(vdir, 'modeled_matches.csv')
lease_count <- 
    modeled %>% 
    select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
    inner_join(desc, by='api_no') %>% 
    left_join(cleaned_300, by='curr_oper_name') %>% 
    mutate(replacement = 
        if_else(is.na(replacement), curr_oper_name, replacement)) %>% 
    select(-curr_oper_name) %>% 
    rename(name = replacement) %>% 
    count(name)

verify_names(name_matches, address_matches, lease_count, output_file)