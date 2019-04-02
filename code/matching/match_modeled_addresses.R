# Created by Michael Cahana in mid Feb. 2019
# Determines address matches for modeled data

#===========
# inputs: 
#===========
# pden_desc-2018-09-26.fst
# modeled_prices.Rds
# nph_oper_addr-2017-04-30.Rdata
# coded_addresses.csv

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

source(file.path(root, 'code', 'functions', 'match_addresses.R'))

#===========
# data read in
#===========

desc <- read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst'), 
    columns = c('api_no', 'curr_oper_id', 'curr_oper_no', 'curr_oper_name')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        stri_pad_right(14, 0)) 
modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) 
# named: nph_oper_addr
load(file.path(rdir, 'addresses', 'nph_oper_addr-2017-04-30.Rdata'))
already_coded_addresses <- read_csv(file.path(ddir, 'address_backups', 
    'coded_addresses.csv'))

#===========
# match addresses within modeled
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)
addresses_to_google <- 
	nph_oper_addr %>% 
	as_tibble() %>% 
	rename(curr_oper_id = assoc_id) %>% 
	filter(addr_1!='') %>% 
	mutate(address = if_else(addr_2!='', paste(addr_1, addr_2), addr_1)) %>% 
	mutate(address = if_else(city!='', 
		paste(address, city, sep=', '), address)) %>% 
	mutate(address = if_else(state_abrv!='', 
		paste(address, state_abrv, sep=', '), address)) %>% 
	mutate(address = if_else(zip!='', 
		paste(address, zip, sep=', '), address)) %>%
	select(curr_oper_id, address) 
df <- 
	modeled %>% 
	select(api_no, county, state) %>% 
    left_join(desc, by='api_no') %>% 
    left_join(addresses_to_google, by='curr_oper_id') %>% 
    rename(name = curr_oper_name)
output_file <- file.path(ddir, 'matches', 'addresses', 
    'modeled_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)