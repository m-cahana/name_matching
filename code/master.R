# Created by Michael Cahana in mid Feb. 2018
# Determines name matches given name and address inputs

#===========
# inputs
#===========
# pden_desc-2018-09-26.fst
# modeled_prices.Rds
# names_edited.xlsx
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

source(file.path(root, 'code', 'match_names.R'))
source(file.path(root, 'code', 'match_addresses.R'))
source(file.path(root, 'code', 'filter_names.R'))

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
# named: nph_oper_addr
load(file.path(rdir, 'nph_oper_addr-2017-04-30.Rdata'))
already_coded_addresses <- read_csv(file.path(ddir, 'coded_addresses.csv'))

#===========
# match names within modeled
#===========

df <- 
    modeled %>% 
    select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
    inner_join(desc, by='api_no') %>% 
    left_join(cleaned_300, by='curr_oper_name') %>% 
    mutate(replacement = 
        if_else(is.na(replacement), curr_oper_name, replacement)) %>% 
    select(-curr_oper_name) %>% 
    rename(name = replacement)
output_file <- file.path(ddir, 'matches', 'modeled_name_matches.csv')

match_names(df, output_file)

#===========
# match addresses within modeled
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)
addresses_to_google <- 
	nph_oper_addr %>% 
	as_tibble() %>% 
	rename(curr_oper_id = assoc_id) %>% 
	filter(addr_1!='') %>% 
	# drop PO boxes
	filter(!str_detect(toupper(addr_1), 'BOX')) %>% 
	filter(!str_detect(toupper(addr_1), 'P.O.')) %>%
	filter(!str_detect(toupper(addr_2), 'BOX')) %>% 
	filter(!str_detect(toupper(addr_2), 'P.O.')) %>%
	# concatenate addresses
	mutate(address = if_else(addr_2!='', paste(addr_1, addr_2), addr_1)) %>% 
	mutate(address = if_else(city!='', 
		paste(address, city, sep=', '), address)) %>% 
	mutate(address = if_else(state_abrv!='', 
		paste(address, state_abrv, sep=', '), address)) %>% 
	mutate(address = if_else(zip!='', 
		paste(address, zip, sep=', '), address)) %>%
	mutate(address = str_replace_all(address, '%', '')) %>%  
	select(curr_oper_id, address) %>% 
	mutate(address = trimws(address))
df <- 
	modeled %>% 
	select(api_no, county, state) %>% 
    left_join(desc, by='api_no') %>% 
    left_join(addresses_to_google, by='curr_oper_id')
output_file <- file.path(ddir, 'matches', 'modeled_address_matches.csv')

match_addresses(df, output_file)

#===========
# filter down name match list
#===========

name_matches <- read_csv(file.path(ddir, 'matches', 'modeled_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 
	'modeled_address_matches.csv'))
output_file <- file.path(vdir, 'modeled_name_matches.csv')

filter_names(name_matches, address_matches, output_file)