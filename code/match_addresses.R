# Created by Michael Cahana in early February 2018
# Matches operator names within our modeled data by checking whether 
# their addresses match
library(sf)
library(tidyverse)
library(fst)
library(lubridate)
library(stringi)
library(googleway)

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

desc <- read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        	stri_pad_right(14, 0)) %>% 
        select(api_no, curr_oper_id, curr_oper_name)

modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) %>% 
    select(api_no, county, state) 

# named: nph_oper_addr
load(file.path(rdir, 'nph_oper_addr-2017-04-30.Rdata'))

#===========
# functions
#===========

geocode <- function(address) {
	google_output <- google_geocode(address = address, simplify = TRUE)
	coded_address <- google_output$results$formatted_address
	return(coded_address[1])
}


alpha_order <- function(name, match, order) {
    vec <- c(name, match)
    a1 <- sort(vec)[order]
    return(a1)
}

#===========
# data preparation
#===========

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
	select(curr_oper_id, address)

modeled <- 
	modeled %>% 
    left_join(desc, by='api_no') %>% 
    left_join(addresses_to_google, by='curr_oper_id')

#===========
# geocode
#===========

set_key(google_api_key)

tic()
coded_addresses <- 
	modeled %>% 
	filter(!is.na(address)) %>%
	count(address) %>% 
	arrange(desc(n)) %>% 
	slice(1:100) %>% 
	rowwise() %>% 
	mutate(coded_address = geocode(address)) %>% 
	select(-n)
toc()

# later: 
# tic()
# coded_addresses <- 
# 	modeled %>% 
# 	filter(!is.na(address)) %>%
# 	count(address) %>% 
# 	rowwise() %>% 
# 	mutate(coded_address = geocode(address)) %>% 
# 	select(-n)
# toc()

write_csv(coded_addresses, file.path(ddir, 'coded_addresses.csv'))

#===========
# determine matches 
#===========

modeled <- 
	modeled %>% 
	left_join(coded_addresses, by='address') %>% 
	filter(!is.na(coded_address))

modeled <- 
	split(modeled$curr_oper_name, modeled$coded_address) %>% 
	lapply(unique)

master <- tibble(name = NA, match = NA)
for (address_group in modeled) {
	for (i in 1:length(address_group)) {
		for (j in 1:length(address_group)) {
			if (i!=j) {
				row <- tibble(name = address_group[i], match = address_group[j])
				master <- bind_rows(master, row)
			}
		}
	}
}

master <- 
	master %>% 
	rowwise() %>% 
	mutate(a1 = alpha_order(name, match, 1)) %>% 
    mutate(a2 = alpha_order(name,  match, 2)) %>% 
    select(a1, a2) %>% 
    rename(name = a1, match = a2) %>% 
    unique() %>% 
    na.omit() %>% 
    mutate(method = 'geocode')

write_csv(master, file.path(ddir, 'address_matches.csv'))




