# Created by Michael Cahana in early February 2018
# Geocodes addresses of specified file and determines matches accordingly
# To be called by master.csv

#===========
# inputs: 
#===========
# df tibble
# already_coded_addresses list
# output_file string

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
library(fst)
library(stringi)
library(googleway)
library(tictoc)

#===========
# functions
#===========

geocode <- function(address, row) {
	print(paste(row, address, sep=' - ')) 
	google_output <- google_geocode(address = address, simplify = TRUE) 
	coded_address <- google_output$results$formatted_address %>% .[1]

	if(is.null(coded_address)) {
		return ('error')
	}
	return (coded_address)
}

code_address_chunk <- function(address_chunk) {
	cat('\n ************ new chunk ************ \n') 
	print(dim(address_chunk))
	cat('*********************************** \n') 
	address_chunk <- 
		address_chunk %>% 
		mutate(row = row_number()) %>% 
		rowwise() %>% 
		mutate(coded_address = geocode(address, row)) %>% 
		select(-c(n, row)) 
	write_csv(address_chunk, file.path(ddir, 'address_backups', 
		'coded_addresses.csv'), append=T)
}

alpha_order <- function(name, match, order) {
    vec <- c(name, match)
    a1 <- sort(vec)[order]
    return(a1)
}

match_addresses <- function(df, already_coded_addresses, output_file) {
	#===========
	# clean PO boxes
	#===========

	df <- 
		df %>% 
		filter(address!='') %>% 
		mutate(address =toupper(address)) %>% 
		mutate(address = str_trim(address)) %>% 
		mutate(address = str_squish(address)) 

	# extract PO Boxes
	po_variations <- c('BOX', 'B OX', 'P\\.O\\.', 'P\\.O', 'PO\\.', 
			'PO', 'POB', 'P O', 'P\\. O\\.', 'DRAWER', 'BO', 'OX', 'BX', 
			'OBOX', 'POBOX')
	po_regex <- paste('\\b', 
		paste(po_variations,  
			collapse='\\b|\\b'), 
		'\\b', sep = '')

	po_num_regex <- paste(
		paste('\\b', paste(po_variations, collapse=' \\d+|\\b'), ' \\d+', 
			sep = ''), 
		paste('\\b', paste(po_variations, collapse='\\d+|\\b'), '\\d+', 
			sep = ''), 
		sep='|')

	po <- 
		df %>% 
		filter(str_detect(address, po_regex)) %>% 
		mutate(address = str_replace_all(address, '#', '')) %>% 
		mutate(city = str_extract(address, ',(.*)')) %>% 
		mutate(city = str_replace_all(city, ',', '')) %>% 
		mutate(zip = str_extract(city, '\\d+')) %>% 
		mutate(city = str_replace_all(city, '[\\d-]+', '')) %>% 
		mutate(city = str_squish(str_trim(city))) %>% 
		mutate(city = str_squish(str_trim(city))) %>% 
		mutate(po = str_extract(address, po_num_regex) %>% 
			str_extract('\\d+')) %>% 
		# drop outliers (ex: PO DRAWER V, PO BOX [blank])
		filter(!is.na(po)) %>% 
		mutate(coded_address = paste(po, city, zip)) %>% 
		select(name, address, coded_address) 

	write_csv(unique(select(po, address, coded_address)), 
		file.path(ddir, 'address_backups', 'coded_pos.csv'))

	#===========
	# geocode
	#===========

	# clean addresses
	non_po <- 
		df %>% 
		# drop PO boxes
		filter(!str_detect(address, po_regex)) %>% 
		# remove other outliers
		filter(!substring(address, 0,1)==' ') %>% 
		filter(!substring(address, 0,1)==',') %>% 
		filter(!substring(address, 0,1)=='-') %>% 
		filter(!substring(address, 0,1)=='(') %>% 
		# clean up formatting to remove duplicates
		mutate(address = str_replace_all(address, '%', '')) %>% 
		mutate(address = str_replace_all(address, '\\*', '')) %>%
		mutate(address = str_replace_all(address, '\\+', '')) %>%  
		mutate(address = str_replace_all(address, '£', '')) %>% 
		mutate(address = str_replace_all(address, '&', '')) %>%
		mutate(address = str_replace_all(address, '#', '')) 

	# set Google Maps API Key (specified in paths.R)
	set_key(google_api_key)

	coded_addresses <- 
		non_po %>% 
		filter(!is.na(address)) %>%
		mutate(address = str_trim(str_squish(address))) %>%
		filter(!(address %in% already_coded_addresses)) %>% 
		count(address) 
	if (dim(coded_addresses)[1]>0) {
		# divide coded addresses into chunks of ~500 rows, such that we save 
		# geocoding results in increments instead of all at once
		coded_addresses <- split(coded_addresses, 
			seq(1,dim(coded_addresses)[1] %/% 500))
		# geocode chunk by chunk
		coded_addresses <-
			coded_addresses %>%  
			lapply(code_address_chunk) %>% 
			bind_rows()
	}

	#===========
	# determine matches 
	#===========

	coded_addresses <- 
		read_csv(file.path(ddir, 'address_backups', 'coded_addresses.csv')) %>% 
		bind_rows(read_csv(file.path(ddir, 'address_backups', 'coded_pos.csv')))

	df <- 
		bind_rows(select(po, name, address), select(non_po, name, address)) %>% 
		left_join(coded_addresses, by='address') %>% 
		filter(!is.na(coded_address)) %>% 
		filter(coded_address!='error') 

	bad_addresses <- 
		df %>% 
		group_by(coded_address) %>% 
		summarize(n = n_distinct(name)) %>% 
		filter(n==1) %>% 
		pull(coded_address)

	df <- 
		df %>% 
		filter(!(coded_address %in% bad_addresses))

	df <- 
		split(df$name, df$coded_address) %>% 
		lapply(unique)

	# enumerate every distinct 1:1 match of operators (n choose 2) within
	# a group of operators sharing the same coded address
	# although we hate for loops, we need to keep the list object and its title
	# and i didn't know how to preserve both in an apply call 
	master <- tibble(name = NA, match = NA)
	for (i in 1:length(df)) {
		address_group <- df[[i]]
		address <- names(df[i])
		row <- 
			address_group %>% 
			combn(2) %>% 
			t() %>% 
			as_tibble() %>% 
			setNames(c('name', 'match')) %>% 
			mutate(address = address)
		master <- bind_rows(master, row)
	}

	# sort matches alphabetically to remove duplicates, save output
	master <- 
		master %>% 
		rowwise() %>% 
		mutate(a1 = alpha_order(name, match, 1)) %>% 
	    mutate(a2 = alpha_order(name,  match, 2)) %>% 
	    select(a1, a2, address) %>% 
	    rename(name = a1, match = a2) %>%  
	    na.omit() %>% 
	    distinct(name, match, .keep_all = T)

	write_csv(master, output_file) 
}