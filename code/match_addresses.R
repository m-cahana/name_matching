# Created by Michael Cahana in early February 2018
# Geocodes addresses of specified file and determines matches accordingly
# To be called by master.csv

#===========
# inputs
# df tibble
# file_name string
# already_coded_addresses list
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
library(fst)
library(stringi)
library(googleway)
library(tictoc)

#===========
# functions
#===========

geocode <- function(address) {
	google_output <- google_geocode(address = address, simplify = TRUE) 
	coded_address <- google_output$results$formatted_address %>% .[1]

	if(is.null(coded_address)) {
		return ('error')
	}
	return (coded_address)
}

alpha_order <- function(name, match, order) {
    vec <- c(name, match)
    a1 <- sort(vec)[order]
    return(a1)
}

#===========
# geocode
#===========

set_key(google_api_key)

tic()
coded_addresses <- 
	df %>% 
	filter(!is.na(address)) %>%
	filter(!(address %in% already_coded_addresses)) %>% 
	count(address) 
if (dim(coded_addresses)[1]>0) {
	coded_addresses <- 
		coded_addresses %>% 
		rowwise() %>%
		mutate(coded_address = geocode(address)) %>% 
		select(-n)
	write_csv(coded_addresses, file.path(ddir, 'coded_addresses.csv'), append=T)
}
toc()

coded_addresses <- read_csv(file.path(ddir, 'coded_addresses.csv'))

#===========
# determine matches 
#===========

df <- 
	df %>% 
	left_join(coded_addresses, by='address') %>% 
	filter(!is.na(coded_address)) %>% 
	filter(coded_address!='error')

df <- 
	split(df$curr_oper_name, df$coded_address) %>% 
	lapply(unique)

master <- tibble(name = NA, match = NA)
for (a in 1:length(df)) {
	address_group <- df[[a]]
	address <- names(df[a])
	# print(address)
	for (i in 1:length(address_group)) {
		for (j in 1:length(address_group)) {
			if (i!=j) {
				row <- tibble(name = address_group[i], match = address_group[j], 
					address = address, method = 'geocode')
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
    select(a1, a2, address, method) %>% 
    rename(name = a1, match = a2) %>%  
    na.omit() %>% 
    distinct(name, match, .keep_all = T)

write_csv(master, output_file)