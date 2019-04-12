# Modified by Michael Cahana in early Apr. 2019
# Generates dictionary of valid names

#===========
# INPUTS
# pden_desc-2018-09-26.fst
# modeled_prices.Rds
# landtrac_tx.Rds
# female_names.txt
# male_names.txt
# surnames.csv
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
library(stringi)
library(fst)
library(readxl)
library(sf)
library(readtext)

#===========
# read in 
#===========

desc <- read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst'), 
    columns = c('api_no', 'curr_oper_id', 'curr_oper_no', 
    	'curr_oper_name', 'common_oper_name')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        stri_pad_right(14, 0)) 
modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) 
leases <- 
	readRDS(file.path(rdir, 'leases', 'landtrac_tx.Rds')) %>% 
	st_set_geometry(NULL) %>% 
	as_tibble()
female_names <- 
	readtext(file.path(rdir, 'human_names', 'female_names.txt')) %>% 
	select(text) %>% 
	separate_rows(text, sep = '\n') %>% 
	slice(-1:-6) %>%
	mutate(text = str_extract(text, '\\w+')) %>% 
	rename(name = text)
male_names <- 
	readtext(file.path(rdir, 'human_names', 'male_names.txt')) %>% 
	select(text) %>% 
	separate_rows(text, sep = '\n') %>% 
	slice(-1:-6) %>%
	mutate(text = str_extract(text, '\\w+')) %>% 
	rename(name = text)
surnames <- read_csv(file.path(rdir, 'human_names', 'surnames.csv'))

#===========
# select names
#===========

leases_words <- 
	leases %>% 
	count(grnte_al) %>% 
	arrange(desc(n)) %>% 
	mutate(cum_pct = cumsum(n)/sum(n)) %>% 
	filter(cum_pct<=.95) %>% 
	pull(grnte_al)

modeled_words <- 
    modeled %>% 
    select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
    inner_join(desc, by='api_no') %>% 
    count(common_oper_name) %>% 
    arrange(desc(n)) %>% 
	mutate(cum_pct = cumsum(n)/sum(n)) %>% 
	filter(cum_pct<=.95) %>% 
	pull(common_oper_name)

#===========
# specify common words
#===========

common_words <- c('PROD', 'INC', 'CORP', 'CORPORATION', 'CO', 'COMPANY', 
	'LLC', 
    'ENERGY', 'OIL', 'GAS', 'O&G', 'OG', '&', 'OPERATIONS', 'PRODUCTIONS', 
    'ENGY', 'ENGINEERING', 'BOB', 'CONSULTING', 'ROYALTIES', 'EP', 
    'LM', 'CHARLES', 'CRAIG', 'RESERVES', 'ROBERT', 'SCOTT', 'STEVEN', 
    'HOLDING', 'ACQUISITION', 'CONSULTANTS', 'CONSULTANT', 'TRUST',
    'GREG', 'LE', 'DIVERSIFIED', 
    'ENERGY', 'ROYALTY', 'TEXAS', 'PETR', 'TOM', 'RANDY', 'PATRICIA', 'MARK', 
    'SERV', 'MINERAL', 'MIN', 'OPERATING', 'RESOURCES', 'LTD', 'LIMITED', 
    'WELL', 'OPERATOR', 'PRODUCTION', '', ' ', 'AND', 'THE', 'COMPANY', 'USA', 
    'PETROLEUM', 'JR', 'MANAGEMENT', 'MGMT', 'ET', 'AL', 'DRILLING', 'ETAL', 
    'CONSTRUCTION', 'ASSOC', 'PRODUCING', 'SYNDICATE', 'EXPLORATION', 'DRLG', 
    'OF', 'STATE', 'EXPL', 'PETRO', 'LAND', 'DRILLIN', 'INVESTMENTS', 
    'INVESTMENT', 'DEVELOPMENT', 'NATIONAL', 'PROPERTIES', 'SERVICES', 'LP', 
    'PIPELINE', 'MINING', 'DEV', 'GROUP', 'AMERICAN', 'REFINING', 'REFINERY', 
    'VALLEY', 'EL', 'SERVICE', 'PUMPING', 'PARTNERS', 'OILFIELD', 'BROTHERS', 
    'FUNDS', 'PET', 'COAST', 'CRUDE', 'SANDS', 'FAMILY', 'ASSOCIATES', 
    'PARTNERSHIP', 'MINERALS', 'ESTATE', 'US', 'MRS', 'MR', 'BROS', 'LEASE', 
    'SOLUTIONS', 'OPER', 'NATURAL', 'ENTERPRISES', 'ENTERPRISE', 
    'INTERNATIONAL', 'DBA', 'INCORPORATED', 'UNITED', 'STATES', 'SONS', 'SON', 
    'I', 'II', 'III', 'RES', 'CAPITAL', 'FARM', 'FARMS', 'CREEK', 'STREET', 
    'HILL', 'HOLDINGS', 'OP', 'SUPPLY', 'E&P', 'LC', 'RESOURCE', 
    'PRODUCERS', 'OPERATORS', 'UPSTREAM', 'MGMNT', 'INTERESTS', 'COAL', 
    'ROCK', 'RIVER', 'CATTLE', 'LAND', 'VENTURES', 'RIDGE', 'RUN', 'LAKE', 
    'WILLIAM', 'JOHN', 'GEORGE', 'DAVE', 'MICHAEL', 'ROBERT', 
    'RICHARD', 'JAMES', 'DAVID', 'JAMES', 'HENRY', 'JACK', 'STEPHEN', 'THOMAS', 
    'RONALD', 'LARRY', 'DONALD', 'RALPH', 'PIPE', 'FRANK', 'SALES', 'KENNETH', 
    'DON', 'RAY',  'HAROLD', 'DALE', 'MARY', 'BASIN', 'LEASING', 'FUND', 
    'EQUITIES', 'VENTURE', 'AMERICA', 'PERMIAN', 'MARCELLUS', 'UTICA', 
    'HAYNESVILLE-BOSSIER', 'HAYNESVILLE', 'BOSSIER', 'BARNETT', 'WOODFORD', 
    'EAGLE', 'FORD', 'FAYETTEVILLE', 'NIOBRARA', 'BAKKEN', 'ANTRIM', 'CENTURY', 
    'WESTERN', 'WEST', 'NORTHERN', 'NORTH', 'SOUTH', 'SOUTHERN', 'EAST', 
    'EASTERN', 'NEW')

#===========
# join all words 
#===========

words <- 
	c(common_words, leases_words, modeled_words, 
		pull(female_names), pull(male_names)) %>% 
	enframe(name = NULL, value = 'word') %>% 
	mutate(word = str_replace_all(word, '[[:punct:]]', '')) %>% 
	separate_rows(word, sep = ' ') %>% 
	mutate(word = toupper(word)) %>% 
	distinct() %>% 
	filter(word != '')
	
write_csv(words, file.path(ddir, 'words_dict.csv'))

