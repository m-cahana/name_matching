# Created by Michael Cahana in mid Feb. 2019
# Determines modeled name matches

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

source(file.path(root, 'code', 'functions', 'match_names.R'))

#===========
# data read in
#===========

desc <- read_fst(file.path(rdir, 'pden_desc-2018-09-26.fst'), 
    columns = c('api_no', 'curr_oper_id', 'curr_oper_no', 
        'common_oper_name')) %>% 
        mutate(api_no = str_replace_all(api_no, '-', '') %>% 
        stri_pad_right(14, 0)) 
modeled <- readRDS(file.path(rdir, 'modeled_prices.Rds')) 
cleaned_300 <- read_excel(file.path(rdir, 'names_edited.xlsx')) %>% 
    select(common_oper_name, replacement)

#===========
# match names within modeled
#===========

df <- 
    modeled %>% 
    select(api_no, county, state, shale_play, total_prod, price_per_boe) %>% 
    inner_join(desc, by='api_no') %>% 
    left_join(cleaned_300, by='common_oper_name') %>% 
    mutate(replacement = 
        if_else(is.na(replacement), common_oper_name, replacement)) %>% 
    select(-common_oper_name) %>% 
    rename(name = replacement)
output_file <- file.path(ddir, 'matches', 'names', 'modeled_name_matches.csv')

match_names(df, output_file)