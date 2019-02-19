# Created by Yixin Sun in February 2019
# Read in DI's existing standardized firm names
# Normalize punctuation, spacing etc
# Use dictionary of company list created by intern and apply this mapping
	# on the DI aliases using a regex call

# ==============================================================================
# standard setup
# ==============================================================================
library(tidyverse)
library(sf)
library(lubridate)
library(fst)
library(stringi)
library(readxl)
library(fuzzyjoin)

root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

# ==============================================================================
# Reading in data
# ==============================================================================
desc <- 
  file.path(rdir, 'pden_desc-2018-09-26.fst') %>%
  read_fst(columns = c("api_no", "curr_oper_id", "curr_oper_name", 
  	"common_oper_name")) %>% 
  as_tibble() %>%
  mutate(api_no = str_replace_all(api_no, '-', ''), 
    api_no = stri_pad_right(api_no, 14, 0)) 

modeled <- 
  readRDS(file.path(rdir, 'modeled_prices.Rds')) %>% 
  select(api_no, county, state) %>%
  as_tibble() %>%
  left_join(desc, by = "api_no") %>%
  distinct()

# there are 10 current-common pairs where there is more than one common oper
  # name for a listed current operator name - some of them do look like 
  # different companies (example quicksilver vs brietburn)
  # Take DI's word for it right now that common operator name is legit
check_oper <-
  modeled %>%
  count(curr_oper_name, common_oper_name) %>%
  group_by(curr_oper_name) %>%
  filter(n() > 1) 

operators <-
  modeled %>%
  count(common_oper_name) %>%
  rename(firm = common_oper_name)


# ==============================================================================
# Functions
# ==============================================================================
# fix character encoding issues
fixstrings <- function(s) {
  return(if_else(stri_enc_mark(s) == "UTF-8",
                 stri_encode(s, from = "UTF-8", to = "ASCII"),
                 s))
}

# Normalize punctuation and get rid of common company words
to_drop <- 
  c('PROD$', 'INC$', 'CORP$', 'CORPORATION$', 'CO$', 'COMPANY$', 'LLC$', 'LP$', 
  'USA$', '^USA', 'ENERGY$', '&', '(?<=\\s)(AND)(?=\\s)', 'OPERATIONS$', 
  'PRODUCTIONS$', 'ENGY$', 'SERV$', 'MINERAL$', 'MINERALS$', 'OPERATING$', 
  'RESOURCES$', 'LTD$', 'LIMITED$', 'OPERATOR$', 'PRODUCTION$', '^THE ', 
  'PETROLEUM', '\\s(JR)', 'MANAGEMENT$', 'MGMT$', 'DRILLING$', 
  '(?<=\\s)(ET)(?=\\s)', '(?<=\\s)(AL)(?=\\s)', 'ETAL',   'ASSOCIATES$', 
  'CONSTRUCTION$', 'ASSOC$', 'SYNDICATE$', 'EXPLORATION$', 'DRLG$', 
  'EXPL$', 'PETRO$', 'DRILLIN$', 'INVESTMENTS$', 'INVESTMENT$', 
  'DEVELOPMENT$', 'PROPERTIES$', 'SERVICES$', 'PIPELINE$', 'MINING$', 
  'GROUP$', 'AMERICAN', 'VALLEY$', 'SERVICE$', 'PARTNERS$', 'PET$', 'FAMILY$', 
  'PARTNERSHIP$', 'MINERALS$', 'ESTATE$', 'OPER$', 'ENTERPRISES$', 'ENTERPRISE$', 
  'INTERNATIONAL$', 'INCORPORATED$', 'HOLDINGS$', 'OP$', 'SUPPLY$', 'E&P$', 
  'EP$', 'RESOURCE$', 'PRODUCERS$', 'OPERATORS$', 'MGMNT$','VENTURES$') %>%
  paste(., collapse = "|") %>%
  regex(ignore_case = TRUE)

clean_firm <- function(firms){
	firms %>% 
    str_replace_all('[[:punct:]]', '') %>%
    str_replace_all(to_drop, '') %>%
    str_trim() %>%
    str_squish()
}

# regex join existing company names file
regex_firm <- function(df, firm_name){
  company_list <-
    file.path(rdir,"company_regex.xlsx") %>%
    read_excel() %>%
    rename(alias_new = Alias, firm_regex = Company) %>%
    mutate(alias_new = fixstrings(alias_new),
           firm_regex = fixstrings(firm_regex)) %>%
    mutate(alias_new = str_replace(alias_new, "\032", " "))

  df %>%
    rename(firm_merge := !!firm_name) %>%
    regex_left_join(company_list,
                  by = c("firm_merge" = "firm_regex"),
                  ignore_case = T) %>%
    mutate(alias_new = if_else(is.na(alias_new), firm_merge, alias_new)) %>%
    #select(-firm_regex) %>%
    rename(!!firm_name := firm_merge) 
}

# ==============================================================================
# run basic cleaning and regex join
# ==============================================================================

operators <-
  operators %>%
  mutate(alias = clean_firm(firm)) %>%
  regex_firm("alias") %>%
  distinct()

check_regex <-
  operators %>%
  group_by(alias) %>%
  filter(n_distinct(alias_new) > 1)