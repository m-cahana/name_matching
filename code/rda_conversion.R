# Created by Yixin Sun in February 2019
# Converts raw shapefiles and txt files to raw Rda files

# ==============================================================================
# standard setup
# ==============================================================================
library(tidyverse)
library(sf)
library(lubridate)

root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

min_sentinel_date <- ymd(19000101)

# ==============================================================================
# convert landtrac files
# ==============================================================================
landtrac_tx <-
  list.dirs(file.path(rdir, "leases", "TX"),
             full.names = TRUE, recursive = FALSE) %>%
  paste0(., "/", basename(.), ".shp") %>%
  map(~ st_read(., stringsAsFactors = FALSE)) %>%
  reduce(rbind) %>%
  mutate_at(vars(contains("_dt")),
            funs(if_else(. < min_sentinel_date, as_date(NA), .))) %>%
  mutate(HasEffective = !is.na(effct_dt),
         effct_dt = if_else(is.na(effct_dt) & term_mos != 0,
                            expir_dt - months(term_mos), effct_dt),
         effct_dt = if_else(is.na(effct_dt) & !is.na(inst_dt),
                            inst_dt, effct_dt))%>%
  st_buffer(0)
save(landtrac_tx, file = file.path(rdir, "leases", "landtrac_tx.Rda"))


# ==============================================================================
# convert DI flatfile of addresses from .txt to Rda
# ==============================================================================
oper_addr <-
  file.path(rdir, "addresses", "NPH_OPER_ADDR.txt") %>%
  read_delim(delim = ",", col_names = FALSE) %>%
  setNames(c("assoc_id", "addr_no", "addr_type", "addr_1", "addr_2", "city", 
  	"state_abrv", "zip", "country", "phone", "fax"))
  
save(oper_addr, file = file.path(rdir, "addresses", "oper_addr.Rda"))