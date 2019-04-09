# Thom Covert, April 2019
# example use of grf to possibly classify our name pairs better

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

# TC's random forest utilities
source(file.path(root, "code", "functions", "random_forest_utils.R"))

#===========
# needed libraries
#===========
library(tidyverse)
library(grf)

#===========
# Michael Cahana's canned and pre-labeled pairs
#===========
df <- 
  c(paste(file.path(dropbox, 'archive'), 
    c('lease_match_sample.csv', 'new_lease_sample.csv'), sep='/')) %>% 
  map_df(read_csv) %>% 
  filter(!is.na(keep)) %>% 
  distinct(name, match, .keep_all = T)

#===========
# example grf use
#===========
rf <-
  paste("shared_words", "cosine_similarity", "jw_distance", sep = "+") %>%
  paste("keep", ., sep = "~") %>%
  as.formula %>%
  regression_forest2(df)

fig <-
  rf %>%
  predict %>%
  as_tibble %>%
  bind_cols(df) %>% 
  ggplot(aes(x = predictions, fill = as.factor(keep))) + 
  geom_histogram(position = 'dodge', binwidth = .02) + 
  scale_x_continuous(breaks = seq(.0,1,.1)) + 
  scale_fill_discrete(name = "keep")

