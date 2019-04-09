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
  read_csv(file.path(dropbox, 'archive', 'lease_match_sample.csv')) %>%
  filter(!is.na(keep))

#===========
# example grf use
#===========
rf <-
  paste("shared_words", "cosine_similarity", "dist_score", sep = "+") %>%
  paste("keep", ., sep = "~") %>%
  as.formula %>%
  regression_forest2(df)

fig <-
  rf %>%
  predict %>%
  as_tibble %>%
  ggplot(aes(x = predictions)) + geom_histogram()

