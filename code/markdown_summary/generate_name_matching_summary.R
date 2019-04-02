# Created by Michael Cahana in early Apr. 2019

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

library(rmarkdown)

#===========
# render markdown file
#===========

render(file.path(root, 'code', 'markdown_summary', 'name_matching_summary.Rmd'))