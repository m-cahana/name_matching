# To run this makefile, make sure you have a local data.txt that holds 
# the directory path to the data

# ===========================================================================
# establish directory paths
# ===========================================================================
# load in a local .txt file that holds the directory path to the data
ifeq ($(OS), Windows_NT)
	DATA = $(file < data.txt)
else
	DATA = $(shell cat data.txt)
endif

DIR = ${CURDIR}

CDIR = $(DIR)/code
CDIR_prep = $(CDIR)/prep
CDIR_matching = $(CDIR)/matching
CDIR_review = $(CDIR)/review
CDIR_pre_screen = $(CDIR)/pre_screen
CDIR_grouping = $(CDIR)/grouping
CDIR_markdown_summary = $(CDIR)/markdown_summary
ODIR = $(DIR)/output

# data files
DATA_raw = $(DATA)/raw_data
DATA_gen = $(DATA)/generated_data
DATA_rev = $(DATA)/reviewed_data

INSTALL := $(shell Rscript $(CDIR)/package_installation.R)

all : $(CDIR_markdown_summary)/name_matching_summary.html

# ===========================================================================
# First-stage name match dependencies
# ===========================================================================

$(DATA_gen)/matches/names/modeled_name_matches.csv: \
	$(DATA_raw)/pden_desc-2018-09-26.fst \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/names_edited.xlsx
	Rscript $(CDIR_matching)/match_modeled_names.R 

$(DATA_gen)/matches/names/leases_name_matches.csv: \
	$(DATA_raw)/leases/landtrac_tx.Rds
	Rscript $(CDIR_matching)/match_leases_names.R 

# ===========================================================================
# First-stage address match dependencies
# ===========================================================================

$(DATA_gen)/matches/addresses/modeled_address_matches.csv: \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/addresses/nph_oper_addr-2017-04-30.Rdata 
	Rscript $(CDIR_matching)/match_modeled_addresses.R 

$(DATA_gen)/matches/addresses/leases_address_matches.csv: \
	$(DATA_raw)/leases/landtrac_tx.Rds 
	Rscript $(CDIR_matching)/match_leases_addresses.R 

# ===========================================================================
# Address/pre-checked matches as verification for name matches 
# ===========================================================================

$(DATA_rev)/modeled_matches.csv: \
	$(DATA_raw)/pden_desc-2018-09-26.fst \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/names_edited.xlsx \
	$(DATA_gen)/matches/addresses/modeled_address_matches.csv \
	$(DATA_gen)/matches/names/modeled_name_matches.csv 
	Rscript $(CDIR_pre_screen)/pre_screen_modeled_names.R

$(DATA_rev)/leases_matches.csv: \
	$(DATA_raw)/leases/landtrac_tx.Rds \
	$(DATA_gen)/matches/names/leases_name_matches.csv \
	$(DATA_gen)/matches/addresses/leases_address_matches.csv
	Rscript $(CDIR_pre_screen)/pre_screen_leases_names.R

# ===========================================================================
# Group all matches
# ===========================================================================

$(DATA_gen)/grouped_matches/all_groups.csv: \
	$(DATA_rev)/leases_matches.csv \
	$(DATA_rev)/modeled_matches.csv 
	Rscript $(CDIR_grouping)/group_all_matches.R

# ===========================================================================
# Check for clusters that refer to same entity
# ===========================================================================

$(DATA_rev)/group_name_matches.csv: \
	$(DATA_gen)/grouped_matches/all_groups.csv 
	Rscript $(CDIR_matching)/match_group_names.R 

# ===========================================================================
# Group together duplicate clusters
# ===========================================================================

$(DATA_gen)/grouped_matches/grouped_groups.csv : \
	$(DATA_gen)/grouped_matches/all_groups.csv \
	$(DATA_rev)/group_name_matches.csv
	Rscript $(CDIR_grouping)/group_grouped_clusters.R 

# ===========================================================================
# Generate summary file
# ===========================================================================

$(CDIR_markdown_summary)/name_matching_summary.html : \
	$(DATA_rev)/group_name_matches.csv \
	$(DATA_rev)/modeled_matches.csv \
	$(DATA_rev)/leases_matches.csv \
	$(DATA_gen)/grouped_matches/grouped_groups.csv
	Rscript $(CDIR_markdown_summary)/generate_name_matching_summary.R 

