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
ODIR = $(DIR)/output

# data files
DATA_raw = $(DATA)/raw_data
DATA_gen = $(DATA)/generated_data
DATA_rev = $(DATA)/reviewed_data

INSTALL := $(shell Rscript $(CDIR)/package_installation.R)

# all : FILL IN 

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
# Address matches as verification for name matches 
# ===========================================================================

$(DATA_rev)/modeled_matches.csv: \
	$(DATA_raw)/pden_desc-2018-09-26.fst \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/names_edited.xlsx \ 
	$(DATA_gen)/matches/addresses/modeled_address_matches.csv \
	$(DATA_gen)/matches/names/modeled_name_matches.csv
	Rscript $(CDIR_pre_screen)/verify_modeled_names_with_addresses.R

$(DATA_rev)/leases_matches.csv: \
	$(DATA_raw)/leases/landtrac_tx.Rds \
	$(DATA_gen)/matches/names/leases_name_matches.csv \ 
	$(DATA_gen)/matches/addresses/leases_address_matches.csv
	Rscript $(CDIR_pre_screen)/verify_leases_names_with_addresses.R

