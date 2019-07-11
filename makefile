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
CDIR_functions = $(CDIR)/functions
ODIR = $(DIR)/output

# data files
DATA_raw = $(DATA)/raw_data
DATA_gen = $(DATA)/generated_data
DATA_rev = $(DATA)/reviewed_data
lease_csvs = $(DATA_raw)/leases/csvs
INSTALL := $(shell Rscript $(CDIR)/package_installation.R)

all : $(DATA_gen)/notifications/name_matching_summary.html

# ===========================================================================
# Lease aggregation
# ===========================================================================

$(DATA_raw)/leases/all_leases.Rds: \
	$(CDIR_prep)/combine_leases.R \
	$(lease_csvs)
	Rscript $<

# ===========================================================================
# First-stage name match dependencies
# ===========================================================================

$(DATA_gen)/matches/names/modeled_name_matches.csv: \
	$(CDIR_matching)/match_modeled_names.R \
	$(DATA_raw)/pden_desc-2018-09-26.fst \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/names_edited.xlsx \
	$(CDIR_functions)/match_names.R \
	$(CDIR_functions)/utils.R
	Rscript $<

$(DATA_gen)/matches/names/leases_name_matches.csv: \
	$(CDIR_matching)/match_leases_names.R \
	$(DATA_raw)/leases/all_leases.Rds \
	$(CDIR_functions)/match_names.R \
	$(CDIR_functions)/utils.R
	Rscript $<

# ===========================================================================
# First-stage address match dependencies
# ===========================================================================

$(DATA_gen)/matches/addresses/modeled_address_matches.csv: \
	$(CDIR_matching)/match_modeled_addresses.R \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/addresses/nph_oper_addr-2017-04-30.Rdata \
	$(CDIR_functions)/match_addresses.R \
	$(CDIR_functions)/utils.R
	Rscript $<

$(DATA_gen)/matches/addresses/leases_address_matches.csv: \
	$(CDIR_matching)/match_leases_addresses.R \
	$(DATA_raw)/leases/all_leases.Rds \
	$(CDIR_functions)/match_addresses.R \
	$(CDIR_functions)/utils.R
	Rscript $<

# ===========================================================================
# Address/pre-checked matches as verification for name matches
# ===========================================================================

$(DATA_rev)/modeled_matches.csv: \
	$(CDIR_pre_screen)/pre_screen_modeled_names.R \
	$(DATA_raw)/pden_desc-2018-09-26.fst \
	$(DATA_raw)/modeled_prices.Rds \
	$(DATA_raw)/names_edited.xlsx \
	$(DATA_gen)/matches/addresses/modeled_address_matches.csv \
	$(DATA_gen)/matches/names/modeled_name_matches.csv \
	$(CDIR_functions)/pre_screen_names.R \
	$(CDIR_functions)/random_forest_utils.R \
	$(CDIR_functions)/utils.R
	Rscript $<

$(DATA_rev)/leases_matches.csv: \
	$(CDIR_pre_screen)/pre_screen_leases_names.R \
	$(DATA_raw)/leases/all_leases.Rds \
	$(DATA_gen)/matches/names/leases_name_matches.csv \
	$(DATA_gen)/matches/addresses/leases_address_matches.csv \
	$(CDIR_functions)/pre_screen_names.R \
	$(CDIR_functions)/random_forest_utils.R \
	$(CDIR_functions)/utils.R
	Rscript $<

# ===========================================================================
# Group all matches
# ===========================================================================

$(DATA_gen)/grouped_matches/all_groups.csv: \
	$(CDIR_grouping)/group_all_matches.R \
	$(DATA_rev)/leases_matches.csv \
	$(DATA_rev)/modeled_matches.csv \
	$(CDIR_functions)/group_matches.R \
	$(CDIR_functions)/utils.R
	Rscript $<

# ===========================================================================
# Check for clusters that refer to same entity
# ===========================================================================

$(DATA_rev)/group_name_matches.csv: \
	$(CDIR_matching)/match_group_names.R \
	$(DATA_gen)/grouped_matches/all_groups.csv \
	$(CDIR_functions)/match_names.R \
	$(CDIR_functions)/utils.R
	Rscript $<

# ===========================================================================
# Group together duplicate clusters
# ===========================================================================

$(DATA_gen)/grouped_matches/grouped_groups.csv : \
	$(CDIR_grouping)/group_grouped_clusters.R \
	$(DATA_gen)/grouped_matches/all_groups.csv \
	$(DATA_rev)/group_name_matches.csv \
	$(CDIR_functions)/group_matches.R \
	$(CDIR_functions)/utils.R
	Rscript $<

# ===========================================================================
# Generate summary file
# ===========================================================================

$(DATA_gen)/notifications/name_matching_summary.html : \
	$(CDIR_markdown_summary)/generate_name_matching_summary.R \
	$(DATA_gen)/grouped_matches/grouped_groups.csv \
	$(CDIR_markdown_summary)/name_matching_summary.Rmd
	Rscript $<
