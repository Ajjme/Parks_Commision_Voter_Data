source("./02_Scripts/000_Init.R")
### Inputs------------------
#may have to change downloaded name
ccc_voter_data <- read_tsv("../../Voter_data_analysis/Contra_Costa/6000RP2_20240711_105525_DeschambaultLyndaAnn.txt") %>% 
  clean_names()
#has email address 
# has "registration_date"   
#"birth_date"           "birth_place"          "party_name"  "language"
#vbm_program_status vote by mail status
# use distric_name_#
#pull info on voting method

ccc_voter_data_2 <- read_tsv("../../Voter_data_analysis/Contra_Costa/MVMJ004_6000RP2.txt") %>% 
  clean_names()
hist_ccc_voter_data <- read_tsv("../../Voter_data_analysis/Contra_Costa/MVMJ004_Hist_20240711_104944.txt") %>% 
  clean_names() %>% 
  mutate(dt_election_date = mdy(dt_election_date))

date_counts <- hist_ccc_voter_data %>%
  count(sz_election_desc)

PrecinctList <- read_tsv("../../Voter_data_analysis/Contra_Costa/PrecinctList_6000RP2.txt") %>% 
  clean_names()
