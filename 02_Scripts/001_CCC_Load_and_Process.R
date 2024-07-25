source("./02_Scripts/000_Init.R")
### Inputs------------------
#may have to change downloaded name
# ccc_voter_data <- read_tsv("../../Voter_data_analysis/Contra_Costa/6000RP2_20240711_105525_DeschambaultLyndaAnn.txt") %>%
#   clean_names()
#wont use as we have to join info with Alameda data and then do the filtering this one is to pre processed

#has email address 
# has "registration_date"   
#"birth_date"           "birth_place"          "party_name"  "language"
#vbm_program_status vote by mail status
# use distric_name_#
#pull info on voting method

ccc_voter_location_info <- read_tsv("../../Voter_data_analysis/Contra_Costa/MVMJ004_6000RP2.txt") %>% 
  clean_names()

hist_ccc_voter_data <- read_tsv("../../Voter_data_analysis/Contra_Costa/MVMJ004_Hist_20240711_104944.txt") %>% 
  clean_names() %>% 
  mutate(dt_election_date = mdy(dt_election_date)) %>% 
#filterd to just counted ballots
  filter(sz_counted_flag == "Yes",
         #filter to Year
         year(dt_election_date) >= 2016) %>% 
 # Create count if DEM Vote
  groupby(l_voter_unique_id) %>% 
  #I want most recent precient 
  # Percent vote by mail 
  #most recent party
  #count of times voted
  mutate()
#join on ID
ccc_wide_voter <- full_join(ccc_voter_location_info, hist_ccc_voter_data, by = "l_voter_unique_id")

PrecinctList <- read_tsv("../../Voter_data_analysis/Contra_Costa/PrecinctList_6000RP2.txt") %>% 
  clean_names()

# Steps
#Filter Hist by Precinct
#Gather the top five elections that match Alameda 2024 - 2012
# only counted are in this file pull party and voting method and unique voter ID
#Pivot wider then join to Location data set

#Filter by Precinct again (Make list of applicable)

#Standardize column names
#Select from locational data so matches alameda
#Identification
[6] "name_prefix"                                                             
[7] "name_last"                                                               
[8] "name_first"
house_number,
[18] "apartment_number"                                                        
[19] "city"                                                                    
[20] "state"                                                                   
[21] "zip"                                                                     
[22] "precinct"
[29] "phone_1"                                                                 
[30] "phone_2" 

#Vote filters
[5] "last_voted"
[26] "party"                                                                   
[27] "reg_date"
[31] "military"                                                                
[32] "gender"                                                                  
[33] "pav"  
[35] "birth_place"                                                             
[36] "birth_date" 
[43] "ltd"        #last transaction date                                                             
[44] "language"
[53] "precinct_name"

#Address
[38] "mail_street"                                                             
[39] "mail_city"                                                               
[40] "mail_state"                                                              
[41] "mail_zip"                                                                
[42] "mail_country"
[46] "email" 


date_counts <- hist_ccc_voter_data %>%
  count(sz_election_desc)