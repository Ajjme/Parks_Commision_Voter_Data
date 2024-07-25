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
  filter(year(dt_election_date) >= 2016)

# List of election dates
election_dates <- as.Date(c("2021-09-14", "2022-04-05", "2022-11-08", "2020-05-05", 
                            "2021-05-04", "2016-06-07", "2020-11-03", "2022-06-07", 
                            "2016-11-08", "2018-03-06", "2018-06-05", "2018-11-06", 
                            "2019-05-07", "2019-11-05", "2020-03-03", "2024-03-05", 
                            "2023-03-07", "2024-05-07"))

# Summarizing the dataset
summarized_data <- hist_ccc_voter_data %>%
  group_by(l_voter_unique_id) %>%
  summarize(
    most_recent_precinct = s_voting_precinct[which.max(dt_election_date)],
    percent_voted_by_mail = sum(sz_voting_method %in% c("Voted by Mail Ballot", "Voted by Absentee Ballot")) / n(),
    percent_voted_by_primary = sum(s_elec_type_desc %in% c("Special Vacated Primary", "Primary")) / n(),
    most_recent_party = sz_party_name[which.max(dt_election_date)],
    count_times_voted = n(),
    #number of times voted divided by oportunies
    voted_in_2024_primary = ifelse(any(dt_election_date == as.Date("2024-03-05")), "Yes", "No"),
    voted_in_2020_general = ifelse(any(dt_election_date == as.Date("2020-11-03")), "Yes", "No"),
    party_changed = ifelse(n_distinct(na.omit(sz_party_name)) > 1, "Yes", "No"),
    changed_to_democratic = ifelse(n_distinct(na.omit(sz_party_name)) > 1 & most_recent_party == "Democratic", "Yes", "No"),
    changed_to_green = ifelse(n_distinct(na.omit(sz_party_name)) > 1 & most_recent_party == "Green", "Yes", "No"),
    voting_opportunities = sum(election_dates >= min(dt_election_date)),
    voted_vs_opportunities = count_times_voted / sum(election_dates >= min(dt_election_date))
    
  )


PrecinctList <- read_tsv("../../Voter_data_analysis/Contra_Costa/PrecinctList_6000RP2.txt") %>% 
  clean_names() 


ccc_wide_voter <- full_join(ccc_voter_location_info, summarized_data, by = "l_voter_unique_id") %>%
  #will check this
  filter(s_precinct_id %in% PrecinctList$s_precinct_id) %>% 
  select(
    "l_voter_unique_id"         ,
    name_prefix = "s_voter_title"         ,
    name_last = "sz_name_last"             ,
    name_first = "sz_name_first"         ,

    
    "s_gender"                 ,
    "sz_situs_address"      ,
    city = "sz_situs_city"            ,
    state = "s_situs_state"         ,
    zip = "s_situs_zip"              ,
    "s_house_num"           ,
    "s_unit_abbr"              ,
    "s_unit_num"            ,
    "sz_street_name"           ,
    "s_street_suffix"       ,
    
    "sz_mail_address1"         ,"sz_mail_address2"      ,
    "sz_mail_address3"         ,"sz_mail_address4"      ,
    "sz_mail_zip"              ,"sz_phone"              ,
    "sz_email_address"         ,
    
    "dt_birth_date"         ,
    "s_birth_place"            ,
    "sz_language_name"      ,
    dt_last_update_dt
    "sz_precinct_name"      ,
    "s_precinct_id"            ,
    #calculated
    "most_recent_precinct"     ,"percent_voted_by_mail" ,
    "percent_voted_by_primary" ,"most_recent_party"     ,
    "count_times_voted"        ,"voted_in_2024_primary" ,
    "voted_in_2020_general"    ,"party_changed"         ,
    "changed_to_democratic"    ,"changed_to_green"      ,
    "voting_opportunities"     ,"voted_vs_opportunities")

# TO CHANGE NAMES TOO

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
