source("./02_Scripts/000_Init.R")
### Inputs------------------
#may have to change downloaded name
ala_voter_data <- read_tsv("../../Voter_data_analysis/Alameda/MultiPurposeVoterFile-EBRPD-District-2.txt") %>% 
  clean_names() %>% 
  select(voter_id, 
         most_recent_precinct = precinct,
         "name_prefix"        ,                                                    
         "name_last"          ,                                                    
         "name_first"         ,
         
         #address info 1 not used
         house_number,
         "apartment_number"   ,
         # "city"               ,                                                    
         # "state"              ,                                                    
         # "zip"                , 
         
         "precinct"           ,
         "phone_1"            ,                                                    
         "phone_2" ,
         
         #Vote filters
         "last_voted",
         "party"        ,                                                          
         "reg_date",
         reg_date_original,
         "military"     ,                                                          
         "gender"       ,                                                          
         "pav"  ,
         "birth_place"  ,                                                          
         "birth_date" ,
         "ltd"          ,   #last transaction date                                                             
         "language",
         "precinct_name",
         
         #Address 2? (this section will be used)
         "mail_street"  ,                                                          
         "mail_city"    ,                                                          
         "mail_state"   ,                                                          
         "mail_zip"     ,                                                          
         "mail_country",
         
         "email" ,
         "x06_03_05_2024_2024_presidential_primary_election_3183"                  ,
         "x08_11_08_2022_november_8_2022_statewide_general_election_3180"          ,
         "x14_06_07_2022_june_7_2022_statewide_direct_primary_election_3162"       ,
         "x24_09_14_2021_september_14_2021_california_gubernatorial_recall_el_3165",
         "x30_11_03_2020_2020_presidential_election_3130"                          ,
         "x36_03_03_2020_2020_presidential_primary_election_3001"                  ,
         "x47_11_06_2018_2018_statewide_general_election_2841"                     ,
         "x48_06_05_2018_2018_statewide_direct_primary_election_2818"              ,
         "x52_11_08_2016_2016_general_election_127"                                ,
         "x53_06_07_2016_presidential_primary_election_126"  )


df <- ala_voter_data

df_clean <- df %>%
    # Replace values in columns to "A", "V", or "N"
    #mutate(across(starts_with("x"), ~ gsub("[^AVN]", "", .))) %>%
   mutate(across(starts_with("x"), ~ substr(., 1, 1))) %>%
  #using reg date and throwing all others out
  mutate(
    across(
      starts_with("x"),
      .fns = list(
        eligibility = ~ ifelse(
          as.Date(reg_date_original, format = "%m/%d/%Y") <= as.Date(str_extract(cur_column(), "\\d{2}_\\d{2}_\\d{4}"), format = "%m_%d_%Y"),
          ., # Retain the original value if eligible
          "not eligible" # Mark as "not eligible" if not eligible
        )
      ),
      .names = "{.col}_eligibility"
    )
  ) %>%
    # Create new columns based on the conditions
  select(        -x06_03_05_2024_2024_presidential_primary_election_3183                ,
                 -x08_11_08_2022_november_8_2022_statewide_general_election_3180          ,
                 -x14_06_07_2022_june_7_2022_statewide_direct_primary_election_3162       ,
                 -x24_09_14_2021_september_14_2021_california_gubernatorial_recall_el_3165,
                 -x30_11_03_2020_2020_presidential_election_3130                         ,
                 -x36_03_03_2020_2020_presidential_primary_election_3001                 ,
                 -x47_11_06_2018_2018_statewide_general_election_2841                    ,
                 -x48_06_05_2018_2018_statewide_direct_primary_election_2818              ,
                 -x52_11_08_2016_2016_general_election_127                               ,
                 -x53_06_07_2016_presidential_primary_election_126  ) 

df_calc <- df_clean %>%
    mutate(
      percent_voted_by_mail    = rowMeans(across(ends_with("eligibility"), ~ . == "A") / 
                                         rowSums(across(ends_with("eligibility"), ~ . %in% c("A", "V", "N", "not eligible", NA )))),
      percent_voted_by_primary = rowMeans(across(contains("primary"), ~ . %in% c("A", "V") & str_detect(cur_column(), "primary"))),
      count_times_voted        = rowSums(across(starts_with("x"), ~ . %in% c("A", "V"))),
      voting_opportunities     = rowSums(across(starts_with("x"), ~ . %in% c("A", "V", "N"))),
      voted_vs_opportunities   = count_times_voted/voting_opportunities,
      voted_in_2024_primary    = ifelse(df_clean[["x06_03_05_2024_2024_presidential_primary_election_3183_eligibility"]] %in% c("A", "V"), "Yes", "No"),
      voted_in_2020_general    = ifelse(df_clean[["x30_11_03_2020_2020_presidential_election_3130_eligibility"]] %in% c("A", "V"), "Yes", "No"),
      percent_voted_by_mail    = ifelse(
        is.na(percent_voted_by_mail) , 
        0, (percent_voted_by_mail*10))
    )%>%
  # Create a new column 'recently_updated'
  mutate(
    recently_updated = if_else(
      grepl("2024", reg_date) | grepl("2024", ltd),
      "Yes",
      "No"
    ))
  #add column fro recent register?



saveRDS(df_calc, file = "alameda_processed_voter_data.rds")
                      
        # Does A mean Absentee ballet, N mean not voted and V means voted? A) A Does mean Absentee (vote by mail),  N means did not vote, and V means voted.

  
