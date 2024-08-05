source("./02_Scripts/000_Init.R")
library(knitr)
### Inputs------------------
ala_data <- readRDS("alameda_processed_voter_data.rds") %>% 
  mutate(most_recent_precinct = as.character(most_recent_precinct),
         mail_zip = as.character(mail_zip),
         phone_1 = as.character(phone_1))

ccc_data <- readRDS("contra_costa_processed_voter_data.rds") %>%
  mutate(most_recent_precinct = as.character(most_recent_precinct),
         mail_zip = as.character(mail_zip),
         phone_1 = as.character(phone_1))

EBRPD_district_2_voter_data <- bind_rows(ala_data, ccc_data) %>% 
  mutate(
    percent_voted_by_mail_group = cut(
      percent_voted_by_mail * 100, 
      breaks = seq(0, 100, by = 10), 
      include.lowest = TRUE, 
      right = FALSE, 
      labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
    ),
    voted_vs_opportunities_group = cut(
      voted_vs_opportunities * 100, 
      breaks = seq(0, 100, by = 10), 
      include.lowest = TRUE, 
      right = FALSE, 
      labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
    ),
      percent_voted_by_primary_group = cut(
        percent_voted_by_primary * 100,
        breaks = seq(0, 100, by = 10),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
      )) %>% 
      mutate(
        # Convert birth_date to Date type
        birth_date = as.Date(birth_date, format = "%m/%d/%Y"),
        # Extract the year from the birth_date
        birth_year = format(birth_date, "%Y"),
        # Group birth years into 10-year increments
        birth_year_group = cut(
          as.numeric(birth_year),
          breaks = seq(1900, 2030, by = 10), # Adjust the range according to your data
          right = FALSE,
          labels = c(
            "1900-1909", "1910-1919", "1920-1929", "1930-1939", 
            "1940-1949", "1950-1959", "1960-1969", "1970-1979", 
            "1980-1989", "1990-1999", "2000-2009", "2010-2019", 
            "2020-2029"
          )
        )
      ) %>% 
  select(-"x06_03_05_2024_2024_presidential_primary_election_3183_eligibility"                  ,
         -"x08_11_08_2022_november_8_2022_statewide_general_election_3180_eligibility"          ,
         -"x14_06_07_2022_june_7_2022_statewide_direct_primary_election_3162_eligibility"       ,
         -"x24_09_14_2021_september_14_2021_california_gubernatorial_recall_el_3165_eligibility",
         -"x30_11_03_2020_2020_presidential_election_3130_eligibility"                          ,
         -"x36_03_03_2020_2020_presidential_primary_election_3001_eligibility"                  ,
         -"x47_11_06_2018_2018_statewide_general_election_2841_eligibility"                     ,
         -"x48_06_05_2018_2018_statewide_direct_primary_election_2818_eligibility"              ,
         -"x52_11_08_2016_2016_general_election_127_eligibility"                                ,
         -"x53_06_07_2016_presidential_primary_election_126_eligibility" ,
         -"sz_situs_address"                                                                    ,
         -"s_unit_abbr"                                                                         ,
         -"s_street_suffix"                                                                     ,
         -"sz_mail_address1"                                                                    ,
         -"sz_mail_address2"                                                                    ,
         -"sz_mail_address3"                                                                    ,
         -"sz_mail_address4"                                                                    ,
         -"sz_mail_zip"                                                                                 )
  

# Define the categorization function
categorize_party <- function(party) {
  if (party %in% c("Democratic", "DEM")) {
    return("Democratic")
  } else if (party %in% c("Republican", "REP")) {
    return("Republican")
  } else if (party %in% c("Green", "GRN")) {
    return("Green")
  } else if (party %in% c("No Party Preference", "NPP")) {
    return("No Party Preference")
  } else if (party %in% c("Libertarian", "LIB")) {
    return("Libertarian")
  } else {
    return("Other")
  }
}

# Apply the function to the dataframe
EBRPD_district_2_voter_data <- EBRPD_district_2_voter_data %>%
  mutate(Party_Category = sapply(party, categorize_party))
#to add to Ala currently in CCC
# "party_changed"         ,
# "changed_to_democratic"    ,"changed_to_green"      ,
# "voting_opportunities"     ,"voted_vs_opportunities"


### Summary Stats
# Define the columns of interest
contact_information_columns <- c("email", "phone_1", "phone_2", 
                         "precinct_name", "mail_street", 
                         "mail_city", "mail_state", 
                         "mail_zip")

# Create a summary table for completeness
contact_completeness_summary <- EBRPD_district_2_voter_data %>%
  summarise(across(all_of(contact_information_columns), 
                   ~ mean(!is.na(.)) * 100, 
                   .names = "percent_complete_{col}"))

# Convert to a tidy format for better readability
contact_completeness_summary <- contact_completeness_summary %>%
  pivot_longer(everything(), names_to = "column", values_to = "percent_complete") %>% 
  mutate(count = as.numeric(percent_complete*260451),
         percent_complete = round(percent_complete, .1),
         count = round(count, 1))


# Print the summary table in a nicely formatted way
kable(contact_completeness_summary, format = "pipe", col.names = c("Column", "Percent Complete", "count"), digits = 1)


#OUTPUT -----------------------


saveRDS(EBRPD_district_2_voter_data, file = "EBRPD_district_2_voter_data.rds")
