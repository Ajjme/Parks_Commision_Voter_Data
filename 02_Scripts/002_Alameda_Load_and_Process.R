source("./02_Scripts/000_Init.R")
### Inputs------------------
#may have to change downloaded name
ala_voter_data <- read_tsv("../../Voter_data_analysis/Alameda/MultiPurposeVoterFile-EBRPD-District-2.txt") %>% 
  clean_names() %>% 
  filter(status == "A",
         )
# Use columns party reg_date, gender, birth place
# reach out with email column but mostly NAs phone 1 and 2 available
# Further analysis - group by PrecinctListconvert addresses to lat and long then plot outer()