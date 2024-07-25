source("./02_Scripts/000_Init.R")
### Inputs------------------
#may have to change downloaded name
ala_voter_data <- read_tsv("../../Voter_data_analysis/Alameda/MultiPurposeVoterFile-EBRPD-District-2.txt") %>% 
  clean_names() %>% 
  filter(status == "A",
         )
select(voter_id, 
                                                                     
       
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
#Voting patterns
       [57] "x06_03_05_2024_2024_presidential_primary_election_3183"                  
       [58] "x08_11_08_2022_november_8_2022_statewide_general_election_3180"          
       [59] "x14_06_07_2022_june_7_2022_statewide_direct_primary_election_3162"       
       [60] "x24_09_14_2021_september_14_2021_california_gubernatorial_recall_el_3165"
       [61] "x30_11_03_2020_2020_presidential_election_3130"                          
       [62] "x36_03_03_2020_2020_presidential_primary_election_3001"                  
       [63] "x47_11_06_2018_2018_statewide_general_election_2841"                     
       [64] "x48_06_05_2018_2018_statewide_direct_primary_election_2818"              
       [65] "x52_11_08_2016_2016_general_election_127"                                
       [66] "x53_06_07_2016_presidential_primary_election_126"                        
       # [67] "x56_11_04_2014_general_election_123"                                     
       # [68] "x57_06_03_2014_statewide_direct_primary_119"                             
       # [69] "x62_11_06_2012_general_election_111"                                     
       # [70] "x63_06_05_2012_presidential_primary_election_110" )



# Use columns party reg_date, gender, birth place
# reach out with email column but mostly NAs phone 1 and 2 available
# Further analysis - group by PrecinctListconvert addresses to lat and long then plot outer()