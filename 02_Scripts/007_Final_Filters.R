#Filters
source("./02_Scripts/000_Init.R")
# upload combined file
EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data <- EBRPD_district_2_voter_data %>% 
   rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable"))

#Target 100,000
#first mailer
#Dem 
#voted by mail 50%
desired_levels <- c( "80-90%", "90-100%") #"70-80%", "80-90%",
birth_year <- c("1900-1909", "1910-1919", "1920-1929", "1930-1939", "1940-1949", "1950-1959")
low_levels <- c(  "0-10%", "10-20%", "20-30%")

# percent_voted_by_mail_group    ,
# percent_voted_by_primary_group, 
# voted_in_2024_primary   , 
# voted_in_2020_general   , 
# Party_Category,
# gender ,
# birth_year_group,
# recently_updated,
# voted_vs_opportunities_group
# military
# birth_place
#half of data has if party changed recently

# voted_in_2024_primary == "Yes",
# #!(birth_year_group %in% birth_year)


#percent_voted_by_mail_group %in% desired_levels,
# Filter the data frame
# Request
# I.e. 260 k voters In Ward q.
# 130 Households. 
# 80K are dems. 
# 60K have voted in last election. 
# 40 k voted in last 2 (2016 and 2020) 
# Plus added in 10K new or re-regsterd voters for a recomendwd mailing list of 50 Homes.
# 20K vote by mail list to go out sept 25. 
# List attached in format needed for mail house30 K vote by ballot to go out bt Oct 25.
# List attached in format needed for mail hiuse.
# Also please idenrify how many have phine nymber along with name address and phone numbers..
# list attachwd in format for phone banking
#
#   A.    How many unique households who voted in both 2016 and 2020 elections?
#   
#   B.    How many unique households who voted in all three 2016 and 2020 elections and 2024 primary?
#   
#   C.    and I think I need another explanation or understanding of the Vote by mail 80-100% - -what that means compared to A and B
# 
# 
# 
# 1. for both A and B how many voted by mail or absentee for my mailing to go out September 25 
# 
# 2. for both A and B how many voted by Ballot. in person to go out  and target Oct 25"
# 
#  
# 
#  
# 
# PHONE: and of these, how many do we have phone numbers for?



first_mailer <- EBRPD_district_2_voter_data %>% 
  filter(
    #party_category == "Democratic",
    #      voted_vs_opportunities_group %in% desired_levels,
     percent_voted_by_mail_group %in% desired_levels,

    #      recently_updated == "Yes",
    # count_times_voted >= "2"
    voted_in_2020_general == "Yes" #&#, 88,459
    # voted_in_2016_general == "Yes",
    # voted_in_2024_primary == "Yes" &
     # voted_in_2016_general == "Yes" #75,400 add people that voted in 2020 genral and not in 2024 - as or
         ) %>%
  mutate(
    full_address = case_when(
      !str_detect(mail_street, "[0-9]") ~ paste(house_number, mail_street, mail_city, mail_zip, sep = " "),
      str_detect(mail_street, "[0-9]") ~ paste(mail_street, mail_city, mail_zip, sep = " ")
    ),
    full_address = ifelse(!is.na(apartment_number) & apartment_number != "", 
                          paste(full_address, "Apartment", apartment_number, sep = " "), 
                          full_address),
    name_first = str_to_title(name_first),
    name_last = str_to_title(name_last)
  ) %>% 
  select(-c(#percent_voted_by_mail, 
            percent_voted_by_primary, count_times_voted,
            voting_opportunities, voted_vs_opportunities, voted_in_2024_primary,
            voted_in_2020_general, recently_updated, party_changed,
            changed_to_democratic, changed_to_green, 
            #percent_voted_by_mail_group,
            voted_vs_opportunities_group, percent_voted_by_primary_group,
            #birth_year, birth_year_group, party_category,
            last_voted, #party,
            reg_date, reg_date_original, military, gender, pav, birth_place,
            birth_date, ltd, language,
            house_number, apartment_number)) %>%
  #filter(!is.na(phone_1)) %>% 
  select("voter_id"              ,                                                 
         "most_recent_precinct"  ,                                                 
         "name_prefix"           ,                                                 
         "name_last"             ,                                                 
         "name_first"            ,                                                 
         full_address,                                                
         #"phone_1"               ,
         email,
          #"party"                ,
          birth_year_group, party_category, percent_voted_by_mail_group) #%>% 
  # mutate(clean_phone = gsub("[^0-9]", "", phone_1) %>%  # Remove non-numeric characters
  #                 str_sub(-10))
# %>%
#   distinct(full_address, .keep_all = TRUE)
  
summary_by_mail <- count_unique_voters(first_mailer, percent_voted_by_mail_group)
   
saveRDS(first_mailer, file = "56679_mailer.rds")

write.csv(first_mailer,"56679_mailer.csv")
#Include when voted did they vote by mail

# goal best 50,000

# start with 
# party_category == "Democratic",
# voted_vs_opportunities_group %in% desired_levels

# then add in recently registered group
#create new column for registered in 2024 

#call about abasetee ballot stuff

#Then timing - are they a mailing voter group



   # Lynda's For sure
   # filter(
   #   party_category == "Democratic",
   #   recently_updated == "Yes",
   #   count_times_voted >= "2"
  # 9166 households
