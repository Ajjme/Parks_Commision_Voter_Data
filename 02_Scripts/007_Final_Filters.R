#Filters
source("./02_Scripts/000_Init.R")
# upload combined file
EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data <- EBRPD_district_2_voter_data %>% 
   rename(party_category = Party_Category)

#Target 100,000
#first mailer
#Dem 
#voted by mail 50%
desired_levels <- c(  "90-100%") #"70-80%", "80-90%",
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

first_mailer <- EBRPD_district_2_voter_data %>% 
  filter(
    party_category == "Democratic",
         voted_vs_opportunities_group %in% desired_levels,
    percent_voted_by_mail_group %in% low_levels,

    #      recently_updated == "Yes",
    # count_times_voted >= "2"
         ) %>%
  select(-c(#percent_voted_by_mail, 
            percent_voted_by_primary, count_times_voted,
            voting_opportunities, voted_vs_opportunities, voted_in_2024_primary,
            voted_in_2020_general, recently_updated, party_changed,
            changed_to_democratic, changed_to_green, 
            #percent_voted_by_mail_group,
            voted_vs_opportunities_group, percent_voted_by_primary_group,
            birth_year, birth_year_group, party_category, last_voted, party,
            reg_date, reg_date_original, military, gender, pav, birth_place,
            birth_date, ltd, language,
            house_number, apartment_number)) %>%
  mutate(full_address = paste(
    # house_number, 
    #              ifelse(!is.na(apartment_number), apartment_number, ""), 
                 mail_street, 
                 mail_city, 
                 sep = " ")
  ) %>%
  distinct(full_address, .keep_all = TRUE)
  
summary_by_mail <- count_unique_voters(first_mailer, percent_voted_by_mail_group)
   
saveRDS(first_mailer, file = "first_mailer.rds")
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
