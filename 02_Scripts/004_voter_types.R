source("./02_Scripts/000_Init.R")
# Function to count unique voter_id in each group
count_unique_voters <- function(data, group_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(count_voter_id = n_distinct(voter_id)) %>%
    arrange(desc({{ group_var }}))
}
# upload combined file
EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

#Match dfs Match ---------------------

summary_EBRPD <- EBRPD_district_2_voter_data %>% 
  mutate() %>% 
  select(voter_id,
         percent_voted_by_mail_group    ,
         percent_voted_by_primary_group, 
         voted_in_2024_primary   , 
         voted_in_2020_general   , 
         Party_Category,
         gender ,
         birth_year_group,
         recently_updated,
         voted_vs_opportunities_group
         ) 

summary_by_mail <- count_unique_voters(summary_EBRPD, percent_voted_by_mail_group)

summary_by_primary <- count_unique_voters(summary_EBRPD, percent_voted_by_primary_group)

summary_by_voted_vs_opportunities <- count_unique_voters(summary_EBRPD, voted_vs_opportunities_group)


summary_by_voted_2024_primary <- count_unique_voters(summary_EBRPD, voted_in_2024_primary)
summary_by_voted_2020_general <- count_unique_voters(summary_EBRPD, voted_in_2020_general)
summary_by_party <- count_unique_voters(summary_EBRPD, Party_Category)
summary_by_gender <- count_unique_voters(summary_EBRPD, gender)
summary_by_recently_updated <- count_unique_voters(summary_EBRPD, recently_updated)

# Combine all summaries into a list 
summary_list <- list(
  by_mail = summary_by_mail,
  by_primary = summary_by_primary,
  summary_by_voted_vs_opportunities,
  by_voted_2024_primary = summary_by_voted_2024_primary,
  by_voted_2020_general = summary_by_voted_2020_general,
  by_party = summary_by_party,
  by_gender = summary_by_gender,
  summary_by_recently_updated
)
# Save each summary as a CSV file
lapply(names(summary_list), function(name) {
  write.csv(summary_list[[name]], file = paste0("summary_", name, ".csv"))
})
# Print the summary tables
summary_list




















### Groupby Precinct-----------------------------------------------------------

# "percent_voted_by_mail", 
# "percent_voted_by_primary", 
# "count_times_voted", 
# "voted_in_2024_primary", 
# "voted_in_2020_general"


# Summary Table
EBRPD_district_2_voter_data <- EBRPD_district_2_voter_data_raw %>% 
  mutate(
    #change words to 1 and 0 to sum
    party_changed = as.numeric(party_changed == "Yes"),
    changed_to_democratic = as.numeric(changed_to_democratic == "Yes"),
    changed_to_green = as.numeric(changed_to_green == "Yes"),
    voting_opportunities = as.numeric(voting_opportunities),
    voted_vs_opportunities = as.numeric(voted_vs_opportunities),
    voted_in_2024_primary = as.numeric(voted_in_2024_primary == "Yes"),
    voted_in_2020_general = as.numeric(voted_in_2020_general == "Yes")
  )
# Create a summary table grouped by most_recent_precinct
summary_table <- EBRPD_district_2_voter_data %>%
  group_by(most_recent_precinct) %>%
  summarise(
    # Count the number of registrations in each precinct
    total_registered = n(),
    
    # Summary of registration dates
    earliest_reg_date = min(reg_date, na.rm = TRUE),
    latest_reg_date = max(reg_date, na.rm = TRUE),
    
    # Count the most recent party affiliations
    most_recent_party_counts = list(table(most_recent_party)),
    
    # Count the number of voters who changed party affiliation
    total_party_changed = sum(party_changed, na.rm = TRUE),
    
    # Count the number of voters who changed to Democratic or Green
    changed_to_democratic_count = sum(changed_to_democratic, na.rm = TRUE),
    changed_to_green_count = sum(changed_to_green, na.rm = TRUE),
    
    # Summary of voting opportunities and actual votes
    total_voting_opportunities = sum(voting_opportunities, na.rm = TRUE),
    total_voted_vs_opportunities = sum(voted_vs_opportunities, na.rm = TRUE),
    
    # Include new columns
    avg_percent_voted_by_mail = mean(percent_voted_by_mail, na.rm = TRUE),
    avg_percent_voted_by_primary = mean(percent_voted_by_primary, na.rm = TRUE),
    total_count_times_voted = sum(count_times_voted, na.rm = TRUE),
    total_voted_in_2024_primary = sum(voted_in_2024_primary, na.rm = TRUE),
    total_voted_in_2020_general = sum(voted_in_2020_general, na.rm = TRUE)
  )

# Display the summary table
print(summary_table)