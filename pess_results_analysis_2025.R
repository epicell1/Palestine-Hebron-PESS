#==============================================================================
#
# PATIENT EXPERIENCE AND SATISFACTION SURVEY RESULTS ANALYSIS
# JULY 2025
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr,         # Manipulate data
              lubridate,      # Work with dates
              readxl,         # Import XLS format
              writexl,         # Import XLS format
              ggplot2,        # Create plots
              httr,
              jsonlite,
              stringr
)              

#==============================================================================

### Import PESS results data from Kobo ###

# Specify the Kobo URL and credentials
kobo_url <- "https://kobo.msf.org"
username <- "msfe_jerusalem_epi"
password <- "45Pancakes##kobo"
# form_id <- "aKeCUiFySkThxJekQ8n3e3"
form_id <- "awNvXgQqe6bUTWGhPZtzou"   # get from Url of Form in Kobo, 
                                      # identical with the file name on download xls
api_token <- "e485119d17c7a2def540665a44be18e7ca3f9704"


# Define the data URL
data_url <- paste0(kobo_url, "/api/v2/assets/", form_id, "/data/")


# Define the data location
response <- GET(
  url = data_url,
  add_headers(Authorization = paste("Token", api_token)),
  query = list(format = "json"))


# Check the status and content type of the response
#(200 is successful HTML request)
print(response)


# Check for successful response
if (response$status_code == 200) {
  
  
# Retrieve dataset in JSON format
  data <- content(response, "text", encoding = "UTF-8")
  
  data_json <- fromJSON(data, flatten = TRUE)
  
  
# Extract the results and convert to a data frame
  pess_data_raw <- as.data.frame(data_json$results)
  
  
  print("Data has been successfully retrieved")} else {
    print(paste("Failed to retrieve data. Status code:", response$status_code))}

#==================================================================

### Data cleaning ###

# Create new data frame to edit from
pess_data_edit <- pess_data_raw


# Replace / with _ in all headers
colnames(pess_data_edit) <- gsub("/", "_", colnames(pess_data_edit))

#$$$ delete in pess_data_edit all the superfluous prefixes that had not been there in the 2024 version
#$$$ "group_ne8hp50_group_mh_visit_", 
#$$$ "group_ne8hp50_"
colnames(pess_data_edit) <- gsub("group_ne8hp50_group_mh_visit_", "", colnames(pess_data_edit))
colnames(pess_data_edit) <- gsub("group_ne8hp50_", "", colnames(pess_data_edit))
#colnames(pess_data_edit) <- gsub("group_ne8hp50_group_mh_visit_sw_", "", colnames(pess_data_edit))


# Remove patients that did not consent to the survey
pess_data_edit <- pess_data_edit %>%
  filter(demographics_consent == 'y')

#==============================================================================

### Calculate the mean and median patient ages ###

# Convert patient age to a numeric value
pess_data_edit$patient_age <- as.numeric(pess_data_edit$patient_age)


# Calculate the mean and median patient age
summary_stats_age <- pess_data_edit %>%
  summarise(
    min_value = min(pess_data_edit$patient_age, na.rm = TRUE),
    max_value = max(pess_data_edit$patient_age, na.rm = TRUE),
    range_value = max_value - min_value,
    median_value = median(pess_data_edit$patient_age, na.rm = TRUE),
    mean_value = mean(pess_data_edit$patient_age, na.rm = TRUE),
    sd_value = sd(pess_data_edit$patient_age, na.rm = TRUE))

print(summary_stats_age)

# Define bins for age distribution of participants
bins_age <- c(0, 10, 20, 30, 40, 50, 60, 70, Inf)


# Create labels for the bins
labels <- c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", ">=70")


# Create a new column with age bins   
pess_data_edit$patient_age_bin <- cut(pess_data_edit$patient_age, bins_age, labels = labels, include.lowest = TRUE, right=FALSE)
# $$$ had to include right=False for ranges as "[a,b)"

# Summarize the data to count patients by age group and sex
age_sex_counts <- pess_data_edit %>%
  group_by(patient_age_bin, patient_sex) %>%
  summarise(count = n()) %>%
  ungroup()

# $$$ had to add a third color for "other"
# Create a population pyramid by age and sex
age_sex_plot <- ggplot(age_sex_counts, aes(x = patient_age_bin, y = count, fill = patient_sex)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#fcbba1", "#de2d26", "blue"), name = "Sex", labels = c("Female", "Male", "Other")) +
  labs(x = "Age Group (Years)", y = "Patients (n)") +
  theme_minimal() +
  theme(legend.position = "right")

print(age_sex_plot)

#==============================================================================

# Identify sex distribution

num_male <- sum(pess_data_edit$patient_sex == 'male')
num_female <- sum(pess_data_edit$patient_sex == 'female')
num_other <- sum(pess_data_edit$patient_sex == 'other')

#==============================================================================

# Patient and caregiver numbers
num_caregiver <- sum(pess_data_edit$demographics_patient == 'caregiver')
num_patient <- sum(pess_data_edit$demographics_patient == 'patient')

#==============================================================================

# Identify service types
categories <- c("medical", "mh", "sw", "srh")


# Count instances of all services recieved (including multiple services)
counts <- rep(0, length(categories))


# Loop through each category and count occurrences
for (i in seq_along(categories)) {
  counts[i] <- sum(grepl(categories[i], pess_data_edit$services_services_type))}


# Create a data frame to display the counts
services_counts <- data.frame(Category = categories, Count = counts)

print(services_counts)

#==============================================================================
# $$$ location for this study is always "mh" 
# LAST SERVICES RECIEVED BY MSF

# Clean service location since only MH services are provided in MH clinic
pess_data_edit$locations_service_locations <- c("mh_clinic")
pess_data_edit$services_services_type_last[pess_data_edit$locations_service_locations == 'mh_clinic'] <- 'mh'


# Summarize a table of last service received from MSF
service_type_last <- table(pess_data_edit$services_services_type_last)


# Convert table to data frame
service_type_last <- as.data.frame(service_type_last)


# Rename columns
colnames(service_type_last) <- c("Service", "Count")


# Rename rows 
service_type_last <- service_type_last %>%
  mutate(Service = case_when(
    Service == 'mh' ~ 'Mental Health',
    Service == 'srh' ~ 'Sexual and Reproductive Health',
    Service == 'medical' ~ 'Medical',
    Service == 'sw' ~ 'Social Work',
    TRUE ~ as.character(Service) # Keep other values unchanged
  ))

print(service_type_last)

#==============================================================================
#$$$$
# Identify MH service
#mh_services <- c('counselling', 'psychology', 'psychiatry')

mh_services_count <- table(pess_data_edit$services_services_mh_services)
# Convert table to data frame
mh_services_count <- as.data.frame(mh_services_count)

# Rename columns
colnames(mh_services_count) <- c("mh_service", "Count" )#, "Percentage")

# Calculate total count
mh_total <- sum(mh_services_count$Count)


# Add percentage column and round to nearest whole number
mh_services_count <- mh_services_count %>%
  mutate(Percentage = round((Count / mh_total) * 100))


# Create a sum row in the table
mh_sum_row <- mh_services_count %>%
  summarise(mh_service = 'total_mh', Count = sum(Count), Percentage = 100)


# Bind the sum row to the original data frame
mh_services_count <- bind_rows(mh_services_count, mh_sum_row)

print(mh_services_count)



#==============================================================================
#==============================================================================

# SERVICE LOCATION OF LAST SERVICE RECIEVED

# Summarize service location
location_counts <- table(pess_data_edit$locations_service_locations)


# Convert table to data frame
location_counts <- as.data.frame(location_counts)


# Rename columns
colnames(location_counts) <- c("Location", "Count")


# Rename rows 
location_counts <- location_counts %>%
  mutate(Location = case_when(
    Location == 'mobile_clinic' ~ 'Mobile Clinic',
    Location == 'mh_clinic' ~ 'Mental Health Clinic',
    Location == 'home_visit' ~ 'Home Visit',
    Location == 'telephone' ~ 'Telephone',
    TRUE ~ as.character(Location))) # Keep other values unchanged


# Sort columns from highest to lowest count
location_counts <- location_counts %>%
  arrange(desc(Count))


# Calculate total count
total <- sum(location_counts$Count)


# Add percentage column and round to nearest whole number
location_counts <- location_counts %>%
  mutate(Percentage = round((Count / total) * 100))


# Create a sum row in the table
sum_row <- location_counts %>%
  summarise(Location = 'Total', Count = sum(Count))


# Bind the sum row to the original data frame
location_counts <- bind_rows(location_counts, sum_row)

print(location_counts)

loc_num_mc <- location_counts$Count[location_counts$Location == "Mobile Clinic"]
loc_num_mhc <- location_counts$Count[location_counts$Location == "Mental Health Clinic"]
loc_num_tel <- location_counts$Count[location_counts$Location == "Telephone"]
loc_num_hv <- location_counts$Count[location_counts$Location == "Home Visit"]
loc_num_tot <- location_counts$Count[location_counts$Location == "Total"]

loc_per_mc <- location_counts$Percentage[location_counts$Location == "Mobile Clinic"]
loc_per_mhc <- location_counts$Percentage[location_counts$Location == "Mental Health Clinic"] 
loc_per_tel <- location_counts$Percentage[location_counts$Location == "Telephone"]
loc_per_hv <- location_counts$Percentage[location_counts$Location == "Home Visit"]


#==============================================================================

# TRANSPORTATION AND TRAVEL TIME

# Convert time to numeric values
pess_data_edit$arrival_arrival_time <- as.numeric(pess_data_edit$arrival_arrival_time)


# Filter out NA values
transport_data <- pess_data_edit %>%
  filter(!is.na(arrival_arrival_mode) & arrival_arrival_mode != 'other')


# Set seed number to reproduce stats
set.seed(001)


# Calculate summary statistics
summary_stats_transport <- transport_data %>%
  group_by(arrival_arrival_mode) %>%
  summarise(
    min_value = min(arrival_arrival_time, na.rm = TRUE),
    max_value = max(arrival_arrival_time, na.rm = TRUE),
    range_value = max_value - min_value,
    median_value = median(arrival_arrival_time, na.rm = TRUE),
    mean_value = mean(arrival_arrival_time, na.rm = TRUE),
    sd_value = sd(arrival_arrival_time, na.rm = TRUE))


# Print summary statistics
print(summary_stats_transport)

# Rename factor levels of transport mode for final report
transport_data$arrival_arrival_mode <- factor(transport_data$arrival_arrival_mode,
        levels = c("car", "public_trans", "taxi", "walk"),
        labels = c("Car", "Public Transit", "Taxi", "Walk"))



# Create boxplot of travel time and transportation mode
travel_time_plot <- boxplot(arrival_arrival_time ~ arrival_arrival_mode, data = transport_data,
        xlab = "Transportation", ylab = "Travel time (minutes)")

print(travel_time_plot)


# Conduct ANOVA test to determine if there is statistically significant difference of travel time among transportation type 
anova_trav_time <- aov(arrival_arrival_time ~ arrival_arrival_mode, data = transport_data)

#==============================================================================

# Count of patients requiring to miss school or work
table(pess_data_edit$arrival_access_absent)


# Summarize the data of patients requiring to miss school or work by age group and sex
age_sex_counts_missed_school_work <- pess_data_edit %>%
  filter(arrival_access_absent == "y") %>%
  group_by(patient_age_bin, patient_sex) %>%
  summarise(count = n()) %>%
  ungroup()

# $$$ had to add a third color for "other"
# Create a population pyramid by age and sex
age_sex_plot_missed_school_work <- ggplot(age_sex_counts_missed_school_work, aes(x = patient_age_bin, y = count, fill = patient_sex)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#fcbba1", "#de2d26", "blue"), name = "Sex", labels = c("Female", "Male", "Other")) +
  labs(x = "Age Group (Years)", y = "Patients who missed school or work (n)") +
  theme_minimal() +
  theme(legend.position = "right")

print(age_sex_plot_missed_school_work)

#==============================================================================

# TRAVEL TIME AND MH SERVICES 
#$$$ modification of PESS 2024: mh_services
# Filter out NA values
mh_services_data <- pess_data_edit %>%
  filter(!is.na(services_services_mh_services) &
           services_services_mh_services %in% c('counselling', 'psychology', 'psychiatry'))

# Set seed number to reproduce stats
set.seed(002)


# Calculate summary statistics
summary_stats_mh_services <- mh_services_data %>%
  group_by(services_services_mh_services) %>%
  summarise(
    min_value = min(arrival_arrival_time, na.rm = TRUE),
    max_value = max(arrival_arrival_time, na.rm = TRUE),
    range_value = max_value - min_value,
    median_value = median(arrival_arrival_time, na.rm = TRUE),
    mean_value = mean(arrival_arrival_time, na.rm = TRUE),
    sd_value = sd(arrival_arrival_time, na.rm = TRUE))


# Print summary statistics
print(summary_stats_mh_services)

mh_services_data$services_services_mh_services <- factor(mh_services_data$services_services_mh_services,
    levels = c('counselling', 'psychology', 'psychiatry'),
    labels = c('Counselling', 'Psychology', 'Psychiatry'))


# Create boxplot of travel time
#boxplot(mh_services_data$arrival_arrival_time , data = mh_services_data,
#        xlab = "Mental health services", ylab = "Travel time (minutes)")


# Create boxplot of travel time and mh_services
boxplot(arrival_arrival_time ~ services_services_mh_services, data = mh_services_data,
        xlab = "MH Services", ylab = "Travel time (minutes)")

# Conduct ANOVA test to determine if there is statistically significant difference of travel time among service locations
lm_model_mh_services <- lm(arrival_arrival_time ~ services_services_mh_services, data = mh_services_data)
summary(lm_model_mh_services)

anova_model_mh_services <- aov(arrival_arrival_time ~ services_services_mh_services, data = mh_services_data)
summary(anova_model_mh_services)


# Conduct t test to determine if there is significant difference of travel time among mh_services
#ttest_travel_time <- t.test(arrival_arrival_time ~ services_services_mh_services, data = mh_services_data)


# View the result
#print(ttest_travel_time)

#==============================================================================

# MH_SERVICES AND CHECKPOINTS

# Construct a contingency table to conduct chi square test
summary_stats_checkpoint <- mh_services_data %>%
  group_by(services_services_mh_services) %>%
  summarise(
    count_y = sum(arrival_access_checkpoint == 'y', na.rm = TRUE),
    count_n = sum(arrival_access_checkpoint == 'n', na.rm = TRUE)
  )

# Create contingency table
contingency_table <- matrix(c(summary_stats_checkpoint$count_y, 
                              summary_stats_checkpoint$count_n), 
                            nrow = 3, byrow = TRUE,
                            dimnames = list(c('counselling', 'psychology', 'psychiatry'),
                                            c("y", "n")))

# Summarize 



# Perform chi-square test
chi_square_result <- chisq.test(contingency_table)

print(chi_square_result)


# EFFECTS OF CHECKPOINT PRESENCE ON TIME TO ARRIVE TO LOCATION

table(mh_services_data$arrival_access_checkpoint)

summary_stats_checkpoint <- mh_services_data %>%
  group_by(arrival_access_checkpoint) %>%
  summarise(
    min_value = min(arrival_arrival_time, na.rm = TRUE),
    max_value = max(arrival_arrival_time, na.rm = TRUE),
    range_value = max_value - min_value,
    median_value = median(arrival_arrival_time, na.rm = TRUE),
    mean_value = mean(arrival_arrival_time, na.rm = TRUE),
    sd_value = sd(arrival_arrival_time, na.rm = TRUE))

print(summary_stats_checkpoint)

# Conduct t-test to compare if the presence of checkpoint affects transportation time to the MSF services

ttest_checkpoint <- t.test(arrival_arrival_time ~ arrival_access_checkpoint, data = mh_services_data)

print(ttest_checkpoint)


# Test data for normality using Shapiro Test
shapiro.test(mh_services_data$arrival_arrival_time)


# Separate data to separate data frames for Wilcox test
time_no_cp <- (mh_services_data$arrival_arrival_time[mh_services_data$arrival_access_checkpoint == "n"])
time_yes_cp <- mh_services_data$arrival_arrival_time[mh_services_data$arrival_access_checkpoint == "y"]


# Since data does not follow normal distribution, conduct Wilcox Sum Rank Test
wilcox_time_checkpoint <- wilcox.test(time_no_cp, time_yes_cp, exact=FALSE)

print(wilcox_time_checkpoint)


# Plot checkpoint effects on travel time
cp_time_plot <- boxplot(time_yes_cp, time_no_cp,
        names = c("Checkpoint", "No Checkpoint"),
        ylab = "Arrival time (minutes)")


print(cp_time_plot)

#==============================================================================

# Alternate route due to roadblocks or checkpoints

table(mh_services_data$arrival_access_route)



summary_stats_route <- mh_services_data %>%
  group_by(arrival_access_route) %>%
  summarise(
    min_value = min(arrival_arrival_time, na.rm = TRUE),
    max_value = max(arrival_arrival_time, na.rm = TRUE),
    range_value = max_value - min_value,
    median_value = median(arrival_arrival_time, na.rm = TRUE),
    mean_value = mean(arrival_arrival_time, na.rm = TRUE),
    sd_value = sd(arrival_arrival_time, na.rm = TRUE))

summary_stats_route

# Conduct t-test to compare if the presence of checkpoint affects transportation time to the MSF services

ttest_route <- t.test(arrival_arrival_time ~ arrival_access_route, data = mh_services_data)

print(ttest_route)

#==============================================================================

# EXPERIENCED VIOLENCE FROM SETTLERS AND/OR IDF

# Create table to show the frequency of violence against patients from IDF and/or settlers



# Create summary statistics for violence and the effect on time to arrive to service location
summary_stats_violence <- mh_services_data %>%
  group_by(arrival_access_violence) %>%
  summarise(
    min_value = min(arrival_arrival_time),
    max_value = max(arrival_arrival_time),
    range_value = max_value - min_value,
    median_value = median(arrival_arrival_time),
    mean_value = mean(arrival_arrival_time),
    sd_value = sd(arrival_arrival_time))


# Conduct t-test to compare if violence affects transportation time to the MSF services
ttest_violence <- t.test(arrival_arrival_time ~ arrival_access_violence, data = mh_services_data)

print(ttest_violence)

#==============================================================================

# Create table to show the frequency of patients required to miss school or work to attend MSF services
table(mh_services_data$arrival_access_absent)

#==============================================================================

# Create frequency table to show the number of patients who had difficulty finding the service location
table(mh_services_data$access_location_found)

#==============================================================================

# Create frequency table to show the number of patients who found the service location inaccessible
table(mh_services_data$access_location_accessible)


#==============================================================================

# Create frequency table to show the number of patients were able to identify basic requirements including water and restrooms
table(mh_services_data$access_location_facilities_basics)

#==============================================================================

# Create frequency table to show the number of patients were able to identify medical locations including registration and the pharmacy
table(mh_services_data$access_location_facilities_medical)

#==============================================================================

# Create frequency table to show the number of patients who were greeted upon arrival to the service location
table(mh_services_data$access_location_greeted)

#==============================================================================

# Create frequency table to show the number of patients who recall seeing public health messaging during their visit
table(mh_services_data$access_location_messaging)

#==============================================================================
#$$$
# Convert wait time to numeric value
pess_data_edit$visit_visit_wait_time <- as.numeric(pess_data_edit$visit_visit_wait_time)

wait_time_edit <- pess_data_edit

# Rename for final report
wait_time_edit$services_services_mh_services <- recode(wait_time_edit$services_services_mh_services,
                                                     "counselling" = "Counselling",
                                                     "psychology" = "Psychology",
                                                     "psychiatry" = "Psychiatry" )

# Create summary statistics table of patient waiting time
summary_stats_wait_time <- wait_time_edit %>%
  filter(services_services_mh_services %in% c('Counselling', 'Psychology', 'Psychiatry')) %>%
  group_by(services_services_mh_services) %>%
  summarise(
    min_value = min(visit_visit_wait_time, na.rm = TRUE),
    max_value = max(visit_visit_wait_time, na.rm = TRUE),
    range_value = max_value - min_value,
    median_value = median(visit_visit_wait_time, na.rm = TRUE),
    mean_value = mean(visit_visit_wait_time, na.rm = TRUE),
    sd_value = sd(visit_visit_wait_time, na.rm = TRUE)
  )


# Create a boxplot to present wait time at mh clinic vs MH services
boxplot(visit_visit_wait_time ~ services_services_mh_services, data = wait_time_edit,
        xlab = "MH Services", ylab = "Wait time (minutes)")

# Conduct ANOVA test to determine if there is statistically significant difference of wait time among mh_services 
lm_wait_model_mh_services <- lm(visit_visit_wait_time ~ services_services_mh_services, data = wait_time_edit)
summary(lm_wait_model_mh_services)

anova_wait_model_mh_services <- aov(visit_visit_wait_time ~ services_services_mh_services, data = wait_time_edit)
summary(anova_wait_model_mh_services)

TukeyHSD(anova_wait_model_mh_services)
#ttest_wait <- t.test(visit_visit_wait_time ~ services_services_mh_services, data = wait_time_edit)

#print(ttest_wait)


#==============================================================================

# Construct a contingency table to conduct chi square test
privacy <- pess_data_edit %>%
  filter(services_services_mh_services %in% c('counselling', 'psychology', 'psychiatry'))

#$$$ privacy question: "Was your visit in a private area where you felt comfortable to discuss your concerns?"  
privacy <- privacy %>%
  group_by(services_services_mh_services) %>%
  summarise(
    count_y = sum(visit_visit_private == 'y', na.rm = TRUE),
    count_n = sum(visit_visit_private == 'n', na.rm = TRUE))

print(privacy)

# Create contingency table
contingency_table_private <- matrix(c(
  privacy$count_y[privacy$services_services_mh_services == "counselling"], 
  privacy$count_y[privacy$services_services_mh_services == "psychiatry"],
  privacy$count_y[privacy$services_services_mh_services == "psychology"],
  privacy$count_n[privacy$services_services_mh_services == "counselling"], 
  privacy$count_n[privacy$services_services_mh_services == "psychiatry"],
  privacy$count_n[privacy$services_services_mh_services == "psychology"]  
), nrow = 2, byrow = TRUE,
dimnames = list(c("y", "n"),
                c("counselling", "psychiatry", "psychology")))


print(contingency_table_private)

# Perform chi-square test
chi_square_private <- chisq.test(contingency_table_private)

print(chi_square_result)


# Perform Fisher's Exact Test since there are small variables in some cells
fisher_result_private <- fisher.test(contingency_table_private)

# Print the result of Fisher's Exact Test
print(fisher_result_private)

#==============================================================================

### Visit outcome ###

# Summarize visit outcome string data
outcome <- pess_data_edit %>%
  mutate(
    referral_moh_count = str_detect(visit_visit_outcome, "referral_moh"),
    referral_msf_count = str_detect(visit_visit_outcome, "referral_msf"),
    diagnosis_count = str_detect(visit_visit_outcome, "diagnosis"),
    treatment_count = str_detect(visit_visit_outcome, "treatment"),
    none_count = str_detect(visit_visit_outcome, "none"),
    no_visit_count = str_detect(visit_visit_outcome, "no_visit")
  ) %>%
  summarise(
    referral_moh_total = sum(referral_moh_count, na.rm = TRUE),
    referral_msf_total = sum(referral_msf_count, na.rm = TRUE),
    diagnosis_total = sum(diagnosis_count, na.rm = TRUE),
    treatment_total = sum(treatment_count, na.rm = TRUE),
    none_total = sum(none_count, na.rm = TRUE),   # "none of the above"
    no_visit_total = sum(no_visit_count, na.rm = TRUE)
  )

print(outcome)

last_vis_med <- sum(pess_data_edit$services_services_type_last == "medical", na.rm = TRUE)
last_vis_mh <- sum(pess_data_edit$services_services_type_last == "mh", na.rm = TRUE)

print(last_vis_med)
print(last_vis_mh)


outcome_mh <- pess_data_edit %>%
  filter(services_services_type_last == "mh") %>%
  mutate(
    referral_moh_count = str_detect(visit_visit_outcome, "referral_moh"),
    referral_msf_count = str_detect(visit_visit_outcome, "referral_msf"),
    diagnosis_count = str_detect(visit_visit_outcome, "diagnosis"),
    treatment_count = str_detect(visit_visit_outcome, "treatment"),
    none_count = str_detect(visit_visit_outcome, "none"),
    no_visit_count = str_detect(visit_visit_outcome, "no_visit") 
  ) %>%
  summarise(
    referral_moh_total = sum(referral_moh_count, na.rm = TRUE),
    referral_msf_total = sum(referral_msf_count, na.rm = TRUE),
    diagnosis_total = sum(diagnosis_count, na.rm = TRUE),
    treatment_total = sum(treatment_count, na.rm = TRUE),
    none_total = sum(none_count, na.rm = TRUE),   # "none of the above"
    no_visit_total = sum(no_visit_count, na.rm = TRUE)
  )

print(outcome_mh)

# Create summary statistics for visit outcomes
denom_outcome <- sum(!is.na(pess_data_edit$visit_visit_outcome))


# Ratio of patients referred to MOH
ratio_outcome_referral <- outcome_mh$referral_moh_total / denom_outcome


# Ratio of patients that attended clinic but did not get a visit
ratio_outcome_novisit <- outcome_mh$no_visit_total / denom_outcome

#==============================================================================

# Frequency and ratio of patients that were able to ask questions during their visit

table(pess_data_edit$visit_medical_questions)

#==============================================================================

# Frequency and ratio of patients that were able to ask questions during their visit

table(pess_data_edit$visit_medical_treatment)

#==============================================================================

# Frequency and ratio of patients that felt the HCW understood their ,ealthcare concerns

table(pess_data_edit$visit_medical_understanding)

#==============================================================================

# Frequency and ratio of patients that felt the service location was comfortable

table(pess_data_edit$visit_location_comfortable)

#==============================================================================

# Frequency and ratio of patients that felt safe from harm during their visit

table(pess_data_edit$safety_visit_safety)

#==============================================================================

# Frequency and ratio of patients reported having to pay for MSF services

table(pess_data_edit$safety_payment)

#==============================================================================

# Frequency and ratio of patients reported being able to freely express themselves during the visit

table(pess_data_edit$safety_visit_freedom)

#==============================================================================

# Frequency and ratio of patients that reported having overall satisfaction with their MSF experiences

table(pess_data_edit$satisfaction_satisfaction_overall)

#==============================================================================

# Frequency and ratio of patients that reported willingness to return for further MSF services

table(pess_data_edit$satisfaction_satisfaction_return)

#==============================================================================

# Frequency and ratio of patients that reported willingness to recommend MSF services to others

table(pess_data_edit$satisfaction_satisfaction_recommend)

#==============================================================================

# Frequency and ratio of patients that reported being provided the opportunity to ask questions about their prescribed treatment for informed verbal consent to treatment

table(pess_data_edit$visit_medical_treatment)

#==============================================================================

# Frequency and ratio of patients that reported receiving enough information during their social work assessment

table(pess_data_edit$sw_sw_assessment)

#==============================================================================

# Frequency and ratio of patients that reported having their social work assessment conducted in a private location

table(pess_data_edit$sw_sw_private)

#==============================================================================

# Frequency and ratio of patients that reported the social work services met their needs

table(pess_data_edit$sw_sw_support)

#==============================================================================

# Frequency and ratio of patients that reported they were satisfied with the social work services they received

table(pess_data_edit$sw_sw_outcome)

#==============================================================================

### Mental Health Services ###

# Frequency of patients that reported they were able to equally participate in their mental health session

table(pess_data_edit$mh_mh_participate)

#==============================================================================

# Frequency of patients that reported they found their mental health session useful to address their mental health concern

table(pess_data_edit$mh_mh_helpful)

#==============================================================================

#-------------------------------
# Added by Laurence on March 5 (for advocacy update sheet) 
#-------------------------------

# Calculate the proportion of people reporting having been stopped at a checkpoint
proportion_stopped <- mh_services_data %>%
  summarise(
    proportion = mean(arrival_access_checkpoint == "y", na.rm = TRUE),
    n = n()
  )

# Calculate the 95% confidence intervals using the Wilson method
conf_int <- proportion_stopped %>%
  mutate(
    lower_ci = prop.test(proportion * n, n)$conf.int[1],
    upper_ci = prop.test(proportion * n, n)$conf.int[2]
  )

# Print the results
print(conf_int)


# Calculate the average travel time 
average_travel_time <- pess_data_edit %>%
  group_by(arrival_access_checkpoint) %>%
  summarise(
    mean_time = mean(arrival_arrival_time),
    sd_time = sd(arrival_arrival_time),
    n = n()
  ) %>%
  mutate(
    se = sd_time / sqrt(n),
    lower_ci = mean_time - qt(0.975, df = n-1) * se,
    upper_ci = mean_time + qt(0.975, df = n-1) * se
  )

# Print the results
print(average_travel_time)


# Calculate the proportion of people needing to take an alternate road
proportion_alternate <- mh_services_data %>%
  summarise(
    proportion = mean(arrival_access_route == "y", na.rm = TRUE),
    n = n()
  )

# Calculate the 95% confidence intervals using the Wilson method
conf_int <- proportion_alternate %>%
  mutate(
    lower_ci = prop.test(proportion * n, n)$conf.int[1],
    upper_ci = prop.test(proportion * n, n)$conf.int[2]
  )

# Print the results
print(conf_int)


# Calculate the proportion of people experiencing violence at checkpoints
proportion_violence <- mh_services_data %>%
  summarise(
    proportion = mean(arrival_access_violence == "y", na.rm = TRUE),
    n = n()
  )

# Calculate the 95% confidence intervals using the Wilson method
conf_int <- proportion_violence %>%
  mutate(
    lower_ci = prop.test(proportion * n, n)$conf.int[1],
    upper_ci = prop.test(proportion * n, n)$conf.int[2]
  )

# Print the results
print(conf_int)


#-------------------------------
# Export full data to Excel 
#-------------------------------

# Get the current date
current_date <- Sys.Date()

# Create the file path with the date appended
file_path <- paste0("C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/11_PESS_2025/2_Data/Results/pess_data_", current_date, ".xlsx")

# Export to Excel
write_xlsx(pess_data_edit, file_path)
names(pess_data_edit)

#==============================================================================
# End of code
#==============================================================================


