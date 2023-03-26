################################################################################ 
# This file performs additional cleaning of data that is used for final analysis
# of the CorporateMix round 1-4 data.
# 
# AUTHOR: MOSES C. KITI, 12/13/2022
################################################################################ 

library(dplyr)
library(tidyverse)

#  read in the files for rounds
participant_rd1 <- readRDS("./data/df_participant_clean_rd1.RDS")  # Participant information
contact_rd1 <- readRDS("./data/df_contact_clean_rd1.RDS")
day_rd1 <- readRDS("./data/df_day_clean_rd1.RDS")           # Metadata on participant's day

participant_rd2 <- readRDS("./data/df_participant_clean_rd2.RDS")  # Participant information
contact_rd2 <- readRDS("./data/df_contact_clean_rd2.RDS")          # Contact information
df_rd2 <- readRDS("./data/df_merged_rd2.RDS")

#  read in the files for round 3 
participant_rd3 <- readRDS("./data/df_participant_clean_rd3.RDS")  # Participant information
contact_rd3 <- readRDS("./data/df_contact_clean_rd3.RDS")          # Contact information

#  read in the files for round 4 
participant_rd4 <- readRDS("./data/df_participant_clean_rd4.RDS")  # Participant information
contact_rd4 <- readRDS("./data/df_contact_clean_rd4.RDS")          # Contact information


## select and rename vars to same format

# round 1 data
names(participant_rd1)
participant_rd1 <- participant_rd1 %>%
  select(part_id, gender, age_cat, edu, race, hispanic, hh_str) %>%
  mutate(edu = ifelse(edu=="Bachelor's degree in college (4-year)" | 
                        edu=="Doctoral degree or Professional degree (PhD, JD, MD)" | 
                        edu=="Master's degree", "Bachelors or higher", 
                      "Less than Bachelors")) %>%
  mutate(hh_str = ifelse(hh_str=="Live alone","Alone",
                         ifelse(hh_str=="Live with parent", "Extended",
                                ifelse(hh_str=="Spouse and children only", "Nuclear",
                                       ifelse(hh_str=="Spouse only", "Nuclear", 
                                              ifelse(hh_str=="Roommate or sibling", "Roommates", "Other")))))) %>%
  mutate(hispanic = factor(hispanic, 
                           levels = c("Yes", "None of these"),
                           labels = c("Yes", "No"))) %>%
  rename(participant_id = part_id,
         participant_age = age_cat,
         participant_sex = gender,
         education = edu,
         family_structure = hh_str) %>%
  mutate(round = "One") %>%
  arrange(participant_id, participant_age, participant_sex, education, race, hispanic, family_structure)
participant_rd1$participant_sex[participant_rd1$participant_sex == "I don't know"] <- "Not reported"
participant_rd1$hispanic[participant_rd1$hispanic == "None of these"] <- "No"

# round 2 data 
names(participant_rd2)
participant_rd2 <- participant_rd2 %>%
  select(id, participant_sex, participant_age_2, education2, race, hispanic, 
         family_structure) %>%
  rename(participant_id = id,
         participant_age = participant_age_2,
         education = education2) %>%
  mutate(round = "Two") %>%
  arrange(participant_id, participant_age, participant_sex, education, 
          race, hispanic, family_structure)
participant_rd2$hispanic[participant_rd2$hispanic == "None of these"] <- "No"

# round 3 data 
names(participant_rd3)
participant_rd3 <- participant_rd3 %>%
  select(id, participant_sex, participant_age_2, education2, race, hispanic, 
         family_structure) %>%
  rename(participant_id = id,
         participant_age = participant_age_2,
         education = education2) %>%
  mutate(round = "Three") %>%
  arrange(participant_id, participant_age, participant_sex, education, 
          race, hispanic, family_structure)

# round 4 data 
names(participant_rd4)
participant_rd4 <- participant_rd4 %>%
  select(id, participant_sex, participant_age_2, education2, race, hispanic2, 
         family_structure) %>% # num_hh
  rename(participant_id = id,
         participant_age = participant_age_2,
         education = education2,
         hispanic = hispanic2) %>%
  mutate(round = "Four") %>%
  arrange(participant_id, participant_age, participant_sex, education, 
          race, hispanic, family_structure)

## merge participant dfs
participants <- rbind(participant_rd1, participant_rd2, participant_rd3, participant_rd4)
# rm(participant_rd1, participant_rd2)

## factorize
participants$round <- as.factor(participants$round)

participants$participant_age_2 <- participants$participant_age
participants$participant_age_2 <- factor(participants$participant_age_2, order=TRUE,
                                         levels = c("20-29", "30-39", "40-49", "50-59", "60+"))

participants$participant_age[participants$participant_age=="40-49"] <- "40-59"
participants$participant_age[participants$participant_age=="50-59"] <- "40-59"
participants$participant_age <- factor(participants$participant_age, order=TRUE,
                                       levels = c("20-29", "30-39", "40-59", "60+"))

table(participants$participant_sex)
participants$participant_sex[participants$participant_sex=="Prefer not to answer"] <- "Not reported"
participants$participant_sex <- factor(participants$participant_sex,
                                       levels = c("Female", "Male", "Not reported"))

participants$education <- factor(participants$education,
                                 levels = c("Less than Bachelors", "Bachelors or higher"),
                                 labels = c("Lower than Bachelors", "Bachelors or higher"))

participants$race <- factor(participants$race,
                            levels = c("Asian","Black","White","Mixed","Other"))

# new categories of Hispanic as suggested by CDC reviewer
table(participants$race, participants$hispanic)
participants$racehispanic <- "NA"
participants <- participants %>%
  mutate(racehispanic = case_when(hispanic == "No" & race == "Asian" ~ "Asian, Non-Hispanic",
                                  hispanic == "No" & race == "Black" ~ "Black, Non-Hispanic",
                                  hispanic == "No" & race == "White" ~ "White, Non-Hispanic",
                                  hispanic == "No" & race == "Mixed" ~ "Mixed, Non-Hispanic",
                                  hispanic == "No" & race == "Other" ~ "Other, Non-Hispanic",
                                  hispanic == "Yes" ~ "Hispanic"),
         racehispanic = factor(racehispanic,
                               levels = c("Hispanic",
                                          "Asian, Non-Hispanic",
                                          "Black, Non-Hispanic",
                                          "White, Non-Hispanic",
                                          "Mixed, Non-Hispanic",
                                          "Other, Non-Hispanic"),
                               labels = c("Hispanic",
                                          "Asian, NH",
                                          "Black, NH",
                                          "White, NH",
                                          "Mixed, NH",
                                          "Other, NH")))
table(participants$racehispanic, useNA = "always")

participants$family_structure <- factor(participants$family_structure,
                                        levels=c("Alone", "Nuclear", "Extended", 
                                                 "Roommates", "Other"),
                                        labels=c("Live alone", "Nuclear", "Extended", 
                                                 "With roommates", "Other"))

participants$round <- factor(participants$round,
                             levels = c("One", "Two", "Three", "Four"))

###########################
###########################

# 2. contact files

contact_rd1 <- contact_rd1 %>%
  mutate(contact_location = ifelse(cont_home == "1", "Home", 
                                   ifelse(work == "1", "Work", "Community"))) %>%
  
  mutate(cont_attr = ifelse(cont_attr=="conv_only", "Conversation","Physical")) %>%
  mutate(round="One",
         contact_relation=NA) %>%
  dplyr::group_by(part_id) %>% 
  mutate(contact_name2 = 1:n(),
         contact_id = paste(part_id, contact_name2, sep="_")) %>%
  
  ungroup() %>%
  
  rename(participant_id = part_id,
         # contact_id = cont_id,
         fromdayone = contact_fromdayone,
         contact_sex = contact_gender,
         contact_type = cont_attr) %>%
  
  select(participant_id, contact_id, contact_age, contact_sex, contact_relation,
         contact_type, contact_location, contact_duration, round) %>%
  
  arrange(participant_id, contact_id, contact_age, contact_sex, contact_relation,
          contact_location, contact_type, round) # contact_relation2,

contact_rd1$contact_sex[contact_rd1$contact_sex == "I don't know"] <- "Not reported"

day_rd1 <- day_rd1 %>%
  rename(participant_id = part_id,
         num_contacts = social_cont) %>%
  select(participant_id, num_contacts) %>%
  left_join(participant_rd1, by="participant_id")

## round 2
contact_rd2 <- contact_rd2 %>%
  select(id, contact_id, contact_age_4, contact_sex, contact_relation2, 
         contact_type, contact_location2, contact_duration) %>%
  mutate(round="Two") %>%
  mutate(contact_location2 = ifelse(contact_location2 == "Home", "Home", 
                                    ifelse(contact_location2 == "Work", "Work", "Community"))) %>%
  rename(participant_id = id,
         contact_age = contact_age_4,
         contact_location = contact_location2,
         contact_relation = contact_relation2) %>%
  arrange(participant_id, contact_id, contact_age, contact_sex, contact_relation, 
          contact_type, contact_location, contact_duration, round)

temp_contacts_rd2 <- contact_rd2 %>%
  dplyr::group_by(participant_id) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n(), .groups = "drop")

# round 3
contact_rd3 <- contact_rd3 %>%
  select(id, contact_id, contact_age_4, contact_sex, contact_type, contact_relation2,
         contact_location2, contact_type, contact_duration) %>%
  mutate(round="Three") %>%
  mutate(contact_location2 = ifelse(contact_location2 == "Home", "Home", 
                                    ifelse(contact_location2 == "Work", "Work", "Community"))) %>%
  rename(participant_id = id,
         contact_age = contact_age_4,
         contact_location = contact_location2,
         contact_relation = contact_relation2) %>%
  arrange(participant_id, contact_id, contact_age, contact_sex, contact_relation, 
          contact_type, contact_location, contact_duration, round)

# round 4
contact_rd4 <- contact_rd4 %>%
  select(id, contact_id, contact_age_4, contact_sex, contact_type, contact_relation2,
         contact_location2, contact_duration) %>%
  mutate(round="Four") %>%
  mutate(contact_location2 = ifelse(contact_location2 == "Home", "Home", 
                                    ifelse(contact_location2 == "Work", "Work", "Community"))) %>%
  rename(participant_id = id,
         contact_age = contact_age_4,
         contact_location = contact_location2,
         contact_relation = contact_relation2) %>%
  arrange(participant_id, contact_id, contact_age, contact_sex, contact_relation, 
          contact_type, contact_location, contact_duration, round)

contacts <- rbind(contact_rd1, contact_rd2, contact_rd3, contact_rd4)
contacts$round <- factor(contacts$round,
                         levels = c("One", "Two", "Three","Four"))

contacts$contact_duration <- factor(contacts$contact_duration,
                                    levels = c("<5 mins", "5-15 mins", "15 mins-1 hr", 
                                               "1-4 hrs", "4+ hrs"))
###########################
###########################

# join participant and contact files for rounds
vars1 <- c("participant_id","participant_sex","participant_age",
           "participant_age_2","education","race","hispanic", "racehispanic",
           "family_structure", "contact_relation", "round", 
           "contact_id", "contact_age", "contact_sex", 
           "contact_location", "contact_duration", "contact_type")

rd1 <- participants %>%
  filter(round=="One") %>%
  left_join(contact_rd1, by="participant_id")  %>%
  drop_na(contact_age) %>%
  rename(round = round.x) %>%
  select(vars1)

rd2 <- participants %>%
  filter(round=="Two") %>%
  left_join(contact_rd2, by="participant_id") %>%
  rename(round = round.x) %>%
  select(vars1)

rd3 <- participants %>%
  filter(round=="Three") %>%
  left_join(contact_rd3, by="participant_id") %>%
  rename(round = round.x) %>%
  select(vars1)

rd4 <- participants %>%
  filter(round=="Four") %>%
  left_join(contact_rd4, by="participant_id") %>%
  rename(round = round.x) %>%
  select(vars1)

df_all <- rbind(rd1, rd2, rd3, rd4)
df_all$round <- factor(df_all$round,
                       levels = c("One", "Two", "Three", "Four"))


############################
##SAVE DATA
############################
write_rds(participants, "./data/publication_corporatemix_ms2/participants.RDS")
write_rds(day_rd1, "./data/publication_corporatemix_ms2/day_rd1.RDS")

write_rds(contacts, "./data/publication_corporatemix_ms2/contacts.RDS")
write_rds(df_all, "./data/publication_corporatemix_ms2/df_all.RDS")


# rm(participant_rd1, participant_rd2, participant_rd3, participant_rd4)
# rm(contact_rd1, contact_rd2, contact_rd3, contact_rd4)
# rm(rd1, rd2, rd3, rd4)

