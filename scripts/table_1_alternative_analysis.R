variables <- data.frame(var=c("participant_sex","participant_age", "family_structure",
                              "contact_relation", "contact_location", 
                              "company", "race", "hispanic"),
                        name=c("Sex","Age Group", "Family structure", 
                               "Relationship to contact", "Location of contact",
                               "Company","Race","Hispanic")) %>% # "Relationship to contact"
  mutate(var = as.character(var),
         name = as.character(name))


quant1 <- quantile(day_rd1$num_contacts)
quant2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id) %>%
  dplyr::summarize(num_contacts = n()) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75)))))
quant3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id) %>%
  dplyr::summarize(num_contacts = n()) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75)))))
quant4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id) %>%
  dplyr::summarize(num_contacts = n()) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75)))))


sex1 <- day_rd1 %>%
  filter(round=="One") %>%
  group_by(participant_sex) %>%   # group by id and count number of contacts
  # dplyr::summarize(num_contacts = n()) %>%
  # group_by(participant_sex) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
sex1$med_contact <- as.character(paste(sex1$X50., "(",sex1$X25.,"-",sex1$X75.,")",sep=""))

sex2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, participant_sex) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(participant_sex) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
sex2$med_contact <- as.character(paste(sex2$X50.," 
                                             (",sex2$X25.,"-",sex2$X75.,")",sep=""))

sex3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, participant_sex) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(participant_sex) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
sex3$med_contact <- as.character(paste(sex3$X50.," 
                                             (",sex3$X25.,"-",sex3$X75.,")",sep=""))

sex4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, participant_sex) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(participant_sex) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
sex4$med_contact <- as.character(paste(sex4$X50.," 
                                             (",sex4$X25.,"-",sex4$X75.,")",sep=""))

sex <- qpcR:::cbind.na(sex1$med_contact, sex2$med_contact, sex3$med_contact, sex4$med_contact)
rm(sex1, sex2, sex3, sex4)


# "participant_age", 
age1 <- day_rd1 %>%
  filter(round=="One") %>%
  group_by(participant_age) %>%   # group by id and count number of contacts
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
age1$med_contact <- as.character(paste(age1$X50.," 
                                             (",age1$X25.,"-",age1$X75.,")",sep=""))

 age2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, participant_age_2) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(participant_age_2) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
age2$med_contact <- as.character(paste(age2$X50.," 
                                             (",age2$X25.,"-",age2$X75.,")",sep=""))

age3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, participant_age_2) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(participant_age_2) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
age3$med_contact <- as.character(paste(age3$X50.," 
                                             (",age3$X25.,"-",age3$X75.,")",sep=""))

age4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, participant_age_2) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(participant_age_2) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
age4$med_contact <- as.character(paste(age4$X50.," 
                                             (",age4$X25.,"-",age4$X75.,")",sep=""))

age <- qpcR:::cbind.na(age1$med_contact, age2$med_contact, age3$med_contact, age4$med_contact)
# rm(age1, age2, age3, age4)

# "company",
comp1 <- day_rd1 %>%
  filter(round=="One") %>%
  group_by(company) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
comp1$med_contact <- as.character(paste(comp1$X50.," (",comp1$X25.,"-",comp1$X75.,")",sep=""))

comp2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, company) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(company) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
comp2$med_contact <- as.character(paste(comp2$X50., "(",comp2$X25.,"-",comp2$X75.,")",sep=""))

comp3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, company) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(company) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
comp3$med_contact <- as.character(paste(comp3$X50.," (",comp3$X25.,"-",comp3$X75.,")",sep=""))

comp4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, company) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(company) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
comp4$med_contact <- as.character(paste(comp4$X50.," (",comp4$X25.,"-",comp4$X75.,")",sep=""))

comp <- qpcR:::cbind.na(comp1$med_contact, comp2$med_contact, comp3$med_contact, comp4$med_contact)
# rm(fam1, fam2, fam3, fam4)


# "contact_location", 
loc1 <- df_all %>%
  filter(round=="One") %>%
  group_by(participant_id, contact_location) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(contact_location) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
loc1$med_contact <- as.character(paste(loc1$X50.," (",loc1$X25.,"-",loc1$X75.,")",sep=""))

loc2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, contact_location) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(contact_location) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
loc2$med_contact <- as.character(paste(loc2$X50.," (",loc2$X25.,"-",loc2$X75.,")",sep=""))

loc3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, contact_location) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(contact_location) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
loc3$med_contact <- as.character(paste(loc3$X50.," (",loc3$X25.,"-",loc3$X75.,")",sep=""))

loc4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, contact_location) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(contact_location) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
loc4$med_contact <- as.character(paste(loc4$X50.," (",loc4$X25.,"-",loc4$X75.,")",sep=""))

loc <- qpcR:::cbind.na(loc1$med_contact, loc2$med_contact, loc3$med_contact, loc4$med_contact)
# rm(loc1, loc2, loc3, loc4)


# "family structure",
fam1 <- df_all %>%
  filter(round=="One") %>%
  # distinct(participant_id, .keep_all = TRUE) %>% 
  group_by(participant_id, family_structure) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(family_structure) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
fam1$med_contact <- as.character(paste(fam1$X50.," (",fam1$X25.,"-",fam1$X75.,")",sep=""))

fam2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, family_structure) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(family_structure) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
fam2$med_contact <- as.character(paste(fam2$X50.,"  (",fam2$X25.,"-",fam2$X75.,")",sep=""))

fam3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, family_structure) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(family_structure) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
fam3$med_contact <- as.character(paste(fam3$X50.," (",fam3$X25.,"-",fam3$X75.,")",sep=""))

fam4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, family_structure) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(family_structure) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
comp4$med_contact <- as.character(paste(fam4$X50.," (",fam4$X25.,"-",fam4$X75.,")",sep=""))

comp <- qpcR:::cbind.na(comp1$med_contact, comp2$med_contact, comp3$med_contact, comp4$med_contact)
# rm(comp1, comp2, comp3, comp4)


# "race", 
race1 <- df_all %>%
  filter(round=="One") %>%
  group_by(participant_id, race) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(race) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
race1$med_contact <- as.character(paste(race1$X50.," 
                                             (",race1$X25.,"-",race1$X75.,")",sep=""))

race2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, race) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(race) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
race2$med_contact <- as.character(paste(race2$X50.," 
                                             (",race2$X25.,"-",race2$X75.,")",sep=""))

race3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, race) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(race) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
race3$med_contact <- as.character(paste(race3$X50.," 
                                             (",race3$X25.,"-",race3$X75.,")",sep=""))

race4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, race) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(race) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
race4$med_contact <- as.character(paste(race4$X50.," 
                                             (",race4$X25.,"-",race4$X75.,")",sep=""))

race <- qpcR:::cbind.na(race1$med_contact, race2$med_contact, race3$med_contact, race4$med_contact)
# rm(race1, race2, race3, race4)


# "hispanic"
hisp1 <- df_all %>%
  filter(round=="One") %>%
  distinct(participant_id, .keep_all = TRUE) %>% 
  group_by(participant_id, hispanic) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(hispanic) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
hisp1$med_contact <- as.character(paste(hisp1$X50.," 
                                             (",hisp1$X25.,"-",hisp1$X75.,")",sep=""))

hisp2 <- df_all %>%
  filter(round=="Two") %>%
  group_by(participant_id, hispanic) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(hispanic) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
hisp2$med_contact <- as.character(paste(hisp2$X50.," 
                                             (",hisp2$X25.,"-",hisp2$X75.,")",sep=""))

hisp3 <- df_all %>%
  filter(round=="Three") %>%
  group_by(participant_id, hispanic) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(hispanic) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
hisp3$med_contact <- as.character(paste(hisp3$X50.," 
                                             (",hisp3$X25.,"-",hisp3$X75.,")",sep=""))

hisp4 <- df_all %>%
  filter(round=="Four") %>%
  group_by(participant_id, hispanic) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n()) %>%
  group_by(hispanic) %>%
  do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
hisp4$med_contact <- as.character(paste(hisp4$X50.," 
                                             (",hisp4$X25.,"-",hisp4$X75.,")",sep=""))

hisp <- qpcR:::cbind.na(hisp1$med_contact, hisp2$med_contact, hisp3$med_contact, hisp4$med_contact)
# rm(hisp1, hisp2, hisp3, hisp4)

