
source("../../publication/scripts/02b_pre_pandemic_matrix.R")

## Source data by round

# contact_rd1 <- readRDS("data/df_contact_clean_rd1.RDS")
# part_rd1 <- readRDS("data/df_participant_clean_rd1.RDS")
# day_rd1 <- readRDS("data/df_day_clean_rd1.RDS")


## Make participant and contact data frames into format that can be inputted into socialmixr packge
df_rd1 <- df_all %>% # df_rd2 %>%
  filter(round == "One") %>%
  ungroup() %>%
  group_by(participant_id) %>%
  # group_by(email) %>%
  mutate(part_id=cur_group_id()) %>%  ## unique id for each participant
  ungroup() %>%
  mutate(cont_id = 1:n(),
         
         ## Recode location into binary for each location type, compatible with socialmixr package
         cnt_school = ifelse(contact_location == "School",1,0),
         cnt_home = ifelse(contact_location == "Home",1,0),
         cnt_work = ifelse(contact_location == "Work",1,0),
         cnt_otherplace = ifelse(contact_location == "Community",1,0),
         
         # cnt_school = ifelse(school == "1",1,0),
         # cnt_home = ifelse(cont_home == "1",1,0),
         # cnt_work = ifelse(work == "1",1,0),
         # cnt_otherplace = ifelse(school=="0"& cont_home=="0" & work == "0",1,0),
         
         cnt_gender = case_when(
           is.na(contact_sex) ~ NA_character_,
           contact_sex == "Female" ~ "F",
           contact_sex == "Male" ~ "M")
  ) %>% 

  # THE AGE GROUPS BELOW HAVE BEEN CHANGED TO REFLECT WHAT WEHAVE IN THE DATAFRAME
  
  mutate(
    cnt_age_est_min = case_when(
      contact_age == "0-9" ~ 0,
      contact_age == "10-19" ~ 10,
      contact_age == "20-29" ~ 20,
      contact_age == "30-39"~ 30,
      contact_age == "40-59" ~ 40,
      contact_age == "60+" ~ 60,
      TRUE~ NA_real_
    ),
    
    cnt_age_est_max = case_when(
      contact_age == "0-9" ~ 9,
      contact_age == "10-19" ~ 19,
      contact_age == "20-29" ~ 29,
      contact_age == "30-39"~ 39,
      contact_age == "40-59" ~ 59,
      contact_age == "60+" ~ 79,
      TRUE~ NA_real_
    )
  )

# contact_rd1_d1 <- contact_rd1 %>% filter(diaryday == "First day") ## Take only day one contacts

rd1 <- survey(
  participants = df_rd1 %>%
    # filter(round == "One") %>%
    select(part_id, participant_age, participant_sex) %>%
    rename(part_age = participant_age,
           part_gender = participant_sex) %>%
    # mutate(country = "usa") %>%
    unique(),
  
  contacts = df_rd1 %>% 
    select(part_id, cont_id, cnt_gender, 
           cnt_age_est_min, cnt_age_est_max,
           cnt_home, cnt_work, cnt_school, 
           cnt_otherplace)
)

rd1_cm_scaling <- function(...){
  cm_filter(df = rd1, boots = 1000,
            filter = TRUE, 
            symmetric = TRUE, 
            ...
  )
}

## Make bootstrapped matrix by location using the filter function
rd1_cm <- cm_filter(rd1, boots=1000)
rd1_cm_school <- rd1_cm_scaling(school = 1)
rd1_cm_home <- rd1_cm_scaling(home = 1)
rd1_cm_work <- rd1_cm_scaling(work = 1)
rd1_cm_other <- rd1_cm_scaling(other = 1)



# Take two matrices and calculate the scale factor for them
## Repeats the above functions to get the ratio of the max eigen values
## Of each matrix
scale_factor <- function(mat1, mat2, ... ){
  #x1 <- split_cm(mat1, ...)
  #x2 <- split_cm(mat2, ...)
  x1 <- mat1[[i]]$matrix[3:7,3:7]
  x2<- mat2[[i]][3:7,3:7]
  
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

# Take a full matrix and scale it based on ratio of mat1 and mat2
scale_matrix <- function(fullmat, mat1, mat2, i = 1, ...){
  #mat1 <- mat1[[i]]$matrix
  
  fullmat[[i]] * scale_factor(mat1, mat2, i = i, ...)
  
}

## Specific for this analysis get imputed values for H2020 matrix
impute_values <- function(i = 1, ...){
  imputed_values <- scale_matrix(
    pre_cm_home, 
    rd1_cm_home, 
    pre_cm_home,
    i = i
  ) +
    scale_matrix(
      pre_cm_work, 
      rd1_cm_work, 
      pre_cm_work, 
      i = i,
      ...
    ) +
    scale_matrix(
      pre_cm_other, 
      rd1_cm_other, 
      pre_cm_other,
      i = i,
      ...
    ) 
  
  # if(school){
  ## Schools were closed so can not scale by this so just add
  #  imputed_values <- imputed_values +
  #    polymod_boot_cm_school[[i]]$matrix 
  #}
  return(imputed_values)
  
}




boots <- length(pre_cm_home)
eigen_scale_rd1 <- numeric(boots)
rd1_impute <- list()

for (i in 1:1000){
  ## Scaling for each matrix
  res <- scale_factor_R(rd1_cm, pre_cm, i = i)
  eigen_scale_rd1[i] <- res[[1]]
  rd1_impute[[i]] <- res[[2]]
}

home_changeR_rd1 <- length(pre_cm_home)
for (i in 1:1000) {
  home_changeR_rd1[[i]]<- scale_factor1(rd1_cm_home, pre_cm_home,i=i)
}


work_changeR_rd1 <- length(pre_cm_home)
for (i in 1:1000) {
  work_changeR_rd1[[i]]<- scale_factor1(rd1_cm_work, pre_cm_work,i=i)
}

other_changeR_rd1 <- length(pre_cm_home)
for (i in 1:1000) {
  other_changeR_rd1[[i]]<- scale_factor1(rd1_cm_other, pre_cm_other,i=i)
}

rd1_res_list <- list(all_changeR = eigen_scale_rd1,
                     home_changeR = home_changeR_rd1,
                     work_changeR = work_changeR_rd1,
                     other_changeR = other_changeR_rd1)

saveRDS(rd1_res_list, "../../data/model/rd1_res.RDS")
