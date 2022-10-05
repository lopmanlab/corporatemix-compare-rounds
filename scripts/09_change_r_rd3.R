
source("scripts/09_pre_pandemic.R")

## Source data by round

cont_rd3 <- readRDS("data/df_contact_clean_rd3.RDS")
part_rd3 <- readRDS("data/df_participant_clean_rd3.RDS")
df_rd3 <- readRDS("data/df_merged_rd3.RDS")

## Make participant and contact data frames into format that can be inputted into socialmixr packge
df_rd3 <- df_rd3 %>% 
  ungroup() %>%
  group_by(email) %>%
  mutate(part_id=cur_group_id()) %>%  ## unique id for each participant
  ungroup() %>%
  mutate(cont_id = 1:n(),## unique id for each contact
         ## Recode location into binary for each location type, compatible with socialmixr package
         cnt_school = ifelse(contact_location == "School",1,0),
         cnt_home = ifelse(contact_location2 == "Home",1,0),
         cnt_work = ifelse(contact_location2 == "Work",1,0),
         cnt_otherplace = ifelse(contact_location2 == "Community",1,0),
         
         cnt_gender = case_when(
           is.na(contact_sex) ~ NA_character_,
           contact_sex == "Female" ~ "F",
           contact_sex == "Male" ~ "M"))  %>% 
  
  mutate(
    cnt_age_est_min = case_when(
      contact_age_1 == "<1" ~ 0,
      contact_age_1 == "1-9" ~ 1,
      contact_age_1 == "10-19" ~ 10,
      contact_age_1 == "20-29" ~ 20,
      contact_age_1 == "30-39"~ 30,
      contact_age_1 == "40-49" ~ 40,
      contact_age_1 == "50-59" ~ 50,
      contact_age_1 == "60-69" ~ 60,
      contact_age_1 == "70+" ~ 70,
      TRUE~ NA_real_
    ),
    
    cnt_age_est_max = case_when(
      contact_age_1 == "<1" ~ 1,
      contact_age_1 == "1-9" ~ 9,
      contact_age_1 == "10-19" ~ 19,
      contact_age_1 == "20-29" ~ 29,
      contact_age_1 == "30-39"~ 39,
      contact_age_1 == "40-49" ~ 49,
      contact_age_1 == "50-59" ~ 59,
      contact_age_1 == "60-69" ~ 69,
      contact_age_1 == "70+" ~ 79,
      TRUE~ NA_real_
    )
  )

df_rd3_d1 <- df_rd3 %>% filter(contact_day != "Day 2 only") ## Take only day one contacts

rd3 <- survey(
  participants = df_rd3_d1 %>% ungroup() %>%
    select(part_id, age, participant_sex) %>% unique() %>%
    rename("part_age" = "age") %>%
    rename("part_gender" = "participant_sex"),
  
  contacts = df_rd3_d1 %>% select(part_id, cont_id, cnt_gender, 
                               cnt_age_est_min, cnt_age_est_max,
                               cnt_home, cnt_work, cnt_school, 
                               cnt_otherplace)
)

rd3_cm_scaling <- function(...){
  cm_filter(df = rd3, boots = 1000,
            filter = TRUE, 
            symmetric = TRUE, 
            ...
  )
}

## Make bootstrapped matrix by location using the filter function
rd3_cm <- cm_filter(rd3, boots=1000)
rd3_cm_school <- rd3_cm_scaling(school = 1)
rd3_cm_home <- rd3_cm_scaling(home = 1)
rd3_cm_work <- rd3_cm_scaling(work = 1)
rd3_cm_other <- rd3_cm_scaling(other = 1)



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
    rd3_cm_home, 
    pre_cm_home,
    i = i
  ) +
    scale_matrix(
      pre_cm_work, 
      rd3_cm_work, 
      pre_cm_work, 
      i = i,
      ...
    ) +
    scale_matrix(
      pre_cm_other, 
      rd3_cm_other, 
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
eigen_scale_rd3 <- numeric(boots)
rd3_impute <- list()

for (i in 1:1000){
  ## Scaling for each matrix
  res <- scale_factor_R(rd3_cm, pre_cm, i = i)
  eigen_scale_rd3[i] <- res[[1]]
  rd3_impute[[i]] <- res[[2]]
}

home_changeR_rd3 <- length(pre_cm_home)
for (i in 1:1000) {
  home_changeR_rd3[[i]]<- scale_factor1(rd3_cm_home, pre_cm_home,i=i)
}


work_changeR_rd3 <- length(pre_cm_home)
for (i in 1:1000) {
  work_changeR_rd3[[i]]<- scale_factor1(rd3_cm_work, pre_cm_work,i=i)
}

other_changeR_rd3 <- length(pre_cm_home)
for (i in 1:1000) {
  other_changeR_rd3[[i]]<- scale_factor1(rd3_cm_other, pre_cm_other,i=i)
}


rd3_res_list <- list(all_changeR = eigen_scale_rd3,
                     home_changeR = home_changeR_rd3,
                     work_changeR = work_changeR_rd3,
                     other_changeR = other_changeR_rd3)

saveRDS(rd3_res_list, "output/rel_transmission_interim/rd3_res.RDS")


