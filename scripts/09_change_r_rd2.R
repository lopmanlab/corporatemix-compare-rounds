
source("scripts/09_pre_pandemic.R")

## Source data by round
## round 2
cont_rd2 <- readRDS("data/df_contact_clean_rd2.RDS")
part_rd1 <- readRDS("data/df_participant_clean_rd2.RDS")
df_rd2 <- readRDS("data/df_merged_rd2.RDS")

## Make participant and contact data frames into format that can be inputted into socialmixr packge
df_rd2 <- df_rd2 %>% 
  ungroup() %>%
  group_by(id) %>%
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
  
df_rd2_d1 <- df_rd2 %>% filter(contact_day != "Day 2 only") ## Take only day one contacts

##weights 

rd2 <- survey(
  participants = df_rd2_d1 %>% ungroup() %>%
                  select(part_id, age, participant_sex) %>% unique() %>%
                  rename("part_age" = "age") %>%
                  rename("part_gender" = "participant_sex") ,
  
  contacts = df_rd2_d1 %>% select(part_id, cont_id, cnt_gender, 
                               cnt_age_est_min, cnt_age_est_max,
                               cnt_home, cnt_work, cnt_school, 
                               cnt_otherplace)
)

rd2_cm_scaling <- function(...){
  cm_filter(df = rd2, boots = 1000,
            filter = TRUE, 
            symmetric = TRUE, 
            ...
  )
}

## Make bootstrapped matrix by location using the filter function
rd2_cm <- cm_filter(rd2, boots=1000)
rd2_cm_school <- rd2_cm_scaling(school = 1)
rd2_cm_home <- rd2_cm_scaling(home = 1)
rd2_cm_work <- rd2_cm_scaling(work = 1)
rd2_cm_other <- rd2_cm_scaling(other = 1)



## Specific for this analysis get imputed values for H2020 matrix
impute_values <- function(i = 1, ...){
  imputed_values <- scale_matrix(
    pre_cm_home, 
    rd2_cm_home, 
    pre_cm_home,
    i = i
  ) +
    scale_matrix(
      pre_cm_work, 
      rd2_cm_work, 
      pre_cm_work, 
      i = i,
      ...
    ) +
    scale_matrix(
      pre_cm_other, 
      rd2_cm_other, 
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


update_cm <- function(
  mat1, mat2, 
  impute_rows = 1:2, impute_cols = 1:7, 
  i = 1,  
  observed_rows = 2:7, 
  observed_col = 2:7,
  school = FALSE,
  ...
){
  
  mat1 <- mat1[[i]]$matrix
  mat2 <- mat2[[i]]
  
  update_mat <- matrix(0, nrow = nrow(mat1), ncol = ncol(mat1))
  row.names(update_mat) <- colnames(mat1)
  colnames(update_mat)  <- colnames(mat1)
  
  #non_imputed_values <- mat1[row, col]
  update_mat <- mat1
  
  imputed_values <- impute_values(i = i, school = school, ... )[impute_rows, impute_cols]
  update_mat[impute_rows, impute_cols] <- imputed_values
  
  symm_mat(update_mat)
}

scale_factor_R <- function(mat1, mat2, i = 1, ...){
  x1 <- update_cm(mat1, mat2, i = i, ...)
  x2 <- mat2[[i]]
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  list(max_eigen = max_eigen_ratio(x1, x2),
       x1=x1)
}


boots <- length(pre_cm_home)
eigen_scale_rd2 <- numeric(boots)
rd2_impute <- list()

for (i in 1:1000){
  ## Scaling for each matrix
  res <- scale_factor_R(rd2_cm, pre_cm, i = i)
  eigen_scale_rd2[i] <- res[[1]]
  rd2_impute[[i]] <- res[[2]]
}


home_changeR_rd2 <- length(pre_cm_home)
for (i in 1:1000) {
  home_changeR_rd2[[i]]<- scale_factor1(rd2_cm_home, pre_cm_home,i=i)
}


work_changeR_rd2 <- length(pre_cm_home)
for (i in 1:1000) {
  work_changeR_rd2[[i]]<- scale_factor1(rd2_cm_work, pre_cm_work,i=i)
}

other_changeR_rd2 <- length(pre_cm_home)
for (i in 1:1000) {
  other_changeR_rd2[[i]]<- scale_factor1(rd2_cm_other, pre_cm_other,i=i)
}


rd2_res_list <- list(all_changeR = eigen_scale_rd2,
                     home_changeR = home_changeR_rd2,
                     work_changeR = work_changeR_rd2,
                     other_changeR = other_changeR_rd2)


saveRDS(rd2_res_list, "output/rel_transmission_interim/rd2_res.RDS")



