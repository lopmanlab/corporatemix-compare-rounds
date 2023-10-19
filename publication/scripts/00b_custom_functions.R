## Functions

### 1. Matrix generation and visualization

## Function used to visualize age-specific contact mixing matrix with controls 
## over title, text size, mid and max points for legend and legend position

contactmatrix_viz <- function(matrix1, title, txt_size, mid, max, legendpos) {
  ggplot(data = matrix1, aes(x=factor(participant_age), y=factor(contact_age), fill=avg_cont)) +  
    ## var1 is age of person, var2 is age of contact
    geom_raster(hjust = 0.5, vjust = 0.5, show.legend=T) +
    # geom_text(aes(participant_age, contact_age, label = round(avg_cont, digits=1)), color = "black", size = 4) +
    scale_fill_gradient2(low = "#2b83ba", mid = "#abdda4", high = "#ffffbf", 
                         midpoint = mid, limit = c(0, max)) +
    xlab("Participant age") + 
    ylab("Contact age") + 
    labs(fill = "Average \ncontact") + # "Avg \ncontact") +
    theme_classic() +
    theme(legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.justification = "right",
          legend.position = legendpos) +
    theme(plot.title = element_text(size = 28), 
          axis.title.x = element_text(size=26, face="bold", angle=0),
          axis.title.y = element_text(size=26, face="bold"),
          axis.text.x = element_text(size = 24),
          axis.text.y = element_text(size= 24)) +
    ggtitle(title) +
    theme(plot.margin = margin(6, 0, 6, 0))
}


make_matrix_rd2 <- function(df1, title, txt_size=12, mid =2.5, max = 5.0, legendpos="top") {
  df1 %>% 
    group_by(participant_age, contact_age) %>% 
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age","contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(num_participants_rd2, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n),
           avg_cont = replace_na(avg_cont, 0),
           avg_cont = na_if(avg_cont, 0)) %>%
    contactmatrix_viz(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}

make_matrix_rd3 <- function(df1, title, txt_size=12, mid =2.5, max = 5.0, legendpos="top") {
  df1 %>% 
    group_by(participant_age, contact_age) %>% 
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age","contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(num_participants_rd3, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n),
           avg_cont = replace_na(avg_cont, 0),
           avg_cont = na_if(avg_cont, 0)) %>%
    contactmatrix_viz(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}

make_matrix_rd4 <- function(df1, title, txt_size=12, mid =2.5, max = 5.0, legendpos="top") {
  df1 %>% 
    group_by(participant_age, contact_age) %>% 
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age","contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(num_participants_rd4, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n),
           avg_cont = replace_na(avg_cont, 0),
           avg_cont = na_if(avg_cont, 0)) %>%
    contactmatrix_viz(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}

# Difference matrix
diff_contactmatrix_viz <- function(matrix1, title, txt_size, mid, max, legendpos) {
  ggplot(data = matrix1, aes(x=factor(participant_age), y=factor(contact_age), fill=avg_cont)) +  
    ## var1 is age of person, var2 is age of contact
    geom_raster(hjust = 0.5, vjust = 0.5, show.legend=T) +
    # geom_text(aes(participant_age, contact_age, label = round(avg_cont, digits=1)), color = "black", size = 4) +
    scale_fill_gradient2(low = "#abdda4", mid = "#2b83ba", high = "#ffffbf", 
                         midpoint = mid, limit = c(0, max)) +
    xlab("Participant age") + 
    ylab("Contact age") + 
    labs(fill = "Average \ncontact") + # "Avg \ncontact") +
    theme_classic() +
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.justification = "right",
          legend.position = legendpos) +
    theme(plot.title = element_text(size = 28), 
          axis.title.x = element_text(size=26, face="bold", angle=0),
          axis.title.y = element_text(size=26, face="bold"),
          axis.text.x = element_text(size = 24),
          axis.text.y = element_text(size= 24)) +
    ggtitle(title)
}

make_diff_matrix <- function(df1, title, txt_size=12, mid =0, max = 2.0, legendpos="top") {
  df1 %>% 
    group_by(participant_age, contact_age) %>% 
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age","contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(num_participants_rd4, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n),
           avg_cont = replace_na(avg_cont, 0),
           avg_cont = na_if(avg_cont, 0)) %>%
    diff_contactmatrix_viz(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}

## for round 1 age groups
make_matrix_rd1 <- function(df1, title, txt_size=12, mid =2.5, max = 5.0, legendpos="top") {
  df1 %>% 
    group_by(participant_age, contact_age) %>% 
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age","contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(num_participants_rd1, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n)) %>%
    contactmatrix_viz_rd1(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}

contactmatrix_viz_rd1 <- function(matrix1, title, txt_size, mid, max, legendpos){
  ggplot(data = matrix1, aes(x=factor(participant_age), y=factor(contact_age), fill=avg_cont)) +  
    ## var1 is age of person, var2 is age of contact
    geom_raster(hjust = 0.5, vjust = 0.5, show.legend=T) +
    # geom_text(aes(participant_age, contact_age, label = round(avg_cont, digits=1)), color = "black", size = 4) +
    scale_fill_gradient2(low = "#2b83ba", mid = "#abdda4", high = "#ffffbf", 
                         midpoint = mid, limit = c(0, max)) +
    xlab("Participant age") + 
    ylab("Contact age") + 
    labs(fill = "") +
    theme_classic() +
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.justification = "right",
          legend.position = legendpos) +
    theme(plot.title = element_text(size = 28), 
          axis.title.x = element_text(size=26, face="bold", angle=0),
          axis.title.y = element_text(size=26, face="bold"),
          axis.text.x = element_text(size = 24),
          axis.text.y = element_text(size= 24)) +
    ggtitle(title)
}



## this generates the values of the age-specific mixing matrix
matrix_values <- function(df1) {
  df1 %>% 
    group_by(participant_age, contact_age) %>% 
    summarize(tot_contacts = n()) %>% 
    full_join(standard_str1, by= c("participant_age","contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(part_age1, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n))
}


### 2. Model functions



### Create a contact matrix with filters

## Put filter = TRUE
## Then put the value to filter by in the various filter variables

cm_filter <- function(
    df  = df,
    matrix_output = TRUE,
    #country = "United States",
    age_limits =c(0,10,20,30,40,50,60),
    symmetric =TRUE,
    filter = FALSE,
    phys_contact = NULL,
    home  = NULL,
    other_house = NULL,
    work = NULL,
    school = NULL,
    gender = NULL,
    other = NULL,
    boots = 1
){
  
  filter_text <- list()
  
  if(filter){
    filter_text$phys_contact <- phys_contact
    filter_text$cnt_home <- home
    filter_text$cnt_other_house <- other_house
    filter_text$cnt_work <- work
    filter_text$cnt_school <- school
    filter_text$cnt_otherplace <- other
    filter_text$cnt_gender <- gender
  }
  
  x <- contact_matrix(
    df, 
    #countries = country, 
    age.limits = age_limits, 
    symmetric = symmetric,
    filter = filter_text,
    estimated.contact.age = "sample",
    n = boots
  )
  if(matrix_output){
    if(boots == 1){
      return(x$matrix)
    } else {
      return(x$matrices)
    }  
  } else {
    
    return(x)
  }  
}

scale_factor_R <- function(mat1, mat2, i = 1, ...){
  x1 <- update_cm(mat1, mat2, i = i, ...)
  x2 <- mat2[[i]]
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  list(max_eigen = max_eigen_ratio(x1, x2),
       x1=x1)
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

# Split contact matrix to only the required rows and columns
## This is because we do not have data for participants <18
split_cm <- function(mat, i = 1, row = 3:7, col = 3:7, ...){
  mat[[i]]$matrix[row,col]
}

## Make the matrix symmetric
symm_mat <- function(x){
  (x + t(x))/ 2  
}

## Calculate ratio of max eigen values 
# This gives a sense of different magnitudes of the matrices
max_eigen_ratio <- function(x, y){
  max(eigen(x, only.values = TRUE)$values)/max(eigen(y, only.values = TRUE)$values)
  
}


# Take two matrices and calculate the scale factor for them
## Repeats the above functions to get the ratio of the max eigen values
## Of each matrix
scale_factor1 <- function(mat1, mat2, i=i... ){
  #x1 <- split_cm(mat1, ...)
  #x2 <- split_cm(mat2, ...)
  x1 <- mat1[[i]]$matrix[3:7,3:7]
  x2<- mat2[[i]][3:7,3:7]
  
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

scale_factor <- function(mat1, mat2, ...){
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

# ## define plotting theme
# my_theme <- function() {theme(
#     # add border
#     panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
#     # color background
#     panel.background = element_blank(),
#     # grids
#     panel.grid.major.x = element_line(colour = "gray", linetype = 3, size = 0.5),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y =  element_line(colour = "gray", linetype = 3, size = 0.5),
#     panel.grid.minor.y = element_blank(),
#     # modify text, axis and colour
#     # axis.text = element_text(colour = "black", family = "Times", size=12),
#     # axis.title = element_text(colour = "black", family = "Times", size=14),
#     axis.ticks = element_line(colour = "black"),
#     plot.title = element_text(size = 16), 
#     legend.title=element_text(size = 12),
#     # legend
#     legend.position = "right"
#   )
# }
