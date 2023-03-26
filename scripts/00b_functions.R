
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
