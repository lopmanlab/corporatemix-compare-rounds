# source("../../publication/scripts/00c_model_functions.R")

## Pre-pandemic data
pre <- readRDS("../../publication/data/contact_usa.rds")
usapop <- read.csv("../../publication/data/usapop.csv")

## US age distribution for original Prem matrix and Corporate Mix age structure
usapop7 <- usapop %>% group_by(agegroup7) %>% summarise(n=sum(tot)) %>% .[[2]]
usapop16 <- usapop%>% group_by(agegroup16) %>% summarise(n=sum(tot)) %>% .[[2]]

weights <- usapop %>% filter(agegroup7 !="10_under" & agegroup7 != "10to19") %>%
  group_by(agegroup7) %>%
  summarise(n=sum(tot)) %>%
  mutate(prop =  n/sum(n))
## Collapse age groups into similar age bands as corporate mix

re_mat <- function(x) {
  x1 <- x*usapop16
  matrix(c(sum(x1[1:2,1:2]), sum(x1[1:2,3:4]), sum(x1[1:2,5:6]), 
           sum(x1[1:2,7:8]),sum(x1[1:2,9:10]), sum(x1[1:2,11:12]), sum(x1[1:2,13:16]),
           sum(x1[3:4,1:2]), sum(x1[3:4,3:4]), sum(x1[3:4,5:6]),
           sum(x1[3:4,7:8]),sum(x1[3:4,9:10]), sum(x1[3:4,11:12]), sum(x1[3:4,13:16]),
           sum(x1[5:6,1:2]), sum(x1[5:6,3:4]), sum(x1[5:6,5:6]),
           sum(x1[5:6,7:8]),sum(x1[5:6,9:10]), sum(x1[5:6,11:12]), sum(x1[5:6,13:16]),
           sum(x1[7:8,1:2]), sum(x1[7:8,3:4]), sum(x1[7:8,5:6]),
           sum(x1[7:8,7:8]),sum(x1[7:8,9:10]), sum(x1[7:8,11:12]), sum(x1[7:8,13:16]),
           sum(x1[9:10,1:2]), sum(x1[9:10,3:4]), sum(x1[9:10,5:6]),
           sum(x1[9:10,7:8]),sum(x1[9:10,9:10]), sum(x1[9:10,11:12]), sum(x1[9:10,13:16]),
           sum(x1[11:12,1:2]), sum(x1[11:12,3:4]), sum(x1[11:12,5:6]),
           sum(x1[11:12,7:8]),sum(x1[11:12,9:10]), sum(x1[11:12,11:12]), sum(x1[11:12,13:16]),
           sum(x1[13:16,1:2]), sum(x1[13:16,3:4]), sum(x1[13:16,5:6]),
           sum(x1[13:16,7:8]),sum(x1[13:16,9:10]), sum(x1[13:16,11:12]), sum(x1[13:16,13:16])),
         ncol=7, nrow=7, byrow=T)/usapop7
}

pre_new <- lapply(pre, re_mat) ## collapse age groups

pre_cm <- rep(list(pre_new$all),1000)
pre_cm_home<- rep(list(pre_new$home),1000)
pre_cm_work<- rep(list(pre_new$work),1000)
pre_cm_school <- rep(list(pre_new$school), 1000)
pre_cm_other <- rep(list(pre_new$others), 1000)
