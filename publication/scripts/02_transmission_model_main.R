
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(socialmixr)

participants <- readRDS("../../publication/data/participants.RDS")
day1_rd1 <- readRDS("../../publication/data/day_rd1.RDS")
contacts <- readRDS("../../publication/data/contacts.RDS")
df_all <- readRDS("../../publication/data/df_all.RDS")

source("../../publication/scripts/00c_model_functions.R")


# r run-models

source("../../publication/scripts/02c_rd1_model.R")
source("../../publication/scripts/02d_rd2_model.R")
source("../../publication/scripts/02e_rd3_model.R")
source("../../publication/scripts/02f_rd4_model.R")



# ata-import}
# changed input data path
rd1_res_list <- readRDS("../../publication/data/model/rd1_res.RDS")
rd2_res_list <- readRDS("../../publication/data/model/rd2_res.RDS")
rd3_res_list <- readRDS("../../publication/data/model/rd3_res.RDS")
rd4_res_list <- readRDS("../../publication/data/model/rd4_res.RDS")

## set data frames for ploting

df <- data.frame(value = c(Reduce(c,rd1_res_list), ## Reduce list of vectors into one vector, combined into column in data frame
                           Reduce(c,rd2_res_list),
                           Reduce(c,rd3_res_list),
                           Reduce(c,rd4_res_list)),
                 round = rep(c("R1 Apr-Jun 20", "R2 Nov 20-Jan 21", "R3 Jun-Aug 21", "R4 Nov-Dec 21"),each = 4000),
                 location = rep(rep(c("All","Home", "Work", "Community"), each = 1000),times=4)) %>%
  mutate(location = factor(location, levels = c("All", "Home", "Work", "Community")))


## Output
fig_rel_transmission <- ggplot(df, aes(value, fill = round)) + geom_density() +
  geom_density(alpha=0.5) +
  geom_vline(xintercept = 1, col="black", lwd=1, lty=2)+
  facet_wrap(~location, nrow=4) +
  theme_classic() +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.justification = "right",
        # legend.position = legendpos,
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size=16, face="bold", angle=0),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size= 14)) +
  scale_fill_manual("Round",values = c("#08519C","#3182BD","#6BAED6","#BDD7E7")) +
  xlab("Relative transmissibility due to changes in contact patterns")+
  ylab("Density") 

ggsave(fig_rel_transmission, filename = "publication/output/fig_rel_transmission.pdf",
       height=8, width=10, dpi=300,
       bg="#FFFFFF")

brewer.pal(n=4,"Blues")




fig_b<- ggplot(df, aes(value, fill = round)) + geom_density()+
  geom_vline(xintercept = 1, col="black", lwd=1, lty=2)+
  geom_density(alpha = 0.4) +
  facet_wrap(~location)+theme_bw() +
  scale_fill_manual("Round",values = rev(brewer.pal(n=4,"Blues")))+
  xlab("Relative transmissability due to contact changes compared to pre-pandemic")+
  ylab("Density") 




fig_c <- df %>% filter(location == "All") %>%
  ggplot(aes(value, fill = round)) + geom_density(alpha = 0.2) +
  theme_bw() +
  xlab("Prop transmissability due to contact changes compared to pre-pandemic")+
  ggtitle("All locations")



fig_d <- df %>% filter(location == "Home") %>%
  ggplot(aes(value, fill = round)) + geom_density(alpha = 0.2) +
  theme_bw() +
  xlab("Prop transmissability due to contact changes compared to pre-pandemic")+
  
  ggtitle("Home")



fig_e <- df %>% filter(location == "Work") %>%
  ggplot(aes(value, fill = round)) + geom_density(alpha = 0.2) +
  theme_bw() +
  xlab("Prop transmissability due to contact changes compared to pre-pandemic")+
  ggtitle("Work")



fig_f <- df %>% filter(location == "Community") %>%
  ggplot(aes(value, fill = round)) + geom_density(alpha = 0.2) +
  theme_bw() +
  xlab("Prop transmissability due to contact changes compared to pre-pandemic")+
  ggtitle("Other community locations")