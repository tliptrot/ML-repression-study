library(tidyverse)
library(gridExtra)
library(readr)

setwd("~/GitHub/ML-repression-study/variable importance csv")

algeria_y2 <- read_csv("algeria_y2")
egypt_y1 <- read_csv("egypt_y1")
egypt_y2 <- read_csv("egypt_y2")
jordan_y1 <- read_csv("jordan_y1")
jordan_y2 <- read_csv("jordan_y2")
kuwait_y1 <- read_csv("kuwait_y1")
morocco_y1 <- read_csv("morocco_y1")
morocco_y2 <- read_csv("morocco_y2")
sudan_y1 <- read_csv("sudan_y1")
sudan_y2 <- read_csv("sudan_y2")

y1 <- full_join(egypt_y1, jordan_y1,by="variable", suffix=c("_eg","_jo"))

y1 <- full_join(y1, morocco_y1,by="variable", suffix=c("_mo","_mo"))

y1 <- full_join(y1, kuwait_y1,by="variable", suffix=c("","_ku"))

y1 <- full_join(y1, sudan_y1,by="variable", suffix=c("","_su"))

view(y1)

y1 <- y1 %>% mutate(sum_appearances = 5 - is.na(vi_eg) - is.na(vi_jo) - is.na(vi) - is.na(vi_ku) - is.na(vi_su),
  vi_avg= (ifelse(is.na(vi_eg),0,vi_eg) + ifelse(is.na(vi_jo),0,vi_jo) + ifelse(is.na(vi),0,vi) + ifelse(is.na(vi_ku),0,vi_ku) + + ifelse(is.na(vi_su),0,vi_su))/sum_appearances,
  vi_sd_avg = sqrt(ifelse(is.na(vi_eg),0,vi_eg)^2 + ifelse(is.na(vi_jo),0,vi_jo)^2 + ifelse(is.na(vi),0,vi)^2 + ifelse(is.na(vi_ku),0,vi_ku)^2 + ifelse(is.na(vi_su),0,vi_su)^2)/sum_appearances)

view(select(y1, variable, sum_appearances))

help(mutate)

#plots average varimp

y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_avg)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=vi_avg-2*vi_sd_avg,ymax=vi_avg+2*vi_sd_avg)) +
  labs(title = "Trust") 
#  theme(axis.text.y = element_blank(),
#        axis.title.y = element_blank(),
#        axis.title.x = element_blank())



labs1 <- y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable,y=vi_avg)) + 
  coord_flip() +
  labs(title = "Trust in Gov") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
#  theme(axis.text.y = element_text(angle = 35, hjust = 1))


eg_y1 <- y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_eg)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_eg,ymax=high_eg)) +
  labs(title = "Egypt") + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())



jo_y1 <- y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_jo)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_jo,ymax=high_jo)) +
  labs(title = "Jordan") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


mo_y1 <- y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Morocco") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

ku_y1 <- y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_ku)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_ku,ymax=high_ku)) +
  labs(title = "Kuwait") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

su_y1 <- y1 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_su)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_su,ymax=high_su)) +
  labs(title = "Sudan") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

trust_cross_val <- grid.arrange(labs1, eg_y1, jo_y1, ku_y1, mo_y1, su_y1, ncol=6)

# same but y2

y2 <- full_join(egypt_y2, jordan_y2,by="variable", suffix=c("_eg","_jo"))

y2 <- full_join(y2, morocco_y2,by="variable", suffix=c("_mo","_mo"))

y2 <- full_join(y2, algeria_y2,by="variable", suffix=c("","_al"))

y2 <- full_join(y2, sudan_y2,by="variable", suffix=c("","_su"))

view(y2)

y2 <- y2 %>% mutate(sum_appearances = 5 - is.na(vi_eg) - is.na(vi_jo) - is.na(vi) - is.na(vi_al) - is.na(vi_su),
                    vi_avg= (ifelse(is.na(vi_eg),0,vi_eg) + ifelse(is.na(vi_jo),0,vi_jo) + ifelse(is.na(vi),0,vi) + ifelse(is.na(vi_al),0,vi_al) + + ifelse(is.na(vi_su),0,vi_su))/sum_appearances,
                    vi_sd_avg = sqrt(ifelse(is.na(vi_eg),0,vi_eg)^2 + ifelse(is.na(vi_jo),0,vi_jo)^2 + ifelse(is.na(vi),0,vi)^2 + ifelse(is.na(vi_al),0,vi_al)^2 + ifelse(is.na(vi_su),0,vi_su)^2)/sum_appearances)

view(select(y2, variable, sum_appearances))

help(mutate)

#plots average varimp

y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_avg)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=vi_avg-2*vi_sd_avg,ymax=vi_avg+2*vi_sd_avg)) +
  labs(title = "Preference for Democracy") 
#  theme(axis.text.y = element_blank(),
#        axis.title.y = element_blank(),
#        axis.title.x = element_blank())



labs2 <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable,y=vi_avg)) + 
  coord_flip() +
  labs(title = "Preference for Democracy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
#  theme(axis.text.y = element_text(angle = 35, hjust = 1))

eg_y2 <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_eg)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_eg,ymax=high_eg)) +
  labs(title = "Egypt") + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())



jo_y2 <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_jo)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_jo,ymax=high_jo)) +
  labs(title = "Jordan") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


mo_y2 <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Morocco") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

al_y2 <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_al)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_al,ymax=high_al)) +
  labs(title = "Algeria") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

su_y2 <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi_su)) + 
  geom_point(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low_su,ymax=high_su)) +
  labs(title = "Sudan") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

dem_pref_cross_val <- grid.arrange(labs2, eg_y2, jo_y2, al_y2, mo_y2, su_y2, ncol=6)




trust_cross_val <- grid.arrange(labs1, eg_y1, jo_y1, ku_y1, mo_y1, su_y1, ncol=6)

dem_pref_cross_val <- grid.arrange(labs2, eg_y2, jo_y2, al_y2, mo_y2, su_y2, ncol=6)

count(df_alg, user_twitter)
count(df_egy, user_twitter)
count(dfjo_trans, user_twitter)
count(df_mor, user_twitter)
count(df_kuw, user_twitter)
df_mor$y_2_dem_pref_not_in_dem
#morocco 22%
(319 + 178)/(2249)

#algeria
(142 + 239)/2089

count(df_alg, user_telegram)
count(df_egy, user_telegram)
count(dfjo_trans, user_telegram)
count(df_mor, user_telegram)
count(df_kuw, user_telegram)
count(df_sud, user_telegram)

count(df_alg, user_snapchat)
count(df_egy, user_snapchat)
count(dfjo_trans, user_snapchat)
count(df_mor, user_snapchat)
count(df_kuw, user_snapchat)
count(df_sud, user_snapchat)

count(df_alg, user_whatsapp)
count(df_egy, user_whatsapp)
count(dfjo_trans, user_whatsapp)
count(df_mor, user_whatsapp)
count(df_kuw, user_whatsapp)
count(df_sud, user_whatsapp)

library(dplyr)
library(tidyr)
df_alg %>%
  count(user_whatsapp) %>% 
  mutate(percent = n/sum(n)) %>% 
  select(-n) %>% 
  spread(user_whatsapp, percent)
df_egy %>%
  count(user_whatsapp) %>% 
  mutate(percent = n/sum(n)) %>% 
  select(-n) %>% 
  spread(user_whatsapp, percent)
dfjo_trans %>%
  count(user_whatsapp) %>% 
  mutate(percent = n/sum(n)) %>% 
  select(-n) %>% 
  spread(user_whatsapp, percent)
df_mor %>%
  count(user_whatsapp) %>% 
  mutate(percent = n/sum(n)) %>% 
  select(-n) %>% 
  spread(user_whatsapp, percent)
df_kuw %>%
  count(user_whatsapp) %>% 
  mutate(percent = n/sum(n)) %>% 
  select(-n) %>% 
  spread(user_whatsapp, percent)
df_sud %>%
  count(user_whatsapp) %>% 
  mutate(percent = n/sum(n)) %>% 
  select(-n) %>% 
  spread(user_whatsapp, percent)


pbinom(0,2,.25) 
