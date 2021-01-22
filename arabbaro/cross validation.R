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

y1 <- full_join(egypt_y1, jordan_y1,by="variable", suffix=c("_eg","_jo"))

y1 <- full_join(y1, morocco_y1,by="variable", suffix=c("_mo","_mo"))

y1 <- full_join(y1, kuwait_y1,by="variable", suffix=c("","_ku"))

y1 <- y1 %>% mutate(vi_avg= (ifelse(is.na(vi_eg),0,vi_eg) + ifelse(is.na(vi_jo),0,vi_jo) + ifelse(is.na(vi),0,vi) + ifelse(is.na(vi_ku),0,vi_ku))/4)

help(mutate)


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

trust_cross_val <- grid.arrange(labs1, eg_y1, jo_y1, ku_y1, mo_y1, ncol=5)

# same but y2

y2 <- full_join(egypt_y2, jordan_y2,by="variable", suffix=c("_eg","_jo"))

y2 <- full_join(y2, morocco_y2,by="variable", suffix=c("_mo","_mo"))

y2 <- full_join(y2, algeria_y2,by="variable", suffix=c("","_al"))

y2 <- y2 %>% mutate(vi_avg= (ifelse(is.na(vi_eg),0,vi_eg) + ifelse(is.na(vi_jo),0,vi_jo) + ifelse(is.na(vi),0,vi) + ifelse(is.na(vi_al),0,vi_al))/4)

help(mutate)


labs <- y2 %>%
  arrange(vi_avg) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable,y=vi_avg)) + 
  coord_flip() +
  labs(title = "Democracy Preference") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
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

regime_pref_cross_val <- grid.arrange(labs, eg_y2, jo_y2, al_y2, mo_y2, ncol=5)

view(y2)

help(grid.arrange)

# old stuff starts here

alg_2 <- algeria_y2 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
    geom_bar(stat="identity", color="black",position=position_dodge()) + 
    coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Algeria, Regime Preference") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))

egy_1 <- egypt_y1 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Egypt, Trust in Gov") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))


egy_2 <- egypt_y2 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Egypt, Regime Preference") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))

jor_1 <- jordan_y1 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high))+
  labs(title = "Jordan, Trust in Gov") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))

jor_2 <- jordan_y2 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Jordan, Regime Preference") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))

mor_1 <- morocco_y1 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Morocco, Trust in Gov") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))

mor_2 <- morocco_y2 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(title = "Morrocco, Regime Preference") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))

kuw_1 <- kuwait_y1 %>%
  arrange(vi) %>%
  mutate(variable=factor(variable, levels=variable)) %>%
  ggplot(aes(x=variable, y=vi)) + 
  geom_bar(stat="identity", color="black",position=position_dodge()) + 
  coord_flip() +
  geom_errorbar(aes(ymin=low,ymax=high)) + 
  labs(title = "Kuwait, Trust in Gov") +
  theme(axis.text.y = element_text(angle = 35, hjust = 1))


trust_cross_val <- grid.arrange(egy_1, jor_1, kuw_1, mor_1, ncol=4)

regime_pref_cross_val <- grid.arrange(alg_2, egy_2, jor_2, mor_2, ncol=4)

trust_cross_val
