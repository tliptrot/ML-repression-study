# Tim Liptrot and Youssef AbdelFatah

#creates country csv's for the classification project

library(tidyverse)
df <- read_csv('https://github.com/tliptrot/ML-repression-study/blob/main/arabbaro/ABV_Release_Data.csv?raw=true')

#creates a tibble called dfjo which has the following properties
# the x variables get simple new names
# the y variables all start with y




dfjo <- df %>%
  filter(country == 8) %>%
  mutate(y = Q201A_1 %in% c(3,4),
         medinc = Q1015A_JO == 2,
         lowinc = Q1015B_JO,
         highinc = Q1015C_JO,
         remit = Q1017,
         hijab = Q1010C,
         married = Q1010==4,
         employed = Q1005==1,
         employed_pub_sec = Q1005==1&Q1006A==1,
         employed_priv_sec = Q1005==1&Q1006A==2,
         selfemp = Q1005==2,
         retired = Q1005==3,
         housewife = Q1005==4,
         student = Q1005==5,
         unemp = Q1005==6,
         neighbrich = Q1001C,
         educ = Q1003,
         male = Q1002,
         age = Q1001,
         orgmem = (Q501==1|Q501A==1),
         charity = Q266,
         petit = Q502_1,
         protest = Q502_2,
         polviol = Q502_4,
         campaign_attend = Q302,
         parlvote_nas_not_asked_1_is_yes = Q301A, 
         locvote= Q301C==1,
         #returns TRUE if uses iternet
         internet = Q409 < 6,
         #1-5, where 1 is using internet most often
         internet_use_ordinal_1_is_top = ifelse(Q409<6,Q409,NA),
         #1-5, where 5 is using 10 hours or more a day
         socmed_use_ordinal_5_is_top = ifelse(Q424<10,Q424,0),
         infs_face2face_tel = (Q421 %in% c(1,2)),
         infs_newspaper = (Q421 == 3),
         infs_radio = (Q421==4),
         infs_television = (Q421==5),
         infs_socmed = (Q421==6)
         ) %>%
  mutate(joorpal = Q1020JO) %>%
  mutate(y_dem_top_issue = Q2061A==3,
         y_dem_pref = Q516A==3,
         y_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
         y_trust_gov = Q201A_1 %in% c(3,4),
         y_trust_army = Q201B_6 %in% c(3,4),
         y_trust_pres_prime = Q201B_31 %in% c(3,4),
         y_trust_ikhwan = Q201B_12 %in% c(3,4))

summary(dfjo$locvote)

dfjo %>% group_by(locvote) %>%
  summarise(spa_1 = sum(splita==1), n = n())


count(dfjo,  locvote, sort = TRUE)      

count(dfjo, internet_use_ordinal_1_is_top, sort = TRUE)      

dfjo$splita

   
help(group_by, .drop)
#saves dfjo as a csv

write.csv(dfjo, 'arabbaro_jo_labeled.csv')

#dfjo_trans is just like dfjo but with all other variabels dropped

dfjo_trans <- df %>%
  filter(country == 8) %>%
  transmute(y = Q201A_1 %in% c(3,4),
         medinc = Q1015A_JO == 2,
         lowinc = Q1015B_JO,
         highinc = Q1015C_JO,
         remit = Q1017,
         hijab = Q1010C,
         married = Q1010==4,
         employed = Q1005==1,
         selfemp = Q1005==2,
         retired = Q1005==3,
         housewife = Q1005==4,
         student = Q1005==5,
         unemp = Q1005==6,
         pubsec = Q1006A==1,
         privsec = Q1006A==2,
         neighbrich = Q1001C,
         educ = Q1003,
         male = Q1002,
         age = Q1001,
         orgmem = (Q501==1|Q501A==1),
         charity = Q266,
         petit = Q502_1,
         protest = Q502_2,
         polviol = Q502_4,
         campaign_attend = Q302,
         parlvote = Q301A,
         locvote= Q301C,
         internet = Q409,
         socmed = Q424,
         infosource = Q421,
         y_dem_top_issue = Q2061A==3,
         y_dem_pref = Q516A==3,
         y_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
         y_trust_gov = Q201A_1 %in% c(3,4),
         y_trust_army = Q201B_6 %in% c(3,4),
         y_trust_pres_prime = Q201B_31 %in% c(3,4),
         y_trust_ikhwan = Q201B_12 %in% c(3,4))

write.csv(dfjo_trans, 'arabbaro_jo_labeled_reduced.csv')
