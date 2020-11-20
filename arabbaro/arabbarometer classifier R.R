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
         infosource = Q421) %>%
  mutate(joorpal = Q1020JO) %>%
  mutate(y_dem_top_issue = Q2061A==3,
         y_dem_pref = Q516A==3,
         y_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
         y_trust_gov = Q201A_1 %in% c(3,4),
         y_trust_army = Q201B_6 %in% c(3,4),
         y_trust_pres_prime = Q201B_31 %in% c(3,4),
         y_trust_ikhwan = Q201B_12 %in% c(3,4))
         
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

#

dfsa <- df

dfsa <- df %>%
  filter(country == 1)

dfsa <- df %>%
  filter(country == 8) %>%
  mutate(y = Q201A_1 %in% c(3,4),
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
         infosource = Q421) %>%
  mutate(joorpal = Q1020JO) %>%
  mutate(y_dem_top_issue = Q2061A==3,
         y_dem_pref = Q516A==3,
         y_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
         y_trust_gov = Q201A_1 %in% c(3,4),
         y_trust_army = Q201B_6 %in% c(3,4),
         y_trust_pres_prime = Q201B_31 %in% c(3,4),
         y_trust_ikhwan = Q201B_12 %in% c(3,4))

