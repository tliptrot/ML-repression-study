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
         internet_use_ordinal_4_is_top = ifelse(Q409<6,6-Q409,0),
         internet_use_bin = ifelse(Q409<6,(6-Q409)/5,0),
         #1-5, where 5 is using 10 hours or more a day
         socmed_use_ordinal = ifelse(Q424>10|is.na(Q424),0,Q424-1),
         socmed_use_bin = socmed_use_ordinal/4,
         socmed_use_dummy = socmed_use_bin > 0,
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


#saves dfjo as a csv
write.csv(dfjo, 'arabbaro_jo_labeled.csv')

#dfjo_trans is just like dfjo but with all other variables dropped
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
         internet_use_ordinal_4_is_top = ifelse(Q409<6,6-Q409,0),
         internet_use_bin = ifelse(Q409<6,(6-Q409)/5,0),
         #1-5, where 5 is using 10 hours or more a day
         socmed_use_ordinal = ifelse(Q424>10|is.na(Q424),0,Q424-1),
         socmed_use_bin = socmed_use_ordinal/4,
         socmed_use_dummy = socmed_use_bin > 0,
         infs_face2face_tel = (Q421 %in% c(1,2)),
         infs_newspaper = (Q421 == 3),
         infs_radio = (Q421==4),
         infs_television = (Q421==5),
         infs_socmed = (Q421==6),
         joorpal = Q1020JO,
         y_dem_top_issue = Q2061A==3,
         y_dem_pref = Q516A==3,
         y_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
         y_trust_gov = Q201A_1 %in% c(3,4),
         y_trust_army = Q201B_6 %in% c(3,4),
         y_trust_pres_prime = Q201B_31 %in% c(3,4),
         y_trust_ikhwan = Q201B_12 %in% c(3,4))
write.csv(dfjo_trans, 'arabbaro_jo_labeled_reduced.csv')

transmog <- function(df, country_num) {
  df <- df %>%
    filter(country == country_num) %>%
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
           internet_use_ordinal_4_is_top = ifelse(Q409<6,6-Q409,0),
           internet_use_bin = ifelse(Q409<6,(6-Q409)/5,0),
           #1-5, where 5 is using 10 hours or more a day
           socmed_use_ordinal = ifelse(Q424>10|is.na(Q424),0,Q424-1),
           socmed_use_bin = socmed_use_ordinal/4,
           socmed_use_dummy = socmed_use_bin > 0,
           infs_face2face_tel = (Q421 %in% c(1,2)),
           infs_newspaper = (Q421 == 3),
           infs_radio = (Q421==4),
           infs_television = (Q421==5),
           infs_socmed = (Q421==6),
           y_dem_top_issue = Q2061A==3,
           y_dem_pref = Q516A==3,
           y_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
           y_trust_gov = Q201A_1 %in% c(3,4),
           y_trust_army = Q201B_6 %in% c(3,4),
           y_trust_pres_prime = Q201B_31 %in% c(3,4),
           y_trust_ikhwan = Q201B_12 %in% c(3,4),
           .keep = "none"
           )
return(df)
}

df_alg <- transmog(df, 1)
write.csv(df_alg, 'arabbaro_alg_labeled_reduced.csv')

df_egy <- transmog(df, 5)
write.csv(df_egy, 'arabbaro_egy_labeled_reduced.csv')

df_iraq <- transmog(df, 1)
write.csv(df_iraq, 'arabbaro_iraq_labeled_reduced.csv')

df_kuw <- transmog(df, 1)
write.csv(df_kuw, 'arabbaro_kuw_labeled_reduced.csv')

df_leb <- transmog(df, 1)
write.csv(df_leb, 'arabbaro_leb_labeled_reduced.csv')

df_mor <- transmog(df, 1)
write.csv(df_mor, 'arabbaro_mor_labeled_reduced.csv')

df_pal <- transmog(df, 1)
write.csv(df_pal, 'arabbaro_pal_labeled_reduced.csv')

df_sud <- transmog(df, 1)
write.csv(df_sud, 'arabbaro_sud_labeled_reduced.csv')

df_tun <- transmog(df, 1)
write.csv(df_tun, 'arabbaro_tun_labeled_reduced.csv')

df_yem <- transmog(df, 1)
write.csv(df_yem, 'arabbaro_yem_labeled_reduced.csv')
