# Tim Liptrot and Youssef AbdelFatah

#clears list
rm(list = ls())
library(tidyverse)

#creates df_sav and consolidates income 

#what if I first used a sav file, then I transform all the income ones to their value, with NAs as 0. Then I could just change transmog a little bit. 
library(sjlabelled)
library(gsub)
help("as_factor")

df <- read_sav("ABV_Release_Data.sav")

inc_names <- variable.names(df)[grepl("^Q1015C|^Q1015B",variable.names(df))]

df_sav <- df %>%
  mutate(across(inc_names, ~ as.numeric(gsub("[^0-9.]","",gsub(".*-","",gsub('\\.','',gsub('>','',gsub('<','',sjlabelled::as_character(.x))))))))) %>%
  select(inc_names) %>%
  mutate(income=rowSums(.,na.rm=TRUE))

df$income <- df_sav$income

# Creates capitals list
capitals <- c(80005,130004,10016,210001,50001,100005,90001,220007,190007,150013,70001)



#dfjo_trans is just like dfjo but with all other variables dropped
dfjo_trans <- df %>%
  dplyr:::filter_rows(Q1001C < 5, Q1003 <10, Q409 < 10,Q266<10) %>%
  dplyr:::filter_rows(country == 8) %>%
  mutate(income = income,
#         remit = Q1017,
#         hijab = Q1010C,
         in_capital = Q1001A %in% capitals,
         married = Q1010==4,
         employed = Q1005==1,
         employed_pub_sec = Q1005==1&Q1006A==1,
         employed_priv_sec = Q1005==1&Q1006A==2,
         selfemp = Q1005==2,
         retired = Q1005==3,
         housewife = Q1005==4,
         student = Q1005==5,
         unemp = Q1005==6,
         neighb_rich = Q1001C==1,
         neighb_poor = Q1001C==2,
         neighb_mixed = Q1001C==3,
         educ = ifelse(Q1003<8,(7-Q1003)/6,0),
         male = Q1002==1,
         age = Q1001,
         orgmem = (Q501==1|Q501A==1),
#         charity = Q266,
#         petit = Q502_1,
#         protest = Q502_2,
#         polviol = Q502_4,
#         campaign_attend = Q302,
#         parlvote_nas_not_asked_1_is_yes = Q301A, 
         locvote= Q301C==1,
         #returns TRUE if uses iternet
         internet = Q409 < 6,
         #1-5, where 1 is using internet most often
#         internet_use_ordinal_4_is_top = ifelse(Q409<6,6-Q409,0),
         internet_use_bin = ifelse(Q409<6,(6-Q409)/5,0),
         #1-5, where 5 is using 10 hours or more a day
#         socmed_use_ordinal = ifelse(Q424>10|is.na(Q424),0,Q424-1),
#         socmed_use_bin = socmed_use_ordinal/4,
#         socmed_use_dummy = socmed_use_bin > 0,
         infs_face2face_tel = (Q421 %in% c(1,2)),
         infs_newspaper = (Q421 == 3),
         infs_radio = (Q421==4),
         infs_television = (Q421==5),
         infs_socmed = (Q421==6),
         country_of_origin_jordan = Q1020JO==1,
         y_dem_top_issue = Q2061A==3,
         y_dem_pref = Q516A==3,
         y_2_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
         y_1_distrust_gov = Q201A_1 %in% c(3,4),
         y_distrust_army = Q201B_6 %in% c(3,4),
         y_distrust_pres_prime = Q201B_31 %in% c(3,4),
         y_trust_ikhwan = Q201B_12 %in% c(1,2),
         .keep = "none")


write.csv(dfjo_trans, 'arabbaro_jo_labeled_reduced.csv')

#options(max.print=10000)

#count(df, Q302)

#sum(is.na(dfjo_trans$age))

#count(df, Q1001A)

#df$Q1001

#df <- mutate(df, Q1015A = Q1015A_PAL + Q1015A_EG + Q1015A_KU + Q1015A_JO + Q1015A_IR + Q1015A_MO + Q1015A_LEB + Q1015A_SUD + Q1015A_AL,na.rm=FALSE)

# combines the median income vairable for all the countries
df$Q1015A <- rowSums(df[,c("Q1015A_PAL", "Q1015A_EG", "Q1015A_KU", "Q1015A_JO", "Q1015A_IR", "Q1015A_MO", "Q1015A_LEB", "Q1015A_SUD", "Q1015A_AL")], na.rm=TRUE)



transmog <- function(df, country_num) {
df <- df %>%
    filter(Q1001C < 5, Q1003 <10, Q409 < 10, Q266<10) %>%
    filter(country == country_num) %>%
    mutate(income = income,
           #         remit = Q1017,
           #         hijab = Q1010C,
           in_capital = Q1001A %in% capitals,
           married = Q1010==4,
           employed = Q1005==1,
           employed_pub_sec = Q1005==1&Q1006A==1,
           employed_priv_sec = Q1005==1&Q1006A==2,
           selfemp = Q1005==2,
           retired = Q1005==3,
           housewife = Q1005==4,
           student = Q1005==5,
           unemp = Q1005==6,
           neighb_rich = Q1001C==1,
           neighb_poor = Q1001C==2,
           neighb_mixed = Q1001C==3,
           educ = ifelse(Q1003<8,(7-Q1003)/6,0),
           male = Q1002==1,
           age = Q1001,
           orgmem = (Q501==1|Q501A==1),
           charity = Q266==1,
           petit = ifelse(Q502_1==2,1,ifelse(Q502_1==1,.5,0)),
           protest = ifelse(Q502_2==2,1,ifelse(Q502_2==1,.5,0)),
           #         polviol = Q502_4,
           campaign_attend = (Q302==1),
           #         parlvote_nas_not_asked_1_is_yes = Q301A, 
           locvote= Q301C==1,
           #returns TRUE if uses iternet
           internet = Q409 < 6,
           #1-5, where 1 is using internet most often
#           internet_use_ordinal_4_is_top = ifelse(Q409<6,6-Q409,0),
           internet_use_bin = ifelse(Q409<6,(6-Q409)/5,0),
           #1-5, where 5 is using 10 hours or more a day
#           socmed_use_ordinal = ifelse(Q424>10|is.na(Q424),0,Q424-1),
#           socmed_use_bin = socmed_use_ordinal/4,
#           socmed_use_dummy = socmed_use_bin > 0,
           infs_face2face_tel = (Q421 %in% c(1,2)),
           infs_newspaper = (Q421 == 3),
           infs_radio = (Q421==4),
           infs_television = (Q421==5),
           infs_socmed = (Q421==6),
#           country_of_origin_jordan = Q1020JO==1,
           y_dem_top_issue = Q2061A==3,
           y_dem_pref = Q516A==3,
           y_2_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
           y_1_distrust_gov = Q201A_1 %in% c(3,4),
           y_distrust_army = Q201B_6 %in% c(3,4),
           y_distrust_pres_prime = Q201B_31 %in% c(3,4),
           y_trust_ikhwan = Q201B_12 %in% c(1,2),
           .keep = "none")
  
return(df)
}

df_alg <- transmog(df, 1)
df_alg <- subset(df_alg, select = -income)
write.csv(df_alg, 'arabbaro_alg_labeled_reduced.csv')

df_egy <- transmog(df, 5)
write.csv(df_egy, 'arabbaro_egy_labeled_reduced.csv')

df_leb <- transmog(df, 10)
write.csv(df_leb, 'arabbaro_leb_labeled_reduced.csv')

df_mor <- transmog(df, 13)
df_mor <- subset(df_mor, select = -income)
write.csv(df_mor, 'arabbaro_mor_labeled_reduced.csv')

df_pal <- transmog(df, 15)
write.csv(df_pal, 'arabbaro_pal_labeled_reduced.csv')

df_sud <- transmog(df, 19)
write.csv(df_sud, 'arabbaro_sud_labeled_reduced.csv')

df_tun <- transmog(df, 21)
write.csv(df_tun, 'arabbaro_tun_labeled_reduced.csv')

df_yem <- transmog(df, 22)
write.csv(df_yem, 'arabbaro_yem_labeled_reduced.csv')

# code for kuwait

df_kuw <- df %>%
    filter(Q1003 <10, Q409 < 10, Q266<10) %>%
    filter(country == 9) %>%
    mutate(below_medinc = Q1015A == 1,
           above_medinc = Q1015A == 2,
           #         remit = Q1017,
           #         hijab = Q1010C,
           in_capital = Q1001A %in% capitals,
           married = Q1010==4,
           employed = Q1005==1,
           employed_pub_sec = Q1005==1&Q1006A==1,
           employed_priv_sec = Q1005==1&Q1006A==2,
           selfemp = Q1005==2,
           retired = Q1005==3,
           housewife = Q1005==4,
           student = Q1005==5,
           unemp = Q1005==6,
           neighb_rich = Q1001C==1,
           neighb_poor = Q1001C==2,
           neighb_mixed = Q1001C==3,
           educ = ifelse(Q1003<8,(7-Q1003)/6,0),
           male = Q1002==1,
           age = Q1001,
           orgmem = (Q501==1|Q501A==1),
           charity = Q266==1,
#           petit = ifelse(Q502_1==2,1,ifelse(Q502_1==1,.5,0)),
#           protest = ifelse(Q502_2==2,1,ifelse(Q502_2==1,.5,0)),
           #         polviol = Q502_4,
           campaign_attend = (Q302==1),
           #         parlvote_nas_not_asked_1_is_yes = Q301A, 
           locvote= Q301C==1,
           #returns TRUE if uses iternet
           internet = Q409 < 6,
           #1-5, where 1 is using internet most often
           #           internet_use_ordinal_4_is_top = ifelse(Q409<6,6-Q409,0),
           internet_use_bin = ifelse(Q409<6,(6-Q409)/5,0),
           #1-5, where 5 is using 10 hours or more a day
           #           socmed_use_ordinal = ifelse(Q424>10|is.na(Q424),0,Q424-1),
           #           socmed_use_bin = socmed_use_ordinal/4,
           #           socmed_use_dummy = socmed_use_bin > 0,
           infs_face2face_tel = (Q421 %in% c(1,2)),
           infs_newspaper = (Q421 == 3),
           infs_radio = (Q421==4),
           infs_television = (Q421==5),
           infs_socmed = (Q421==6),
           #           country_of_origin_jordan = Q1020JO==1,
           y_dem_top_issue = Q2061A==3,
           y_dem_pref = Q516A==3,
#           y_2_dem_pref_not_in_dem = (Q516A==3 & Q511<6 & Q511 != 99 ),
           y_1_distrust_gov = Q201A_1 %in% c(3,4),
           y_distrust_army = Q201B_6 %in% c(3,4),
           y_distrust_pres_prime = Q201B_31 %in% c(3,4),
           y_trust_ikhwan = Q201B_12 %in% c(1,2),
           .keep = "none")

count(df_kuw, y_2_dem_pref_not_in_dem)



write.csv(df_kuw, 'arabbaro_kuw_labeled_reduced.csv')

# code for Iraq, because income data has a problem

df_iraq <- transmog(df, 7)

#spss type file
library(haven)
df_sav <- read_sav("ABV_Release_Data.sav")

#stata iraq
iraq_sav <- filter(df_sav, country==7)

iraq_sav <- iraq_sav %>%
  filter(Q1001C < 5, Q1003 <10, Q409 < 10) %>%
  mutate(below_medinc=Q1015A_IR==1,
         above_medinc=Q1015A_IR==2)

df_iraq$below_medinc <- iraq_sav$below_medinc
df_iraq$above_medinc <- iraq_sav$above_medinc

count(iraq_sav, Q1015A_IR)



typeof()

write.csv(df_iraq, 'arabbaro_iraq_labeled_reduced.csv')

# scraps to get the income into a nice spot

egy_sav <- filter(df, country==5)

count(mor_sav,Q511)

count(df_egy,protest)

tcount(jor_sav,Q511)




