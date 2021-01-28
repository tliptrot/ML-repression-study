# looking at the iraq income file

library(haven)
library(expss)
df_sav <- read_sav("ABV_Release_Data.sav")

iraq <- filter(df_sav, country==7)

val_lab(iraq$Q1015A)
count(iraq, Q1015A)

# kuwait fixing democratic preference

kuw_sav <- filter(df_sav, country==9)

count(kuw_sav,Q516A)

# kuwaitis were not asked for democratic prefernces

# Algeria trust in gov

alg_sav <- filter(df_sav, country==1)

count(alg_sav, Q511)
alg_sav$Q201A_41

df <- read_sav("ABV_Release_Data.sav")

view(df %>% group_by(country) %>%
  count(Q302))

(224+121)/(224+121+2005+12+26)
