# makes publishable results from confusion matrices

library(plyr)
library(readr)

setwd("~/GitHub/ML-repression-study/Confusion_Matrices")

mydir = "Confusion_Martices"

myfiles = list.files(pattern="*.csv", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv, .id=labs)



labs <- c()

for (i in 1:length(myfiles)) {
  
  labs[(2*i-1)] <- myfiles[i]
  
  labs[(2*i)] <- myfiles[i]

}

dat_csv$cm <- labs

dat_csv <- dat_csv %>% 
  mutate(actual = ifelse(X1==0,"no_grievance","grievance"))

names(dat_csv)[2] <- "pred_no"
names(dat_csv)[3] <- "pred_yes"

dat_csv %>% spread(actual,pred_no)

dat_csv

df <- as.data.frame(AFPF=numeric(),
                    AFPT=numeric(),
                    ATPF=numeric(),
                    ATPT=numeric(),
                    country=character())
df <- dat_csv[FALSE,]

df <- as.data.frame(myfiles)

df

for (i in 1:length(myfiles)) {
  df$AFPF[i] <- dat_csv[2*i-1,2]
  df$AFPT[i] <- dat_csv[2*i-1,3]
  df$ATPF[i] <- dat_csv[2*i,2]
  df$ATPT[i] <- dat_csv[2*i,3]
}

df$myfiles[7]

length(myfiles)

dat_csv
