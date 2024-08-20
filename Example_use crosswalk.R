#####################################################################
# Example of using ZIP Code - ZCTA crosswalk linkage file 
# Supplemental to
# title: ZIP Code and ZIP Code Tabulation Area Linkage: Implications for Bias in Epidemiological Research
# Futu Chen; futuchen@usc.edu
# Last update: 7/23/2024 
# Update notes: 
#
#####################################################################
# It's Ezzzzzy, trust me #
##########################
library(dplyr); library(tidyverse)

# read in crosswalk file
cross <-  read_excel("ZIPCodetoZCTACrosswalk2019UDS.xlsx")
str(cross$ZIP_CODE) #chr
# suppose this is your ZIP code data 
dat <- data.frame( ZIP = c("90001", "90002","90003", "90030","90051"),
                   count=c(3,3,3,1,1)
                   )
str(dat$ZIP) #chr

dat2 <- left_join(dat, cross, by=c("ZIP"="ZIP_CODE")) #left join crosswalk file into ZIP code original data
#now summarize to get a summary count at ZCTA level 
dat3 <- dat2 %>% group_by(ZCTA) %>% 
  summarize(n=n(),  #how many ZIP codes inside this ZCTA
            sum_count=sum(count, na.rm=T)) #sum of the total cases/counts at ZCTA level
dat3

# that's it! #