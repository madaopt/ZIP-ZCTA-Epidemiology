#####################################################################
# Code for combining longitudinal UDS crosswalk files (for California) in: 
# Papaer title:
# Authors: 
#
#
#
#
# Last update: 4/18/2024 
# Update notes: 
#
#####################################################################
# I LOVE PRESSURE #
###################

#Download all files into a folder and read in their names
all.files <- list.files("/Users/madaopt/Library/CloudStorage/OneDrive-UniversityofSouthernCalifornia/EVAC study/Data/Boundries/ZCTA/UDS crosswalk/", pattern="UDS.xlsx")  #2010  has a different structure, add later

# List years we want to combine 
year.list <- c(2011:2022)


#######################################
# Check data: can skip this 
#######################################
# check data structure 
temp <- NULL
temp.zcta <- NULL
temp.zip <- NULL
for (i in 1:length(all.files)) {
  try <- read_excel(paste0("/Users/madaopt/Library/CloudStorage/OneDrive-UniversityofSouthernCalifornia/EVAC study/Data/Boundries/ZCTA/UDS crosswalk/",all.files[i]), sheet = 1)
  if (i<=3 | i==5 ) {  #col name changed...
    try <- try %>% filter(StateAbbr %in% "CA") %>% dplyr::select(c("ZIP","ZCTA_USE"))
    zcta <- as.data.frame(unique(try$ZCTA_USE)) %>% rename("ZCTA"="unique(try$ZCTA_USE)")  %>% mutate(year=year.list[i]) 
    zip <- as.data.frame(unique(try$ZIP)) %>% rename("ZIP"="unique(try$ZIP)")  %>% mutate(year=year.list[i]) 
    colnames(try) <- paste(colnames(try), year.list[i], sep = "_")
    nrows <- nrow(try)
    temp <- rbind(temp, nrows)
    temp.zcta <- rbind(temp.zcta, zcta)
    temp.zip <- rbind(temp.zip, zip)
    #temp <- merge(temp, try, by="ZCTA_USE", all = T)
  }else if (i==12){ #col name changed...again...
    try <- try %>% filter(STATE %in% "CA") %>% dplyr::select(c("ZIP_CODE","zcta"))
    zcta <- as.data.frame(unique(try$zcta)) %>% rename("ZCTA"="unique(try$zcta)")  %>% mutate(year=year.list[i]) 
    zip <- as.data.frame(unique(try$ZIP_CODE)) %>% rename("ZIP"="unique(try$ZIP_CODE)")  %>% mutate(year=year.list[i]) 
    colnames(try) <- paste(colnames(try), year.list[i], sep = "_")
    nrows <- nrow(try)
    temp <- rbind(temp, nrows)
    temp.zcta <- rbind(temp.zcta, zcta)
    temp.zip <- rbind(temp.zip, zip)
  }else if (i>=6){ #col name changed again...and again..
    try <- try %>% filter(STATE %in% "CA") %>%  dplyr::select(c("ZIP_CODE","ZCTA"))  
    zcta <- as.data.frame(unique(try$ZCTA)) %>% rename("ZCTA"="unique(try$ZCTA)")  %>% mutate(year=year.list[i])
    zip <- as.data.frame(unique(try$ZIP_CODE)) %>% rename("ZIP"="unique(try$ZIP_CODE)")  %>% mutate(year=year.list[i]) 
    colnames(try) <- paste(colnames(try), year.list[i], sep = "_")
    nrows <- nrow(try)
    temp <- rbind(temp, nrows)
    temp.zcta <- rbind(temp.zcta, zcta)
    temp.zip <- rbind(temp.zip, zip)
  }else{ #...still col name issue 
    try <- try %>% filter(STATE %in% "CA") %>% dplyr::select(c("ZIP","ZCTA")) 
    zcta <- as.data.frame(unique(try$ZCTA)) %>% rename("ZCTA"="unique(try$ZCTA)")  %>% mutate(year=year.list[i])
    zip <- as.data.frame(unique(try$ZIP)) %>% rename("ZIP"="unique(try$ZIP)")  %>% mutate(year=year.list[i]) 
    colnames(try) <- paste(colnames(try), year.list[i], sep = "_")
    nrows <- nrow(try)
    temp <- rbind(temp, nrows)
    temp.zcta <- rbind(temp.zcta, zcta)
    temp.zip <- rbind(temp.zip, zip)
  }
} 
temp
# turned out the number of rows changing every year
# is it because of changing ZCTA or changing of zip code? 
temp.zcta %>% group_by(year) %>% summarize(n=n()) #zcta is changing as well 
temp.zip %>% group_by(year) %>% summarize(n=n())  #zip changes and it changes more than zcta

#######################################
# Combine files
#######################################

#Create a dataset combining all files - in the code I subset to CA only, but you can delete that line and create a national file 
temp <- NULL
for (i in 1:length(all.files)) {
  try <- read_excel(paste0("/Users/madaopt/Library/CloudStorage/OneDrive-UniversityofSouthernCalifornia/EVAC study/Data/Boundries/ZCTA/UDS crosswalk/",all.files[i]), sheet = 1)
  if (i<=3 | i==5 ) { #file name changes...
    try <- try %>% filter(StateAbbr %in% "CA") %>% dplyr::select(c("ZIP","ZCTA_USE")) %>% mutate(year=year.list[i])
  }else if (i==12){ #col name changed...again...
    try <- try %>% filter(STATE %in% "CA") %>% dplyr::select(c("ZIP_CODE","zcta"))%>% mutate(year=year.list[i]) 
  }else if (i>=6){ #name changed again...
    try <- try %>% filter(STATE %in% "CA") %>%  dplyr::select(c("ZIP_CODE","ZCTA"))   %>% mutate(year=year.list[i])
  }else{
    try <- try %>% filter(STATE %in% "CA") %>% dplyr::select(c("ZIP","ZCTA"))  %>% mutate(year=year.list[i])
  }
  colnames(try) <- c("ZIP","ZCTA","Year")
  temp <- rbind(temp,try)
} 
head(temp)
summary(temp)
saveRDS() # temp is the combined file 