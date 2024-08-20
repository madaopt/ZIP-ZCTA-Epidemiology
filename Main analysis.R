#####################################################################
# Code for the analysis in:
# title: ZIP Code and ZIP Code Tabulation Area Linkage: Implications for Bias in Epidemiological Research
# Last update: 8/08/2024 
# Update notes: R1 review - adding sensitivity analysis, tables in SDC and new graph; update figure 3
#
#####################################################################
# I LOVE PRESSURE #
###################

# Install packages or load packages
# install.packages()

library(dplyr); library(tidyverse); library(readxl);library(stringr)
library(ggplot2); library(ggtext); library(ggthemes); library(scales)
library(splines); library(effects); library(mgcv)
# library(mgcViz)
# library(nnet)
# library(ROCR)

"%!in%" <- Negate("%in%")

#### 2019 analysis - whole US ######
#######################################
# Data prep
#######################################

### take a look at zip zcta n and SES use 2019 as an example - whole US ######
todayis <- "" # your file directory 

cross <-  read_excel(paste0(todayis,"Boundries/ZCTA/UDS crosswalk/ZIPCodetoZCTACrosswalk2019UDS.xlsx"))
cross2 <- cross[cross$ZIP_TYPE == "Post Office or large volume customer",] #check PO boxes
table(cross$ZIP_TYPE)
dat <- cross %>% group_by(ZCTA) %>% summarize(n=n())  # how many ZIP inside ZCTA 
# check if 1-1 matching is exactly the same 5 digit
check.cross <- filter(cross, cross$ZCTA %in% dat[dat$n ==1,]$ZCTA)
check.cross$flg <- ifelse(check.cross$ZIP_CODE == check.cross$ZCTA, 0,1)
summary(check.cross$flg) # no flags, yes, all 1-1 matching are the exact same 5 digits 

#how many unique ZIPs?
length(unique(cross$ZIP_CODE))
163696/length(unique(cross$ZIP_CODE))

# how many of non-mathing are PO box?
zcta.pobox <- unique(cross2$ZCTA) #PO box ZIP type 
check.cross <- filter(cross, cross$ZCTA %in% dat[dat$n >1,]$ZCTA) # ZCTA with more than 1 ZIPs
check.cross$same <- ifelse(check.cross$ZCTA == check.cross$ZIP_CODE,1,0)
check.cross2 <- check.cross[check.cross$same ==0,] # ZCTA and ZIPs that are not matching
length(zcta.pobox)/length(unique(check.cross2$ZIP_CODE)) *100
length(unique(check.cross2$ZIP_CODE))
length(zcta.pobox)
#read in SES
ses <- read.csv(paste0(todayis,"ACS ZCTA SES 2019/nhgis0004_ds244_20195_zcta.csv"))
edu <- read.csv(paste0(todayis,"ACS ZCTA SES 2019/nhgis0011_ds244_20195_zcta_edu.csv"))

names(ses)
ses <- ses %>% mutate(pct_NHwhite = ALUKE003/ALUKE001,
                      pct_NHblack= ALUKE004/ALUKE001,
                      pct_Hispanic= ALUKE012/ALUKE001,
                      pct_move1yr =  ALUME003/ ALUME001,
                      pct_belowpoverty =ALWYE002/ALWYE001,
                      pct_occupied=ALZKE002/ALZKE001,
                      pct_renter=ALZLE003/ALZLE001,
                      pct_sf =(AL0AE002+AL0AE003)/AL0AE001
)
edu$pct_college <- (edu$ALWGE022 + edu$ALWGE023 + edu$ALWGE024 + edu$ALWGE025)/edu$ALWGE001

summary(ses[,c(284:292)]) #looks good
ses$ZCTA <- stringr::str_sub(ses$GEOID, -5,-1 )#last 11 digit 
edu$ZCTA <- stringr::str_sub(edu$GEOID, -5,-1 )#last 11 digit 
edu <- edu[,c( "pct_college", "ZCTA")]
ses <- ses[,c( "pct_NHwhite","pct_NHblack","pct_Hispanic","pct_move1yr", "pct_belowpoverty", "pct_occupied" , "pct_renter" , "pct_sf" ,"ZCTA")] #, "STATE"

ses2 <- merge(ses, edu, by= "ZCTA")
dat2 <- left_join(dat, ses2, by="ZCTA")
summary(dat2)

#revise to collapse categories 
table(dat$n)
dat2$n_nestedv2 <- ifelse(dat2$n >=4, 4, dat2$n) #cat var
table(dat2$n_nestedv2)
dat2$n_nestedv2 <-factor(dat2$n_nestedv2, levels = c(1:4), labels = c("1","2","3",">=4"))

# restrict data to only 50 states + DC
state <- cross[!duplicated(cross$ZCTA),c("STATE", "ZCTA")]
statelist <- state %>% filter(STATE %!in% c("AS","GU","PR","VI","MP","PW")) 
statelist <- unique(statelist$STATE)
length(statelist)
state <- cross[!duplicated(cross$ZCTA),c("STATE", "ZCTA")]  %>%  filter(STATE %!in% c("AS","GU","PR","VI","MP","PW"))
#add state into data 
dat2 <- left_join(dat2, state, by="ZCTA")
table(dat2$STATE)
length(unique(dat2$STATE)) # kick NA out
dat2 <- dat2[!is.na(dat2$STATE),]

# summary across states 
length(unique(dat2$ZCTA)) #32983

tb <- dat2 %>%
  group_by(STATE,n_nestedv2) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
summary(tb[tb$n_nestedv2==1,])

temp <- dat2 #temp 
temp$zip_nested <- ifelse(dat2$n >1, 1, 0) #contains more than 1 - indicate dropping or missing
tb2 <- temp %>%
  group_by(STATE,zip_nested) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n)*100)
summary(tb2[tb2$zip_nested==1,])


# national summary tables #
dat2 %>%
  group_by(n_nestedv2) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n)) #remember to *100
# range and median in results - please use tb2, as this would left 0% out
tb3 <- tb2[tb2$zip_nested ==1,]
summary(tb3$Freq) # already *100
sum(tb3$n)
range(tb3$Freq)
median(tb3$Freq)

round(prop.table(table(dat2$n_nestedv2) *100),4)

dat2_long <- gather(dat2, SESind, measurement, pct_NHwhite: pct_college , factor_key=TRUE)

# association between state, SES and zip dropped
# state and n zip nested 
temp <- spread(tb[,-4], n_nestedv2,n) # four category variable indicating how many ZIPs nested with ZCTA
temp[is.na(temp)] <- 0
chisq.test(temp[,c(2:5)])
fisher.test(temp[,c(2:5)],simulate.p.value=TRUE)

temp <- spread(tb2[,-4], zip_nested,n) #binary (=1 or >1)
temp[is.na(temp)] <- 0
fisher.test(temp[,c(2:3)],simulate.p.value=TRUE) 
# both showed significant diff for at least two states

# ses, state
temp <- dat2_long[dat2_long$SESind == "pct_NHwhite",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_NHblack",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_Hispanic",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_move1yr",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_occupied",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_sf",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_belowpoverty",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_renter",]
kruskal.test(measurement ~ STATE, data = temp)
temp <- dat2_long[dat2_long$SESind == "pct_college",]
kruskal.test(measurement ~ STATE, data = temp)
# P<0.0001 for all fo them



### Figure S1 ####
library(ggtext)
#order states by % missing
statelist <- tb3 %>% 
  arrange(desc(Freq)) %>%
  select(STATE) #not including AS and MP but they were excluded from plot
dat2_long$STATE.f <- factor(dat2_long$STATE, levels = statelist$STATE)
library(scales)
dat2_long %>% 
  # filter(STATE %!in% c("AS","GU","MP","PW","VI")) %>% 
  filter(SESind %in% c("pct_NHwhite","pct_renter","pct_belowpoverty")) %>% 
  mutate(SESind.f=factor(SESind, levels=c("pct_NHwhite","pct_renter","pct_belowpoverty"), labels=c("% Non-Hispanic White","% Renter","% Below poverty"))) %>%#reduce volumn
  ggplot(aes(x = n_nestedv2, y = measurement, fill = SESind.f)) +
  geom_boxplot(outlier.size = 0.1) +
  scale_y_continuous(labels = percent)+
  labs(y = "Percentages", x = "Number of ZIP Codes contained within ZCTA", fill="ZCTA Characteristics") +
  facet_wrap(~STATE.f) +
  theme_bw()+
  #  ggtitle("") +
  theme( plot.title = element_textbox_simple(size=14), strip.text.x = element_text(size =10), axis.text=element_text(size=12), legend.text=element_text(size=14)) #plot.title = element_text(size = 10)


dat2_long %>% 
  # filter(STATE %!in% c("AS","GU","MP","PW","VI")) %>% 
  filter(SESind %in% c("pct_college","pct_renter","pct_belowpoverty")) %>% 
  mutate(SESind.f=factor(SESind, levels=c("pct_college","pct_renter","pct_belowpoverty"), labels=c("% Bachelor's degree","% Renter","% Below poverty"))) %>%#reduce volumn
  ggplot(aes(x = n_nestedv2, y = measurement, fill = SESind.f)) +
  geom_boxplot(outlier.size = 0.1) +
  scale_y_continuous(labels = percent)+
  labs(y = "Percentages", x = "Number of ZIP Codes contained within ZCTA", fill="ZCTA Characteristics") +
  facet_wrap(~STATE.f) +
  theme_bw()+
  theme( plot.title = element_textbox_simple(size=14), strip.text.x = element_text(size =10), axis.text=element_text(size=12), legend.text=element_text(size=14)) #plot.title = element_text(size = 10)

#### Appendix table ####
# median (IQRs) for the sociodemographic characteristics and  median population size of the ZCTAs #

pop2019 <-  read.csv("nhgis0021_ds244_20195_zcta.csv")
pop2019$ZCTA <- stringr::str_sub(pop2019$NAME_E, -5,-1 )
pop2019 <- pop2019[c("ZCTA","ALUBE001")]
dat2 <- left_join(dat2, pop2019, by="ZCTA")
# I will hard code through...#
  temp <- dat2 %>%
  group_by(n_nestedv2) %>% summarise(median_pop=median(ALUBE001), 
                                     Q1_pop=quantile(ALUBE001, probs=0.25),
                                     Q3_pop=quantile(ALUBE001, probs=0.75),
                                     summary1=paste0(median_pop, " (", Q1_pop, "-",Q3_pop,")"),
                                     median_nhw= round(median(pct_NHwhite, na.rm=T)*100,2), 
                                     Q1_nhw=round(quantile(pct_NHwhite, probs=0.25, na.rm=T)*100,2),
                                     Q3_nhw=round(quantile(pct_NHwhite, probs=0.75, na.rm=T)*100,2),
                                     summary2=paste0( median_nhw, " (", Q1_nhw, "-",Q3_nhw,")"),
                                     median_nhb=round(median(pct_NHblack , na.rm=T)*100,2), 
                                     Q1_nhb=round(quantile(pct_NHblack , probs=0.25, na.rm=T)*100,2),
                                     Q3_nhb=round(quantile(pct_NHblack , probs=0.75, na.rm=T)*100,2),
                                     summary3=paste0( median_nhb, " (", Q1_nhb, "-",Q3_nhb,")"),
                                     median_h=round(median(pct_Hispanic, na.rm=T)*100,2), 
                                     Q1_h=round(quantile(pct_Hispanic, probs=0.25, na.rm=T)*100,2),
                                     Q3_h=round(quantile(pct_Hispanic, probs=0.75, na.rm=T)*100,2),
                                     summary4=paste0( median_h, " (", Q1_h, "-",Q3_h,")"),
                                     median_m1=round(median(pct_move1yr, na.rm=T)*100,2), 
                                     Q1_m1=round(quantile(pct_move1yr, probs=0.25, na.rm=T)*100,2),
                                     Q3_m1=round(quantile(pct_move1yr, probs=0.75, na.rm=T)*100,2),
                                     summary5=paste0( median_m1, " (", Q1_m1, "-",Q3_m1,")"),
                                     median_bp=round(median(pct_belowpoverty, na.rm=T)*100,2), 
                                     Q1_bp=round(quantile(pct_belowpoverty, probs=0.25, na.rm=T)*100,2),
                                     Q3_bp=round(quantile(pct_belowpoverty, probs=0.75, na.rm=T)*100,2),
                                     summary6=paste0( median_bp, " (", Q1_bp, "-",Q3_bp,")"),
                                     median_o=round(median(pct_occupied, na.rm=T)*100,2), 
                                     Q1_o=round(quantile(pct_occupied, probs=0.25, na.rm=T)*100,2),
                                     Q3_o=round(quantile(pct_occupied, probs=0.75, na.rm=T)*100,2),
                                     summary7=paste0( median_o, " (", Q1_o, "-",Q3_o,")"),
                                     median_r=round(median(pct_renter, na.rm=T)*100,2), 
                                     Q1_r=round(quantile(pct_renter, probs=0.25, na.rm=T)*100,2),
                                     Q3_r=round(quantile(pct_renter, probs=0.75, na.rm=T)*100,2),
                                     summary8=paste0( median_r, " (", Q1_r, "-",Q3_r,")"),
                                     median_sf=round(median(pct_sf, na.rm=T)*100,2), 
                                     Q1_sf=round(quantile(pct_sf, probs=0.25, na.rm=T)*100,2),
                                     Q3_sf=round(quantile(pct_sf, probs=0.75, na.rm=T)*100,2),
                                     summary9=paste0( median_sf, " (", Q1_sf, "-",Q3_sf,")"),
                                     median_c=round(median(pct_college, na.rm=T)*100,2), 
                                     Q1_c=round(quantile(pct_college, probs=0.25, na.rm=T)*100,2),
                                     Q3_c=round(quantile(pct_college, probs=0.75, na.rm=T)*100,2),
                                     summary10=paste0( median_c, " (", Q1_c, "-",Q3_c,")")
  )

temp <- t(temp)


#### Add RUCA #####

# RUCA 
ruca <- read_xlsx(paste0(todayis,"RUCA2010zipcode.xlsx"), sheet=2)
colnames(ruca)[1] <- "ZIP"
# we will join back to 2019, so use 2019 crosswalk
cw2019 <- cross[,c(1,5)]
colnames(cw2019)[1] <- "ZIP"

ruca <- left_join(ruca, cw2019,by="ZIP")
sum(is.na(ruca$ZCTA)) #n=116 not crosswalked 

# Do duplicated ZCTAs have same RUCA results? 
# first take binary varaible. at ZIP
ruca$RUCA1binv2 <- ifelse(ruca$RUCA1 == 1, 1, ifelse(ruca$RUCA1 <=10, 2, NA))# 1 core, 2 not core
table(ruca$RUCA1binv2)
# get ZCTA with mutiple ZIPs
check <- ruca %>% group_by(ZCTA) %>% filter(n() > 1) #[ruca$ZIP_TYPE == "Zip Code Area" ,] 
check <- check[!is.na(check$ZCTA),]
# Flag if zips do not agree inside the same zcta 
check2 <- check %>%
  group_by(ZCTA) %>%
  mutate(min = min(RUCA1binv2, na.rm = T), max= max(RUCA1binv2, na.rm = T) ) %>%
  mutate(flag= ifelse(min == max ,0, 1))

check3 <- check2[check2$flag ==1,] #where mutiple zips not agree within ZCTA 
nrow(check3)/nrow(ruca[!is.na(ruca$ZCTA ),])*100 #  1.21565
length(unique(check3$ZCTA))/length(unique(ruca$ZCTA))*100  #  0.5135952

## within check3, first take....##
#### majority vote #### 
mvote <- function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommon <- names(sortedOutcomes)[1]
  mostCommon
}
check4 <-  check3 %>% 
  group_by(ZCTA) %>% 
  mutate(nvote=n(),vote=mvote(RUCA1binv2)) 

table(check4$nvote)

# if it's a tie, then adding up would be an odd/even number depending on nvote 
check4 <-  check4  %>%
  group_by(ZCTA) %>% 
  mutate(tie.ind= ifelse( ((nvote==2) & ((sum(RUCA1binv2) %% 2) !=0)) | ((nvote==4) & ((sum(RUCA1binv2) %% 2) ==0)) | ((nvote==6) & ((sum(RUCA1binv2) %% 2) !=0)) |  ((nvote==14) & ((sum(RUCA1binv2) %% 2) !=0))  | ((nvote==28) & ((sum(RUCA1binv2) %% 2) ==0)), "tie","not-tie" ))
# among not tie, take the majority vote (use column vote)
# among tie, if difference smaller than 2, then 1 (core), otherwise not core
check4.tie <-  check4[check4$tie.ind == "tie",]  %>%
  group_by(ZCTA) %>% 
  mutate(vote = ifelse(diff(range(RUCA1)) <=2, 1, 2))

## combine tie and nontie results 
check4.1 <- check4[check4$tie.ind == "not-tie", c("ZIP","STATE","ZIP_TYPE" ,"RUCA1", "RUCA2", "ZCTA","RUCA1binv2", "flag","vote" )]
check4.1$vote <- as.numeric(check4.1$vote)
check4.2 <- check4.tie[, c("ZIP","STATE","ZIP_TYPE" ,"RUCA1", "RUCA2", "ZCTA","RUCA1binv2", "flag","vote" )]
ruca.dupzip <- rbind(check4.1, check4.2)
ruca.dupzip$RUCA1binv2 <- NULL #use vote
ruca.dupzip$RUCA1binv2 <- ruca.dupzip$vote
ruca.dupzip <- ruca.dupzip[,c("ZCTA","RUCA1binv2")]
ruca.dupzip.zcta <- ruca.dupzip[!duplicated(ruca.dupzip$ZCTA),] #170
ruca.nondupzip <- filter(ruca, ruca$ZCTA %!in% ruca.dupzip.zcta$ZCTA & !is.na(ruca$ZCTA))
ruca.nondupzip <- ruca.nondupzip[,c("ZIP","ZCTA","RUCA1binv2")] # all zip RUCA bin should be the same within ZCTA 
ruca.nondupzip <- ruca.nondupzip[,c("ZCTA","RUCA1binv2")] 
ruca.nondupzip.zcta <- ruca.nondupzip [!duplicated(ruca.nondupzip$ZCTA),]
# final zcta ruca 
ruca.zcta <- rbind(ruca.nondupzip.zcta, ruca.dupzip.zcta)

# let's look at zip -> zip being dropped versus RUCA
cw2019$flag <- ifelse(cw2019$ZIP != cw2019$ZCTA, 1, 0)
zip.dropped2019 <- filter(ruca, ZIP %in% cw2019[cw2019$flag ==1,]$ZIP) #7949
ruca.drop <- left_join(ruca, cw2019, by="ZIP") #joining flag to zip data

#######################################
# Logistic regression: ZIP level      #
#######################################

## binary ruca 
ruca.drop$RUCA1binv2 <- ifelse(ruca.drop$RUCA1 ==1, 1, ifelse(ruca.drop$RUCA1 <=10, 2, NA))
ruca.drop$RUCA1binv2.f  <- factor(ruca.drop$RUCA1binv2, levels = c(1,2), labels  =c("Core","Non-core"))
ruca.drop$RUCA1binv2.f  <- relevel(ruca.drop$RUCA1binv2.f , ref = "Non-core")
md <- glm(flag~ RUCA1binv2.f+ STATE, data=ruca.drop, family=binomial(link='logit'))
summary(md) #reporting this 
round( exp(cbind("Odds ratio" = coef(md)[2], "Lower"=confint(md)[2,1], "Upper"= confint(md)[2,2])) , 4)
# Odds ratio  Lower  Upper
# RUCA1binv2.fCore     5.3829 5.0695 5.7176

## binary ruca, 123 urban, >4 rural, sensitivity analysis
ruca.drop$RUCA1binv3 <- ifelse(ruca.drop$RUCA1 %in% c(1,2,3), 1, ifelse(ruca.drop$RUCA1 <=10, 2, NA))
ruca.drop$RUCA1binv3.f  <- factor(ruca.drop$RUCA1binv3, levels = c(1,2), labels  =c("Core","Non-core"))
ruca.drop$RUCA1binv3.f  <- relevel(ruca.drop$RUCA1binv3.f , ref = "Non-core")
md2 <- glm(flag~ RUCA1binv3.f+ STATE, data=ruca.drop, family=binomial(link='logit'))
summary(md2) #reporting this as sensitivity
round( exp(cbind("Odds ratio" = coef(md2)[2], "Lower"=confint(md2)[2,1], "Upper"= confint(md2)[2,2])) , 4)
# RUCA1binv3.fCore 3.26 3.053 3.482 


#######################################
#              GAM Analysis           #
#######################################

dat2$zip_nested <- ifelse(dat2$n >1, 1, 0)  # Y
dat3 <- left_join(dat2, ruca.zcta, by="ZCTA") #add ruca 
dat3$RUCA1binv2.f <- factor(dat3$RUCA1binv2)

# wondering about po box
dat4 <- dat3
dat4$pobox <- ifelse(dat4$ZCTA %in% zcta.pobox, 1,0)
table(dat4$pobox)

varlist <- names(dat2)[3:11] # X
# run through models 
mdlist <- lapply(varlist ,
                 function(x, d) gam(as.formula(paste("zip_nested ~ s(", x, ",fx=FALSE,bs='cr')+STATE+ RUCA1binv2.f")), data = d, family=binomial(link='logit')),
                 d = dat3)
length(mdlist)

#### Figure 2 revised with hist & vertical lines indicating dist of ZCTA characteristics ####

# test #
quantile(dat3$pct_NHwhite, 0.05, na.rm=T)
quantile(dat3$pct_NHwhite, 0.95, na.rm=T)
dat3$pct_NHwhite_trim <- ifelse(dat3$pct_NHwhite < quantile(dat3$pct_NHwhite, 0.95, na.rm=T) & dat3$pct_NHwhite > quantile(dat3$pct_NHwhite, 0.05, na.rm=T), 1,0 )

# run it across ZCTA characteristics #
pctlist <- NULL

for (i in 1:length(varlist)) {
  b <- quantile(dat3[[varlist[[i]]]], 0.05, na.rm=T)
  t <-  quantile(dat3[[varlist[[i]]]], 0.95, na.rm=T)
  temp <- rbind(cbind(b,t))
  pctlist <- rbind(pctlist, temp)
}
pctlist <- as.data.frame(pctlist)
rownames(pctlist) <- varlist

for (i in 1:length(varlist)) {
  dat3 [[ paste0(varlist[[i]],"_trim") ]] <- ifelse(dat3[[varlist[[i]]]] < quantile(dat3[[varlist[[i]]]], 0.95, na.rm=T) & dat3[[varlist[[i]]]] > quantile(dat3[[varlist[[i]]]], 0.05, na.rm=T), 1,0 )
}
varlist2 <- paste0(varlist,"_trim")


h1 <- hist(dat3[[varlist[[1]]]])
h1$density = h1$counts/sum(h1$counts)
h2 <- hist(dat3[[varlist[[2]]]],breaks=40)
h2$density = h2$counts/sum(h2$counts)
h3 <- hist(dat3[[varlist[[3]]]], breaks=40)
h3$density = h3$counts/sum(h3$counts)
h4 <- hist(dat3[[varlist[[4]]]], breaks=40)
h4$density = h4$counts/sum(h4$counts)
h5 <- hist(dat3[[varlist[[5]]]], breaks=40)
h5$density = h5$counts/sum(h5$counts)
h6 <- hist(dat3[[varlist[[6]]]])
h6$density = h6$counts/sum(h6$counts)
h7 <- hist(dat3[[varlist[[7]]]])
h7$density = h7$counts/sum(h7$counts)
h8 <- hist(dat3[[varlist[[8]]]])
h8$density = h8$counts/sum(h8$counts)
h9 <- hist(dat3[[varlist[[9]]]])
h9$density = h9$counts/sum(h9$counts)


par(mfrow=c(3,3),cex.axis=1.7,cex.lab=1.7,lwd = 2, mar=c(5, 4.1, 4.1, 4.1)) 
myalpha=0.3

plot(mdlist[[1]],trans = plogis, shift = coef(mdlist[[1]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue", xlab="ZCTA % NH White", ylab=" ",ylim=c(0,0.65),lwd = 2,xaxt = "n",xlim=c(pctlist[1,1],pctlist[1,2]) )
plot(h1, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE, xlim=c(pctlist[1,1],pctlist[1,2]))
abline(v=holder[1,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[1,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[1,5], col="lightblue", lty = 2, lwd = 2)
axis(side=1, at=c(pctlist[1,1],0.4,0.6,0.8,pctlist[1,2]), labels= c("0.18%","40%","60%","80%","100%"), las=1)
axis(side=1, at=c(holder[1,2],holder[1,3],holder[1,5]), labels=c("","",""), col ="lightblue")
text(c(holder[1,2],holder[1,3],holder[1,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[1,2]*100,0),"%"),paste0(round(holder[1,3]*100,0),"%"), paste0(round(holder[1,5]*100,0),"%" )), cex = 1.2, col="gray11")
# draw an axis on the right
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")


plot(mdlist[[2]], trans = plogis, shift = coef(mdlist[[2]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % NH Black",ylab=" ", ylim=c(0,0.65),xaxt = "n",xlim=c(pctlist[2,1],pctlist[2,2]))
plot(h2, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE, xlim=c(pctlist[2,1],pctlist[2,2]))
abline(v=holder[2,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[2,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[2,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.5,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(0,0.1,0.2,0.3,0.4,0.45), labels= c("0%","10%","20%","30%","40%","45%"),las=1)
axis(side=1, at=c(holder[2,2],holder[2,3],holder[2,5]), labels=c("","",""), col ="lightblue")
text(c(holder[2,3],holder[2,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[2,3]*100,0),"%"), paste0(round(holder[2,5]*100,0),"%" )), cex = 1.2, col="gray11") #paste0(round(holder[2,2]*100,0),"%")
text(0, par("usr")[3]+0.52, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = "0%", cex = 1, col="gray11") 
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[2,2],holder[2,3],holder[2,5]), labels= c(paste0(round(holder[2,2]*100,0),"%"),paste0(round(holder[2,3]*100,0),"%"), paste0(round(holder[2,5]*100,0),"%" )),las=2, col ="lightblue")

plot(mdlist[[3]],trans = plogis, shift = coef(mdlist[[3]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Hispanic",ylab=" ", ylim=c(0,0.65),xaxt = "n",xlim=c(pctlist[3,1],pctlist[3,2]))
plot(h3, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE,xlim=c(pctlist[3,1],pctlist[3,2]))
abline(v=holder[3,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[3,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[3,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(0,0.1,0.2,0.3,0.4,0.45), labels= c("0%","10%","20%","30%","40%","45%"),las=1)
axis(side=1, at=c(holder[3,2],holder[3,3],holder[3,5]), labels=c("","",""), col ="lightblue")
text(c(holder[3,2],holder[3,3],holder[3,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[3,2]*100,0),"%"),paste0(round(holder[3,3]*100,0),"%"), paste0(round(holder[3,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[3,2],holder[3,3],holder[3,5]), labels= c(" ",paste0(round(holder[3,3]*100,0),"%"), paste0(round(holder[3,5]*100,0),"%" )), col ="lightblue",las=2)

plot(mdlist[[9]],trans = plogis, shift = coef(mdlist[[9]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Bachelor’s degree" ,ylab="Predicted probability", ylim=c(0,0.65),xaxt = "n", xlim=c(pctlist[9,1],pctlist[9,2]))
plot(h9, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE,xlim=c(pctlist[9,1],pctlist[9,2]))
abline(v=holder[9,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[9,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[9,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"), las=1)
axis(side=1, at=c(0.04,0.1,0.2,0.3,0.4,0.5,0.6), labels= c("4%","10%","20%","30%","40%","50%","60%"),las=1)
axis(side=1, at=c(holder[9,2],holder[9,3],holder[9,5]), labels=c("","",""), col ="lightblue")
text(c(holder[9,2],holder[9,3],holder[9,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[9,2]*100,0),"%"),paste0(round(holder[9,3]*100,0),"%"), paste0(round(holder[9,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[9,2],holder[9,3],holder[9,5]), labels= c(paste0(round(holder[9,2]*100,0),"%"),paste0(round(holder[9,3]*100,0),"%"), paste0(round(holder[9,5]*100,0),"%" )), col ="lightblue", las=2)

plot(mdlist[[5]],trans = plogis, shift = coef(mdlist[[5]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Below poverty",ylab=" ", ylim=c(0,0.65),xaxt = "n",xlim=c(pctlist[5,1],pctlist[5,2]))
plot(h5, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE,xlim=c(pctlist[5,1],pctlist[5,2]))
abline(v=holder[5,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[5,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[5,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(0,0.1,0.2,0.3,0.32), labels= c("0%","10%","20%","30%","32%"),las=1)
axis(side=1, at=c(holder[5,2],holder[5,3],holder[5,5]), labels=c("","",""), col ="lightblue")
text(c(holder[5,2],holder[5,3],holder[5,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[5,2]*100,0),"%"),paste0(round(holder[5,3]*100,0),"%"), paste0(round(holder[5,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[5,2],holder[5,3],holder[5,5]), labels= c(paste0(round(holder[5,2]*100,0),"%"),paste0(round(holder[5,3]*100,0),"%"), paste0(round(holder[5,5]*100,0),"%" )), col ="lightblue",las=2)

plot(mdlist[[4]],trans = plogis, shift = coef(mdlist[[4]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Moved last year",ylab=" ", ylim=c(0,0.65),xaxt = "n", xlim=c(pctlist[4,1],pctlist[4,2]))
plot(h4, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE,xlim=c(pctlist[4,1],pctlist[4,2]))
abline(v=holder[4,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[4,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[4,5], col="lightblue", lty = 2, lwd = 2)
axis(side=1, at=c(0,0.05,0.1,0.15, 0.2, 0.25, 0.28), labels= c("0%","5%","10%","15%","20%","25%","28%"),las=1)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(holder[4,2],holder[4,3],holder[4,5]), labels=c("","",""), col ="lightblue")
text(c(holder[4,2],holder[4,3],holder[4,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[4,2]*100,0),"%"),paste0(round(holder[4,3]*100,0),"%"), paste0(round(holder[4,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# add a title for the right axis
mtext("Density", side=4, col="gray11",srt = 270,par("usr")[2]+2.7)

# axis(side=1, at=c(holder[4,2],holder[4,3],holder[4,5]), labels= c(paste0(round(holder[4,2]*100,0),"%"),paste0(round(holder[4,3]*100,0),"%"), paste0(round(holder[4,5]*100,0),"%" )), las=2,col ="lightblue",las=2)

plot(mdlist[[6]],trans = plogis, shift = coef(mdlist[[6]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Occupied housing units",ylab=" ", ylim=c(0,0.65),xaxt = "n", xlim=c(pctlist[6,1],pctlist[6,2]))
plot(h6, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE, xlim=c(pctlist[6,1],pctlist[6,2]))
abline(v=holder[6,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[6,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[6,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(0.45,0.5,0.6,0.7,0.8,0.9,0.98), labels= c("45%","50%","60%","70%","80%","90%","98%"),las=1)
axis(side=1, at=c(holder[6,2],holder[6,3],holder[6,5]), labels=c("","",""), col ="lightblue")
text(c(holder[6,2],holder[6,3],holder[6,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[6,2]*100,0),"%"),paste0(round(holder[6,3]*100,0),"%"), paste0(round(holder[6,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[6,2],holder[6,3],holder[6,5]), labels= c(paste0(round(holder[6,2]*100,0),"%"),paste0(round(holder[6,3]*100,0),"%"), paste0(round(holder[6,5]*100,0),"%" )), las=2,col ="lightblue",las=2)


plot(mdlist[[7]],trans = plogis, shift = coef(mdlist[[7]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Renter",ylab=" ", ylim=c(0,0.65),xaxt = "n",xlim=c(pctlist[7,1],pctlist[7,2]))
plot(h7, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE, xlim=c(pctlist[7,1],pctlist[7,2]))
abline(v=holder[7,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[7,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[7,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(0.03,0.1,0.2,0.3,0.4,0.5,0.61), labels= c("3%","10%","20%","30%","40%","50%","61%"),las=1)
axis(side=1, at=c(holder[7,2],holder[7,3],holder[7,5]), labels=c("","",""), col ="lightblue")
text(c(holder[7,2],holder[7,3],holder[7,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[7,2]*100,0),"%"),paste0(round(holder[7,3]*100,0),"%"), paste0(round(holder[7,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[7,2],holder[7,3],holder[7,5]), labels= c(paste0(round(holder[7,2]*100,0),"%"),paste0(round(holder[7,3]*100,0),"%"), paste0(round(holder[7,5]*100,0),"%" )), las=2,col ="lightblue",las=2)

plot(mdlist[[8]],trans = plogis, shift = coef(mdlist[[8]])[1], seWithMean = TRUE,  rug = FALSE, shade = TRUE, shade.col = "lightgreen",  col = "blue",  xlab="ZCTA % Single-family homes",ylab=" ", ylim=c(0,0.65),xaxt = "n", xlim=c(pctlist[8,1],pctlist[8,2]))
plot(h8, freq=FALSE, col = alpha("lightblue",0.5), lwd = 2,border = "white",add = TRUE, xlim=c(pctlist[8,1],pctlist[8,2]))
abline(v=holder[8,2], col="lightblue", lty = 2, lwd = 2)
abline(v=holder[8,3], col="lightblue", lty = 3, lwd = 2)
abline(v=holder[8,5], col="lightblue", lty = 2, lwd = 2)
# axis(side=1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0%","20%","40%","60%","80%","100%"),las=1)
axis(side=1, at=c(0.41,0.5,0.6,0.7,0.8,0.9,0.99), labels= c("41%","50%","60%","70%","80%","90%","99%"),las=1)
axis(side=1, at=c(holder[8,2],holder[8,3],holder[8,5]), labels=c("","",""), col ="lightblue")
text(c(holder[8,2],holder[8,3],holder[8,5]), par("usr")[3]+0.6, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c(paste0(round(holder[8,2]*100,0),"%"),paste0(round(holder[8,3]*100,0),"%"), paste0(round(holder[8,5]*100,0),"%" )), cex = 1.2, col="gray11")
axis(4, at=c(0,0.2,0.4,0.6), labels= c(0,0.2,0.4,0.6), las=1, col="lightblue")
# axis(side=1, at=c(holder[8,2],holder[8,3],holder[8,5]), labels= c(paste0(round(holder[8,2]*100,0),"%"),paste0(round(holder[8,3]*100,0),"%"), paste0(round(holder[8,5]*100,0),"%" )), las=2,col ="lightblue",las=2)

##checking max and min yhat and its corresponding x #

p_obj <- plot(mdlist[[1]], trans = plogis, shift = coef(mdlist[[1]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),] #get y hat max (top of inversed u shape)
sm_df[which.min(sm_df$fit),] 

p_obj <- plot(mdlist[[2]], trans = plogis, shift = coef(mdlist[[2]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),]  
sm_df[which.min(sm_df$fit),] 

p_obj <- plot(mdlist[[3]], trans = plogis, shift = coef(mdlist[[3]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),]
sm_df[which.min(sm_df$fit),]

p_obj <- plot(mdlist[[3]], trans = plogis, shift = coef(mdlist[[3]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),]  
sm_df[which.min(sm_df$fit),]
#moved
p_obj <- plot(mdlist[[4]], trans = plogis, shift = coef(mdlist[[4]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),] 
sm_df[which.min(sm_df$fit),]
#poverty
p_obj <- plot(mdlist[[5]], trans = plogis, shift = coef(mdlist[[5]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),] 
sm_df[which.min(sm_df$fit),]
#occu
p_obj <- plot(mdlist[[6]], trans = plogis, shift = coef(mdlist[[6]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),]  
sm_df[which.min(sm_df$fit),]
#renter
p_obj <- plot(mdlist[[7]], trans = plogis, shift = coef(mdlist[[7]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),] 
sm_df[which.min(sm_df$fit),]
#sf
p_obj <- plot(mdlist[[8]], trans = plogis, shift = coef(mdlist[[8]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),] 
sm_df2 <- sm_df[sm_df$x > 0.7,]
sm_df2[which.max(sm_df2$fit),] #look for elbow
sm_df[which.min(sm_df$fit),]
#edu 
p_obj <- plot(mdlist[[9]], trans = plogis, shift = coef(mdlist[[9]])[1], residuals = TRUE)[[1]]
sm_df <- as.data.frame(p_obj[c("x", "se", "fit")])
sm_df[which.max(sm_df$fit),]  
sm_df[which.min(sm_df$fit),]


#######################################
#              CA case study          #
#######################################

#### Mortality ####
death <- read.csv("20221206_deaths_final_2009-2018__zip_year_sup.csv")
table(death$Year)
zip <- unique(death$ZIP_Code)
length(zip) #2664

cw <- readRDS("UDS_corsswalk_till2022.rds")#crosswalk
table(cw$Year)

# manually add 2009 and 2010
cw2009 <- read_xls("ZIPCodetoZCTACrosswalk2009UDS.xls")
cw2009 <- cw2009[,c("ZIP", "ZCTA_USE")]
cw2009$Year <- 2009
cw2010 <- read_xls("ZIPCodetoZCTACrosswalk2010UDS.xls")
cw2010 <- cw2010[,c("ZIP", "ZCTA_USE")]
cw2010$Year <- 2010
cw2 <- rbind(cw2009, cw2010)
colnames(cw2)[2] <- "ZCTA"

cw <-rbind(cw2, cw)

###  each death year, check % of ZIP missing without crosswalk
names(death)
death2 <- death[death$Strata == "Total Population" & death$Cause == "ALL",] #subset by all cause and total pop
length(unique(death2[death2$Year == 2009,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2010,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2011,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2012,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2013,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2014,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2015,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2016,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2017,]$ZIP_Code))#2664
length(unique(death2[death2$Year == 2018,]$ZIP_Code))#2664

# get median and IQR for each year per zip
# get total count by year by zip
death3.tb <- death2 %>% group_by(Year) %>% summarize(total=sum(Count, na.rm = T), median =median(Count, na.rm = T), IQR= IQR(Count, na.rm = T))
sum(death3.tb$total)

# add an indicator of weather it will be loss (if matching ZCTA)
yearlist <- c(2009:2018)
out <- NULL 
for (i in 1:length(yearlist)) {
  temp <- filter(death2, Year %in% yearlist[i]) #each year 
  temp2 <- filter(cw, Year %in% yearlist[i])
  temp$ind_drop <- ifelse(temp$ZIP_Code %in% temp2$ZCTA, 0, 1) #if not in ZCTA, 1, indicating drop without crosswalk
  out <- rbind(out, temp)
}
table(out$ind_drop)
out$ind_drop.f <- factor(out$ind_drop, levels = c(0,1), labels = c("Match","Non-matching"))
# total zip dropped 
round(prop.table(table(out$ind_drop.f)) *100,2)
# % drop each year
round(prop.table(table(out$ind_drop.f, out$Year), margin = 2) *100,2)
range(prop.table(table(out$ind_drop.f, out$Year), margin = 2)[2,])
mean(prop.table(table(out$ind_drop.f, out$Year), margin = 2)[2,])


# total death counts dropped, impute first 
out2 <- out[out$ind_drop == 1,]
min(out[out$Count != 0,]$Count, na.rm = T) #min 11 -> 1-10 is supressed as small cells
# check ZCTA (if use crosswalk, which ZCTA do they belong?) 2016 example
zip.deathdrop <- out2[out2$Year == 2016,"ZIP_Code"]
zip.deathdrop2 <- cw[cw$Year == 2016 & cw$ZIP %in% zip.deathdrop,] #n=828 ZIP Codes
#acs 2012-2016 pop: AF2LE001
pop2016 <- read.csv("/nhgis0020_ds225_20165_zcta.csv")
pop2016$ZCTA <- stringr::str_sub(pop2016$NAME_E, -5,-1 )
pop2016 <- pop2016[c("ZCTA","AF2LE001")]
zip.deathdrop2 <- left_join(zip.deathdrop2 , pop2016, by="ZCTA")
hist(zip.deathdrop2$AF2LE001)
summary(zip.deathdrop2$AF2LE001)
summary(zip.deathdrop2$AF2LE001 ==0) #10 of them is zero population 
length(unique(zip.deathdrop2$ZCTA)) # how many ZCTA; 513
#population distribution- CA ZCTA 
cw2016 <- read_xlsx("ZIPCodetoZCTACrosswalk2016UDS.xlsx")
cw2016  <- cw2016[cw2016$STATE == "CA",]
pop2016.ca <- pop2016[pop2016$ZCTA %in% cw2016$ZCTA ,]#1762
mean(pop2016.ca$AF2LE001)
summary(pop2016.ca$AF2LE001)

# NA treat as zero 
out2$Count_0 <- ifelse(is.na(out2$Count),0,out2$Count)
# NA treat as min 1 
out2$Count_1 <- ifelse(is.na(out2$Count),1,out2$Count)
# NA treat as max, 10
out2$Count_10 <- ifelse(is.na(out2$Count),10,out2$Count)
# NA treat as mean,5
out2$Count_5 <- ifelse(is.na(out2$Count),5,out2$Count)
# NA treat as random draw 
set.seed(12345)
out2$Count_random <- ifelse(is.na(out2$Count), sample(c(1:10),length(out2), replace = T),out2$Count)
#check 
x <- out2[is.na(out2$Count),]

# How much % is suppressed cells?
table(death$Annotation_Desc) # if NA, is small cells or complemntary cells 

sum(is.na(out2$Count))/sum(is.na(death$Count)) * 100
sum(is.na(out2$Count))/sum(is.na(out$Count))  * 100 #44.66 % is small cell! out2 <- out[out$ind_drop == 1,]
sum(is.na(out$Count))# total small

# let's break them down by year
# n supressed cells
ck1 <- out %>% group_by(Year) %>% summarise(n.scell = sum(is.na(Count)), pct=n.scell/2664*100) 
# n among non-matching zip codes 
ck2 <- out2 %>% group_by(Year) %>% summarise(n.scell.dzip = sum(is.na(Count)), total.d=n(),pct.d=n.scell.dzip/total.d*100) 
ck <- cbind(ck1,ck2)
ck$pct2 <- ck$n.scell.dzip/ck$n.scell*100
mean(ck$n.scell.dzip)
mean(ck$pct2)

# total death counts dropped, original data 
sum(out2$Count, na.rm=T) #777
sum(out[out$ind_drop == 0,]$Count, na.rm=T) # 2468870
sum(out$Count, na.rm=T) # 2468870
sum(out2$Count, na.rm=T) / sum(out$Count, na.rm=T)  *100
# by year
tb1 <- out2 %>% group_by(Year) %>% summarize(n.drop =sum(Count, na.rm = T))  
tb2 <- out %>% filter(ind_drop == 0) %>% group_by(Year) %>% summarize(n.total =sum(Count, na.rm = T))
tb3 <- merge(tb1, tb2, by="Year")
tb3$freq <- round(tb3$n.drop/tb3$n.total*100,2)

# total death counts dropped, min impute data 
sum(out2$Count_1, na.rm=T) #3850
out$Count_1 <- ifelse(is.na(out$Count),1,out$Count)
sum(out[out$ind_drop == 0,]$Count_1, na.rm=T) # 2472344
sum(out$Count_1, na.rm=T) #  2475924
sum(out2$Count_1, na.rm=T) / sum(out[out$ind_drop == 0,]$Count_1, na.rm=T)  *100

# mim shouold be 0
sum(out2$Count_1, na.rm=T) #3850
out$Count_1 <- ifelse(is.na(out$Count),1,out$Count)
sum(out[out$ind_drop == 0,]$Count_1, na.rm=T) # 2472344
sum(out$Count_1, na.rm=T) #  2475924
sum(out2$Count_1, na.rm=T) / sum(out[out$ind_drop == 0,]$Count_1, na.rm=T)  *100

# by year
tb1 <- out2 %>% group_by(Year) %>% summarize(n.drop =sum(Count_1, na.rm = T))
tb2 <- out %>% filter(ind_drop == 0) %>% group_by(Year) %>% summarize(n.total =sum(Count_1, na.rm = T))
tb3 <- merge(tb1, tb2, by="Year")
tb3$freq <- round(tb3$n.drop/tb3$n.total*100,2)


# total death counts dropped, max impute data 
sum(out2$Count_10, na.rm=T) #28807
out$Count_10 <- ifelse(is.na(out$Count),10,out$Count)
sum(out[out$ind_drop == 0,]$Count_10, na.rm=T)  #2503610
sum(out$Count_10, na.rm=T)  # 2532417
sum(out2$Count_10, na.rm=T) / sum(out[out$ind_drop == 0,]$Count_10, na.rm=T)  *100
# by year
tb1 <- out2 %>% group_by(Year) %>% summarize(n.drop =sum(Count_10, na.rm = T))
tb2 <- out %>% filter(ind_drop == 0) %>% group_by(Year) %>% summarize(n.total =sum(Count_10, na.rm = T))
tb3 <- merge(tb1, tb2, by="Year")
tb3$freq <- round(tb3$n.drop/tb3$n.total*100,2)

# total death counts dropped, mean impute data 
sum(out2$Count_5, na.rm=T) #14792
out$Count_5 <- ifelse(is.na(out$Count),5,out$Count)
sum(out[out$ind_drop == 0,]$Count_5, na.rm=T) # 2486240
sum(out$Count_5, na.rm=T) #  2501032
sum(out2$Count_5, na.rm=T) / sum(out[out$ind_drop == 0,]$Count_5, na.rm=T)  *100
# by year
tb1 <- out2 %>% group_by(Year) %>% summarize(n.drop =sum(Count_5, na.rm = T))
tb2 <- out %>% filter(ind_drop == 0) %>% group_by(Year) %>% summarize(n.total =sum(Count_5, na.rm = T))
tb3 <- merge(tb1, tb2, by="Year")
tb3$freq <- round(tb3$n.drop/tb3$n.total*100,2)


# total death counts dropped, random impute data 
sum(out2$Count_random, na.rm=T) # 18086
# be careful with ifelse: https://stackoverflow.com/questions/55148371/ifelse-over-each-element-of-a-vector/55148487#55148487
set.seed(12345)
out$Count_random <-  ifelse(is.na(out$Count), sample(c(1:10),length(out), replace = T),out$Count)
sum(out[out$ind_drop == 0,]$Count_random, na.rm=T) #  2491566
sum(out$Count_random, na.rm=T) #  2509940
sum(out2$Count_random, na.rm=T) / sum(out[out$ind_drop == 0,]$Count_random, na.rm=T)  *100
# by year
tb1 <- out2 %>% group_by(Year) %>% summarize(n.drop =sum(Count_random, na.rm = T))
tb2 <- out %>% filter(ind_drop == 0) %>% group_by(Year) %>% summarize(n.total =sum(Count_random, na.rm = T))
tb3 <- merge(tb1, tb2, by="Year")
tb3$freq <- round(tb3$n.drop/tb3$n.total*100,2)

## can we take a look among core vs non core?

# out2 to join with RUCA
ruca$ZIP.num <- as.numeric(ruca$ZIP)
death2.ruca <- left_join(death2, ruca, by=c("ZIP_Code"="ZIP.num"))
death.ruca <- left_join(out2, ruca, by=c("ZIP_Code"="ZIP.num"))
death.ruca$RUCA1binv2.f <- factor(death.ruca$RUCA1binv2, levels = c(1,2), labels  =c("Core","Non-core"))
death2.ruca$RUCA1binv2.f <- factor(death2.ruca$RUCA1binv2, levels = c(1,2), labels  =c("Core","Non-core"))
# summarize by year, total death dropped, split by RUCA, n and %; and total zip dropped? 
tb1 <- death.ruca %>%  group_by(Year, RUCA1binv2.f) %>% 
  summarize(n.total =sum(Count, na.rm = T)) 
tb2 <- death.ruca %>%  group_by(Year, RUCA1binv2.f) %>% 
  summarize(n.total =sum(Count, na.rm = T)) %>% group_by(Year, add=TRUE) %>% summarize(pct = 100* n.total/sum(n.total, na.rm = T)) 
tb <- cbind(tb1,tb2)
tb <- tb[!is.na(tb$RUCA1binv2.f),c("Year...1" , "RUCA1binv2.f" ,"n.total" ,  "pct" )]
# add one col - % of total cases?
tb.total <- death2.ruca %>%  group_by(Year, RUCA1binv2.f) %>% 
  summarize(n.total.d =sum(Count, na.rm = T)) 
tb.total <- tb.total[!is.na(tb.total$RUCA1binv2.f),]

tb <- left_join(tb,death3.tb,by="Year")
tb$pct_total <- tb$n.total/tb$total*100
tb <- cbind(tb,tb.total)
tb$pct_ruca_total <- tb$n.total/tb$n.total.d*100


colnames(tb)[1] <- "Year"
tb <- tb[,-10]
ggplot(tb, aes(fill=RUCA1binv2.f, y=pct, x=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0("n=",n.total)), position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0(round(pct_ruca_total,2),"%")), position = position_stack(vjust = 0.4))+
  xlab("Mortality Year") + 
  ylab("% of total dropped death under non-crosswalk linkages")+
  scale_fill_manual(values=c("#FEFE62", "#D35FB7"),name = "RUCA")+
  theme_minimal() 

# get a range
range(tb[tb$RUCA1binv2.f == "Core","pct"]) #2009 need cautious
range(tb[tb$RUCA1binv2.f == "Core" & tb$Year != 2009,"pct"])

##### Health care #######

cca <- read_xlsx("CoveredCaliforniaEnrollmentByZipCodeData.xlsx", skip = 4) 
sum(cca$Enrollees, na.rm = T)
median(cca$Enrollees, na.rm = T)
IQR(cca$Enrollees, na.rm = T)
cw2016 <- filter(cw, cw$Year == 2016)
# understand that is the min publishable number (relevant to suppressed cells)
min(cca[cca$Enrollees != 0,]$Enrollees, na.rm = T) #min 10
# add indicator of dropping
cca$ind_drop <- ifelse(cca$`Zip Code` %in%  cw2016$ZCTA, 0,1 )
cca$ind_drop.f <- factor(cca$ind_drop, levels = c(0,1), labels = c("Match","Non-matching"))
table(cca$ind_drop)
prop.table(table(cca$ind_drop))

#comparing population
zip.ccadrop <- cca[cca$ind_drop==1,]
zip.ccadrop2 <- cw[cw$Year == 2016 & cw$ZIP %in% zip.ccadrop$`Zip Code`,] #n=573 ZIP Codes
573/length(unique(cca$`Zip Code`))*100 #24.81
576/length(unique(cca$`Zip Code`))*100 #24.94-> 3 zips is not in the crosswalk file - report this number as zip code dropped
zip.ccadrop2 <- left_join(zip.ccadrop2 , pop2016, by="ZCTA")
hist(zip.ccadrop2$AF2LE001)
summary(zip.ccadrop2$AF2LE001)
summary(zip.ccadrop2$AF2LE001 ==0) #2 of them is zero population 
length(unique(zip.ccadrop2$ZCTA)) # how many ZCTA; 454

# look at how much in core vs non metro core
cca.ruca <- left_join(cca, ruca, by=c("Zip Code"="ZIP"))
cca.ruca$RUCA1binv2.f <- factor(cca.ruca$RUCA1binv2, levels = c(1,2), labels  =c("Core","Non-core"))
tb.cca <- cca.ruca %>%  group_by(ind_drop, RUCA1binv2.f) %>% 
  summarize(n.total =sum(Enrollees, na.rm = T)) 
4880/(4880+1253930)*100
1130/(1130+128320)*100
tb2 <- data.frame(Year=c(2016,2016),
                  RUCA1binv2.f=c("Core","Non-Core"),
                  n.total=c(4880, 1130),
                  pct=c(81.2,18.8),
                  total=c(1388260,1388260),
                  pct_ruca_total=c(0.39,0.87)
                  
)
ggplot(tb2, aes(fill=RUCA1binv2.f, y=pct, x=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0("n=",n.total)), position = position_stack(vjust = 0.5))+
  geom_text(aes(label = paste0(round(pct_ruca_total,2),"%")), position = position_stack(vjust = 0.4))+
  xlab("Coverd California Enrollment Year") + 
  ylab("% of total dropped enrollees under non-crosswalk linkages")+
  scale_fill_manual(values=c("#FEFE62", "#D35FB7"),name = "RUCA")+
  theme_minimal() 

sum(is.na(cca$Enrollees))# non NA, all missing treated as 0
sum(cca$Enrollees==0)
sum(cca[cca$ind_drop==1,]$Enrollees == 0)

sum(cca[cca$ind_drop==1,]$Enrollees) #6110
sum(cca[cca$ind_drop==0,]$Enrollees) #1382250
sum(cca$Enrollees) #1388360
sum(cca[cca$ind_drop==1,]$Enrollees)  / sum(cca[cca$ind_drop==0,]$Enrollees)   *100
sum(cca[cca$ind_drop==1,]$Enrollees)  / sum(cca$Enrollees)   *100# 0.4400876


### impute 0 cells with: max=9, median=5, randomdraw  ###
# NA treat as min 0: original anlaysis
# NA treat as max, 9
cca$Enrollees_9 <- ifelse(cca$Enrollees == 0, 9,cca$Enrollees)

sum(cca[cca$ind_drop==1,]$Enrollees_9) #7892
sum(cca$Enrollees_9) #1390781
sum(cca[cca$ind_drop==1,]$Enrollees_9)  / sum(cca$Enrollees_9)   *100


# NA treat as median 
cca$Enrollees_5 <- ifelse(cca$Enrollees == 0, 5,cca$Enrollees)

sum(cca[cca$ind_drop==1,]$Enrollees_5) # 7100
sum(cca$Enrollees_5) #1389705
sum(cca[cca$ind_drop==1,]$Enrollees_5)  / sum(cca$Enrollees_5)   *100


# NA treat as random draw 
set.seed(12345)
cca$Enrollees_random <-  ifelse(cca$Enrollees == 0, sample(c(0:9),length(cca), replace = T),cca$Enrollees)
sum(cca[cca$ind_drop==1,]$Enrollees_random) # 7243
sum(cca$Enrollees_random) #1389899
sum(cca[cca$ind_drop==1,]$Enrollees_5)  / sum(cca$Enrollees_5)   *100

# how many NA cells?
nrow(cca[cca$Enrollees==0,])  / nrow(cca)   *100 # 11.64502


