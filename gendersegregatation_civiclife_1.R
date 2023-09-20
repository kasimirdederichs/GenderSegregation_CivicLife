
rm(list = ls())
setwd("Y:/Documents/")

# 0. LIBRARIES ####
library(haven)
library(tidyfast)
library(readxl) #loading excel sheets
library(xlsx) #export to excel
library(psych, lib.loc = c("Y:/R/"))
library(dplyr)
library(performance) #check inter-item correlation
library(margins)
library(broom)
library(stringr)
library(forcats)
library(patchwork) #wrapping plots
library(gridExtra) #wrapping plots
library(foreign) # for export to stata
library(weights) #for weighted tables (very basic weighted statistics)
library(lmtest) #lr-tests for multinomial models
library(DescTools) #calculate Pseudo R2
library(nnet) #multinomial regression
library(tidyverse)
library(car) #package for recoding and linear hypothesis test for multinomial models
library(lubridate) #package for handling of dates
library(tidyr)
library(effects) #for predicted probabilities and (CIs)
library(ggeffects) # for ggplot of predicted probabilities (and CIs)
library(zoo)
library(gtools) #stars.pval function
library(data.table) #for overview tables
library(ggpubr) #advanced ggplots
library(stargazer) #print regression tables
library(Hmisc, lib.loc = c("Y:/R/")) #package for labeling
# 1. DEFINE FUNCTIONS #####
#get legend for arranged plot:
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# 2. PREPARE DATASET ##############################################################
dat01 <- read_dta("03_data/03_edit/Stata14/SC6_pTarget_R_11-1-0.dta")
#keep relevant variables:
dat01 %>%
  select(ID_t, wave, t44630c, t44630d, t44630a, t44630b, t44613a, t700001, 
         t70000y, t70000m, inty, intm, t435000, tx20001, tx20002, tx20003,
         t405000, t751001_g4R, inty, intm,
         t321101, t66800a_g1, t66800b_g1, t66800c_g1, t66800d_g1, t66800e_g1,
         t521000, t733001) -> dat01

dat01 <- dat01 %>% as_tibble() #convert dataframe into tibble
  
# 3. BASIC DEMOGRAPHICS #############################################################
#gender:
dat01$woman <- NA
dat01$woman[dat01$t700001==1] <- 0
dat01$woman[dat01$t700001==2] <- 1

## Year of birth:
dat01$t70000y[dat01$t70000y<0] <- NA
#age (using the lubridate package):
dat01 <- dat01 %>%
  mutate(intdate=make_date(inty, intm)) %>% # convert into date format
  mutate(birdate=make_date(t70000y, t70000m))
dat01$age <- as.period(interval(start=dat01$birdate, end=dat01$intdate))$year #start is date of birth, end is interview date $year keeps only year

dat01$mdate <- dat01$intdate # to merge the occupation spells later on.

## Place of Birth:
dat01 %>%
  mutate(migrant = recode(t405000, "-97=NA; -54=NA; 1=0; 2=0; 3=1")) -> dat01
label(dat01$migrant) <- "Migrant (first generation)"
## health:
dat01 <- dat01 %>% #higher values represent better health
  mutate(health = as.numeric(recode(t521000, "-98=NA; -97=NA; -54=NA; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0")))

## marital status:
dat01$marstatus <- factor(recode(dat01$t733001, 
                                 "-98=NA; -97=NA; -54=NA; 1=1; 2=1; 3=2; 4=2; 5=3"), 
                          labels=c("married", "divorced_widowed", "single"))

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(marstatus, id=ID_t, .direction="downup") -> dat13
dat01$marstatus <- as.factor(dat13$marstatus)

#correct the initial indications of marital status with current marital status from spell data:
datms <- read_dta("03_data/03_edit/Stata14/SC6_MaritalStates_R_11-1-0.dta")
datms <- datms %>% #extract marital status in wave 6, i.e., before/in 2013:
  mutate(datey = as.numeric(datey)) %>%
  filter(datey <=2013) %>%
  group_by(ID_t) %>%
  top_n(1, number) %>%
  select(c(ID_t, tx27000))
#update marital status in wave 6:
dat01 <- merge(dat01, datms, by="ID_t", all.x = T, all.y = F)
dat01$marstatus[dat01$tx27000==2] <- "married"
dat01$marstatus[dat01$tx27000==3 | dat01$tx27000==4] <- "divorced_widowed"
dat01 <- dat01 %>% select(-c(tx27000, t733001))

#dummy variables for divorced/widowed and single (ref=married)
dat01 <- dat01 %>%
  mutate(marstdw=case_when(marstatus=="divorced_widowed" ~ 1, 
                           marstatus!="divorced_widorwed" ~ 0),
         marstsi=case_when(marstatus=="single" ~ 1,
                           marstatus!="single" ~ 0),
         marstma=case_when(marstatus=="married" ~ 1,
                           marstatus!="married" ~ 0))
## Number of children:
dat01$childu6 <- dat01$tx20001
dat01$childu14 <- dat01$tx20002
dat01$childu18 <- dat01$tx20003
dat01$childb6a14 <- dat01$childu14 - dat01$childu6
dat01$childb14a18 <- dat01$childu18 - dat01$childu14
label(dat01$childu6) <- "Children under 6"
label(dat01$childb6a14) <- "Children between 6 and 14"
label(dat01$childb14a18) <- "Children between 14 and 18"

# 4. Friends, Religiosity and Big Five: ####
# women-share among friends:
dat01$ws_friends <- as.numeric(as.character(factor(recode(dat01$t321101, "-98=NA; -97=NA; -54=NA; -20=NA;
                                  1=0; 2=1/6; 3=2/6; 4=3/6; 5=4/6; 6=5/6; 7=1")))
                           # , labels = c("none", "almost none", "less than half", 
                           #            "about half", "more than half", "almost all",
                           #            "all"))
                            )

dat01$wom_sh_fr <- as.numeric(as.character(factor(recode(dat01$t321101, "-98=NA; -97=NA; -54=NA; -20=NA;
                                  1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7"))))

#religiosity:
dat01$religiosity <- as.numeric(as.character(factor(recode(dat01$t435000, 
                                   "-98=NA; -97=NA; -54=NA; 1=0; 2=1; 3=2; 4=3")))
                            #, labels=c("not at all religious", "rather not religious", "rather religious", "very religious")
                            )
#Personality: Big Five
dat01[, c("b5extra", "b5agree", "b5consc", "b5neuro", "b5openn")] <- lapply(dat01[, c("t66800a_g1", 
                                                                                      "t66800b_g1", "t66800c_g1", "t66800d_g1", "t66800e_g1")], function(x)
                                                                                        recode(x, "-55=NA; -54=NA"))
dat01 <- dat01 %>%
  mutate(b5extra = as.numeric((b5extra-1)/4), b5agree = as.numeric((b5agree-1)/4), 
         b5consc = as.numeric((b5consc-1)/4), b5neuro = as.numeric((b5neuro-1)/4), 
         b5openn = as.numeric((b5openn-1)/4))

#fill "downup" (e.g. first fills downwards, if not possible, fills upwards)
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(migrant, id=ID_t, .direction="downup") -> dat13 #always in first observation
dat01$migrant <- as.factor(dat13$migrant)

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(ws_friends, id=ID_t, .direction="downup") -> dat13
dat01$ws_friends <- dat13$ws_friends

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(wom_sh_fr, id=ID_t, .direction="downup") -> dat13
dat01$wom_sh_fr <- dat13$wom_sh_fr

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(religiosity, id=ID_t, .direction="downup") -> dat13
dat01$religiosity <- dat13$religiosity

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5extra, id=ID_t, .direction="downup") -> dat13
dat01$b5extra <- dat13$b5extra

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5agree, id=ID_t, .direction="downup") -> dat13
dat01$b5agree <- dat13$b5agree

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5consc, id=ID_t, .direction="downup") -> dat13
dat01$b5consc <- dat13$b5consc

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5neuro, id=ID_t, .direction="downup") -> dat13
dat01$b5neuro <- dat13$b5neuro

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5openn, id=ID_t, .direction="downup") -> dat13
dat01$b5openn <- dat13$b5openn

dat01$number <- dat01$t751001_g4R #new variable that identifies the id-number of the municipality.
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(number, id=ID_t, .direction="downup") -> dat13 
dat01$number <- as.factor(dat13$number)

# 5. Gender norms ############
dat01 <- dat01 %>%
  mutate(gendn1 = as.numeric(as.character(recode(t44630c, "-98 = NA; -97=NA; -54=NA; 1=0; 2=1; 3=2; 4=3"))),
         gendn2 = as.numeric(as.character(recode(t44630d, "-98 = NA; -97=NA; -54=NA; 1=3; 2=2; 3=1; 4=0"))),
         gendn3 = as.numeric(as.character(recode(t44630a, "-98 = NA; -97=NA; -54=NA; 1=3; 2=2; 3=1; 4=0"))),
         gendn4 = as.numeric(as.character(recode(t44630b, "-98 = NA; -97=NA; -54=NA; 1=3; 2=2; 3=1; 4=0"))),
         gendn5 = as.numeric(as.character(recode(t44613a, "-98 = NA; -97=NA; -54=NA; 1=0; 2=1; 3=2; 4=3"))),
         gendn_index = gendn1 + gendn2 + gendn3 + gendn4 + gendn5) # higher values -> more trad. gender norms. 

#drop original variables:
dat01 = subset(dat01, select = -c(t44630c, t44630d, t44630a, t44630b, t44613a)) 

#plug gender role values into other waves:
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(gendn1, id=ID_t, .direction="downup") -> dat13
dat01$gendn1 <- dat13$gendn1

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(gendn2, id=ID_t, .direction="downup") -> dat13
dat01$gendn2 <- dat13$gendn2

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(gendn3, id=ID_t, .direction="downup") -> dat13
dat01$gendn3 <- dat13$gendn3

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(gendn4, id=ID_t, .direction="downup") -> dat13
dat01$gendn4 <- dat13$gendn4

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(gendn5, id=ID_t, .direction="downup") -> dat13
dat01$gendn5 <- dat13$gendn5

dat01 %>%
  group_by(ID_t) %>%
  dt_fill(gendn_index, id=ID_t, .direction="downup") -> dat13
dat01$gendn_index <- dat13$gendn_index


rm(dat13) #drop dat13 (not needed anylonger)


# 6. reduce dataset to two relevant waves and variables ####
# (and add weights)
#select relevant variables:
dat01 <- dat01 %>% select(c(ID_t, wave, woman, intdate, age, mdate, migrant, health, 
                            marstatus, marstdw, marstsi, marstma, childu6, childb6a14, childb14a18, ws_friends, wom_sh_fr, 
                            religiosity, b5extra, b5agree, b5consc, b5neuro, b5openn,
                            number, gendn1, gendn2, gendn3, gendn4, gendn5, gendn_index, inty, intm))

#only waves 6 and 10:
dat01 %>%
  filter(wave==6 | wave==10) -> wdat

#indicator for observations that are observed in both waves:
#1.count number of rows in each ID_t
wdat %>%
  group_by(ID_t) %>%
  mutate(row_numberwave = row_number()) -> wdat
#2. extract number of rows per id to distinct data frame
wdat %>%
  group_by(ID_t) %>% 
  summarise(bothwaves = max(row_numberwave)) -> nrowswdat
wdat$bothwaves <- as.vector(wdat$bothwaves)
#3. merge this to existing data
wdat <- merge(wdat, nrowswdat, by="ID_t", all.x = T, all.y = F)

#weights:
datweights <- read_dta("03_data/03_edit/Stata14/SC6_Weights_R_11-1-0.dta")
datweights <- datweights %>%
  mutate(weightsw6 = w_t6_cal) %>%
  select(c(ID_t, weightsw6))
  
wdat <- merge(wdat, datweights, by="ID_t", all.x=T, all.y=F) # merge weights to wdat.
wdat  <- wdat %>% select(-c(row_numberwave))

# 7. Highest Education ##############################################################
dat03 <- read_dta("03_data/03_edit/Stata14/SC6_Education_R_11-1-0.dta")
dat03 %>%
  filter(datey<2014) %>% #only observations prior to first wave of volunteering-analysis --> perhaps, I could look at changes in education
  select(ID_t, number, tx28101, tx28102, tx28103) -> dat03

dat03 %>%
  group_by(ID_t) %>%
  summarise(lastocc=last(number))-> dat04 #identify last observation for each individual (i.e. highest educational degree)
dat05 <- merge(dat04, dat03, by="ID_t", all.x = T, all.y = F)
dat05 %>%
  filter(dat05$lastocc==dat05$number) %>% #keep only the highest educational degree (last observation)
  mutate(casmin = tx28101, educyears = tx28102, isced97 = tx28103) %>%
  select(ID_t, casmin, educyears, isced97)-> dateduc 

wdat <- merge(wdat, dateduc, by="ID_t", all.x = T, all.y = F)
# 8. Occupational Status (from Spell data) #########
wdat %>% #prepare wdat: create variable for interview date:
  mutate(mdate = make_date(inty, intm),
         mdaten = as.numeric(mdate)) -> wdat

spelldata <- read_dta("03_data/03_edit/Stata14/Spelldata_for_merge/episodensplit_ck_kr.dta")
# adjust time (R-counts with 1970-01-01 as natural zero, dataset counts 1960-01-01 as natural zero)
spelldata <- spelldata %>%
  mutate(startadjn = ymd(as.Date("1960-01-01")) %m+% months(start),
         endadjn = ymd(as.Date("1960-01-01")) %m+% months(end))

spelldata$spellid <- spelldata$ID_t * 100 + spelldata$epinr #Spell-ID variable

spellspanel <- uncount(spelldata, dur, .id = "spellid") #generate one obesrvation for each month of the spell
# now its panel data!

# generate identifier for month of each observation
spellspanel$mdate <- spellspanel$startadjn %m+% months(spellspanel$spellid - 1) #numeric identifier for each obs
#spelldata$duration <- interval(spelldata$starttf, spelldata$endtf) %/%months(1) #attach actual month to observation

spellspanel <- spellspanel %>%
  select(c(ID_t, sptype1, miltype1, schooltype1, traintype1, vocpreptype1, 
           alotype1, gaptype1, emptype1, saisonarbeit1, zeitarbeit1, 
           arbeitszeit1, isco081, egp1, isei081, siops081, income1, mdate)) %>%
  mutate(mdaten = as.numeric(mdate)) 

#merge:
wdat <- merge(wdat, spellspanel, by=c("ID_t", "mdaten"), all.x = T, all.y = F)
wdat$sptype <- factor(wdat$sptype1, labels = c("school", "voc.preparation", 
                                               "voc.training", "military",
                                               "employment", "unemployment",
                                               "parental leave", "gap", "Dateneditionslücke"))

wdat <- wdat %>%
  mutate(occst = case_when(sptype=="school" | sptype=="voc.preparation" | sptype=="voc.training" | sptype=="military" ~ "in.educ",
                           sptype=="employment" ~ "employment",
                           sptype=="unemployment" ~ "unemployment",
                           sptype=="parental leave"  | (sptype=="gap" & (gaptype1==1 | gaptype1==3)) ~ "domestic.work",
                           sptype=="gap" & gaptype1==2 ~ "retirement",
                           sptype=="Dateneditionslücke" ~ "unknown"))
wdat <- wdat %>%
  mutate(occstfpt = case_when(sptype=="school" | sptype=="voc.preparation" | sptype=="voc.training" | sptype=="military" ~ "in.educ",
                              sptype=="employment" & arbeitszeit1>=30 & !is.na(arbeitszeit1) ~ "employment.fulltime",
                              sptype=="employment" & arbeitszeit1<30 & !is.na(arbeitszeit1) ~ "employment.parttime",
                              sptype=="unemployment" ~ "unemployment",
                              sptype=="parental leave"  | (sptype=="gap" & (gaptype1==1 | gaptype1==3)) ~ "domestic.work",
                              sptype=="gap" & gaptype1==2 ~ "retirement",
                              sptype=="Dateneditionslücke" ~ "unknown"))

wdat <- wdat %>% #drop irrelevant variables
  select(-c(sptype, sptype1, miltype1, schooltype1, traintype1, vocpreptype1, 
            alotype1, gaptype1, emptype1, saisonarbeit1, zeitarbeit1, 
            isco081, egp1, isei081, siops081, income1, mdate.x, mdate.y, mdaten))

# 9. MUNICIPALITY LEVEL DATA ################################################################

#main dataset on municipalities:
load("03_data/03_edit/DESTATIS/municipality_data_import.Rda")

#create smaller dataset for merge:
mmerge <- municipalities %>%
  select(c(number, populationdensity)) %>%
  mutate(east = case_when(number<11000 ~ 0,
                          number>=11000 ~ 1))
#merge municipality data on individual data:
mdat <- merge(wdat, mmerge, by = "number", all.x = T, all.y = F)

save(mdat, file = "03_data/03_edit/maindata_incl_municipalities.Rda")

# 10. LOAD DATA AND RESHAPE TO WIDE-FORMAT (mw = maindata, wide) ################
load("03_data/03_edit/maindata_incl_municipalities.Rda") #load data including all predictors and municipality information

#XXXX#

#How many of the wave 6 respondents continue to participate in w10?
table(mdat$wave, mdat$bothwaves, useNA="a")
7721/(7721+2918) #72% of all w6 respondents participate in w10 survey
#generate variable indicating respondents with full information:
mdat <- mdat %>% mutate(fullinfo = ifelse(!is.na(woman) & !is.na(age)& !is.na(migrant) & !is.na(childu6) &
                                            !is.na(childb6a14) & !is.na(childb14a18) & !is.na(ws_friends) & 
                                            !is.na(religiosity) & !is.na(gendn_index) & !is.na(occst) &
                                            !is.na(casmin) & !is.na(east), 1,0),
                        fullinfo_w6 = ifelse(wave==6 & fullinfo==1, 1, 0))
table(mdat$fullinfo_w6, mdat$bothwaves, useNA="a")
5320/(5320+1625) #77% of all w6 respondents with full information participate in w10 survey. 

mdat <- mdat%>% filter(bothwaves==2) #keep only individuals observed in both waves
mw <- reshape(mdat,
              direction = "wide",
              v.names = c("number", "intm", "inty", "intdate", "age" ,"childu6" ,
                          "childb6a14", "childb14a18", "health", "religiosity",
                          "b5agree", "b5extra", "b5consc", "b5neuro", "b5openn",
                          "ws_friends", "wom_sh_fr", "gendn1", "gendn2", "gendn3", 
                          "gendn4", "gendn5", "gendn_index", "casmin", 
                          "educyears", "isced97", "arbeitszeit1", "occst", "occstfpt",
                          "populationdensity", "east"),
              timevar = "wave",
              idvar = "ID_t"
)

mw %>% #assume that personality traits are rather constant over time, I don't need the second measurement
  select(-c(b5agree.10, b5extra.10, b5neuro.10, b5consc.10, b5openn.10)) -> mw
# 11. LOAD VOLUNTEERING DATA ##################################################
load("03_data/vw_2021_04_21.Rda")

# Analysis data (ad)
ad <- merge(mw, vw, by=c("ID_t"), all.x = T, all.y = F)

#recode women-share in voluntary organizations variable (missings):
ad$ws.6[ad$ws.6==-97 | ad$ws.6==-98] <- NA
ad$ws.10[ad$ws.10==-97 | ad$ws.10==-98] <- NA


#less fine-grained organization-type variable:
#create new organization categorization - #original group
ad$votreserve.6 <- as.numeric(ad$too.6)
ad$votreserve.10 <- as.numeric(ad$too.10)
#different classification schemes for voluntary organizations:
classifyvolorgnew <- function(x) {
  case_when(x==1 ~ "Social welfare", #Social/working men
            x==2 ~ "Charity", #Charity (possibly more locally oriented)
            x==3 ~ "Other", #military
            x==4 ~ "Professional",
            x==5 ~ "Unknown",
            x==6 ~ "Youth",
            x==7 ~ "Sociability/Karneval",
            x==8 ~ "Marksmen",
            x==9 ~ "Neighborhood", #village association
            x==10 ~ "Neighborhood", #home/tradition
            x==11 ~ "Gardening",
            x==12 ~ "Neighborhood", #neighborhood
            x==13 ~ "Neighborhood", #Appartment/housing
            x==14 ~ "Neighborhood", #Landfrauen
            x==15 ~ "Parents (School/Kindergarten)", #Parents
            x==16 ~ "Parents (School/Kindergarten)", #Kindergarten
            x==17 ~ "Parents (School/Kindergarten)", #Library
            x==18 ~ "Parents (School/Kindergarten)", #School
            x==19 ~ "Professional", #University
            x==20 ~ "Culture", #Culture
            x==21 ~ "Music (instumental)", 
            x==22 ~ "Choir",
            x==23 ~ "Music (instumental)", #band
            x==24 ~ "Music (instumental)", #orchestra
            x==25 ~ "Theatre", #Theatre
            x==26 ~ "Culture", #Museum
            x==27 ~ "Citizens' initiative",
            x==28 ~ "Party/municipal politics",
            x==29 ~ "Party/municipal politics", #Election volunteer
            x==30 ~ "Court",
            x==31 ~ "Technical assistance", #Technical assistance
            x==32 ~ "Firefighters", #Firefighters
            x==33 ~ "Social welfare", #Migration/Integration
            x==34 ~ "Animal/Environment", #Environment
            x==35 ~ "Animal/Environment", #Animal
            x==36 ~ "Other sports", #Sports
            x==37 ~ "Soccer",
            x==38 ~ "Teamsports, no soccer or volleyball", #Handball
            x==39 ~ "Watersports, no swimming", #rowing
            x==40 ~ "Watersports, no swimming", #Sailing
            x==41 ~ "Teamsports, no soccer or volleyball", #Hockey
            x==42 ~ "Volleyball", #Volleyball
            x==43 ~ "Teamsports, no soccer or volleyball", #Basketball
            x==44 ~ "Other sports", #Hiking
            x==45 ~ "Fishing/Hunting",
            x==46 ~ "Hobby (esp. handicraft)", #chess
            x==47 ~ "Horse-riding",
            x==48 ~ "Gymnastics",
            x==49 ~ "Aerobic", #Yoga
            x==50 ~ "Aerobic", #Fitness
            x==51 ~ "Swimming",
            x==52 ~ "Watersports, no swimming", #Diving
            x==53 ~ "Aerobic", #Aerobic
            x==54 ~ "Watersports, no swimming", #Canoeing
            x==55 ~ "Aerobic", #Rehabilitation
            x==56 ~ "Dance",
            x==57 ~ "Martial arts", #Material arts (e.g. Judo)
            x==58 ~ "Racket sports", #Tennis/Tabletennis/Badminton
            x==59 ~ "Other sports", #climbing
            x==60 ~ "Other sports", #Bowling
            x==61 ~ "Cycling/Running/Athletics", #Running
            x==62 ~ "Other sports", #Skiing
            x==63 ~ "Cycling/Running/Athletics", #Athletics
            x==64 ~ "Hobby (esp. handicraft)", #Fanclub
            x==65 ~ "Martial arts", #Material arts (e.g. Judo)
            x==66 ~ "Cycling/Running/Athletics", #Cycling
            x==67 ~ "Other sports", #Golf
            x==68 ~ "Union",
            x==69 ~ "Workplace",
            x==70 ~ "Elderly people care", #Elderly people care
            x==71 ~ "Disabled people care", #Disabled people care
            x==72 ~ "Social welfare", #Hospital/Blood donation
            x==73 ~ "Social welfare", #Poverty
            x==74 ~ "Charity", #Development aid (international charity)
            x==75 ~ "Self-help group/Helpline",
            x==76 ~ "Hobby (esp. handicraft)", #Motorsport/car
            x==77 ~ "Animal/Environment", #Dog
            x==78 ~ "Hobby (esp. handicraft)", #Hobby
            x==79 ~ "Religious (esp. Church)", #Church
            x==80 ~ "Religious (esp. Church)", #Faith-based
            x==81 ~ "Religious (esp. Church)", #Sects
            x==82 ~ "Other")
}
#apply classifyvolorg function
ad <- ad %>%
  mutate(too.6old = too.6,
         too.10old = too.10) %>%
  mutate(too.6new = too.6,
         too.10new = too.10) %>%
  mutate_at(c("too.6new", "too.10new"), classifyvolorgnew)
#choose the one for further analyses: classifyvolorgnew
ad$vot.6 <- as.factor(ad$too.6new)
ad$vot.10 <- as.factor(ad$too.10new)
ad <- ad %>% select(c(ID_t, woman, migrant, marstatus, marstdw, marstsi, marstma, bothwaves, weightsw6, 
                      age.6, childu6.6, childb6a14.6, childb14a18.6, health.6,
                      religiosity.6, b5agree.6, b5openn.6, b5extra.6, b5consc.6,
                      b5neuro.6, ws_friends.6, wom_sh_fr.6, gendn1.6, gendn2.6, gendn3.6,
                      gendn4.6, gendn5.6, gendn_index.6, casmin.6, educyears.6, isced97.6,
                      occstfpt.6, populationdensity.6, east.6, ws.10, ws.6, 
                      voltime.6, voltime.10, vot.6, vot.10))
ad$volplot1 <- case_when(!is.na(ad$vot.6) ~ 1, is.na(ad$vot.6) ~ 0)
table(ad$volplot1) #number of voluntary affiliations
numbervolaff <- table(ad$volplot1)[2]
numbervolaff2 <- paste("N = ", numbervolaff, "affiliations")
ad <- ad %>% select( - volplot1)
# 12. FIGURE 1: WOMEN-SHARE IN DIFFERENT TYPES OF ORG. #################################
ad %>%
  group_by(vot.6) %>%
  summarise(Mean=mean(woman),
            SD=sd(woman),
            N=length(woman),
            SE=sqrt((mean(woman)*(1-mean(woman))/N)),
            ul=mean(woman)+SE*1.96,
            ll=mean(woman)-SE*1.96) -> genderdist6
genderdist6$vot.6 = with(genderdist6, reorder(vot.6, Mean, max))
genderdist6 <- genderdist6 %>% 
  filter(!is.na(x=vot.6)) %>%
  arrange(Mean)

ad %>% filter(!is.na(vot.6)) %>% 
  group_by(ID_t) %>%
  summarise(Unique_elements = n_distinct(vot.6, na.rm=T)) 
#The affiliations at t1 are nested within 3,636 distinct individuals.

#determine positions and widths of the rectangles in the plot: (depending on N in each category)
genderdist6$right <- cumsum(genderdist6$N) + 30*c(0:(nrow(genderdist6)-1))
genderdist6$middle <- cumsum(genderdist6$N) + 30*c(0:(nrow(genderdist6)-1)) - 0.5*genderdist6$N
genderdist6$left <- genderdist6$right - genderdist6$N

orglist <- genderdist6$vot.6
middlepos <- genderdist6$middle
fig1a <- ggplot(genderdist6, aes(ymin=0)) +
  geom_rect(aes(xmin= left, xmax = right, ymax=Mean), fill = "grey40", alpha = 0.9) +
  scale_x_continuous(breaks = middlepos,
                     labels = paste0(orglist),
                     limits = c(-20,6197), expand = c(0.001,0)) +
  geom_errorbar(aes(x = middle, ymin=ll, ymax=ul, width=0.5)) +
  xlab("Type of voluntary association")+
  ylab("Share of women") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.margin = margin(0.02,0.7,0.02,0.2, "cm")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 10),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(0,1), expand = c(0.001,0)) +
  #geom_text(x=100, y = 0.8, label = numbervolaff2) +
  coord_flip()
fig1a
ggsave("05_outputs/for_export/fig_1_seg_across.jpg", dpi = 600, width = 40, height = 40, units = c("cm"))


# 13. FIGURE 2: GENDER DISTRIBUTION WITHIN MOST POPULAR ORGANIZATIONS #####
#Is the men/women contact the same within each type of org. for women and men?
vd <- ad %>% 
  filter(!is.na(vot.6) & !is.na(ws.6)) %>%
  as_tibble() %>%
  mutate(vo_type.6 = case_when(ws.6==1 | ws.6==2 | ws.6==3 ~ "menmajority",
                               ws.6==4 ~ "equal",
                               ws.6==5 | ws.6==6 | ws.6==7 ~ "womenmajority")) %>%
  select(c(ID_t, vo_type.6, vot.6, woman))

#create aggregated dataframe by org type, composition and gender
vd_agg <- vd %>% 
  select(-c(ID_t)) %>%
  count(vot.6, vo_type.6, woman) %>%
  mutate(vo_type.6 = fct_relevel(vo_type.6, "womenmajority", "equal", "menmajority")) %>%
  group_by(vot.6, woman) %>%
  mutate(group_id = cur_group_id()) # group id by orgtype and gender
#by org type and gender: calculate share of VOM/VOE/VOW
vd_agg_2 <- transform(vd_agg, perc = round(ave(n, group_id, FUN = prop.table), digits = 4)) #change group_id to vot.6 for share among all members of a type of org. 
#total number of people involved in an organization:
vd_agg_2$group_total <- ave(vd_agg_2$n, vd_agg_2$vot.6, FUN=sum)
#most important types of organizations:
vd_agg_2 <- vd_agg_2 %>%
  arrange(desc(group_total), vot.6, woman) %>%
  mutate(SE=sqrt(perc*(1-perc)/n),
         ul=round(perc+SE*1.96, digits = 4),
         ll=round(perc-SE*1.96, digits = 4)) %>%
  ungroup() %>%
  filter(group_total>200) #only organizations with at least 200 members in survey!
head(vd_agg_2, 50)

vd_agg_2 <- vd_agg_2 %>%
  mutate(assoc_context = case_when(vo_type.6=="equal" ~ "gender-integrated",
                                   vo_type.6=="menmajority" ~ "men-dominated",
                                   vo_type.6=="womenmajority" ~ "women-dominated"),
         assoc_context = fct_relevel(assoc_context, "men-dominated", "gender-integrated", "women-dominated"),
         vot.6 = fct_relevel(vot.6, "Religious (esp. Church)", "Parents (School/Kindergarten)",
                             "Other sports", "Soccer", "Neighborhood", "Choir", "Party/municipal politics"),
         Gender = case_when(woman==1 ~ "Women", woman==0 ~ "Men"))

#restrict error bars (CI) between 0 and 1:
vd_agg_2 <- vd_agg_2 %>%
  mutate(ll = ifelse(ll>=0, ll, 0))

fig_within_seg <- ggplot(vd_agg_2, aes(fill=Gender, y=perc, x=assoc_context)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  ylab("Share among all individuals involved 
in a given type of association") +
  xlab("Gender composition of associational context") +
  facet_wrap(~vot.6, dir="v", ncol=7, labeller = labeller(vot.6 = label_wrap_gen(5))) +
  geom_errorbar(aes(ymin=ll, ymax=ul), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1, size = 15),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t= 20), size = 20),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 15),
        strip.text = element_text(size = 15))
fig_within_seg
ggsave("05_outputs/for_export/fig_2_within_seg.jpg", dpi = 600, width = 40, height = 20, units = c("cm"))


# 14. RESHAPE/PIVOT IN FINAL WIDE-WIDE FORMAT #####
#one row per respondent (2 time points and up to 6 vol affiliations)
#volunteering trajectories:
ad <- ad %>%
  mutate(vtrajectory = ifelse(bothwaves!=2, 0, #no trajectory, as only observed once -> drop from sample later
                              case_when(is.na(vot.10) & is.na(vot.6) ~ 1, #stay uninvolved
                                        vot.10==vot.6 & !is.na(vot.10) & !is.na(vot.6) ~ 2, #stay involved
                                        is.na(vot.10) & !is.na(vot.6) ~ 3, #quit
                                        !is.na(vot.10) & is.na(vot.6) ~ 4))) #start

#arrange volunteer affiliations according to importance: (order by voltime at w10, then by voltime at w6, pick the most important starting/quitting
#transitions later on).
ad <- ad %>%
  group_by(ID_t) %>%
  arrange(desc(voltime.10), desc(voltime.6), .by_group = T) %>%
  mutate(volid = row_number())
# ad <- ad %>%
#   group_by(ID_t, voltime.10) %>%
#   arrange(desc(voltime.6), .by_group = T)

#assess how many individuals have more than one volunteer engagement at a time:
ad %>% 
  mutate(volw.6 = case_when(is.na(vot.6)~0, #indicate volunteer engagements
                           !is.na(vot.6)~1),
         volaff.6 = ave(volw.6, ID_t, FUN = sum), #sum within each individual
         volw.10 = case_when(is.na(vot.10)~0,
                            !is.na(vot.10)~1),
         volaff.10 = ave(volw.10, ID_t, FUN = sum),
         voltraj_one = 1,
         n_vtraj = ave(voltraj_one, ID_t, FUN = sum)) %>%
  select(-c(volw.6, volw.10))-> ad #drop unnecessary variables that vary within individuals

ad <- ad %>%
  mutate(start_indicator = case_when(vtrajectory==4 ~ 1,
                                     vtrajectory==1 | vtrajectory==2 | vtrajectory==3 ~ 0),
         quit_indicator = case_when(vtrajectory==3 ~ 1,
                                    vtrajectory==1 | vtrajectory==2 | vtrajectory==4 ~ 0),
         start_nr = ave(start_indicator, ID_t, FUN = sum),
         quit_nr = ave(quit_indicator, ID_t, FUN = sum)) %>%
  select(-c(start_indicator, quit_indicator)) #drop unnecessary variables that vary within individuals

#use pivot_wider here to pivot volunteering variables (rest stays constant for each individual of course)
aw <- pivot_wider(ad,
                  names_from = volid,
                  values_from = c(ws.6, ws.10, vtrajectory,
                                  voltime.6, voltime.10, vot.6, vot.10))
PercTable(aw$start_nr, margins=c(1,2))
PercTable(aw$quit_nr, margins=c(1,2))
#initial share of those with multiple start/quit transitions - but do the same with final sample later. 

#many people have no first/second/third... trajectory -> the all receive the value 1 (i.e. remain uninvolved) for these trajectories.
aw <- aw %>%
  mutate(vtrajectory_1 = ifelse(is.na(vtrajectory_1), 1, vtrajectory_1),
         vtrajectory_2 = ifelse(is.na(vtrajectory_2), 1, vtrajectory_2),
         vtrajectory_3 = ifelse(is.na(vtrajectory_3), 1, vtrajectory_3),
         vtrajectory_4 = ifelse(is.na(vtrajectory_4), 1, vtrajectory_4),
         vtrajectory_5 = ifelse(is.na(vtrajectory_5), 1, vtrajectory_5),
         vtrajectory_6 = ifelse(is.na(vtrajectory_6), 1, vtrajectory_6))

# 15. GENERATE VARIABLES FOR MULTINOMIAL REGRESSION ANALYSES #####################
# COVARIATES: 
#children variables -> dummies
aw$cdu6[aw$childu6.6==0] <- 0
aw$cdu6[aw$childu6.6>0] <- 1
aw$cd614[aw$childb6a14.6==0] <- 0
aw$cd614[aw$childb6a14.6>0] <- 1
aw$cd1418[aw$childb14a18.6==0] <- 0
aw$cd1418[aw$childb14a18.6>0] <- 1

#age categories:
aw <- aw %>%
  mutate(age_cat = as.factor(case_when(age.6>=20 & age.6<30 ~ "20s",
                                       age.6>=30 & age.6<40 ~ "30s",
                                       age.6>=40 & age.6<50 ~ "40s",
                                       age.6>=50 & age.6<60 ~ "50s",
                                       age.6>=60 & !is.na(age.6) ~ "60s+")))
#dummy variables for age_cat:
aw$age_cat_20 <- 0
aw$age_cat_20[aw$age_cat=="20s"] <- 1
aw$age_cat_20[is.na(aw$age_cat)] <- NA
aw$age_cat_30 <- 0
aw$age_cat_30[aw$age_cat=="30s"] <- 1
aw$age_cat_30[is.na(aw$age_cat)] <- NA
aw$age_cat_40 <- 0
aw$age_cat_40[aw$age_cat=="40s"] <- 1
aw$age_cat_40[is.na(aw$age_cat)] <- NA
aw$age_cat_50 <- 0
aw$age_cat_50[aw$age_cat=="50s"] <- 1
aw$age_cat_50[is.na(aw$age_cat)] <- NA
aw$age_cat_60 <- 0
aw$age_cat_60[aw$age_cat=="60s+"] <- 1
aw$age_cat_60[is.na(aw$age_cat)] <- NA

#education/isced variable:
aw$isced97 <- case_when(aw$isced97.6==0 ~ 1,
                        aw$isced97.6==1 | aw$isced97.6==2 ~ 2,
                        aw$isced97.6==3 | aw$isced97.6==4 | aw$isced97.6==5 ~ 3,
                        aw$isced97.6==6 | aw$isced97.6==7 ~ 4,
                        aw$isced97.6==8 | aw$isced97.6==9 ~ 5,
                        aw$isced97.6==10 ~ 6)
aw$isced97f <- factor(aw$isced97, labels =c("less than primary", "primary", "lower secondary", "upper secondary", "tertiary", "PhD"))
aw$isced97r <- as.factor(case_when(aw$isced97.6==0 | aw$isced97.6==1 | aw$isced97.6==2 ~ "ISCED_0_1_2",
                                   aw$isced97.6==3 | aw$isced97.6==4 | aw$isced97.6==5 | aw$isced97.6==6 | aw$isced97.6==7 ~ "ISCED_3_4",
                                   aw$isced97.6==8 | aw$isced97.6==9 | aw$isced97.6==10 ~ "ISCED_5_6"))
aw$college <- case_when(aw$isced97.6>=0 & aw$isced97.6<=8 ~ 0,
                        aw$isced97.6>=9 & aw$isced97.6<=10 ~1)
#occupational status:
aw$occstfpt.6[aw$occstfpt.6=="unknown"] <- NA  #unknowns to missings
aw <- aw %>% 
  mutate(occstfpt.6 = factor(occstfpt.6))

#final adaptations to variables:
aw$lpopulationdensity.6 <- log(aw$populationdensity.6) #log of populationdensity, because it's skewed.
aw$occstfpt.6 <- fct_relevel(aw$occstfpt.6, "employment.fulltime", "employment.parttime",
                             "in.educ", "domestic.work", "retirement", "unemployment")
aw$occst_ft <- 0 #create dummy variables for occupational status for regressions later on.
aw$occst_ft[aw$occstfpt.6=="employment.fulltime"] <- 1
aw$occst_ft[is.na(aw$occstfpt.6)] <- NA
aw$occst_pt <- 0
aw$occst_pt[aw$occstfpt.6=="employment.parttime"] <- 1
aw$occst_pt[is.na(aw$occstfpt.6)] <- NA
aw$occst_ie <- 0
aw$occst_ie[aw$occstfpt.6=="in.educ"] <- 1
aw$occst_ie[is.na(aw$occstfpt.6)] <- NA
aw$occst_dw <- 0
aw$occst_dw[aw$occstfpt.6=="domestic.work"] <- 1
aw$occst_dw[is.na(aw$occstfpt.6)] <- NA
aw$occst_re <- 0
aw$occst_re[aw$occstfpt.6=="retirement"] <- 1
aw$occst_re[is.na(aw$occstfpt.6)] <- NA
aw$occst_ue <- 0
aw$occst_ue[aw$occstfpt.6=="unemployment"] <- 1
aw$occst_ue[is.na(aw$occstfpt.6)] <- NA


aw$womanf <- as.factor(aw$woman)

#is an individual "at risk"/eligible of experiencing a start/quit?
#always at risk of starting (as long as observed in both waves)
aw$starteligible <- NA
aw$starteligible[aw$bothwaves==2] <- 1
#at risk of quitting if one was observed in both waves and was involved in wave 6
# i.e., transition either staying involved or quitting.
aw$quiteligible <- NA
aw$quiteligible[aw$bothwaves==2 & ((aw$vtrajectory_1==2 | aw$vtrajectory_1==3) |
                                     (aw$vtrajectory_2==2 | aw$vtrajectory_2==3) |
                                     (aw$vtrajectory_3==2 | aw$vtrajectory_3==3) |
                                     (aw$vtrajectory_4==2 | aw$vtrajectory_4==3) |
                                     (aw$vtrajectory_5==2 | aw$vtrajectory_5==3) |
                                     (aw$vtrajectory_6==2 | aw$vtrajectory_6==3))] <- 1

#Do people start or quit at all? (-> generate a dummy variable, without paying 
#attention to women share of org.)
aw <- aw %>%
  mutate(anystart = case_when(starteligible==1 & vtrajectory_1!=4 & vtrajectory_2!=4 &
                                vtrajectory_3!=4 & vtrajectory_4!=4 & vtrajectory_5!=4 & vtrajectory_6!=4 ~ 0,
                              starteligible==1 & vtrajectory_1==4 | vtrajectory_2==4 |
                                vtrajectory_3==4 | vtrajectory_4==4 | vtrajectory_5==4 | vtrajectory_6==4 ~ 1),
         anyquit = case_when(quiteligible==1 & vtrajectory_1!=3 & vtrajectory_2!=3 &
                               vtrajectory_3!=3 & vtrajectory_4!=3 & vtrajectory_5!=3 & vtrajectory_6!=3 ~ 0,
                             quiteligible==1 & vtrajectory_1==3 | vtrajectory_2==3 |
                               vtrajectory_3==3 | vtrajectory_4==3 | vtrajectory_5==3 | vtrajectory_6==3 ~ 1),
         anyvol.6 = ifelse(vtrajectory_1==2 | vtrajectory_1==3 | vtrajectory_2==2 | vtrajectory_2==3 | 
                             vtrajectory_3==2 | vtrajectory_3==3 | vtrajectory_4==2 | vtrajectory_4==3 |
                             vtrajectory_5==2 | vtrajectory_5==3 | vtrajectory_6==2 | vtrajectory_6==3, 1, 0),
         anyvol.10 = ifelse(vtrajectory_1==2 | vtrajectory_1==4 | vtrajectory_2==2 | vtrajectory_2==4 | 
                              vtrajectory_3==2 | vtrajectory_3==4 | vtrajectory_4==2 | vtrajectory_4==4 |
                              vtrajectory_5==2 | vtrajectory_5==4 | vtrajectory_6==2 | vtrajectory_6==4, 1, 0))

#Women share of the org. one joins: 0=no volunteering at all, 1-7 different women shares
#because I arranged the order of the voluntary engagements based on their time above 
# (before reshaping), this step-wise command makes sure that the first - and most
# time-consuming- volunteer engagement is taken into account. (here, I take the women share of w10)
aw$starttype <- ifelse(aw$anystart==0, 0, 
                       ifelse(aw$vtrajectory_1==4, aw$ws.10_1,
                       ifelse(aw$vtrajectory_2==4, aw$ws.10_2,
                       ifelse(aw$vtrajectory_3==4, aw$ws.10_3,
                       ifelse(aw$vtrajectory_4==4, aw$ws.10_4,
                       ifelse(aw$vtrajectory_5==4, aw$ws.10_5,
                       ifelse(aw$vtrajectory_6==4, aw$ws.10_6, NA)))))))
aw$starttype_f <- factor(aw$starttype, labels= c("uninvolved", "none", "almost none", "less than half",
                                                 "about half", "more than half",
                                                 "almost all", "all"))

aw$startvot <- ifelse(aw$anystart==0, 0,
                      ifelse(aw$vtrajectory_1==4, aw$vot.10_1,
                      ifelse(aw$vtrajectory_2==4, aw$vot.10_2,
                      ifelse(aw$vtrajectory_3==4, aw$vot.10_3,
                      ifelse(aw$vtrajectory_4==4, aw$vot.10_4,
                      ifelse(aw$vtrajectory_5==4, aw$vot.10_5,
                      ifelse(aw$vtrajectory_6==4, aw$vot.10_6, NA)))))))

aw$quitvot <- ifelse(is.na(aw$quiteligible), NA,
                      ifelse(aw$vtrajectory_1==2 | aw$vtrajectory_1==3, aw$vot.6_1,
                      ifelse(aw$vtrajectory_2==2 | aw$vtrajectory_2==3, aw$vot.6_2,
                      ifelse(aw$vtrajectory_3==2 | aw$vtrajectory_3==3, aw$vot.6_3,
                      ifelse(aw$vtrajectory_4==2 | aw$vtrajectory_4==3, aw$vot.6_4,
                      ifelse(aw$vtrajectory_5==2 | aw$vtrajectory_5==3, aw$vot.6_5,
                      ifelse(aw$vtrajectory_6==2 | aw$vtrajectory_6==3, aw$vot.6_6, NA)))))))

aw$quitvot_f <- as.factor(ifelse(is.na(aw$quiteligible), NA_real_,
                     ifelse(aw$vtrajectory_1==2 | aw$vtrajectory_1==3, as.character(aw$vot.6_1),
                            ifelse(aw$vtrajectory_2==2 | aw$vtrajectory_2==3, as.character(aw$vot.6_2),
                                   ifelse(aw$vtrajectory_3==2 | aw$vtrajectory_3==3, as.character(aw$vot.6_3),
                                          ifelse(aw$vtrajectory_4==2 | aw$vtrajectory_4==3, as.character(aw$vot.6_4),
                                                 ifelse(aw$vtrajectory_5==2 | aw$vtrajectory_5==3, as.character(aw$vot.6_5),
                                                        ifelse(aw$vtrajectory_6==2 | aw$vtrajectory_6==3, as.character(aw$vot.6_6), NA_real_))))))))


#only 4 categories: uninvolved, minority, half, majority --> for later analyses
aw <- aw %>%
  mutate(starttype_4 = as.factor(case_when(starttype_f == "uninvolved" ~ "uninvolved",
                                           starttype_f == "none" | starttype_f == "almost none" | starttype_f == "less than half" ~ "menmajority",
                                           starttype_f == "about half" ~ "equal",
                                           starttype_f == "more than half" | starttype_f == "almost all" | starttype_f == "all" ~ "womenmajority")))
aw <- aw %>% #relevel (adjust order)
  mutate(starttype_4ur = fct_relevel(starttype_4, "uninvolved", "menmajority", 
                                     "equal", "womenmajority"))

aw <- aw %>%
  mutate(starttype_6cat = as.factor(case_when(starttype_f == "uninvolved" ~ "uninvolved",
                                           starttype_f == "none" | starttype_f == "almost none" ~ "no women",
                                           starttype_f == "less than half" ~ "less than half women",
                                           starttype_f == "about half" ~ "half women",
                                           starttype_f == "more than half" ~ "more than half women" ,
                                           starttype_f == "almost all" | starttype_f == "all" ~ "only women")))
aw <- aw %>% #relevel (adjust order)
  mutate(starttype_6cat = fct_relevel(starttype_6cat, "uninvolved", "no women", "less than half women", 
                                     "half women", "more than half women", "only women"))


aw <- aw %>%
  mutate(starttype_4robust = as.factor(case_when(starttype_f == "uninvolved" ~ "uninvolved",
                                           starttype_f == "none" | starttype_f == "almost none"  ~ "men_org",
                                           starttype_f == "about half" | starttype_f == "less than half" | starttype_f == "more than half"  ~ "equal_org",
                                           starttype_f == "almost all" | starttype_f == "all" ~ "women_org")))
aw <- aw %>% #relevel (adjust order)
  mutate(starttype_4robustur = fct_relevel(starttype_4robust, "uninvolved", "men_org", 
                                     "equal_org", "women_org"))

#quitting events:
aw$ws_qmodel <- ifelse(is.na(aw$quiteligible), NA,
                ifelse(aw$vtrajectory_1==3 | aw$vtrajectory_1==2, aw$ws.6_1,
                ifelse(aw$vtrajectory_2==3 | aw$vtrajectory_1==2, aw$ws.6_2,
                ifelse(aw$vtrajectory_3==3 | aw$vtrajectory_1==2, aw$ws.6_3,
                ifelse(aw$vtrajectory_4==3 | aw$vtrajectory_1==2, aw$ws.6_4,
                ifelse(aw$vtrajectory_5==3 | aw$vtrajectory_1==2, aw$ws.6_5,
                ifelse(aw$vtrajectory_6==3 | aw$vtrajectory_1==2, aw$ws.6_6, NA)))))))
aw$ws_qmodel <- factor(aw$ws_qmodel, labels= c("none", "almost none", "less than half", #no uninvolved because conditional on being involved in w6
                                               "about half", "more than half",
                                               "almost all", "all"))

#only 4 categories: uninvolved, minority, half, majority --> for later analyses
aw <- aw %>%
  mutate(ws_qmodel_3 = as.factor(case_when(ws_qmodel == "none" | ws_qmodel == "almost none" | ws_qmodel == "less than half" ~ "menmajority",
                                           ws_qmodel == "about half" ~ "equal",
                                           ws_qmodel == "more than half" | ws_qmodel == "almost all" | ws_qmodel == "all" ~ "womenmajority")))

#STEPWISE AND BACKWARDS filling of the quitevent variables (takes into account
#that individuals can have multiple volunteer jobs and only the one previously 
#selected is considered)
aw$quitevent <- NA
aw$quitevent[aw$vtrajectory_6==3] <- 1
aw$quitevent[aw$vtrajectory_6==2] <- 0
aw$quitevent[aw$vtrajectory_5==3] <- 1
aw$quitevent[aw$vtrajectory_5==2] <- 0
aw$quitevent[aw$vtrajectory_4==3] <- 1
aw$quitevent[aw$vtrajectory_4==2] <- 0
aw$quitevent[aw$vtrajectory_3==3] <- 1
aw$quitevent[aw$vtrajectory_3==2] <- 0
aw$quitevent[aw$vtrajectory_2==3] <- 1
aw$quitevent[aw$vtrajectory_2==2] <- 0
aw$quitevent[aw$vtrajectory_1==3] <- 1
aw$quitevent[aw$vtrajectory_1==2] <- 0

#generate less precise variable for women-share in vol org. in wave 6:
aw$qevows <- NA

aw$qevows[aw$quiteligible==1 & (aw$ws.6_1==1 | aw$ws.6_1==2 | aw$ws.6_1==3)] <- "menmajority"
aw$qevows[aw$quiteligible==1 & (aw$ws.6_1==4)] <- "equal"
aw$qevows[aw$quiteligible==1 & (aw$ws.6_1==5 | aw$ws.6_1==6 | aw$ws.6_1==7)] <- "womenmajority"
aw$qevows <- as.factor(aw$qevows)

aw <- aw %>% #relevel (adjust order)
  mutate(qevows = fct_relevel(qevows, "menmajority", "equal", 
                              "womenmajority"))

# 16. Sample definition: ##############
# before reducing the sample for the final analyses, I construct weights:
# variable that indicates whether one is part of the final sample:
aw$finsamstart <- 0 # evtl add other variables that I use for the analyses later on. 
aw$finsamstart[aw$bothwaves==2 & !is.na(aw$starttype_4ur) & !is.na(aw$college) & !is.na(aw$cdu6) &
                 !is.na(aw$cd614) & !is.na(aw$cd1418) & !is.na(aw$age.6) & !is.na(aw$religiosity.6) &
                 !is.na(aw$gendn_index.6) & !is.na(aw$east.6) & !is.na(aw$migrant) & !is.na(aw$occstfpt.6) &
                 !is.na(aw$woman) & !is.na(aw$wom_sh_fr.6)] <- 1
table(aw$finsamstart, useNA = "a") # I end up with 7490 observations. 

#define sample of analyses:
aw <- aw %>%
  mutate(ws_friends_none = ifelse(ws_friends.6==0, 1, ifelse(!is.na(ws_friends.6), 0, NA)),
         ws_friends_almost_none = ifelse(ws_friends.6==0.166666666666667, 1, ifelse(!is.na(ws_friends.6), 0, NA)),
         ws_friends_less_half = ifelse(ws_friends.6==0.333333333333333, 1, ifelse(!is.na(ws_friends.6), 0, NA)),
         ws_friends_half = ifelse(ws_friends.6==0.5, 1, ifelse(!is.na(ws_friends.6), 0, NA)),
         ws_friends_more_half = ifelse(ws_friends.6==0.666666666666667, 1, ifelse(!is.na(ws_friends.6), 0, NA)),
         ws_friends_almost_all = ifelse(ws_friends.6==0.833333333333333, 1, ifelse(!is.na(ws_friends.6), 0, NA)),
         ws_friends_all = ifelse(ws_friends.6==1, 1, ifelse(!is.na(ws_friends.6), 0, NA)))
aw <- aw %>%
  mutate(ws_friends_f = case_when(ws_friends.6==0 ~ 1,
                                  ws_friends.6==0.166666666666667 ~ 2,
                                  ws_friends.6==0.333333333333333 ~ 3,
                                  ws_friends.6==0.5 ~ 4,
                                  ws_friends.6==0.666666666666667 ~ 5,
                                  ws_friends.6==0.833333333333333 ~ 6,
                                  ws_friends.6==1 ~ 7))

#quitting variable: 
aw <- aw %>%
  mutate(qmodel_3mew = factor(case_when(ws_qmodel %in% c("none", "almost none", "less than half") ~ "VOM",
                                        ws_qmodel=="about half" ~ "VOE",
                                        ws_qmodel %in% c("more than half", "almost all", "all") ~ "VOW"),
                              levels = c("VOM", "VOE", "VOW")))
aw <- aw %>%
  mutate(ws_friends_3cat = factor(case_when(ws_friends.6 %in% c(0, 0.166666666666667, 0.333333333333333) ~ "men-dominated",
                                            ws_friends.6==0.5 ~ "gender-integrated",
                                            ws_friends.6 %in% c(0.666666666666667, 0.833333333333333, 1) ~ "women-dominated"),
                              levels = c("men-dominated", "gender-integrated", "women-dominated")))
aw <- aw %>%
  mutate(ws_volorg6_3cat = factor(case_when(ws_qmodel %in% c("none", "almost none", "less than half") ~ "men-dominated",
                                            ws_qmodel=="about half" ~ "gender-integrated",
                                            ws_qmodel %in% c("more than half", "almost all", "all") ~ "women-dominated"),
                                  levels = c("men-dominated", "gender-integrated", "women-dominated")))

aw %>% filter(finsamstart==1) -> smd #overall sample
smd %>% filter(woman==0) -> smdm #sample men
smd %>% filter(woman==1) -> smdw #sample women
#export datasets to stata for the calculation of AMEs:
write.dta(smd, "03_data/stata_datasets/smd.dta")

# 17. FIGURE A3: Benchmark against friendship networks: ####
#restrict figure to people for whom we observe the gender composition of both, vol org and friends:
fig2_df <- smd %>%
  filter(!is.na(ws_volorg6_3cat) & !is.na(ws_friends_3cat))

fig2_df <- fig2_df %>%
  mutate(ws_friends_7cat = case_when(ws_friends_f==1 ~ "none",
                                     ws_friends_f==2 ~ "almost none",
                                     ws_friends_f==3 ~ "less than half",
                                     ws_friends_f==4 ~ "about half",
                                     ws_friends_f==5 ~ "more than half",
                                     ws_friends_f==6 ~ "almost all",
                                     ws_friends_f==7 ~ "all"))

#generate separate dataframes for women and men:
fig2_df_w <- fig2_df %>% filter(woman==1)
fig2_df_m <- fig2_df %>% filter(woman==0)

#using just three response categories (men-dominated, gender-integrated, women-dominated)
fig2_df_wf <- as.data.frame(table(fig2_df_w$ws_friends_3cat)) %>%
  mutate(Perc = Freq / sum(Freq),
         SE = sqrt((Perc*(1-Perc)/sum(Freq))),
         UCI = Perc + 1.96*SE,
         LCI = Perc - 1.96*SE,
         Gender = "Women",
         Setting = "Network of close friends")
fig2_df_wv <- as.data.frame(table(fig2_df_w$ws_volorg6_3cat)) %>%
  mutate(Perc = Freq / sum(Freq),
         SE = sqrt((Perc*(1-Perc)/sum(Freq))),
         UCI = Perc + 1.96*SE,
         LCI = Perc - 1.96*SE,
         Gender = "Women",
         Setting = "Voluntary association")
fig2_df_mf <- as.data.frame(table(fig2_df_m$ws_friends_3cat)) %>%
  mutate(Perc = Freq / sum(Freq),
         SE = sqrt((Perc*(1-Perc)/sum(Freq))),
         UCI = Perc + 1.96*SE,
         LCI = Perc - 1.96*SE,
         Gender = "Men",
         Setting = "Network of close friends")
fig2_df_mv <- as.data.frame(table(fig2_df_m$ws_volorg6_3cat)) %>%
  mutate(Perc = Freq / sum(Freq),
         SE = sqrt((Perc*(1-Perc)/sum(Freq))),
         UCI = Perc + 1.96*SE,
         LCI = Perc - 1.96*SE,
         Gender = "Men",
         Setting = "Voluntary association")

fig2_df_fin <- rbind(fig2_df_wf, fig2_df_wv, fig2_df_mf, fig2_df_mv)

fig_comp <- ggplot(fig2_df_fin, aes(fill=Setting, y=Perc, x=Var1)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  ylab("Relative frequency") +
  xlab("Gender composition of setting") +
  facet_wrap(~Gender) +
  geom_errorbar(aes(ymin=LCI, ymax=UCI), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        #axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 20), size = 20),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        legend.position = "bottom", legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))
fig_comp
ggsave("05_outputs/for_export/fig_A3_comp_vol_snet.jpg", dpi = 600, width = 40, height = 20, units = c("cm"))

# 18. TABLE A4: summary table: ####
smd_relvar <- smd %>%
  ungroup() %>%
  select(c(woman, anyvol.6, anyvol.10, anystart, anyquit, ws_friends_all, 
           ws_friends_almost_all, ws_friends_more_half, ws_friends_half,
           ws_friends_less_half, ws_friends_almost_none, ws_friends_none, 
           gendn_index.6, migrant, college, childu6.6, childb6a14.6, childb14a18.6,
           age.6, religiosity.6, east.6, occst_ft,
           occst_pt, occst_ie, occst_dw, occst_re, occst_ue))

smd_relvar$migrant <- as.numeric(smd_relvar$migrant)-1 # correct factor into numeric (migrant =1, native=0)
table1 <- psych:: describeBy(smd_relvar, smd_relvar$woman, mat = T, digits = 3)
table1 <- table1 %>%
  #arrange(group1) %>%
  select(group1, n, mean, sd, se)
table1 <- setDT(table1, keep.rownames = "Variable")
table1 <- table1 %>%
  mutate(Variable = str_sub(Variable, 1, str_length(Variable)-1)) %>%
  select(-c("se")) %>%
  filter(Variable!="woman")

table1 <- reshape(table1, direction = "wide",
                  v.names = c("n", "mean", "sd"),
                  timevar = "group1",
                  idvar = "Variable")
table1$tvalue <- round((table1$mean.0 - table1$mean.1)/ 
                         sqrt(((table1$sd.0^2)/table1$n.0)+((table1$sd.1^2)/table1$n.1)), digits=3)

varvector <- c("Involved at t1", "Involved at t2", "Any joining", "Any quitting",
               "Women share among friends - all", "Women share among friends - almost all",
               "Women share among friends - more than half", "Women share among friends - half",
               "Women share among friends - less than half", "Women share among friends - almost none",
               "Women share among friends - none", "Gender norms",
               "Born outside Germany", "Higher tertiary education", 
               "Child <6 years", "Child 6-13 years", "Child 14-18 years",
               "Age (in years)", "Religiosity", "Eastern Germany",
               "Fulltime empl.", "Parttime empl.", "In education",
               "Domestic Work", "Retired", "Unemployed")
table1 <- cbind(varvector, table1)

lastrow <- list(varvector="N", Variable="nobs", n.0="", mean.0=table1$n.0[1], sd.0="", n.1="", mean.1=table1$n.1[1], sd.1="", tvalue="")
lastrow <- data.frame(t(unlist(lastrow)))

table1 <- rbind(table1, lastrow)
table1 <- select(table1, varvector, mean.0, sd.0, mean.1, sd.1, tvalue)

stargazer(table1, summary = F, type = "text", digits=3, 
          dep.var.labels = c("Variable", "Mean", "SD", "Mean", "SD", "t-test"), 
          out = "05_outputs/for_export/table1.doc")
write.xlsx(table1, "05_outputs/for_export/table1.xlsx")
# difference in volunteering between women and men is insignificant in both waves:
t.test(anyvol.6 ~ woman, data = smd)
t.test(anyvol.10 ~ woman, data = smd)

# 19. FIGURE 3: Starting and quitting women and men, baseline plot ####
startgg <- read_excel("03_data/stata_datasets/pp_baseline_start.xlsx")
startgg <- startgg %>%
  mutate(Organization = case_when(Organization == "VOM" ~ "men-dominated",
                                  Organization == "VOE" ~ "gender-integrated",
                                  Organization == "VOW" ~ "women-dominated"),
    Organization = fct_relevel(Organization, "men-dominated", "gender-integrated", "women-dominated"))
startplot <- ggplot(startgg, aes(fill=Gender, y=b, x=Organization)) + #final ggplot
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  xlab("Gender composition of associational context") +
  ylab("Predicted probability of starting") +
  geom_errorbar(aes(ymin=ll, ymax=ul), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.18), expand = c(0.01,0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
startplot


quitgg <- read_excel("03_data/stata_datasets/quit.xlsx")
quitgg <- quitgg %>%
  mutate(Organization = case_when(Organization == "VOM" ~ "men-dominated",
                                  Organization == "VOE" ~ "gender-integrated",
                                  Organization == "VOW" ~ "women-dominated"),
         Organization = fct_relevel(Organization, "men-dominated", "gender-integrated", "women-dominated"))
quitplot <- ggplot(quitgg, aes(fill=Gender, y=coef, x=Organization)) + #final ggplot
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  xlab("Gender composition of associational context") +
  ylab("Predicted probability of quitting") +
  geom_errorbar(aes(ymin=lci, ymax=hci), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6), expand = c(0.01,0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
quitplot


quitplot_legend <- ggplot(quitgg, aes(fill=Gender, y=coef, x=Organization)) + #final ggplot
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  xlab("Gender composition of associational context") +
  ylab("Predicted probability of quitting") +
  geom_errorbar(aes(ymin=lci, ymax=hci), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.direction = "horizontal", 
                                         legend.title = element_blank(), legend.text = element_text(size = 12)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6), expand = c(0.01,0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20))) 

#get legend for arranged plot:
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend_sqplot <- get_legend(quitplot_legend)

#wrap the final plot:
design_sqplot <- "AB
                  CC"
fig3 <- wrap_plots(list(A= startplot, B=quitplot, C=legend_sqplot), heights = c(0.9, 0.1), design = design_sqplot)
fig3
ggsave("05_outputs/for_export/fig_3_start_quit.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))


# 20. FIGURE A4: Starting and quitting women and men, baseline plot including controls (Robustness check) ####
startgg_r <- read_excel("03_data/stata_datasets/pp_baseline_start_r.xlsx")
startgg_r <- startgg_r %>%
  mutate(Organization = case_when(Organization == "VOM" ~ "men-dominated",
                                  Organization == "VOE" ~ "gender-integrated",
                                  Organization == "VOW" ~ "women-dominated"),
         Organization = fct_relevel(Organization, "men-dominated", "gender-integrated", "women-dominated"))
startplot_r <- ggplot(startgg_r, aes(fill=Gender, y=b, x=Organization)) + #final ggplot
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  xlab("Gender composition of associational context") +
  ylab("Predicted probability of starting") +
  geom_errorbar(aes(ymin=ll, ymax=ul), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.18), expand = c(0.01,0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
startplot_r


quitgg_r <- read_excel("03_data/stata_datasets/quit_r.xlsx")
quitgg_r <- quitgg_r %>%
  mutate(Organization = case_when(Organization == "VOM" ~ "men-dominated",
                                  Organization == "VOE" ~ "gender-integrated",
                                  Organization == "VOW" ~ "women-dominated"),
         Organization = fct_relevel(Organization, "men-dominated", "gender-integrated", "women-dominated"))
quitplot_r <- ggplot(quitgg_r, aes(fill=Gender, y=b, x=Organization)) + #final ggplot
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  xlab("Gender composition of associational context") +
  ylab("Predicted probability of quitting") +
  geom_errorbar(aes(ymin=ll, ymax=ul), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6), expand = c(0.01,0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
quitplot_r

fig3_r <- wrap_plots(list(A= startplot_r, B=quitplot_r, C=legend_sqplot), heights = c(0.9, 0.1), design = design_sqplot)
fig3_r
ggsave("05_outputs/for_export/fig_A4_robstartquit.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))


# 21. FIGURE 4: Gender Norms-Probability differentials plot ####
# Import Predicted probability differentials for men and women across quartiles of gender norms index (models without controls)
gn_men <- read_excel("03_data/stata_datasets/gendn_index_men.xlsx")
gn_men <- gn_men %>%
  mutate(`Gender norms` = c("Very egalitarian", "Egalitarian", "Traditional", "Very traditional"),
         `Gender norms` = fct_relevel(`Gender norms`, "Very egalitarian", "Egalitarian", 
                                      "Traditional", "Very traditional"),
         Gender = "Men")

gn_women <- read_excel("03_data/stata_datasets/gendn_index_women.xlsx")
gn_women <- gn_women %>%
  mutate(`Gender norms` = c("Very egalitarian", "Egalitarian", "Traditional", "Very traditional"),
         `Gender norms` = fct_relevel(`Gender norms`, "Very egalitarian", "Egalitarian", 
                                      "Traditional", "Very traditional"),
         Gender = "Women")

gn_all <- rbind(gn_men, gn_women)

fig_gn_all <- ggplot(gn_all) +
  geom_bar(aes(x = `Gender norms`, y = coef), stat = "identity") +
  geom_errorbar(aes(x = `Gender norms`, ymin = lci, ymax = hci), width = 0.4) +
  ylab("Predicted probability differential (joining men-dominated 
       vs. women-dominated associational context)        ") +
  xlab("Gender norms") +
  facet_wrap("Gender") +
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  theme_bw() + scale_fill_grey() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12)) 

fig_gn_all
ggsave("05_outputs/for_export/fig_4_gn.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))

# 22. FIGURE A5: Gender Norms-Probability differentials plot including controls (Robustness Check): ####
# Import Predicted probability differentials for men and women across quartiles of gender norms index (models with controls)
gn_men_r <- read_excel("03_data/stata_datasets/gendn_index_men_r.xlsx")
gn_men_r <- gn_men_r %>%
  mutate(`Gender norms` = c("Very egalitarian", "Egalitarian", "Traditional", "Very traditional"),
         `Gender norms` = fct_relevel(`Gender norms`, "Very egalitarian", "Egalitarian", 
                                      "Traditional", "Very traditional"),
         Gender = "Men")

gn_women_r <- read_excel("03_data/stata_datasets/gendn_index_women_r.xlsx")
gn_women_r <- gn_women_r %>%
  mutate(`Gender norms` = c("Very egalitarian", "Egalitarian", "Traditional", "Very traditional"),
         `Gender norms` = fct_relevel(`Gender norms`, "Very egalitarian", "Egalitarian", 
                                      "Traditional", "Very traditional"),
         Gender = "Women")

gn_all_r <- rbind(gn_men_r, gn_women_r)

fig_gn_all_r <- ggplot(gn_all_r) +
  geom_bar(aes(x = `Gender norms`, y = coef), stat = "identity") +
  geom_errorbar(aes(x = `Gender norms`, ymin = lci, ymax = hci), width = 0.4) +
  ylab("Predicted probability differential (joining men-dominated 
       vs. women-dominated associational context)        ") +
  xlab("Gender norms") +
  facet_wrap("Gender") +
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  theme_bw() + scale_fill_grey() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12)) 

fig_gn_all_r
ggsave("05_outputs/for_export/fig_A5_robustgn.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))


# 23. FIGURE A1 & A2 Re-do analysis with cross-sectional involvement at t6: #########################
invgg <- read_excel("03_data/stata_datasets/pp_baseline_cross_sect.xlsx")
invgg <- invgg %>%
  mutate(Organization = case_when(Organization == "VOM" ~ "men-dominated",
                                  Organization == "VOE" ~ "gender-integrated",
                                  Organization == "VOW" ~ "women-dominated"),
         Organization = fct_relevel(Organization, "men-dominated", "gender-integrated", "women-dominated"))
invplot <- ggplot(invgg, aes(fill=Gender, y=b, x=Organization)) + #final ggplot
  geom_bar(position = "dodge", stat = "identity", width = 0.6) +
  xlab("Gender composition of associational context") +
  ylab("Predicted probability of involvement") +
  geom_errorbar(aes(ymin=ll, ymax=ul), position = position_dodge(width = 0.6), width = 0.2) +
  theme_bw() + scale_fill_grey() + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.32), expand = c(0.01,0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.text = element_text(size = 12)) 
invplot
design_invplot <- "A
                  C"
invplot_final <- wrap_plots(list(A= invplot, C=legend_sqplot), heights = c(0.9, 0.1), design = design_invplot)
invplot_final
ggsave("05_outputs/for_export/fig_A1_cross_sect_involvement.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))

gn_men_cs <- read_excel("03_data/stata_datasets/gendn_index_men_cs.xlsx")
gn_men_cs <- gn_men_cs %>%
  mutate(`Gender norms` = c("Very egalitarian", "Egalitarian", "Traditional", "Very traditional"),
         `Gender norms` = fct_relevel(`Gender norms`, "Very egalitarian", "Egalitarian", 
                                      "Traditional", "Very traditional"),
         Gender = "Men")

gn_women_cs <- read_excel("03_data/stata_datasets/gendn_index_women_cs.xlsx")
gn_women_cs <- gn_women_cs %>%
  mutate(`Gender norms` = c("Very egalitarian", "Egalitarian", "Traditional", "Very traditional"),
         `Gender norms` = fct_relevel(`Gender norms`, "Very egalitarian", "Egalitarian", 
                                      "Traditional", "Very traditional"),
         Gender = "Women")

gn_all_cs <- rbind(gn_men_cs, gn_women_cs)

fig_gn_all_cs <- ggplot(gn_all_cs) +
  geom_bar(aes(x = `Gender norms`, y = coef), stat = "identity") +
  geom_errorbar(aes(x = `Gender norms`, ymin = lci, ymax = hci), width = 0.4) +
  ylab("Predicted probability differential (involvement in men-dominated 
       vs. women-dominated associational context)        ") +
  xlab("Gender norms") +
  facet_wrap("Gender") +
  scale_y_continuous(breaks = c(-0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30, 0.35)) +
  theme_bw() + scale_fill_grey() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(margin = margin(t=20), size = 12),
        axis.text = element_text(size = 12)) 

fig_gn_all_cs
ggsave("05_outputs/for_export/fig_A2_gn_cs.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))

