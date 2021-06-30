#################################################
# Joao Souto-Maior, June 2021

################## set up
rm(list=ls())
setwd("~/Research projects/outness-engagement-GSAs/outness-engagement-GSAs-code")
source("helper_heading.R")

################## load data (originally in .sav)
gsa_dat <- read.spss("~/Research data/GSA_BC/GSA_T1Y1_08-04-20_updated.sav", 
                     use.value.labels = TRUE, 
                     to.data.frame = FALSE,
                     max.value.labels = Inf)
dat <- as_tibble(gsa_dat)

################## Note
# There are two typos in the original file. 
# 1. IDT1 1809 should be 1810. And variable name "bisexual" is misspelled with a double "e" = "biseexual".
# 2. COMPUTE NegAffT1 = MEAN(affect1, affect, affect4, affect6, affect9). Needs to define number of second affect variable.
# make sure to fix these issues in the file to reproduce results.

# check for duplicated IDs.
x <- duplicated(dat$IDT1)
any(x == TRUE)
# no duplicates (issued fixed in ID 1809 - transformed to 1810).

################## select variables of interest
dat_original <- dat
dat <- dat %>%
  select(school,
         IDT1, 
         BehavEngT1,
         EmotEngT1,
         ################### raw survey responses ################
         #classengage15, classengage16, classengage17, classengage18, classengage19,
         #classengage11, classengage12, classengage13, classengage14,
         classengage6, classengage7, classengage8, classengage9, classengage10,
         classengage1, classengage2, classengage3, classengage4 , classengage5,
         globalworth1, globalworth2, globalworth3r, globalworth4r, globalworth5r,
         involve1, involve2, involve3, involve4, involve5, involve6,  involve7,
         peersens1, peersens2, peersens3, peersens4, peersens5, peersens6, peersens7,
         engage1, engage2, engage3, engage4, engage5,
         victim1, victim2, victim3, victim4, victim5,
         lgbvictim1, lgbvictim2, lgbvictim3, lgbvictim4, lgbvictim5,
         belonging1, belonging2, belonging3, belonging4, belonging5, belonging6, belonging7, 
         belonging8, belonging9, belonging10,
         ################### raw measures of outness #############
         sorientout1, sorientout2, sorientout3, sorientout4, 
         sorientout5, sorientout6, sorientout7,
         genderout1, genderout2, genderout3, genderout4, 
         genderout5, genderout6, genderout7,
         ################### measures of sex/gen outness #########
         SexualMinority, GenderMinority, 
         ################### GSA engagement ######################
         EngageT1,# measures of GSA engagement
         ################### controls ############################
         REMinority, sex, parenteducation, memberyr, grade, age,
         ################### potential mechanisms ################
         GlobalWorthT1,SocSupT1,LGBVictimT1, VictimT1,PeerSensT1,BelongingT1) 

# rename variables
dat <- dat %>%
  rename(ID_school = school,
         ID_student = IDT1,
         behavioral.engagement = BehavEngT1,
         emotional.engagement = EmotEngT1,
         ################### raw measures of outness #########
         SexOut.parents = sorientout1, 
         SexOut.siblings = sorientout2, 
         SexOut.GSA.peers = sorientout4, 
         SexOut.ext.family = sorientout3, 
         SexOut.school.peers = sorientout5, 
         SexOut.school.adults = sorientout6, 
         SexOut.others = sorientout7,
         GenOut.parents = genderout1, 
         GenOut.siblings = genderout2, 
         GenOut.ext.family = genderout3, 
         GenOut.GSA.peers = genderout4, 
         GenOut.school.peers = genderout5, 
         GenOut.school.adults = genderout6, 
         GenOut.others = genderout7,
         ################### measures of sex/gen #############
         sexual.minority = SexualMinority, 
         gender.minority = GenderMinority, 
         ################### GSA engagement ##################
         GSA.engagement = EngageT1, 
         ################### general controls ################
         race.non.white = REMinority, 
         sex.female  = sex, 
         parental.ed = parenteducation, 
         GSA.membership = memberyr, 
         grade = grade, 
         age = age,
         ################### potential mechanisms ############
         global.worth = GlobalWorthT1,
         social.support = SocSupT1,
         LGBT.victimization = LGBVictimT1, 
         victimization = VictimT1,
         peer.sensitivity = PeerSensT1,
         GSA.belonging = BelongingT1,
         ################### selected survey items
         item.1.classroom = classengage1,
         item.2.classroom = classengage2,
         item.3.classroom = classengage3,
         item.4.classroom = classengage4,
         item.5.classroom = classengage5,
         item.6.classroom = classengage6,
         item.7.classroom = classengage7,
         item.8.classroom = classengage8,
         item.9.classroom = classengage9,
         item.10.classroom = classengage10,
         #"item.11" = classengage11,
         #"item.12" = classengage12,
         #"item.13" = classengage13,
         #"item.14" = classengage14,
         #"item.15" = classengage15,
         #"item.16" = classengage16,
         #"item.17" = classengage17,
         #"item.18" = classengage18,
         #"item.19" = classengage19,
         item.1.GW = globalworth1, 
         item.2.GW = globalworth2, 
         item.3.GW = globalworth3r, 
         item.4.GW = globalworth4r, 
         item.5.GW = globalworth5r,
         item.1.involvement = involve1, 
         item.2.involvement = involve2, 
         item.3.involvement = involve3, 
         item.4.involvement = involve4, 
         item.5.involvement = involve5, 
         item.6.involvement = involve6,  
         item.7.involvement = involve7,
         item.1.PS = peersens1, 
         item.2.PS = peersens2, 
         item.3.PS = peersens3, 
         item.4.PS = peersens4, 
         item.5.PS = peersens5, 
         item.6.PS = peersens6, 
         item.7.PS = peersens7,
         item.1.engagement = engage1, 
         item.2.engagement = engage2, 
         item.3.engagement = engage3, 
         item.4.engagement = engage4, 
         item.5.engagement = engage5,
         item.1.victimization = victim1, 
         item.2.victimization = victim2, 
         item.3.victimization = victim3, 
         item.4.victimization = victim4, 
         item.5.victimization = victim5,
         item.1.victimization.LGBT = lgbvictim1, 
         item.2.victimization.LGBT = lgbvictim2, 
         item.3.victimization.LGBT = lgbvictim3, 
         item.4.victimization.LGBT = lgbvictim4, 
         item.5.victimization.LGBT = lgbvictim5,
         item.1.belonging = belonging1, 
         item.2.belonging = belonging2, 
         item.3.belonging = belonging3, 
         item.4.belonging = belonging4, 
         item.5.belonging = belonging5, 
         item.6.belonging = belonging6, 
         item.7.belonging = belonging7, 
         item.8.belonging = belonging8, 
         item.9.belonging = belonging9, 
         item.10.belonging = belonging10)

# name relevant variable groups
source("helper_var-groups.R")

################## checking for issues in categorical variables
dat_categorical <- dat %>%
  select(all_of(vars_categorical))
lapply(dat_categorical, freq_table)

# adjust variables
dat$parental.ed[dat$parental.ed == 77 | 
                      dat$parental.ed == 4.5] <- NA

dat$SexOut.parents[dat$SexOut.parents == 1.5] <- NA
dat$SexOut.school.adults[dat$SexOut.school.adults == 2.5] <- NA
dat$SexOut.school.peers[dat$SexOut.school.peers == 2.5] <- NA

################## Check for issues in numerical vars
table_vars <- dat %>%
  select(all_of(vars_numerical),
         -ID_school,
         -ID_student)
tab <- summary_of_data(table_vars)
tab

# fix some data input issues
dat$GSA.membership[dat$GSA.membership == 88] <- NA
dat$grade[dat$grade == 88] <- NA

# check
table_vars <- dat %>%
  select(all_of(vars_numerical),
         -ID_school,
         -ID_student)
tab <- summary_of_data(table_vars)
tab

################## Adjust survey items
# this need to be done in more detail if we are asked to
# report the items behind all composite variables.

vars <- dat %>%
  select(starts_with("item"))
lapply(vars, summary)

items_1_4 <- dat %>%
  select(starts_with("item") & (contains("GW") | 
                                  contains("classroom"))) %>%
  names()

items_0_4 <- dat %>%
  select(starts_with("item") & (contains("involvement") | 
                                  contains("engagement") |
                                  contains("victimization"))) %>%
  names()

items_1_5 <- dat %>%
  select(starts_with("item") & (contains("belonging") | 
                                  contains("PS"))) %>%
  names()

# create factors
dat <- dat %>%
  mutate(across(.cols = all_of(c(items_1_4)),
                .fns = ~factor(x = .,
                               levels = c(1, 2, 3, 4),
                               labels = c( "1-Not true at all",
                                           "2-Somewhat false",
                                           "3-Somewhat true",
                                           "4-Very true"),
                               ordered = TRUE))) %>%
  mutate(across(.cols = all_of(c(items_0_4)),
                .fns = ~factor(x = .,
                               levels = c(0, 1, 2, 3, 4),
                               labels = c( "0-Very low",
                                           "1-Low",
                                           "2-Mid",
                                           "3-High",
                                           "4-Very high"),
                               ordered = TRUE))) %>%
  mutate(across(.cols = all_of(c(items_1_5)),
                .fns = ~factor(x = .,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c( "1-Very low",
                                           "2-Low",
                                           "3-Mid",
                                           "4-High",
                                           "5-Very high"),
                               ordered = TRUE))) %>%
  mutate(across(.cols = all_of(c(vars_raw_gen_out,
                                 vars_raw_sex_out)),
                .fns = ~factor(x = .,
                               levels = c(1, 2, 3, 4),
                               labels = c( "1-Definitely not",
                                           "2-Might",
                                           "3-Probably",
                                           "4-Definitely"),
                               ordered = TRUE))) %>%
  mutate(across(.cols = all_of(c("race.non.white",
                               "gender.minority",
                               "sexual.minority")),
                .fns = ~factor(x = .,
                               levels = c(0, 1),
                               labels = c( "No",
                                           "Yes"),
                               ordered = TRUE))) %>%
  mutate(parental.ed = factor(x = parental.ed,
                               levels = c(1, 2, 3, 4, 5, 6, 7),
                               labels = c("No high school",
                                          "Some high school",
                                          "High school degree",
                                          "Some college",
                                          "Associate's degree",
                                          "Bachelor's degree",
                                          "Advanced/graduate degree"),
                               ordered = TRUE))

# check
vars <- dat %>%
  select(starts_with("item"))
lapply(vars, summary)

################### var labels (if needed...)
# e.g.
# data = expss::apply_labels(dat, parental.ed = "Parental education")

