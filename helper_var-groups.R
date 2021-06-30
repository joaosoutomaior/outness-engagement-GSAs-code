#################################################
# Joao Souto-Maior, June 2021

vars_raw_gen_out <- dat %>%
  select(starts_with("GenOut")) %>%
  names()

vars_raw_sex_out <- dat %>%
  select(starts_with("SexOut")) %>%
  names()

vars_raw_eng <- dat %>%
  select(starts_with("item"),
         contains("classroom")) %>%
  names()

vars_controls <- dat %>%
  select(GSA.engagement,
         race.non.white, 
         sex.female, 
         parental.ed,
         GSA.membership,
         grade, 
         age) %>%
  names()

vars_controls_2 <- dat %>%
  select(GSA.engagement,
         race.non.white, 
         sex.female,
         parental.ed,
         grade, 
         age) %>%
  names()

vars_omit <- dat %>%
  select(race.non.white, 
         sex.female,
         parental.ed,
         grade, 
         age) %>%
  names()

var_mechanisms <- dat %>%
  select(global.worth,
         social.support,
         LGBT.victimization, 
         victimization,
         peer.sensitivity,
         GSA.belonging) %>%
  names()

vars_categorical <- dat %>%
  select(gender.minority,
         sexual.minority,
         starts_with("GenOut"),
         starts_with("SexOut"),
         starts_with("item"),
         race.non.white,
         sex.female,
         parental.ed) %>%
  names()

vars_numerical <- dat %>%
  select(-all_of(c(vars_categorical))) %>%
  names()
