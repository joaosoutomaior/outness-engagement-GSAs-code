################### Heading ##################
# JSM, June 2021

##################### packages
library(foreign)
library(sjPlot)
library(corrplot)
library(psych)
library(ltm)
library(kableExtra)
library(texreg)
library(tidyverse)
library(expss)
library(mice)
library(lavaan)

##################### some functions
freq_table <- function(x){
  cnt <- table(x, useNA = "always")
  tbl <- cbind(cnt, paste0("(",round(prop.table(cnt)*100,2), "%)"))
  colnames(tbl) <- c('Count','Percentage')
  tbl
}

mean_of_data <- function(dataset){
  dataset %>% 
    gather(Variable, value) %>%
    group_by(Variable) %>%
    summarise(Mean = mean(value, na.rm = T)) %>%
    mutate(Mean = round(Mean, 1)) %>%
    pivot_wider(names_from = Variable, values_from = Mean)
}

summary_of_data <- function(dataset){
  dataset %>% 
    gather(Variable, value) %>%
    group_by(Variable) %>%
    summarise(Mean = mean(value, na.rm = T),
              S.d. = sd(value, na.rm = T),
              Min. = min(value, na.rm = T),
              Max. = max(value, na.rm = T),
              pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = paste0(round(100 * pct_missing, 2), "%"),
           Mean = round(Mean, 2),
           S.d. = round(S.d., 2),
           Max. = round(Max., 2),
           Min. = round(Min., 2)) %>%
    rename("% Missing" = pct_missing)
}

pct_missing_variable <- function(dataset){
  dataset %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = paste0(round(100 * pct_missing, 2), "%"))
}

summary_levels <- function(x) {
  idCt <- table(x)  #counts of unique ids
  return(list(UniqGroups=length(idCt),
              minPerGroup=min(idCt),
              meanPerGroup=mean(idCt), 
              maxPerGroup=max(idCt)))
}

summary_cat <- function(x){
  sjPlot::view_df(x, 
                  weight.by = NULL, 
                  alternate.rows = F, 
                  show.id = F,
                  show.type = F, 
                  show.values = F, 
                  show.string.values = F, 
                  show.labels = T, 
                  show.frq = F, 
                  show.prc = T, 
                  show.wtd.prc = F, 
                  show.na = T,
                  max.len = 15, 
                  sort.by.name = T)
}

get_name <- function(x) {
  deparse(substitute(x))
}
