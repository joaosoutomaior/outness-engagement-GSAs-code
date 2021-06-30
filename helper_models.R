#################################################
# Joao Souto-Maior, June 2021

library(lme4)
library(sandwich)
library(lmtest)
library(broom.mixed)

##################### OLS with corrected SEs

ols_model1 <- function(y, x, list, dat_mids){
  formula_1 <- as.formula(paste0(y, "~", x, " + ",
                                 paste(vars_controls_2, collapse = " + ")))
  m1 <- lapply(list, function(x){
    m <- lm(formula = formula_1, data = x)
    coeftest(m, 
             vcov = vcovHC(m, type="HC0"), 
             save = TRUE)
  })
}

ols_model2 <- function(y, x, dat){
  formula_2 <- as.formula(paste0(y, "~", x, "+ GSA.engagement*", x, " + ", 
                                 paste(c(vars_controls_2), collapse = " + ")))
  m2 <- lapply(list, function(x){
    m <- lm(formula = formula_2, data = x)
    coeftest(m, 
             vcov = vcovHC(m, type="HC0"), 
             save = TRUE)
  })
}

ols_model3 <- function(y, x, dat){
  formula_3 <- as.formula(paste0(y, "~",x, "+ GSA.engagement*", x, " + ", 
                                 paste(c(vars_controls_2, var_mechanisms), collapse = " + ")))
  m3 <- lapply(list, function(x){
    m <- lm(formula = formula_3, data = x)
    coeftest(m, 
             vcov = vcovHC(m, type="HC0"), 
             save = TRUE)
  })
}

##################### MLM

mlm_model1 <- function(y, x, list, dat_mids){
  require(lmerTest)
  formula_1 <- as.formula(paste0(y, "~", x, " + ",
                                 paste(vars_controls_2, "+ (1|ID_school)", collapse = " + ")))
  m1 <- lapply(list, function(x){
    m <- lmer(formula = formula_1, data = x)
  })
}

mlm_model2 <- function(y, x, dat){
  require(lmerTest)
  formula_2 <- as.formula(paste0(y, "~", x, "+ GSA.engagement*", x, " + ", 
                                 paste(c(vars_controls_2), "+ (1|ID_school)", collapse = " + ")))
  lapply(list, function(x){
    m <- lmer(formula = formula_2, data = x)
  })
}

mlm_model3 <- function(y, x, dat){
  require(lmerTest)
  formula_3 <- as.formula(paste0(y, "~",x, "+ GSA.engagement*", x, " + ", 
                                 paste(c(vars_controls_2, var_mechanisms), "+ (1|ID_school)", collapse = " + ")))
  m3 <- lapply(list, function(x){
    m <- lmer(formula = formula_2, data = x)
  })
}