#################################################
# Joao Souto-Maior, June 2021

check_nested_structure <- function(y_var, x_var, ID_j, ID_i, data){
  set.seed(123)
  # function describing within group correlations
  # commands follow lecture 1 from Marc Scott's nested data course at NYU
  attach(data)
  x <- matrix(unlist(tapply(y_var, ID_j,
                            sample,
                            size = 2)), 
              ncol = 2,
              byrow = T)
  r1 <- paste0("Take a random draw of pairs of subjects within each group (repeating for all groups) and correlate the outcomes. If these have a significant correlation, then they are dependent. Correlation = ", round(cor(x)[1, 2], 3))
  p1 <- data %>%
    ggplot(aes(x = x_var, 
               y = y_var)) +
    labs(title = "Naive OLS") + 
    geom_point() + 
    geom_smooth(formula = y ~ x,
                method = "lm", 
                se = FALSE, 
                col = 2)
  p2 <- dat1 %>%
    ggplot(aes(x = reorder(ID_j, y_var, 
                         FUN = median), 
             y = y_var)) +
    geom_boxplot() +
    labs(title = "Distribution of outcome variable within group") 
  
  x.df <- data.frame(id = row(x)[, 1], pairElement1 = x[, 1], pairElement2 = x[,2])
  p3 <- ggplot(x.df, 
               aes(x = pairElement1, 
                   y = pairElement2, 
                   col = id)) + 
    geom_point() +
    scale_color_gradientn(colours = rainbow(length(x.df))) +
    labs(title = "Correlation of outcome for random pairs within schools") 
  
  p4 <- data %>%
    ggplot(aes(y = y_var, x = x_var)) +
    geom_smooth(formula = y ~ x,
                method = "lm", 
                se = FALSE, 
                col = 2) + 
    geom_point() + 
    labs(title = "Response surface for group ID") +
    facet_wrap(~as.factor(ID_j),
               nrow = 3)
  
  list(r1, p1, p2, p3, p4)
}
