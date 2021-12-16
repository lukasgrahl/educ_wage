library(tidyr)
library(tidyverse)
library(modelsummary)
library(flextable)
library(sandwich)

filter_df <- function(df){
  ## FILTERING
  x <- nrow(df)
  
  # civilians
  df <- df[df$popstat == 1, ]
  
  # labour force
  df <- df[df$labforce == 2, ]
  
  # in education exclude
  df <- df[df$age > 20, ]
  # df <- df[df$schlcoll == 5, ]
  
  # part time work 
  df <- df[df$fullpart == 1, ]
  
  # no wage
  df <- df[df$incwage > 0, ]
  
  # lenght
  print(sprintf("Data now has %f of org size", round((x - nrow(df)) / x, 2)))
  
  return(df)
}

asstype_df <- function(df){
  
  df$age <- as.numeric(df$age)
  df$occ <- as.factor((df$occ))
  df$educ <- as.factor(df$educ)
  
  return(df)
}

is_outlier <- function(arr, method = "iqr"){
  mean <- median(arr)
  iqr <- quantile(arr, 0.75) - quantile(arr, 0.25)
  up_bound <- mean + 2 * iqr
  low_bound <- mean - 2 * iqr
  
  return (arr > up_bound | arr < low_bound)
}

featureeng_df <- function(df){
  
  # Education dummy encode
  df$is_master <- df$educ == 123
  df$is_bachel <- df$educ == 111
  df$is_phd <- df$educ == 125
  df$is_higsch <- (df$educ == 81 | df$educ == 73)
  df$is_bachel_cum <- (df$is_master == TRUE | df$is_bachel == TRUE)
  
  # Wage log
  df$log_wage <- log(df$incwage)
  
  # singel parent
  df$sing_par <- c(
    (df$marst == 4 | df$marst == 5 | df$marst == 3) &
      (df$nchild > 0))
  
  # has children
  df$has_children <- (df$nchild > 0)
  df$has_many_child <- (df$nchild > 3)
  
  # immigrant first or second gen
  df$immig_first <- df$nativity == 5 & df$yrimmig > 0
  df$immig_secn <- df$nativity == 4 | df$nativity == 3 |df$nativity == 2
  
  # race dummy
  df$is_hispan <- df$hispan != 0 & (df$hispan != 902 | df$hispan != 903)
  df$is_asian <- (df$race == 650 | df$race == 651)
  df$is_negro <- (df$race == 200 | df$race == 801)
  df$is_nativamec <- df$race == 300
  
  # dummy firmsize
  x <- fastDummies::dummy_cols(df$firmsize, remove_first_dummy = TRUE)
  x <- x[, c(2:ncol(x))]
  x <- x == 1
  colnames(x) <- c("firmsize4", "firmsize6", "firmsize7", "firmsize8", "firmsize9")
  df <- cbind(df, x)
  df$is_firm_large <- (df$firmsize8 == TRUE | df$firmsize9 == TRUE | df$firmsize7 == TRUE)
  
  # dummy pension
  df$pension_inc <- df$pension == 3 # included in pension
  df$pension_exl <- df$pension == 2 # pension exists but not incl

  # dummy union
  df$union_mem <- df$union == 2 # union member
  df$union_mem_incl <- df$union == 3 # not member but coverage

  # dummy health
  df$is_poorheal <- df$health == 5 # poor health

  # dummy disabled
  df$is_disabled <- df$incdisab > 0 # is disabled

  # sex
  df$male <- df$sex == 1

  # paid by hour
  df$paidhour[df$paidhour == 0] <- NA
  df$is_paidhour <- df$paidhour == 2

  # citizen
  df$is_citizen <- df$citizen != 5
  
  return(df)
}

dropp_zero_cols <- function(df){
  
  x <- data.frame(sapply(df, sum))
  colnames(x) <- c("Sum")
  x <- c(rownames(filter(x, Sum == 0)))
  print(sprintf("Dropping %s cols", x))
  df <- df[ , !(names(df) %in% x)]
  
  return(df)
}

corr_matrix <- function(df, thresh = 0.3){
  
  x <- abs(cor(df)) > thresh
  x <- x * 1
  x[is.na(x)] = 0
  heatmap(x)
}

get_outputtab <- function(model_list, coefficient_names, to_ppt = FALSE){
  
  globalstats_names <- list(
    list("raw" = "nobs", "clean" = "No. of observations", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 4),
    list("raw" = "adj.r.squared", "clean" = "Adjusted R²", "fmt" = 4)
  )
  
  tab <- modelsummary(
    ## This refers to the list 'models' specified above.
    model_list,
    ## A technical adjustment referring to the method used to estimate standard errors
    vcov = "stata",
    ## Include asterisks to indicate level of significance
    stars = TRUE,
    ## This refers to the list 'coefficient_names' specified above.
    coef_map = coefficient_names,
    ## This refers to the list 'globalstats_names' specified above.
    gof_map = globalstats_names,
    ## This specifies which R-package is used to produce the table.
    output = 'flextable'
  )
  
  if(to_ppt == TRUE){
    print(autofit(tab), preview = "pptx")
  }
  
  return(tab)
}

plot_inc_dist <- function(df, sex_val){
  
  if(sex_val == 1){
    x <- "male"
  }else if(sex_val == 2){
    x <- "female"
  }
  
  inc_plot = 
    ggplot() +
    geom_vline(mapping = 
                 aes(xintercept = 
                       mean(df[df[, "sex"] == sex_val & df[, "is_higsch"] == TRUE, ]$incwage),
                     colour = "red")) +
    geom_density(data = df[df[, "sex"] == sex_val & df[, "is_higsch"] == "TRUE", ],
                 mapping = aes(x = incwage),
                 colour = "red",
                 fill = "red",
                 size = .5,
                 alpha = 0.3) +
    geom_vline(mapping = 
                 aes(xintercept = 
                       mean(df[df[, "sex"] == sex_val & df[, "is_bachel"] == TRUE, ]$incwage),
                     colour = "green")) +
    geom_density(data = df[df[, "sex"] == sex_val & df[, "is_bachel"] == "TRUE", ],
                 mapping = aes(x = incwage),
                 colour = "green",
                 fill = "green",
                 size = .5,
                 alpha = 0.3) +
    geom_vline(mapping = 
                 aes(xintercept = 
                       mean(df[df[, "sex"] == sex_val & df[, "is_master"] == TRUE, ]$incwage),
                     colour = "blue")) +
    geom_density(data = df[df[, "sex"] == sex_val & df[, "is_master"] == "TRUE", ],
                 mapping = aes(x = incwage),
                 colour = "blue",
                 fill = "blue",
                 size = .5,
                 alpha = 0.3) +
    scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
    scale_colour_manual(name = 'Mean Wage Income', 
                        values =c('red' = 'red',
                                  'green' = 'green',
                                  'blue'='blue'),
                        labels = c('highschool', 'bachelor', 'master')) +
    ggtitle(sprintf("Wage Income distribution of %s across educational levels", x)) +
    theme(plot.title = element_text(size = 11, face = "bold")) +
    theme_minimal()
  
  return(inc_plot)
}

  
lm_cols <- c(
    # wage
    "log_wage",
    
    # educ
    "is_master", 
    "is_bachel", 
    "is_higsch",
    "is_phd",
    "is_bachel_cum",
    
    # age
    "age",
    
    # sex
    "male",
    
    # famsize
    "famsize",
    
    # singel_parent
    "sing_par",
    # has children
    "has_children",
    "has_many_child",
    "nchild",
    
    # immigration
    "immig_first",
    "immig_secn",
    "citizen",
    # race
    "is_hispan",
    "is_negro",
    "is_asian",
    "is_nativamec",
    # firm size
    "firmsize4",
    "firmsize6", 
    "firmsize7",
    "firmsize8",
    "firmsize9",
    "is_firm_large",
    # dummy pension 
    "pension_inc",
    "pension_exl",
    # union
    "union_mem",
    "union_mem_incl",
    # health
    "is_poorheal",
    # disabled
    "is_disabled",
    # paid by hour
    "is_paidhour"
  )


