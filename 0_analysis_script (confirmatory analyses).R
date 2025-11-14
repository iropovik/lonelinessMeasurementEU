#' ---
#' title: "Evaluating Loneliness Measurements across the European Union"
#' author: "Ivan Ropovik"
#' date: "`r Sys.Date()`"
#' output:
#'    html_document:
#'       toc: true
#'       toc_float: true
#'       code_folding: show
#'       fig_retina: 2
#' always_allow_html: yes
#' ---
#+ setup, include=FALSE
knitr::opts_chunk$set(echo=FALSE, warning = FALSE, fig.width = 10, fig.height = 10)

#' **This is the analytic output for the exploratory phase**
#' 

# Libraries and script sourcing -------------------------------------------
# Load libraries (and install if not installed already)
list.of.packages <- c("readr", "lubridate", "tidyverse", "psych", "EFA.dimensions", "lavaan", "openxlsx", "semPlot", "semTools", "magrittr", "ggplot2", "patchwork")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load required libraries
#+ include = FALSE
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
#devtools::install_github("KimDeRoover/mixmgfa")
library(mixmgfa)
source("0_BF01snippet.R") #code for the bayesian analyses
set.seed(1)

# Function that rounds numeric values in a data frame
roundDf <- function(df) {
  as.data.frame(lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x))
}
# Data --------------------------------------------------------------------
# Change the value to "confirmatory_data.csv" to run the analyses on the confirmatory dataset
filename <- "confirmatory_data.csv"
data <- read_csv(filename)

# Data wrangling ----------------------------------------------------------
# Recoding of the country variable
countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden"
)
data$country_chr <- data$country

# Variable names
variables <- c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f", "loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c", "social_support_a", "social_support_b", "social_support_c", "social_support_d", "health_general", "feelings_depr", "feelings_happy", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "neighbours", "social_activities_a")

# Turning 997 (not applicable), 998 (don't know), and 999 (prefer not to say) into NAs
for (variable in c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f", "loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c", "social_support_a", "social_support_b", "social_support_c", "social_support_d", "health_general", "feelings_depr", "feelings_happy", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "neighbours", "social_activities_a", "loneliness_direct")){
  data[[variable]] <- ifelse(data[[variable]] == "Prefer not to say", NA, data[[variable]])
  data[[variable]] <- ifelse(data[[variable]] == "Don’t know", NA, data[[variable]])
  data[[variable]] <- ifelse(data[[variable]] == "Not applicable", NA, data[[variable]])
}

# Custom recoding function with explicit mappings for each variable group
recode_variable <- function(x, name) {
  # Define the mappings for each group of variables
  mappings <- list(
    "loneliness_djg" = c("No" = 0, "More or less" = 1, "Yes" = 2),
    "loneliness_ucla" = c("Hardly ever or never" = 0, "Some of the time" = 1, "Often" = 2),
    "social_support" = c("None of the time" = 1, "A little of the time" = 2, "Some of the time" = 3, "Most of the time" = 4, "All of the time" = 5),
    "health_general" = c("Very poor" = 1, "Fairly poor" = 2, "Average" = 3, "Fairly good" = 4, "Very good" = 5),
    "meet" = c("Never" = 0, "Every two months or less frequently" = 1, "Once a month" = 2, "Every two weeks" = 3, "Every week" = 4, "More than once a week" = 5, "Daily" = 6),
    "social_activities_a" = c("Never" = 0, "Every two months or less frequently" = 1, "Once a month" = 2, "Every two weeks" = 3, "Every week" = 4, "More than once a week" = 5, "Daily" = 6),
    "neighbours" = c("Never or hardly ever" = 0, "Less than once a month" = 1, "1-3 times a month" = 2, "About once a week" = 3, "Several times a week" = 4, "Almost every day" = 5),
    "feelings" = c("Never" = 0, "Very rarely" = 1, "Rarely" = 2, "Occasionally" = 3, "Very frequently" = 4, "Always" = 5),
    "loneliness_direct" = c("None of the time" = 0, "A little of the time" = 1, "Some of the time" = 2, "Most of the time" = 3, "All of the time" = 4)
  )
  
  # Determine the correct mapping based on the variable name
  for (prefix in names(mappings)) {
    if (grepl(prefix, name)) {
      map <- mappings[[prefix]]
      if(is.character(x)) x <- factor(x, levels = names(map))
      return(as.numeric(map[as.character(x)]))
    }
  }
  
  # Return original if no mapping found
  return(x)
}

# Apply the recoding function to each column in 'data'
names(data) <- make.names(names(data)) # Ensure valid column names
for (name in names(data)) {
  data[[name]] <- recode_variable(data[[name]], name)
}

# Recoding of variables so that higher scores indicate greater loneliness (DJGLS-6)
for(variable in c("loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f")){
  data[[variable]] <- ifelse(is.na(data[[variable]]), NA, 2 - data[[variable]])
}

#'#### Missing data proportion
#'
# Missing data ------------------------------------------------------------
#+ include = TRUE
lonelinessDataEF <- data[grep("ucla|djg|direct", names(data), ignore.case = TRUE)]
# Calculate and print the percentage of NA values in the subset
round((sum(is.na(lonelinessDataEF)) / (nrow(lonelinessDataEF) * ncol(lonelinessDataEF))) * 100, 2)

#'# Descriptive statistics 
#'
# Table 1 -----------------------------------------------------------------
# Calculate age
data$age <- year(Sys.Date()) - data$date_birth_year - (month(Sys.Date()) < data$date_birth_month)

# Calculate loneliness scores (no sum score is computed for a given scale in case the participant did not answer to all items of that given scale)
data$loneliness_djgls_6 <- ifelse(
  rowSums(is.na(data[, c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f")])) == 0,
  rowSums(data[, c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f")], na.rm = FALSE),
  NA
)

data$loneliness_t_ils <- ifelse(
  rowSums(is.na(data[, c("loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c")])) == 0,
  rowSums(data[, c("loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c")], na.rm = FALSE),
  NA
)

# Checks the number of NA for age, gender, educational attainment, djgls-6, t-ils, and direct loneliness
sum(is.na(data$age))
sum(data$gender == "Prefer not to say")
sum(data$education == "Prefer not to say")
sum(is.na(data$loneliness_djgls_6))
sum(is.na(data$loneliness_t_ils))
sum(is.na(data$loneliness_direct))

# Calculate statistics by country
statistics_by_country <- data %>%
  group_by(country) %>%
  summarise(
    n = n(),
    age_mdn = median(age, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    male = sum(gender == "Male", na.rm = TRUE) / sum(!is.na(gender)) * 100,
    female = sum(gender == "Female", na.rm = TRUE) / sum(!is.na(gender)) * 100,
    other = sum(gender == "In another way", na.rm = TRUE) / sum(!is.na(gender)) * 100,
    djgls_6_mdn = median(loneliness_djgls_6, na.rm = TRUE),
    djgls_6_mean = mean(loneliness_djgls_6, na.rm = TRUE),
    djgls_6_sd = sd(loneliness_djgls_6, na.rm = TRUE),
    t_ils_mdn = median(loneliness_t_ils, na.rm = TRUE),
    t_ils_mean = mean(loneliness_t_ils, na.rm = TRUE),
    t_ils_sd = sd(loneliness_t_ils, na.rm = TRUE),
    direct_mdn = median(loneliness_direct, na.rm = TRUE),
    direct_mean = mean(loneliness_direct, na.rm = TRUE),
    direct_sd = sd(loneliness_direct, na.rm = TRUE)
  ) 

statistics_overall <- data %>%
  summarise(
    country = "Overall",
    n = n(),
    age_mdn = median(age, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    male = sum(gender == "Male", na.rm = TRUE) / sum(!is.na(gender)) * 100,
    female = sum(gender == "Female", na.rm = TRUE) / sum(!is.na(gender)) * 100,
    other = sum(gender == "In another way", na.rm = TRUE) / sum(!is.na(gender)) * 100,
    djgls_6_mdn = median(loneliness_djgls_6, na.rm = TRUE),
    djgls_6_mean = mean(loneliness_djgls_6, na.rm = TRUE),
    djgls_6_sd = sd(loneliness_djgls_6, na.rm = TRUE),
    t_ils_mdn = median(loneliness_t_ils, na.rm = TRUE),
    t_ils_mean = mean(loneliness_t_ils, na.rm = TRUE),
    t_ils_sd = sd(loneliness_t_ils, na.rm = TRUE),
    direct_mdn = median(loneliness_direct, na.rm = TRUE),
    direct_mean = mean(loneliness_direct, na.rm = TRUE),
    direct_sd = sd(loneliness_direct, na.rm = TRUE)
  )

statisticsByCountryEF <- rbind(statistics_by_country, statistics_overall)
statisticsByCountryEF <- roundDf(as.data.frame(statisticsByCountryEF))

# Save the results to a CSV file
write.csv(statisticsByCountryEF, "output_statistics_by_country_confirmatory.csv")

# Factor analysis and internal consistency --------------------------------
#'# Factor analysis and internal consistency
#'
#----------------
#DJGLS-6
#----------------
#'## DJGLS-6

# Keeping only the items of interest, and removing rows with missing values
data_djg <- subset(data, select = c(loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f, w_country_04))
data_djg <- na.omit(data_djg)

# 2F Model
djg_model_2F <- 
'emotionalLoneliness =~ loneliness_djg_a + loneliness_djg_b + loneliness_djg_c
socialLoneliness =~ loneliness_djg_d + loneliness_djg_e + loneliness_djg_f
emotionalLoneliness ~~ socialLoneliness'

# Confirmatory factor analysis across all EU member states, on the a-priori factor structure
fit_djg_model <- cfa(model = djg_model_2F, 
                     data = data_djg,
                     ordered = TRUE,
                     estimator = "WLSMV",
                     sampling.weights = "w_country_04")
#'### 2-factor model summary
(djg2fModelEF <- summary(fit_djg_model, standardized = TRUE))
#'### 2-factor model fit measures
(djg2fFitEF <- fitmeasures(fit_djg_model, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")))

#'### Parallel analysis
djg_paralell <- fa.parallel(x = data_djg,
                            cor = "poly") #3-point Likert-type measures are best handled with polychoric correlations

# Empirical Kaiser Criterion
#+ include=FALSE
djg_empkc <- EMPKC(data = data_djg, corkind = "polychoric", verbose = FALSE)

djg_model <- 'loneliness =~ loneliness_djg_a + loneliness_djg_b + loneliness_djg_c + loneliness_djg_d + loneliness_djg_e + loneliness_djg_f'
djg_factors <- 2

# Confirmatory factor analysis across all EU member states
#+ include=TRUE
fit_djg_modelUni <- cfa(model = djg_model, 
                     data = data_djg,
                     ordered = TRUE,
                     estimator = "WLSMV",
                     sampling.weights = "w_country_04")
#'### 1-factor model summary
summary(fit_djg_modelUni, standardized = TRUE)
#'### 1-factor model fit measures
fitmeasures(fit_djg_modelUni, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

#'### Internal consistency across all EU member states
#'
#'#### For the 2-factor model
(icDjgEF <- semTools::compRelSEM(fit_djg_model))
#'#### For the 1-factor model
(icDjgUniEF <- semTools::compRelSEM(fit_djg_modelUni))
#'#### LR test
lavTestLRT(fit_djg_model, fit_djg_modelUni)

#'### CFA and internal consistency separately for countries
#+ include=FALSE
djgPerCountryEF <- list()
for(country in countries){
  # Keeping participants who belong to the country of interest, the items of interest, and removing rows with missing values
  temp_data <- data[data$country_chr == country, ]
  temp_data <- subset(temp_data, select = c(loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f))
  temp_data <- na.omit(temp_data)
  
  # CFA
  temp_fit <- cfa(model = djg_model_2F, 
                  data = temp_data,
                  ordered = TRUE,
                  estimator = "WLSMV")
  
  temp_paralell <- fa.parallel(x = data_djg, plot = FALSE,
                              cor = "poly")$nfact #3-point Likert-type measures are best handled with polychoric correlations
  
  # Empirical Kaiser Criterion
  temp_empkc <- EMPKC(data = data_djg, verbose = FALSE, corkind = "polychoric")$NfactorsEMPKC
  
  
  # Internal consistency
  temp_ic <- semTools::compRelSEM(temp_fit)
  
  # Adding the results in a list
  temp_list <- list(cfa=fitmeasures(temp_fit, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")), paralell = temp_paralell, KC = temp_empkc, ic=temp_ic)
  djgPerCountryEF[[country]] <- temp_list
}

#'#### Across-countries internal consistency descriptives
#+ include=TRUE
#'#### For emotional loneliness
# lapply(djgPerCountryEF, function(x)x$ic[1])
(icDjgEmotCountryDescEF <- describe(unlist(lapply(djgPerCountryEF, function(x)x$ic[1]))))
#'#### For social loneliness
# lapply(djgPerCountryEF, function(x)x$ic[2])
(icDjgSocCountryDescEF <- describe(unlist(lapply(djgPerCountryEF, function(x)x$ic[2]))))

#'### Detailed per-country results
djgPerCountryEF

#'## T-ILS
#'
#----------------
#T-ILS
#----------------
# Keeping only the items of interest, and removing rows with missing values
data_tils <-subset(data, select = c(loneliness_ucla_a, loneliness_ucla_b, loneliness_ucla_c, w_country_04))
data_tils <- na.omit(data_tils)

# As the T-ILS is a three-item scale, only a one factor model can fit the data
# As a consequence, we don't run parallel analysis and empirical kaiser criterion extraction techniques on the T-ILS, and directly run the confirmatory factor analysis on the a-priori factor structure
tils_model <- 'loneliness =~ loneliness_ucla_a + loneliness_ucla_b + loneliness_ucla_c'

# Confirmatory factor analysis across all EU member states, on the a-priori factor structure
fit_tils_model <- cfa(model = tils_model,
                      data = data_tils,
                      ordered = TRUE,
                      estimator = "WLSMV",
                      sampling.weights = "w_country_04")
#'### Model summary
(tilsModelEF <- summary(fit_tils_model, standardized = T))

#'### Internal consistency across all EU member states
(ictilsEF <- semTools::compRelSEM(fit_tils_model))

# Confirmatory factor analysis and internal consistency for each member state separately
tilsPerCountryEF <- list()
for (country in countries){
  # Keeping participants who belong to the country of interest, the items of interest, and removing rows with missing values
  temp_data <- data[data$country_chr == country, ]
  temp_data <- subset(temp_data, select = c(loneliness_ucla_a, loneliness_ucla_b, loneliness_ucla_c))
  temp_data <- na.omit(temp_data)
  
  # Confirmatory factor analysis
  temp_fit <- cfa(model = tils_model, 
                  data = temp_data,
                  ordered = TRUE,
                  estimator = "WLSMV")
  
  # Internal consistency
  temp_ic <- semTools::compRelSEM(temp_fit)
  
  # Adding the results in a list
  temp_list <- list(cfa=parameterestimates(temp_fit, standardized = T)[1:3, c(3, 11)], ic=temp_ic)
  tilsPerCountryEF[[country]] <- temp_list
}

#'#### Across-countries internal consistency descriptives
describe(unlist(lapply(tilsPerCountryEF, function(x)x$ic)))

#'### Detailed per-country results
tilsPerCountryEF

#'## Summary of CFA and internal consistency analyses
#'
# Table 2 -----------------------------------------------------------------
# Initialize vectors to store extracted data for djgPerCountryEF
countries <- names(tilsPerCountryEF)
chisq_djg <- numeric(length(countries))
cfi_djg <- numeric(length(countries))
rmsea_djg <- numeric(length(countries))
omega_emotional_djg <- numeric(length(countries))
omega_social_djg <- numeric(length(countries))

# Initialize vectors to store extracted data for tilsPerCountryEF
factor_loading_a <- numeric(length(countries))
factor_loading_b <- numeric(length(countries))
factor_loading_c <- numeric(length(countries))
omega_tils <- numeric(length(countries))

# Extract data for djgPerCountryEF
for (i in seq_along(countries)) {
  country <- countries[i]
  chisq_djg[i] <- djgPerCountryEF[[country]]$cfa["chisq"]
  cfi_djg[i] <- djgPerCountryEF[[country]]$cfa["cfi"]
  rmsea_djg[i] <- djgPerCountryEF[[country]]$cfa["rmsea"]
  omega_emotional_djg[i] <- djgPerCountryEF[[country]]$ic["emotionalLoneliness"]
  omega_social_djg[i] <- djgPerCountryEF[[country]]$ic["socialLoneliness"]
}

# Extract data for tilsPerCountryEF
for (i in seq_along(countries)) {
  country <- countries[i]
  factor_loading_a[i] <- tilsPerCountryEF[[country]]$cfa$std.all[tilsPerCountryEF[[country]]$cfa$rhs == "loneliness_ucla_a"]
  factor_loading_b[i] <- tilsPerCountryEF[[country]]$cfa$std.all[tilsPerCountryEF[[country]]$cfa$rhs == "loneliness_ucla_b"]
  factor_loading_c[i] <- tilsPerCountryEF[[country]]$cfa$std.all[tilsPerCountryEF[[country]]$cfa$rhs == "loneliness_ucla_c"]
  omega_tils[i] <- tilsPerCountryEF[[country]]$ic["loneliness"]
}

# Create a dataframe to fill in the Excel table
summaryFitIC <- data.frame(
  Country = countries,
  Chisq_DJG = chisq_djg,
  CFI_DJG = cfi_djg,
  RMSEA_DJG = rmsea_djg,
  Omega_Emotional_DJG = omega_emotional_djg,
  Omega_Social_DJG = omega_social_djg,
  Factor_Loading_A = factor_loading_a,
  Factor_Loading_B = factor_loading_b,
  Factor_Loading_C = factor_loading_c,
  Omega_TILS = omega_tils
)
roundDf(summaryFitIC)

# Save the results to a CSV file
write.csv(summaryFitIC, "model_fit_by_country_confirmatory.csv")


#'# Measurement invariance
#'
# Measurement invariance --------------------------------------------------

fitMeasuresSelection <- c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")

#'## DJGLS-6
#----------------
#DJGLS-6
#----------------

# Testing for measurement invariance (configural; metric; scalar) using multigroup confirmatory factor analysis on the invariant clusters identified in the exploratory fold

data_djg <- subset(data, select = c(country, gender, age_class, loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f))

data_djg <- na.omit(data_djg)

#recode the coutry names to codes
data_djg[[1]] <- as.numeric(as.factor(data_djg[[1]]))

#cluster solution that reached scalar invariance with MGCFA on the exploratory fold (cluster A is excluded as it did not reach scalar invariance with MGCFA)
djg_clusters_list <- list(cluster_b = c(3, 9, 12, 19, 20), 
                          cluster_c = c(8, 16, 17, 18, 21, 25, 26, 27))

djg_clusters_invariance <- list()

# Loop through each cluster in the list
for (i in seq_along(djg_clusters_list)) {
  cluster_name <- names(djg_clusters_list)[i]
  cluster <- djg_clusters_list[[i]]
  
  # Keep participants who belong to the cluster of interest, removing rows with missing values
  temp_data <- data_djg[data_djg$country %in% cluster, ]
  temp_data <- na.omit(temp_data)
  temp_config <- cfa(model = djg_model_2F,
                     data = temp_data,
                     estimator = "WLSMV",
                     ordered=TRUE,
                     group = "country")
  
  temp_metric <- cfa(model = djg_model_2F,
                     data = temp_data,
                     estimator = "WLSMV",
                     ordered=TRUE,
                     group = "country",
                     group.equal = c("loadings"))
  temp_metric_sign <- lavTestLRT(temp_config, temp_metric)
  
  temp_scalar <- cfa(model = djg_model_2F,
                     data = temp_data,
                     estimator = "WLSMV",
                     ordered=TRUE,
                     group = "country",
                     group.equal = c("loadings", "intercepts"))
  temp_scalar_sign <- lavTestLRT(temp_metric, temp_scalar)
  
  # Add the results in a nested list under the cluster name
  djg_clusters_invariance[[cluster_name]] <- list(
    configural = fitmeasures(temp_config, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea")),
    metric = list(model = fitmeasures(temp_metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea")), sign = temp_metric_sign %$% c("chisq" = .$`Chisq diff`[2], "df" = .$`Df diff`[2], "p" = .$`Pr(>Chisq)`[2])),
    scalar = list(model = fitmeasures(temp_scalar, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea")), sign = temp_scalar_sign  %$% c("chisq" = .$`Chisq diff`[2], "df" = .$`Df diff`[2], "p" = .$`Pr(>Chisq)`[2]))
  )
}

# Initialize a dataframe to store the results
djgMmgResultsEF <- data.frame(clusterID = character(),
                         chisq_configural = character(),
                         cfi_configural = numeric(),
                         rmsea_configural = numeric(),
                         chisq_metric = character(),
                         cfi_metric = character(),
                         rmsea_metric = character(),
                         #chisq_metric_diff = character(),
                         chisq_scalar = character(),
                         cfi_scalar = character(),
                         rmsea_scalar = character(),
                         #chisq_scalar_diff = character(),
                         stringsAsFactors = FALSE)

# Function to format chisq, df, and pvalue into a string with specified rounding and conditions
format_chisq <- function(chisq, df, pvalue) {
  pvalue_str <- ifelse(pvalue < 0.001, "<.001", sprintf("%.2e", pvalue))
  sprintf("%.0f (%.0f, %s)", chisq, df, pvalue_str) # Chisq rounded to 0 decimals
}

# Function to round values to two decimals
round_two_decimals <- function(value) {
  round(value, 2)
}

clusters <- c("cluster_b", "cluster_c")

for (cluster in clusters) {
  configural <- djg_clusters_invariance[[cluster]]$configural
  metric_model <- djg_clusters_invariance[[cluster]]$metric$model
  metric_sign <- djg_clusters_invariance[[cluster]]$metric$sign
  scalar_model <- djg_clusters_invariance[[cluster]]$scalar$model
  scalar_sign <- djg_clusters_invariance[[cluster]]$scalar$sign
  
  # Extract and format the information with rounding
  chisq_configural <- format_chisq(configural["chisq"], configural["df"], configural["pvalue"])
  cfi_configural <- round_two_decimals(configural["cfi"])
  rmsea_configural <- round_two_decimals(configural["rmsea"])
  chisq_metric <- format_chisq(metric_model["chisq"], metric_model["df"], metric_model["pvalue"])
  cfi_metric <- round_two_decimals(djg_clusters_invariance[[cluster]]$metric$model["cfi"])
  rmsea_metric <- round_two_decimals(djg_clusters_invariance[[cluster]]$metric$model["rmsea"])
  #chisq_metric_diff <- format_chisq(metric_sign["chisq"], metric_sign["df"], metric_sign["p"])
  chisq_scalar <- format_chisq(scalar_model["chisq"], scalar_model["df"], scalar_model["pvalue"])
  cfi_scalar <- round_two_decimals(djg_clusters_invariance[[cluster]]$scalar$model["cfi"])
  rmsea_scalar <- round_two_decimals(djg_clusters_invariance[[cluster]]$scalar$model["rmsea"])
  #chisq_scalar_diff <- format_chisq(scalar_sign["chisq"], scalar_sign["df"], scalar_sign["p"])
  
  # Append the row to the dataframe
  djgMmgResultsEF <- rbind(djgMmgResultsEF, data.frame(clusterID = cluster,
                                             chisq_configural = chisq_configural,
                                             cfi_configural = cfi_configural,
                                             rmsea_configural = rmsea_configural,
                                             chisq_metric = chisq_metric,
                                             cfi_metric = cfi_metric,
                                             rmsea_metric = rmsea_metric,
                                             #chisq_metric_diff = chisq_metric_diff,
                                             chisq_scalar = chisq_scalar,
                                             cfi_scalar = cfi_scalar,
                                             rmsea_scalar = rmsea_scalar,
                                             #chisq_scalar_diff = chisq_scalar_diff,
                                             stringsAsFactors = FALSE))
}

#+ include=TRUE
#'### Mixture multigroup factor analysis
print(djgMmgResultsEF)

# Save the results to a CSV file
write.csv(djgMmgResultsEF, "clusters_invariance_confirmatory.csv", row.names = FALSE)

#'### Within-cluster invariance for gender, age
#'
# Within-cluster invariance w.r.t. gender and age -------------------------
data_djg <- data_djg %>%
  mutate(gender = ifelse(gender %in% c("Male", "Female"), gender, NA))

# Function to perform invariance testing for a given grouping variable, filtered by cluster
test_invariance_by_cluster <- function(grouping_var, cluster_countries, data, model) {
  # Filter data to include only specified countries
  filtered_data <- data[data$country %in% cluster_countries, ]
  
  safe_cfa <- function(model, data, grouping_var, group_equal="") {
    tryCatch({
      cfa(model = model,
          data = data,
          ordered = TRUE,
          estimator = "WLSMV",
          group = grouping_var,
          group.equal = group_equal)
    }, error = function(e) {
      NA
    })
  }
  
  # Configural invariance
  configural_model <- safe_cfa(model, filtered_data, grouping_var)
  
  # Metric invariance
  metric_model <- safe_cfa(model, filtered_data, grouping_var, group_equal = "loadings")
  
  # Scalar invariance
  scalar_model <- safe_cfa(model, filtered_data, grouping_var, group_equal = c("loadings", "intercepts"))
  
  # Only compute further if models were successfully estimated
  if (is.na(configural_model) || is.na(metric_model) || is.na(scalar_model)) {
    return(list(configural = NA, metric = NA, scalar = NA))
  }
  
  # Calculate fit measures and LRT tests
  config_fit <- fitmeasures(configural_model, fit.measures = fitMeasuresSelection)
  metric_fit <- fitmeasures(metric_model, fit.measures = fitMeasuresSelection)
  scalar_fit <- fitmeasures(scalar_model, fit.measures = fitMeasuresSelection)
  
  metric_lrt <- lavTestLRT(configural_model, metric_model)
  scalar_lrt <- lavTestLRT(metric_model, scalar_model)
  
  list(
    configural = list(fit = config_fit),
    metric = list(fit = metric_fit, lrt = metric_lrt),
    scalar = list(fit = scalar_fit, lrt = scalar_lrt)
  )
}

# Define clusters with names
clusters <- list(
  clusterB = c(3, 9, 12, 19, 20),
  clusterC = c(8, 16, 17, 18, 21, 25, 26, 27)
)

# Run invariance tests for each cluster and grouping variable
djgClustersInvarianceResults <- list()
for (cluster_name in names(clusters)) {
  cluster_countries <- clusters[[cluster_name]]
  cluster_results <- list()
  for (var in c("gender", "age_class")) {
    result <- test_invariance_by_cluster(grouping_var = var, cluster_countries = cluster_countries, data = data_djg, model = djg_model_2F)
    cluster_results[[var]] <- result
  }
  djgClustersInvarianceResults[[cluster_name]] <- cluster_results
}

# Naming the inner lists
for (cluster_name in names(clusters)) {
  for (var in c("gender", "age_class")) {
    name <- paste(cluster_name, var, sep = "_")
    djgClustersInvarianceResults[[cluster_name]][[var]] <- setNames(djgClustersInvarianceResults[[cluster_name]][[var]], c("configural", "metric", "scalar"))
  }
}

#'### DJGLS-6 invariance results for gender, age
djgClustersInvarianceResults

# Prepare an empty data.frame for gender/age invariance results
djgGenderAgeResults <- data.frame(
  clusterID = character(),
  grouping = character(),
  model = character(),
  chisq = numeric(),
  df = numeric(),
  pvalue = numeric(),
  cfi = numeric(),
  rmsea = numeric(),
  stringsAsFactors = FALSE
)

for (cluster_name in names(djgClustersInvarianceResults)) {
  for (var in names(djgClustersInvarianceResults[[cluster_name]])) {
    result <- djgClustersInvarianceResults[[cluster_name]][[var]]
    
    for (model_type in c("configural", "metric", "scalar")) {
      model_result <- result[[model_type]]
      
      # Safely skip if model_result is NA or doesn't have $fit
      if (is.atomic(model_result) && all(is.na(model_result))) next
      if (!("fit" %in% names(model_result))) next
      
      fit <- model_result$fit
      
      djgGenderAgeResults <- rbind(djgGenderAgeResults, data.frame(
        clusterID = cluster_name,
        grouping = var,
        model = model_type,
        chisq = round_two_decimals(fit["chisq"]),
        df = round_two_decimals(fit["df"]),
        pvalue = fit["pvalue"],
        cfi = round_two_decimals(fit["cfi"]),
        rmsea = round_two_decimals(fit["rmsea"]),
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Optional: format p-values nicely
djgGenderAgeResults$pvalue <- ifelse(as.numeric(djgGenderAgeResults$pvalue) < 0.001,
                                     "<.001",
                                     sprintf("%.3f", as.numeric(djgGenderAgeResults$pvalue)))

write.csv(djgGenderAgeResults, "clusters_invariance_confirmatory_gender_age.csv", row.names = FALSE)
print(djgGenderAgeResults)

#'## T-ILS
#'
#----------------
#T-ILS
#----------------
# Prepare the data, assuming missing values are handled as needed
data_tils <- subset(data, select = c(country, gender, age_class, loneliness_ucla_a, loneliness_ucla_b, loneliness_ucla_c))
data_tils <- na.omit(data_tils)

# Function to perform invariance testing for a given grouping variable
test_invariance <- function(grouping_var, data, model) {
  # Metric invariance
  metric_model <- cfa(model = model,
                      data = data,
                      ordered = TRUE,
                      estimator = "WLSMV",
                      group = grouping_var,
                      group.equal = c("loadings"))
  
  # Scalar invariance
  scalar_model <- cfa(model = model,
                      data = data,
                      ordered = TRUE,
                      estimator = "WLSMV",
                      group = grouping_var,
                      group.equal = c("loadings", "intercepts"))
  
  # Fit measures for each model
  metric_fit <- fitmeasures(metric_model, fit.measures = fitMeasuresSelection)
  #print(metric_fit)
  scalar_fit <- fitmeasures(scalar_model, fit.measures = fitMeasuresSelection)
  #print(scalar_fit)
  # LRT tests
  scalar_lrt <- lavTestLRT(metric_model, scalar_model)
  
  list(
    metric = list(fit = metric_fit),
    scalar = list(fit = scalar_fit, lrtScalarVsMetric = scalar_lrt)
  )
}

# List of variables to test invariance for
group_vars <- c("country", "gender", "age_class")

# Run invariance tests for all group variables and store results in a nested list
tilsInvarianceResults <- lapply(group_vars, function(var) {
  test_invariance(grouping_var = var, data = data_tils, model = tils_model)
})

# Naming the list for clarity
names(tilsInvarianceResults) <- group_vars

#'### T-ILS invariance results for county, gender, and age
tilsInvarianceResults

# Prepare empty data frame for T-ILS invariance results
tilsInvarianceDF <- data.frame(
  grouping = character(),
  model = character(),
  chisq = numeric(),
  df = numeric(),
  pvalue = character(),
  cfi = numeric(),
  rmsea = numeric(),
  stringsAsFactors = FALSE
)

# Flatten the list into a data frame
for (var in names(tilsInvarianceResults)) {
  result <- tilsInvarianceResults[[var]]
  
  for (model_type in c("metric", "scalar")) {
    model_result <- result[[model_type]]
    
    # Safely skip if model_result is missing
    if (is.null(model_result) || !("fit" %in% names(model_result))) next
    
    fit <- model_result$fit
    
    tilsInvarianceDF <- rbind(tilsInvarianceDF, data.frame(
      grouping = var,
      model = model_type,
      chisq = round_two_decimals(fit["chisq"]),
      df = round_two_decimals(fit["df"]),
      pvalue = ifelse(fit["pvalue"] < 0.001, "<.001", sprintf("%.3f", fit["pvalue"])),
      cfi = round_two_decimals(fit["cfi"]),
      rmsea = round_two_decimals(fit["rmsea"]),
      stringsAsFactors = FALSE
    ))
  }
}

# Save and print
write.csv(tilsInvarianceDF, "tils_invariance_confirmatory.csv", row.names = FALSE)
print(tilsInvarianceDF)

# Social support ----------------------------------------------------------
data_social_support <- subset(data, select = c(social_support_a, social_support_b, social_support_c, social_support_d))
data_social_support <- na.omit(data_social_support)
fit_social_support_modelEF <- cfa(model = 'social_suppport =~ social_support_a + social_support_b + social_support_c + social_support_d', 
                                data = data_social_support,
                                estimator = "MLR") # Maximum Likelihood (ML) methods work fine with measures answered on a 5-point Likert type scale. Here we apply the Robust Maximum Likelihood (MLR) method as it is robust to the violation of multivariate normality, an assumption that undermines the use of the ML method and that barely holds with such measures.
social_support_ic <- semTools::reliability(fit_social_support_modelEF)

#'# Construct validity
#'
# Construct validity ------------------------------------------------------
# djg_emot_mean, djg_social_mean - higher score, more lonely
# tils_mean - higher score, more lonely
# loneliness_direct - higher score, more lonely

# feelings_depr, feelings_happy, and neighbours - higher scores indicate greater depression, greater happiness, and greater contacts with neighbours
# health_general - higher scores indicate better subjective health
# "social_support_mean", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "social_activities_a"

# We expect positive correlations between the primary measures (djg_emot_mean, tils_mean, loneliness_direct) and feelings_depr
# We expect negative correlations between the primary measures (djg_emot_mean, tils_mean, loneliness_direct) and the following secondary measures: "social_support_mean", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "social_activities_a", "neighbours", "feelings_happy", "health_general"

#separating confirmatory data from exploratory data
con_data <- data
save(con_data, file="con_data.RData")
load("exp_data.RData")
load("con_data.RData")

# Factor score estimation -------------------------------------------------
# data <- dataOrg
# Define models for each construct
models <- list(
  djg_emot = 'djg_emot =~ loneliness_djg_a + loneliness_djg_b + loneliness_djg_c',
  djg_social = 'djg_social =~ loneliness_djg_d + loneliness_djg_e + loneliness_djg_f',
  tils = 'tils =~ loneliness_ucla_a + loneliness_ucla_b + loneliness_ucla_c',
  social_support = 'social_support =~ social_support_a + social_support_b + social_support_c + social_support_d'
)

# Manually specify relevant variables for each construct to avoid any mismatches
relevant_vars_per_construct <- list(
  djg_emot = c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c"),
  djg_social = c("loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f"),
  tils = c("loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c"),
  social_support = c("social_support_a", "social_support_b", "social_support_c", "social_support_d")
)

# Add single-indicator constructs to the models and relevant_vars_per_construct lists
single_indicators <- c("loneliness_direct",  "health_general", "feelings_depr", "feelings_happy", 
                       "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", 
                       "family_n__1__open", "friends_n__1__open", "neighbours", "social_activities_a")

# For single indicators, define a CFA model for each, assuming a loading of .70
for (indicator in single_indicators) {
  models[indicator] <- sprintf('%s =~ .7*%s', paste0(indicator, "_lv"), indicator)
  relevant_vars_per_construct[indicator] <- c(indicator)
}

# Loop through each new single-indicator construct to compute and store factor scores
for (construct in names(models)) {
  # Filter data for current construct, ensuring specified variables are complete and have variance
  con_temp_data <- na.omit(con_data[, relevant_vars_per_construct[[construct]]])
  exp_temp_data <- na.omit(exp_data[, relevant_vars_per_construct[[construct]]])

  if(construct == "social_support") {
    estimator <- "MLR"
    ordered <- FALSE
  } else {
    estimator <- "WLSMV"
    ordered <- TRUE
  }
  
  if(nrow(con_temp_data) > 0) {
    fit <- cfa(models[[construct]], data = con_temp_data, estimator = estimator, ordered = ordered)
    # Initialize factor score columns for single-indicator constructs with NA
    con_data[[paste0(construct, "_fs")]] <- NA  
    valid_indices <- which(complete.cases(con_data[, relevant_vars_per_construct[[construct]]]))
    # Extract factor scores for valid indices
    con_data[valid_indices, paste0(construct, "_fs")] <- lavPredict(fit, type = "lv")[,1]
  } else {
    warning(paste("No valid data for construct:", construct))
  }

  if(nrow(exp_temp_data) > 0) {
    fit <- cfa(models[[construct]], data = exp_temp_data, estimator = estimator, ordered = ordered)
    # Initialize factor score columns for single-indicator constructs with NA
    exp_data[[paste0(construct, "_fs")]] <- NA  
    valid_indices <- which(complete.cases(exp_data[, relevant_vars_per_construct[[construct]]]))
    # Extract factor scores for valid indices
    exp_data[valid_indices, paste0(construct, "_fs")] <- lavPredict(fit, type = "lv")[,1]
  } else {
    warning(paste("No valid data for construct:", construct))
  }
}

# Nomological nets --------------------------------------------------------
variableList <- list(
  fullNet = c("social_support_fs", "friends_n__1__open_fs", "family_n__1__open_fs", "friends_meet_face_fs", "family_meet_face_fs", "friends_meet_tele_fs", "family_meet_tele_fs", "neighbours_fs", "social_activities_a_fs", "feelings_depr_fs", "feelings_happy_fs", "health_general_fs"),
  socialActivities = c("social_support_fs", "friends_n__1__open_fs", "family_n__1__open_fs", "friends_meet_face_fs", "family_meet_face_fs", "friends_meet_tele_fs", "family_meet_tele_fs", "neighbours_fs", "social_activities_a_fs"),
  deprHappy = c("feelings_depr_fs", "feelings_happy_fs"),
  health = c("health_general_fs")
)

# Initialize a list to store results for each variable set
results_list <- list()

# Initialize a list to store frequency tables for each variable set
#freq_tables_list <- list()

for (set_name in names(variableList)) {
  variables <- variableList[[set_name]]
  
  # Initialize a list for frequency tables of the current variable set
  #freq_tables <- list()
  
  # Iterate over the variables to create and store their frequency tables
  #for (variable in variables) {
    # Store frequency table in the list instead of printing
  #  freq_tables[[variable]] <- table(data[[variable]], useNA = "ifany")
  #}
  
  # Store the frequency tables list for the current set
  #freq_tables_list[[set_name]] <- freq_tables
  
  # Initialize the list for storing network per country for the current variable set
  nom_network_per_country <- list()
  for (country in countries) {
    country_list <- list()
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      primary_measure_list <- list()
      
      for (secondary_measure in variables) {
        primary_measure_list[[secondary_measure]] <- NA
      }
      country_list[[primary_measure]] <- primary_measure_list
    }
    nom_network_per_country[[country]] <- country_list
  }
  
  # Computing the correlations for the current variable set
  for (country in countries) {
    con_temp_data <- con_data[con_data$country_chr == country, ]
    exp_temp_data <- exp_data[exp_data$country_chr == country, ]
    
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      for (secondary_measure in variables) {
        
        # Ensure measures are numeric
        if(is.numeric(con_temp_data[[primary_measure]]) && is.numeric(con_temp_data[[secondary_measure]])) {
          con_valid_data_indices <- complete.cases(con_temp_data[, c(primary_measure, secondary_measure)])
          exp_valid_data_indices <- complete.cases(exp_temp_data[, c(primary_measure, secondary_measure)])
          
          if (sum(con_valid_data_indices) > 0) {
            con_valid_data <- con_temp_data[con_valid_data_indices, ]
            exp_valid_data <- exp_temp_data[exp_valid_data_indices, ]
            
            # Compute Pearson correlation (confirmatory data)
            pearson_cor_con <- cor(con_valid_data[[primary_measure]], con_valid_data[[secondary_measure]], use = "pairwise.complete.obs")

            # Compute Pearson correlation (confirmatory data)
            pearson_cor_exp <- cor(exp_valid_data[[primary_measure]], exp_valid_data[[secondary_measure]], use = "pairwise.complete.obs")
            
            # Fit linear model and compute summary
            linear_model_formula <- as.formula(paste(secondary_measure, "~", primary_measure))

            linear_model_con <- lm(linear_model_formula, data = con_valid_data)
            summary_linear_model_con <- summary(linear_model_con)
            p_value_con <- summary_linear_model_con$coefficients[2, 4]

            linear_model_exp <- lm(linear_model_formula, data = exp_valid_data)
            summary_linear_model_exp <- summary(linear_model_exp)
            p_value_exp <- summary_linear_model_exp$coefficients[2, 4]
            
            # Compute Bayes Factor
            BF01 <- compute_bf_correlation(exp_valid_data, con_valid_data, primary_measure, secondary_measure)
            BF01 <- BF01$BF01
            
            # Store results
            nom_network_per_country[[country]][[primary_measure]][[secondary_measure]] <- list(p_value_con = p_value_con, p_value_exp = p_value_exp, pearson_cor_con = pearson_cor_con, pearson_cor_exp = pearson_cor_exp, BF01 = BF01)
          } else {
            cat(sprintf("Insufficient valid data for primary measure: %s or secondary measure: %s in country: %s\n", primary_measure, secondary_measure, country))
          }
        } else {
          cat(sprintf("Data for primary measure: %s or secondary measure: %s in country: %s is not numeric\n", primary_measure, secondary_measure, country))
        }
      }
    }
  }
  
  # Initialize an empty data frame to store the results for the current variable set
  results_df <- data.frame(country = character(), 
                           primary_measure = character(), 
                           secondary_measure = character(), 
                           p_value_con = numeric(), 
                           p_value_exp = numeric(), 
                           pearson_cor_con = numeric(), 
                           pearson_cor_exp = numeric(), 
                           BF01 = numeric(), 
                           stringsAsFactors = FALSE)
  
  # Loop through each level of the nested list structure for the current variable set
  for (country in names(nom_network_per_country)) {
    for (primary_measure in names(nom_network_per_country[[country]])) {
      for (secondary_measure in names(nom_network_per_country[[country]][[primary_measure]])) {
        # Extract the list containing p_value and pearson_cor
        measure_data <- nom_network_per_country[[country]][[primary_measure]][[secondary_measure]]
        if(is.list(measure_data) && !is.null(measure_data[['p_value_con']]) && !is.null(measure_data[['p_value_exp']]) && !is.null(measure_data[['pearson_cor_con']]) && !is.null(measure_data[['pearson_cor_exp']]) && !is.null(measure_data[['BF01']])) {
          p_value_con <- measure_data[['p_value_con']]
          p_value_exp <- measure_data[['p_value_exp']]
          pearson_cor_con <- measure_data[['pearson_cor_con']]
          pearson_cor_exp <- measure_data[['pearson_cor_exp']]
          BF01 <- measure_data[['BF01']]
        } else {
          p_value_con <- NA
          p_value_exp <- NA
          pearson_cor_con <- NA
          pearson_cor_exp <- NA
          BF01 <- NA
        }
        
        # Append to the results data frame
        results_df <- rbind(results_df, data.frame(country, primary_measure, secondary_measure, pearson_cor_con, pearson_cor_exp, p_value_con, p_value_exp, BF01, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Apply rounding to all numeric columns in the dataframe
  results_df <- data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))
  
  # Store the processed dataframe in the results_list under the current set name
  results_list[[set_name]] <- results_df
}

# Define the measures and their expected direction
measure_sets <- list(
  djg = c("djg_emot_fs", "djg_social_fs"),
  tils = "tils_fs",
  loneliness = "loneliness_direct_fs"
)

expected_direction <- list(
  positive = "feelings_depr_fs",
  negative = c("social_support_fs", "friends_n__1__open_fs", "family_n__1__open_fs", "friends_meet_face_fs", "family_meet_face_fs", "friends_meet_tele_fs", "family_meet_tele_fs", "neighbours_fs", "social_activities_a_fs", "feelings_happy_fs", "health_general_fs")
)

# on the exploratory fold: Function to determine if correlation is in the expected direction and meets criteria
is_cor_meeting_criteria_exp <- function(cor, measure, p_value, expected_positive, expected_negative) {
  if (p_value <= 0.004 && abs(cor) >= 0.10) {
    if (measure %in% expected_positive && cor > 0) {
      return(TRUE)
    } else if (measure %in% expected_negative && cor < 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# on the confirmatory fold: Function to determine if the correlation is in the expected direction and meets criteria, or if it does not differ from the correlation coefficient from the exploratory fold
is_cor_meeting_criteria_con <- function(cor_con, cor_exp, measure, p_value_con, p_value_exp, BF01, expected_positive, expected_negative) {
  if (p_value_con <= 0.004 && abs(cor_con) >= 0.10 && p_value_exp <= 0.004 && abs(cor_exp) >= 0.10) {
    if (measure %in% expected_positive && cor_con > 0 && cor_exp > 0) {
      return(TRUE) #coefficients from the exploratory and confirmatory folds both meet criteria (replication)
    } else if (measure %in% expected_negative && cor_con < 0 && cor_exp < 0) {
      return(TRUE) #coefficients from the exploratory and confirmatory folds both meet criteria (replication)
    }
  }
  if (BF01 > 3) {
    return(TRUE) #coefficients from the exploratory and confirmatory fold don't differ from each other (replication)
  }
  if ((abs(cor_con) < 0.10 | p_value_con > 0.004) && (abs(cor_exp) < 0.10 | p_value_exp > 0.004)) {
    return(TRUE) #coefficients from the exploratory and confirmatory folds both don't meet the criteria (replication)
  }
  return(FALSE) #other cases = no replication (likely cases: coefficient from the exploratory fold doesn't meet the criteria, coefficient from the confirmatory fold meets the criteria, and BF01 < 3)
}

# Adapt the loop to work with results from each set in variableList
proportions_list <- list()
options(max.print = 1e6)

for (set_name in names(variableList)) {
  results_df <- results_list[[set_name]]  # Retrieve the results dataframe for the current set
  
  # Following your original analysis but scoped within the current set of variables
  for (measure_set_name in names(measure_sets)) {
    measure_group <- measure_sets[[measure_set_name]]
    
    filtered_results <- results_df %>%
      filter(primary_measure %in% measure_group) %>%
      mutate(meets_criteria_exp = mapply(is_cor_meeting_criteria_exp, pearson_cor_exp, secondary_measure, p_value_exp, 
                                   MoreArgs = list(expected_positive = expected_direction$positive, 
                                                   expected_negative = expected_direction$negative))) %>% #returns TRUE if the correlation coefficient from the exploratory fold is in the expected direction and meets criteria
      mutate(meets_criteria_con = mapply(is_cor_meeting_criteria_con, pearson_cor_con, pearson_cor_exp, secondary_measure, p_value_con, p_value_exp, BF01,
                                          MoreArgs = list(expected_positive = expected_direction$positive, 
                                                          expected_negative = expected_direction$negative))) %>% #returns TRUE if the correlation coefficient from the confirmatory fold is in the expected direction and meets criteria, or if it does not differ from the correlation coefficient from the exploratory fold  (preregistered criteria for the analyses on the confirmatory fold)
      mutate(meets_criteria_final = meets_criteria_exp & meets_criteria_con) #returns TRUE if (a) the correlation coefficient from the exploratory fold is in the expected direction and meets criteria, and if (b) the correlation coefficient from the confirmatory fold is in the expected direction and meets criteria, or if it does not differ from the correlation coefficient in the exploratory fold.
    #sink("nomologicalNetCorr_fullresults.txt", append=TRUE)
    #print(filtered_results)
    #sink()
    proportions <- filtered_results %>%
      group_by(country) %>%
      summarize(proportion = mean(meets_criteria_final), .groups = 'drop')
    proportions_list[[paste(set_name, measure_set_name, sep = "_")]] <- proportions
  }
}

# Combine and process all proportions for output
combined_proportions_df <- bind_rows(lapply(names(proportions_list), function(name) {
  set_df <- proportions_list[[name]]
  set_df$measure_set <- name
  set_df
}), .id = "measure_set_id")

# Output the combined dataframe to an Excel file
write.xlsx(combined_proportions_df, "nomologicalNetCorr_confirmatory.xlsx") #the "proportion" column indicates the proportion of correlations that are in the expected direction and meets criteria across the two folds

final_results <- data.frame(
  Measure_Set = character(),
  Proportion_Over_Two_Thirds = numeric(),
  Count_Over_Two_Thirds = integer(),
  stringsAsFactors = FALSE
)

for (name in names(proportions_list)) {
  current_table <- proportions_list[[name]]
  
  proportion_over_two_thirds <- mean(current_table$proportion >= 2/3)
  count_over_two_thirds <- sum(current_table$proportion >= 2/3)
  
  final_results <- rbind(final_results, data.frame(
    Measure_Set = name,
    Proportion_Over_Two_Thirds = proportion_over_two_thirds,
    Count_Over_Two_Thirds = count_over_two_thirds
  ))
}

#'## Nomological net proportions per country
#'
#' Separately for all countries, shows the proportion of correlation effects that were significant, |r|>=.10, and in the expected direction
#' 
#' fullNet = all constructs; deprHappy = depression and happiness, socialActivities = social support, in-person and remote contact with family, friends, neighbours, closeness to family and friends, social activities; health = self-reported general health.
(nomologicalNetPropsPerCountryEF <- roundDf(as.data.frame(combined_proportions_df[,c(-1)])))

#'## Nomological net proportions overall
#'
#' Shows for each nomological network (loneliness instrument + relevant variables), for how many countries was the number of relationships in the nomological network at least 2/3.
nomologicalNetProportionsEF <- final_results
roundDf(nomologicalNetProportionsEF)

# Save nomological net correlation - full results
nomologicalNetCorrsEF <- results_list

# By-country heatmaps for nomological nets --------------------------------
# Initialize a list to hold the correlation matrices for each country
corMatricesEF <- list()

for (country in countries) {
  # Filter data for the current country
  country_data <- results_list$fullNet[results_list$fullNet$country == country,]
  
  # Get unique primary and secondary measures for the current country
  primary_measures <- unique(country_data$primary_measure)
  secondary_measures <- unique(country_data$secondary_measure)
  
  # Create an empty matrix for the current country
  cor_matrix <- matrix(NA, nrow = length(primary_measures), ncol = length(secondary_measures),
                       dimnames = list(primary_measures, secondary_measures))
  
  # Populate the matrix with correlation values (with the correlation from the confirmatory data)
  for (row in seq_along(primary_measures)) {
    for (col in seq_along(secondary_measures)) {
      cor_value <- country_data$pearson_cor_con[country_data$primary_measure == primary_measures[row] & country_data$secondary_measure == secondary_measures[col]]
      if (length(cor_value) == 1) {
        cor_matrix[row, col] <- cor_value
      }
    }
  }
  
  # Add the correlation matrix to the list, named by country
  corMatricesEF[[country]] <- cor_matrix
}

# The result is a list of correlation matrices, one for each country
#'## Per-country correlation matrices
corMatricesEF

# Define the custom labels for primary and secondary measures
primary_labels <- setNames(c("DJGLS-6 emotion", "DJGLS-6 social", "T-ILS", "Single-item"),
                           unique(unlist(lapply(corMatricesEF, rownames))))
secondary_labels <- setNames(c("Social support", "Friends closeness", "Family closeness", "Friends meet in-person",
                               "Family meet in-person", "Friends meet remote", "Family meet remote", 
                               "Neighbours contact", "Social activities", "Feeling depressed", "Feeling happy", "Health"),
                             unique(unlist(lapply(corMatricesEF, colnames))))

#'## Heatmaps for the per-country correlation matrices
# Loop through the list of correlation matrices and create a heatmap for each
for (country in names(corMatricesEF)) {
  cor_matrix <- corMatricesEF[[country]]
  
  # Apply the custom labels
  rownames(cor_matrix) <- primary_labels[rownames(cor_matrix)]
  colnames(cor_matrix) <- secondary_labels[colnames(cor_matrix)]
  
  # Convert the matrix to a long format data frame for ggplot2
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Scale", "Correlate", "Correlation")
  
  # Set correlations with absolute value smaller than 0.1 to NA
  cor_data$Correlation[abs(cor_data$Correlation) < 0.1] <- NA
  
  # Plot the heatmap
  p <- ggplot(cor_data, aes(x = Scale, y = Correlate, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         na.value = "white", name="Factor score\ncorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 9)) +
    labs(x = "Scale", y = "Correlate") +
    ggtitle(paste("Heatmap of latent correlations\nfor", country))
  
  print(p)
}
##################################
#'#### Figure 1
# Initialize an empty list to store the ggplot objects
heatmap_list <- list()

# Generate heatmaps for the first 27 (or fewer) matrices
for (country in names(corMatricesEF)[1:27]) {
  cor_matrix <- corMatricesEF[[country]]
  
  # Convert the matrix to a long format data frame for ggplot2
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Correlate", "Scale", "Correlation")
  
  # Create the heatmap
  p <- ggplot(cor_data, aes(x = Scale, y = Correlate, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 9)) +
    scale_x_discrete(labels = c("SoS", "FrC", "FaC", "FrMI", "FaMI", "FrMR", "FaMR", "NC", "SA", "FD", "FH", "He")) +
    scale_y_discrete(labels = c("De", "Ds", "T", "S")) +  
    ggtitle(country)
  
  # Add the plot to the list
  heatmap_list[[country]] <- p
}

# Combine the plots using patchwork, with 6 columns
combined_plot <- wrap_plots(heatmap_list, ncol = 6)

# Print the combined plot
print(combined_plot)

# Correlations of study variables -------------------------------------

#'## Correlations of study variables
#'
#'### Overall
#'
#' **Abbreviations of matrix variables**
#' De = DJGLS-6 emotion, Ds = DJGLS-6 social, T = T-ILS, S = Single-item loneliness measure, 
#' SoS = Social support, He = Health, FD = Feeling depressed, FH = Feeling happy, 
#' FaMI = Family meet in-person, FaMR = Family meet remote, FrMI = Friends meet in-person, FrMR = Friends meet remote, 
#' FaC = Family closeness, FrC = Friends closeness, NC = Neighbours contact, SA = Social activities

# Specify the labels for the variables
var_labels <- c("De", "Ds", "T", "S", "SoS", "FrC", "FaC", "FrMI", "FaMI", "FrMR", 
                "FaMR", "NC", "SA", "FD", "FH", "He")

# Assuming 'data' is your dataset and 'countries' is a vector of country names
# Overall correlation matrix
overallCorrEF <- round(cor(con_data[, c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs", 
                                    "social_support_fs", "friends_n__1__open_fs", "family_n__1__open_fs", 
                                    "friends_meet_face_fs", "family_meet_face_fs", "friends_meet_tele_fs", 
                                    "family_meet_tele_fs", "neighbours_fs", "social_activities_a_fs", 
                                    "feelings_depr_fs", "feelings_happy_fs", "health_general_fs")], 
                           use = "pairwise.complete.obs"), 2)

# Assign the labels to the correlation matrix
rownames(overallCorrEF) <- colnames(overallCorrEF) <- var_labels
overallCorrEF

# Initialize a list to store correlation matrices for each country
corMatricesCountryEF <- list()

# Initialize a vector to store average correlations for each country
avgCorCountryEF <- numeric()

# Calculate correlation matrix for each country
for (country in countries) {
  # Subset data for the country
  subset_data <- con_data[data$country == country, ]
  
  # Compute correlation matrix for the subset
  cor_matrix <- round(cor(subset_data[, c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs", 
                                          "social_support_fs", "friends_n__1__open_fs", "family_n__1__open_fs", 
                                          "friends_meet_face_fs", "family_meet_face_fs", "friends_meet_tele_fs", 
                                          "family_meet_tele_fs", "neighbours_fs", "social_activities_a_fs", 
                                          "feelings_depr_fs", "feelings_happy_fs", "health_general_fs")], 
                          use = "pairwise.complete.obs"), 2)
  
  # Assign the labels to the correlation matrix
  rownames(cor_matrix) <- colnames(cor_matrix) <- var_labels
  
  # Store the matrix in the list
  corMatricesCountryEF[[country]] <- cor_matrix
  
  # Compute and store the average correlation among the first four measures
  avg_cor <- round(mean(cor_matrix[1:4, 1:4][lower.tri(cor_matrix[1:4, 1:4])], na.rm = TRUE), 2)
  avgCorCountryEF[country] <- avg_cor
}

#'### Per-country
#'
# Print the correlation matrices for each country
corMatricesCountryEF

#'### Per-country average correlation among loneliness measures
#'
# Print the average correlation for each country
avgCorCountryEF

#'#### Session info
sessionInfo()
'R version 4.5.1 (2025-06-13 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 22631)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=French_Belgium.utf8  LC_CTYPE=French_Belgium.utf8    LC_MONETARY=French_Belgium.utf8 LC_NUMERIC=C                   
[5] LC_TIME=French_Belgium.utf8    

time zone: Europe/Brussels
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] mixmgfa_0.2.0          patchwork_1.3.1        magrittr_2.0.3         semTools_0.5-7         semPlot_1.1.6         
 [6] openxlsx_4.2.8         lavaan_0.6-19          EFA.dimensions_0.1.8.4 psych_2.5.6            forcats_1.0.0         
[11] stringr_1.5.1          dplyr_1.1.4            purrr_1.0.4            tidyr_1.3.1            tibble_3.3.0          
[16] ggplot2_3.5.2          tidyverse_2.0.0        lubridate_1.9.4        readr_2.1.5           

loaded via a namespace (and not attached):
  [1] splines_4.5.1        later_1.4.2          R.oo_1.27.1          XML_3.99-0.18        rpart_4.1.24         lifecycle_1.0.4     
  [7] Rdpack_2.6.4         vroom_1.6.5          mirt_1.44.0          globals_0.18.0       processx_3.8.6       lattice_0.22-7      
 [13] MASS_7.3-65          rockchalk_1.8.157    backports_1.5.0      Hmisc_5.2-3          rmarkdown_2.29       remotes_2.5.0       
 [19] httpuv_1.6.16        qgraph_1.9.8         zip_2.3.3            sessioninfo_1.2.3    pkgbuild_1.4.8       pbapply_1.7-2       
 [25] minqa_1.2.8          RColorBrewer_1.1-3   abind_1.4-8          pkgload_1.4.0        audio_0.1-11         quadprog_1.5-8      
 [31] R.utils_2.13.0       nnet_7.3-20          listenv_0.9.1        testthat_3.2.3       vegan_2.7-1          arm_1.14-4          
 [37] parallelly_1.45.0    permute_0.9-8        codetools_0.2-20     tidyselect_1.2.1     farver_2.1.2         lme4_1.1-37         
 [43] stats4_4.5.1         base64enc_0.1-3      polycor_0.8-1        ellipsis_0.3.2       progressr_0.15.1     Formula_1.2-5       
 [49] tools_4.5.1          Rcpp_1.1.0           glue_1.8.0           mnormt_2.1.1         gridExtra_2.3        xfun_0.52           
 [55] mgcv_1.9-3           admisc_0.38          usethis_3.1.0        withr_3.0.2          beepr_2.0            fastmap_1.2.0       
 [61] boot_1.3-31          callr_3.7.6          digest_0.6.37        mi_1.1               timechange_0.3.0     R6_2.6.1            
 [67] mime_0.13            colorspace_2.1-1     gtools_3.9.5         jpeg_0.1-11          R.methodsS3_1.8.2    generics_0.1.4      
 [73] data.table_1.17.6    corpcor_1.6.10       SimDesign_2.19.2     htmlwidgets_1.6.4    pkgconfig_2.0.3      sem_3.1-16          
 [79] gtable_0.3.6         brio_1.1.5           htmltools_0.5.8.1    carData_3.0-5        profvis_0.4.0        scales_1.4.0        
 [85] png_0.1-8            reformulas_0.4.1     knitr_1.50           rstudioapi_0.17.1    tzdb_0.5.0           reshape2_1.4.4      
 [91] coda_0.19-4.1        checkmate_2.3.2      nlme_3.1-168         curl_6.4.0           nloptr_2.2.1         cachem_1.1.0        
 [97] parallel_4.5.1       miniUI_0.1.2         foreign_0.8-90       desc_1.4.3           pillar_1.11.0        grid_4.5.1          
[103] vctrs_0.6.5          urlchecker_1.0.1     promises_1.3.3       OpenMx_2.22.7        xtable_1.8-4         Deriv_4.2.0         
[109] cluster_2.1.8.1      dcurver_0.9.2        GPArotation_2025.3-1 htmlTable_2.4.3      evaluate_1.0.4       pbivnorm_0.6.0      
[115] mvtnorm_1.3-3        cli_3.6.5            kutils_1.73          compiler_4.5.1       crayon_1.5.3         rlang_1.1.6         
[121] future.apply_1.20.0  labeling_0.4.3       fdrtool_1.2.18       ps_1.9.1             plyr_1.8.9           fs_1.6.6            
[127] stringi_1.8.7        devtools_2.4.5       lisrelToR_0.3        Matrix_1.7-3         hms_1.1.3            glasso_1.11         
[133] bit64_4.6.0-1        future_1.58.0        shiny_1.11.1         rbibutils_2.3        igraph_2.1.4         memoise_2.0.1       
[139] RcppParallel_5.1.10  bit_4.6.0 '