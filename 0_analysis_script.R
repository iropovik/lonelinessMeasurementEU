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
source("MixtureMG_FA.R")
source("MixtureMG_FA_intercepts.R")
source("MixtureMG_FA_loadings.R")
source("MixtureMG_FA_loadingsandintercepts.R")
set.seed(1)

# Function that rounds numeric values in a data frame
roundDf <- function(df) {
  as.data.frame(lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x))
}
# Data --------------------------------------------------------------------
# Change the value to "confirmatory_data.csv" to run the analyses on the confirmatory dataset
filename <- "exploratory_data.csv"
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
write.csv(statisticsByCountryEF, "output_statistics_by_country.csv")

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
write.csv(summaryFitIC, "model_fit_by_country.csv")


#'# Measurement invariance
#'
# Measurement invariance --------------------------------------------------

fitMeasuresSelection <- c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")

#'## DJGLS-6
#----------------
#DJGLS-6
#----------------

# Testing for measurement invariance (configural; metric; scalar) using multigroup confirmatory factor analysis
# In case measurement invariance fails at any level, we resort to mixture multigroup factor analysis to unravel clusters of countries invariant at the scalar level
data_djg <- subset(data, select = c(country, gender, age_class, loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f))
data_djg <- na.omit(data_djg)

#'### Configural invariance
djg_config <- cfa(model = djg_model_2F,
                  data = data_djg,
                  ordered = TRUE,
                  estimator = "WLSMV",
                  group = "country")
(djgInvConfFit <- fitmeasures(djg_config, fit.measures = fitMeasuresSelection))

#recode the coutry names to codes
data_djg[[1]] <- as.numeric(as.factor(data_djg[[1]]))

#The code below is to be run in case measurement invariance failed at any of three levels above
#+ include=FALSE
djgMmgEF <- MixtureMG_FA(data = as.data.frame(data_djg[,-c(2,3)]),
        cluster.spec = c("loadings", "intercepts"),
        nsclust = c(1,6), #invariant at the scalar level
        nfactors = djg_factors, #number of factors to be determined following the factor analyses // mixmgfa is suppposed to identify the appropriate factor structure automatically (to be confirmed by asking Kim De Roover)
        Maxiter = 5000, #default values
        nruns = 50, #default value; (important setting to avoid local maxima in case of few groups and/or small groups)
        design = 0, #default value (use of EFAs)
        #rotation = "oblimin", #parameter to add in case nfactors = 2
        preselect = 10)

clusters <- as.data.frame(round(djgMmgEF$MMGFA_solutions[[3]]$clustermemberships, 3))
rownames(clusters) <- countries

# Choose the best number of clusters ('K_best') based on the BIC_G and CHull scree ratios and the plots. For plots, use 'plot(OutputObject$overview)'.
# Based on the BIC_G, look for the number of clusters that minimizes the BIC_G or that corresponds to an elbow point in the BIC_G plot (after which the decrease with additional clusters levels off).
# Based on the CHull (scree ratios AND plot), look for the number of clusters that has the maximal scree ratio AND check whether this corresponds to at least a mild elbow point in the lower plot.
# Access the corresponding cluster memberships and parameter estimates by using OutputObject$MMGFAsolutions[[K_best]]$clustermemberships and, for example, OutputObject$MMGFAsolutions[[K_best]]$clusterspecific.loadings.
# The parameter sets are further subdivided in group- and/or cluster-specific parameter sets.

# Here we define the clusters of countries invariant at the scalar level, and subsequently test measurement invariance (configural, metric, scalar) using multigroup confirmatory factor analysis on them
djg_cluster_a_names <- rownames(clusters)[which(clusters$Cluster_1 >= 0.5)] #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(1, 4, 5, 9, 10))
djg_cluster_b_names <- rownames(clusters)[which(clusters$Cluster_2 >= 0.5)] #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(2, 3, 7, 11, 12))
djg_cluster_c_names <- rownames(clusters)[which(clusters$Cluster_3 >= 0.5)] #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(6, 8, 13, 14))

djg_cluster_a <- which(clusters$Cluster_1 >= 0.5) #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(1, 4, 5, 9, 10))
djg_cluster_b <- which(clusters$Cluster_2 >= 0.5) #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(2, 3, 7, 11, 12))
djg_cluster_c <- which(clusters$Cluster_3 >= 0.5) 

djg_clusters_list <- list(cluster_a = djg_cluster_a, 
                          cluster_b = djg_cluster_b, 
                          cluster_c = djg_cluster_c)

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

clusters <- c("cluster_a", "cluster_b", "cluster_c")

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
write.csv(djgMmgResultsEF, "clusters_invariance.csv", row.names = FALSE)

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
  clusterB = djg_cluster_b,
  clusterC = djg_cluster_c
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
  scalar_fit <- fitmeasures(scalar_model, fit.measures = fitMeasuresSelection)
  
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

# Loop through each construct, fit the CFA model, and store factor scores
for (construct in names(models)) {
  # Filter data for current construct, ensuring all specified variables are complete and have variance
  temp_data <- na.omit(data[relevant_vars_per_construct[[construct]]])

  if(construct == "social_support") {
    estimator <- "MLR"
    ordered <- FALSE
  } else {
    estimator <- "WLSMV"
    ordered <- TRUE
  }

  if(nrow(temp_data) > 0) {
    fit <- cfa(models[[construct]], data = temp_data, estimator = estimator, ordered = ordered)
    # Factor scores will be NA for rows/observations removed by na.omit
    data[[paste0(construct, "_lv")]] <- NA
    valid_indices <- which(complete.cases(data[relevant_vars_per_construct[[construct]]]))
    data[valid_indices, paste0(construct, "_lv")] <- lavPredict(fit, type = "lv")[,1]
  } else {
    warning(paste("No valid data for construct:", construct))
  }
}

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
  temp_data <- na.omit(data[, relevant_vars_per_construct[[construct]]])

  if(construct == "social_support") {
    estimator <- "MLR"
    ordered <- FALSE
  } else {
    estimator <- "WLSMV"
    ordered <- TRUE
  }
  
  if(nrow(temp_data) > 0) {
    fit <- cfa(models[[construct]], data = temp_data, estimator = estimator, ordered = ordered)
    # Initialize factor score columns for single-indicator constructs with NA
    data[[paste0(construct, "_fs")]] <- NA  
    valid_indices <- which(complete.cases(data[, relevant_vars_per_construct[[construct]]]))
    # Extract factor scores for valid indices
    data[valid_indices, paste0(construct, "_fs")] <- lavPredict(fit, type = "lv")[,1]
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
freq_tables_list <- list()

for (set_name in names(variableList)) {
  variables <- variableList[[set_name]]
  
  # Initialize a list for frequency tables of the current variable set
  freq_tables <- list()
  
  # Iterate over the variables to create and store their frequency tables
  for (variable in variables) {
    # Store frequency table in the list instead of printing
    freq_tables[[variable]] <- table(data[[variable]], useNA = "ifany")
  }
  
  # Store the frequency tables list for the current set
  freq_tables_list[[set_name]] <- freq_tables
  
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
    temp_data <- data[data$country_chr == country, ]
    
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      for (secondary_measure in variables) {
        
        # Ensure measures are numeric
        if(is.numeric(temp_data[[primary_measure]]) && is.numeric(temp_data[[secondary_measure]])) {
          valid_data_indices <- complete.cases(temp_data[, c(primary_measure, secondary_measure)])
          
          if (sum(valid_data_indices) > 0) {
            valid_data <- temp_data[valid_data_indices, ]
            
            # Compute Pearson correlation
            pearson_cor <- cor(valid_data[[primary_measure]], valid_data[[secondary_measure]], use = "pairwise.complete.obs")
            
            # Fit linear model and compute summary
            linear_model_formula <- as.formula(paste(secondary_measure, "~", primary_measure))
            linear_model <- lm(linear_model_formula, data = valid_data)
            summary_linear_model <- summary(linear_model)
            p_value <- summary_linear_model$coefficients[2, 4]
            
            # Store results
            nom_network_per_country[[country]][[primary_measure]][[secondary_measure]] <- list(p_value = p_value, pearson_cor = pearson_cor)
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
                           p_value = numeric(), 
                           pearson_cor = numeric(), 
                           stringsAsFactors = FALSE)
  
  # Loop through each level of the nested list structure for the current variable set
  for (country in names(nom_network_per_country)) {
    for (primary_measure in names(nom_network_per_country[[country]])) {
      for (secondary_measure in names(nom_network_per_country[[country]][[primary_measure]])) {
        # Extract the list containing p_value and pearson_cor
        measure_data <- nom_network_per_country[[country]][[primary_measure]][[secondary_measure]]
        if(is.list(measure_data) && !is.null(measure_data[['p_value']]) && !is.null(measure_data[['pearson_cor']])) {
          p_value <- measure_data[['p_value']]
          pearson_cor <- measure_data[['pearson_cor']]
        } else {
          p_value <- NA
          pearson_cor <- NA
        }
        
        # Append to the results data frame
        results_df <- rbind(results_df, data.frame(country, primary_measure, secondary_measure, pearson_cor, p_value, stringsAsFactors = FALSE))
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

# Function to determine if correlation is in the expected direction and meets criteria
is_cor_meeting_criteria <- function(cor, measure, p_value, expected_positive, expected_negative) {
  if (p_value <= 0.004 && abs(cor) >= 0.10) {
    if (measure %in% expected_positive && cor > 0) {
      return(TRUE)
    } else if (measure %in% expected_negative && cor < 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Adapt the loop to work with results from each set in variableList
proportions_list <- list()

for (set_name in names(variableList)) {
  results_df <- results_list[[set_name]]  # Retrieve the results dataframe for the current set
  
  # Following your original analysis but scoped within the current set of variables
  for (measure_set_name in names(measure_sets)) {
    measure_group <- measure_sets[[measure_set_name]]
    
    filtered_results <- results_df %>%
      filter(primary_measure %in% measure_group) %>%
      mutate(meets_criteria = mapply(is_cor_meeting_criteria, pearson_cor, secondary_measure, p_value, 
                                     MoreArgs = list(expected_positive = expected_direction$positive, 
                                                     expected_negative = expected_direction$negative)))
    
    proportions <- filtered_results %>%
      group_by(country) %>%
      summarize(proportion = mean(meets_criteria), .groups = 'drop')
    
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
write.xlsx(combined_proportions_df, "nomologicalNetCorr.xlsx")

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
  
  # Populate the matrix with correlation values
  for (row in seq_along(primary_measures)) {
    for (col in seq_along(secondary_measures)) {
      cor_value <- country_data$pearson_cor[country_data$primary_measure == primary_measures[row] & country_data$secondary_measure == secondary_measures[col]]
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
overallCorrEF <- round(cor(data[, c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs", 
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
  subset_data <- data[data$country == country, ]
  
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
