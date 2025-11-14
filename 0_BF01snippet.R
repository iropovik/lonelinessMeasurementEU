compute_bf_correlation <- function(data_exp, data_conf, var1, var2) {
  # data_exp: exploratory dataset
  # data_conf: confirmatory dataset
  # var1, var2: The names (as strings) of the two variables to be correlated

  # Add a group identifier and combine the datasets
  data_exp$group <- "exploratory"
  data_conf$group <- "confirmatory"
  combined_data <- rbind(data_exp, data_conf)

  # Model H1: Allow the correlation between var1 and var2 to differ by group
  model_H1 <- sprintf('
    %s ~~ c(r_exp, r_conf)*%s
  ', var1, var2)
  fit_H1 <- sem(model_H1, data = combined_data, group = "group")

  # Model H0: Constrain the correlations to be equal across groups
  model_H0 <- sprintf('
    %s ~~ c(r_equal, r_equal)*%s
  ', var1, var2)
  fit_H0 <- sem(model_H0, data = combined_data, group = "group")

  # Compute BIC values for both models
  BIC_H1 <- BIC(fit_H1)
  BIC_H0 <- BIC(fit_H0)

  # Calculate the Bayes Factor (BF01) using the BIC difference (Wagenmakers, 2007)
  delta_BIC <- BIC_H1 - BIC_H0
  BF01 <- exp(delta_BIC / 2)

  # Compute the posterior probability, assuming equal (1:1) prior odds
  posterior <- BF01 / (1 + BF01)

  list(BIC_H0 = BIC_H0,
       BIC_H1 = BIC_H1,
       BF01 = BF01,
       posterior = posterior)
}

# Example
'set.seed(123)
n_exp <- 100
n_conf <- 100
rho1 <- 0.4    # True correlation in expl data
rho2 <- 0.3    # True correlation in conf data

data_exp <- data.frame(varA = rnorm(n_exp))
data_exp$varB <- rho1 * data_exp$varA + sqrt(1 - rho1^2) * rnorm(n_exp)

data_conf <- data.frame(varA = rnorm(n_conf))
data_conf$varB <- rho2 * data_conf$varA + sqrt(1 - rho2^2) * rnorm(n_conf)

bf_results <- compute_bf_correlation(data_exp, data_conf, "varA", "varB")
print(bf_results$BF01)'