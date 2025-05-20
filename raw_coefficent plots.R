##white/non wite

##raw plot: # Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_race_immigration_interaction)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (non-Hispanic, Non-Immigration)",
                            "Immigration Issue (Effect for non-Hispanic)",
                            "Hispanic (Effect on Non-Immigration)",
                            "Interaction: Hispanic × Immigration"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for Hispanic/Non-Hispanic - Immigration",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






##raw plot:blm  # Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_race_blm_interaction)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Hispanic, Non-BLM)",
                            "BLM Issue (Effect for Non-Hispanic)",
                            "White (Effect on Non-BLM)",
                            "Interaction: White × BLM"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Hispanic - BLM",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))








##raw plot: forp# Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_race_forignp_interaction)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Hispanic, Non-Foreign_p)",
                            "Foreign_p Issue (Effect for Hispanic)",
                            "White (Effect on Non-Foreign_p)",
                            "Interaction: White × Foreign_p"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Hispanic - Foreign_p",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






##raw plot: decen# Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_race_dec_interaction)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Black, Non-Decency)",
                            "Decency Issue (Effect for Black)",
                            "White (Effect on Non-Decency)",
                            "Interaction: White × Decency"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Black - Decency",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))







##raw plot:fear # Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_fear)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Hispanic, Non-Fear)",
                            "Fear (Effect for Hispanic)",
                            "White (Effect on Non-Fear)",
                            "Interaction: White × Fear"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Hispanic - Fear",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






##raw plot: disgust# Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_disgust)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Hispanic, Non-Disgust)",
                            "Disgust (Effect for Hispanic)",
                            "White (Effect on Non-Disgust)",
                            "Interaction: White × Disgust"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Hispanic - Disgust",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






##raw plot: #combined Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_combined_immigration_issue)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Hispanic, Non-Primary)",
                            "Primary Issues (Effect for Hispanic)",
                            "White (Effect on Non-Primary)",
                            "Interaction: White × Primary"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Hispanic - Primary Issues",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






##raw plot: # Load required library
library(ggplot2)

# Extract fixed-effect summary
model_summary <- summary(meta_fit_combined_immigration_foreignp)

# Build a data frame with fixed effect estimates and CIs
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Optional: Clean up term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (Hispanic, Non-National_Security)",
                            "National_Security Issue (Effect for Hispanic)",
                            "White (Effect on Non-National_Security)",
                            "Interaction: White × National_Security"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for White/Hispanic - National Security",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

