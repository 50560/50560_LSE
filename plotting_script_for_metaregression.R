# This script was used to produce figure x which took the raw regression outputs and transformed them into to plots to visualise how the coefficient chnaged based on what was interacted.
#load package
library(ggplot2)

# Extract summary from the previous run model for example:
model_summary <- summary(meta_fit_race_immigration_interaction)

# Build a data frame with the desired variable names 
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# Clean up term names to ones that will be plotted
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-Immigration)",
                            "Immigration Issue (Effect for racegroup)",
                            "racegroup (Effect on Non-Immigration)",
                            "Interaction: racegroup × Immigration"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - Immigration",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






# This is a repetition on above using the second primary variable of interest, BLM


model_summary <- summary(meta_fit_race_blm_interaction)

# Build a data frame 
plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

# term names
plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-BLM)",
                            "BLM Issue (Effect for racegroup)",
                            "racegroup (Effect on Non-BLM)",
                            "Interaction: racegroup × BLM"
                          ))

# Plot
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - BLM",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))








# This is a repetition on above using the third primary variable of interest, Foreign Policy
model_summary <- summary(meta_fit_race_forignp_interaction)


plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)


plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-Foreign_p)",
                            "Foreign_p Issue (Effect for racegroup)",
                            "racegroup (Effect on Non-Foreign_p)",
                            "Interaction: racegroup × Foreign_p"
                          ))

# Plot
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - Foreign_p",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))




# The next 5 plots are for the secondary variables of interest and are a repetition of above 

#decency


model_summary <- summary(meta_fit_race_dec_interaction)


plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)


plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-Decency)",
                            "Decency Issue (Effect for racegroup)",
                            "racegroup (Effect on Non-Decency)",
                            "Interaction: racegroup × Decency"
                          ))


ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - Decency",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))







#fear

model_summary <- summary(meta_fit_fear)


plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)


plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-Fear)",
                            "Fear (Effect for racegroup)",
                            "racegroup (Effect on Non-Fear)",
                            "Interaction: racegroup × Fear"
                          ))


ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - Fear",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






#Disgust
model_summary <- summary(meta_fit_disgust)


plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)

plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-Disgust)",
                            "Disgust (Effect for racegroup)",
                            "racegroup (Effect on Non-Disgust)",
                            "Interaction: racegroup × Disgust"
                          ))


ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - Disgust",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






#Combined Primary 


model_summary <- summary(meta_fit_combined_immigration_issue)


plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)


plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-Primary)",
                            "Primary Issues (Effect for racegroup)",
                            "racegroup (Effect on Non-Primary)",
                            "Interaction: racegroup × Primary"
                          ))


ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - Primary Issues",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))




# National Security

model_summary <- summary(meta_fit_combined_immigration_foreignp)


plot_data <- data.frame(
  Term = rownames(model_summary$beta),
  Estimate = model_summary$beta[, 1],
  SE = model_summary$se,
  CI.lb = model_summary$ci.lb,
  CI.ub = model_summary$ci.ub,
  pval = model_summary$pval
)


plot_data$Label <- factor(plot_data$Term, levels = plot_data$Term,
                          labels = c(
                            "Intercept (racegroup, Non-National_Security)",
                            "National_Security Issue (Effect for racegroup)",
                            "racegroup (Effect on Non-National_Security)",
                            "Interaction: racegroup × National_Security"
                          ))


ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Meta-Regression Coefficients for racegroup - National Security",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

