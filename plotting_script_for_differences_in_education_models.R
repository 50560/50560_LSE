#this script was used to plot the difference results in persuasiveness for the three primary issues by how education was conceptualised.

#load packages
library(tibble)
library(dplyr)
library(ggplot2)
#1. immigration
meta_summary_dual <- tibble(
  model = rep(c("Model 1: ≤ School", "Model 2: <High School", "Model 3: <Some College"), each = 2),
  type = rep(c("Immigration x High Eductaion", "Immigration x Low Eductaion"), times = 3),
  estimate = c(0.394, -1.448, 0.55, -0.684, 0.009, 0.515), #this was the raw data from the regressions previously run
  std.error = c(0.371, 0.730, 0.524, 0.896, 0.68, 0.872)
) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )




ggplot(meta_summary_dual, aes(x = model, y = estimate, color = type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Effect of Ad Type by Education Stratification Model: Immigration",
    x = "Education Model",
    y = "Effect Size (Estimate ± 95% CI)",
    color = "Variables"
  ) +
  theme_minimal(base_size = 13)


#2. blm
meta_summary_dual <- tibble(
  model = rep(c("Model 1: ≤ School", "Model 2: <High School", "Model 3: <Some College"), each = 2),
  type = rep(c("Immigration x High Eductaion", "Immigration x Low Eductaion"), times = 3),
  estimate = c(-0.238, 0.312, -0.291, 0.246, -0.372, 0.257),
  std.error = c(0.206, 0.722, 0.246, 0.41, 0.315, 0.404)
) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )


ggplot(meta_summary_dual, aes(x = model, y = estimate, color = type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Effect of Ad Type by Education Stratification Model:BLM",
    x = "Education Model",
    y = "Effect Size (Estimate ± 95% CI)",
    color = "Variables"
  ) +
  theme_minimal(base_size = 13)



#3. foreign policy
meta_summary_dual <- tibble(
  model = rep(c("Model 1: ≤ School", "Model 2: <High School", "Model 3: <Some College"), each = 2),
  type = rep(c("Immigration x High Eductaion", "Immigration x Low Eductaion"), times = 3),
  estimate = c(-0.146, 0.176, -0.499, 2.074, 0.078, 0.084),
  std.error = c(0.402, 1.233, 0.479, 0.791, 0.608, 0.784)
) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )




ggplot(meta_summary_dual, aes(x = model, y = estimate, color = type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Effect of Ad Type by Education Stratification Model:Foreign Policy",
    x = "Education Model",
    y = "Effect Size (Estimate ± 95% CI)",
    color = "Variables"
  ) +
  theme_minimal(base_size = 13)







