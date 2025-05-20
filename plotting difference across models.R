meta_summary_immi <- tibble(
  model = c("Model 1: ≤ School", "Model 2: <High School", "Model 3: <Some College"),
  estimate = c(-1.448, -0.684, 0.515),
  std.error = c(0.602, 0.524, 0.395)
) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )


library(ggplot2)

ggplot(meta_summary_immi, aes(x = model, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +  # horizontal layout
  labs(
    title = "Meta-Regression Estimates by Education Cutoff",
    x = "Model (Education Level Cutoff)",
    y = "Estimated Effect (with 95% CI)"
  ) +
  theme_minimal()





##########BLM




meta_summary_blm <- tibble(
  model = c("Model 1: ≤ School", "Model 2: <High School", "Model 3: <Some College"),
  estimate = c(0.312, 0.246, 0.257),
  std.error = c(0.722, 0.410, 0.404)
) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )


library(ggplot2)

ggplot(meta_summary_blm, aes(x = model, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +  # horizontal layout
  labs(
    title = "Meta-Regression Estimates by Education Cutoff",
    x = "Model (Education Level Cutoff)",
    y = "Estimated Effect (with 95% CI)"
  ) +
  theme_minimal()







##########fori_P




meta_summary_for <- tibble(
  model = c("Model 1: ≤ School", "Model 2: <High School", "Model 3: <Some College"),
  estimate = c(0.176, 2.074, 0.084),
  std.error = c(1.233, 0.791, 0.784)
) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )


library(ggplot2)

ggplot(meta_summary_for, aes(x = model, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +  # horizontal layout
  labs(
    title = "Meta-Regression Estimates by Education Cutoff",
    x = "Model (Education Level Cutoff)",
    y = "Estimated Effect (with 95% CI)"
  ) +
  theme_minimal()
