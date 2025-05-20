#load in packages
library(tidyverse)
library(metafor)
library(broom)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(metafor)
library(Matrix)



# Read data in
setwd("/Users/laurahurn/Desktop/LSE/diss/replication_archive/output/processed_data")
df_responses2020 <- readRDS("responses.rds") %>% filter(dataset_year == "2020")
df_tags2020 <- readRDS("tagging_2020.rds")

list_study_ids <- df_responses2020$study_id %>% unique



#1. race subset, white or none white  
df_responses2020$race_binary <- recode(df_responses2020$ethnicity,
                                       "white" = 1,
                                       .default = 0)

df_filtered <- df_responses2020 %>%
  filter(!is.na(race_binary))   
# Create two subsets
df_white <- df_filtered %>%
  filter(race_binary == 1)

df_nonwhite <- df_filtered %>%
  filter(race_binary == 0)
table(df_nonwhite$ethnicity)

#white group
list_study_ids_white <- df_white$study_id %>% unique

# Get studies in to df
df_sample_white <-
  df_white %>% 
  filter(study_id %in% list_study_ids_white) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model comparing each content_id to the control group 
df_sample_combined_fav_choice_white <- df_sample_white %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )


# Fitting the model 
out_loop_white <- map(list_study_ids_white, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_white %>% filter(study_id == .x)
  
  # Skip if no data exists for this study_id
  if (nrow(df_filtered) == 0) {
    return(NULL)
  }
  
  # Fit the linear model 
  lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
  
  # Tidying up the estimates
  tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
  
  # Extract the variance-covariance matrix
  vcov_matrix <- vcov(lm_fit)
  vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
  
  # Return the tidied estimates and covariance matrix
  list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
})

# Remove NULLs if any were skipped
out_loop_white <- compact(out_loop_white)

# Combine tidied estimates across all studies
tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates)

# Combine the matrices
giant_vcov_matrix_white <- map(out_loop_white, function(.x) .x$vcov_matrix) %>% bdiag()

# Check if symmetric
stopifnot(isSymmetric(giant_vcov_matrix_white))

# Check if the standard errors from the model match those from the tidy results
stopifnot(identical(giant_vcov_matrix_white %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_white$std.error %>% round(10)))

# check for 1 study id
out_loop_white[[1]]$tidied_estimates
out_loop_white[[1]]$vcov_matrix

tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates)
# Then we can loop over the covariance matrices and bdiag them all together at the end


#################################
#2. subset for nonwhite group
list_study_ids_nonwhite <- df_nonwhite$study_id %>% unique

# Get studies in to df
df_sample_nonwhite <-
  df_nonwhite %>% 
  filter(study_id %in% list_study_ids_nonwhite) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model 
df_sample_combined_fav_choice_nonwhite <- df_sample_nonwhite %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )


# Fitting the model 
out_loop_nonwhite<- map(list_study_ids_nonwhite, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_nonwhite %>% filter(study_id == .x)
  
  # Skip if no data exists for this study_id
  if (nrow(df_filtered) == 0) {
    return(NULL)
  }
  
  # Fit the linear model using the correct filtered data
  lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
  
  # Tidying up the estimates
  tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
  
  # Extract the variance-covariance matrix
  vcov_matrix <- vcov(lm_fit)
  vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
  
  # Return the tidied estimates and covariance matrix
  list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
})

# Remove NULLs if any were skipped
out_loop_nonwhite <- compact(out_loop_nonwhite)


out_loop_nonwhite[[1]]$tidied_estimates
out_loop_nonwhite[[1]]$vcov_matrix

tidied_estimates_nonwhite <- map_dfr(out_loop_nonwhite, function(x) x$tidied_estimates)

giant_vcov_matrix_nonwhite <-
  map(out_loop_nonwhite,
      function(.x) {
        .x$vcov_matrix
      }) %>% 
  bdiag()

stopifnot(isSymmetric(giant_vcov_matrix_nonwhite))
stopifnot(identical(giant_vcov_matrix_nonwhite %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_nonwhite$std.error %>% round(10)))


# Sanity checks
final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonwhite, giant_vcov_matrix_white)
stopifnot(isSymmetric(final_giant_vcov_matrix))
# Extract standard errors from covariance matrix
computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
# Combine and chcekc
expected_se <- c(tidied_estimates_nonwhite$std.error, tidied_estimates_white$std.error) %>% round(10)
stopifnot(identical(computed_se, expected_se))

#META reg by race
tidied_estimates_white <- tidied_estimates_white %>%
  mutate(race_binary = "white")

tidied_estimates_nonwhite <- tidied_estimates_nonwhite %>%
  mutate(race_binary = "nonwhite")

# Combine estimates from both racial groups
lm_estimates_race <- bind_rows(tidied_estimates_nonwhite,tidied_estimates_white) %>%
  mutate(
    content_id = gsub("factor\\(content_id\\)", "", term),  # Clean term names
    race_binary = factor(race_binary, levels = c("nonwhite", "white"))  # Set nonwhite as reference
  ) %>%
  left_join(df_tags2020, by = "content_id")  # Merge with tags dataset

# Meta-regressions model with race level as a moderator
meta_fit_race_immigration_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_immigrant*race_binary,  
  data = lm_estimates_race
)
summary(meta_fit_race_immigration_interaction)

meta_fit_race_blm_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_blm_race*race_binary,  
  data = lm_estimates_race
)
summary(meta_fit_race_blm_interaction)



meta_fit_race_forignp_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_foreign_p*race_binary, 
  data = lm_estimates_race
)
summary(meta_fit_race_forignp_interaction)


meta_fit_race_dec_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_decency*race_binary,  
  data = lm_estimates_race
)
summary(meta_fit_race_dec_interaction)
#sink("meta_fit_race_dec_interaction.txt")
#print(meta_fit_race_dec_interaction)
#sink()

meta_fit_disgust <- rma.mv(yi = estimate,   
                           V = final_giant_vcov_matrix,      
                           mods = ~ emotion_disgust*race_binary, 
                           data = lm_estimates_race)
summary(meta_fit_disgust)



meta_fit_fear <- rma.mv(yi = estimate,   
                        V = final_giant_vcov_matrix,      
                        mods = ~ emotion_fear*race_binary,  
                        data = lm_estimates_race)
summary(meta_fit_fear)

lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant

meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                              V = final_giant_vcov_matrix,      
                                              mods = ~ immigrationissue_combined*race_binary,  
                                              data = lm_estimates_race)
summary(meta_fit_combined_immigration_issue)


#immigration and foreign p
lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p

meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                 V = final_giant_vcov_matrix,      
                                                 mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                 data = lm_estimates_race)
summary(meta_fit_combined_immigration_foreignp)







#visualisations 


#  Store model objects and labels in a list
library(metafor)
library(dplyr)
library(ggplot2)

model_list <- list(
  Immigration = meta_fit_race_immigration_interaction,
  BLM = meta_fit_race_blm_interaction,
  ForeignPolicy = meta_fit_race_forignp_interaction,
  Decency = meta_fit_race_dec_interaction,
  Disgust = meta_fit_disgust,
  Fear = meta_fit_fear,
  CombinedImmigration = meta_fit_combined_immigration_issue,
  CombinedImmigrationForeign = meta_fit_combined_immigration_foreignp
)


#  empty dataframe for results
results_df <- data.frame()

# loop
for (issue_name in names(model_list)) {
  
  model <- model_list[[issue_name]]
  
  # Get coefficients and vcov
  coef_est <- coef(model)
  vcov_mat <- vcov(model)
  
  # Contrast vectors
c_white <- c(0, 0, 0, 1)      # Intercept + immigrant + race_binary + interaction
  c_nonwhite <- c(0, 1, 0, 0)   # Intercept + immigrant only (race_binary = 0)
  

  
  # extract Coeff Estimates
  est_white <- sum(c_white * coef_est)
  est_nonwhite <- sum(c_nonwhite * coef_est)
  
  # extract SE
  se_white <- sqrt(t(c_white) %*% vcov_mat %*% c_white)
  se_nonwhite <- sqrt(t(c_nonwhite) %*% vcov_mat %*% c_nonwhite)
  
  # Bind results into 1dataframe
  temp_df <- data.frame(
    Issue = rep(issue_name, 2),
    Race = c("White", "Nonwhite"),
    Estimate = c(est_white, est_nonwhite),
    SE = c(se_white, se_nonwhite)
  )
  
  results_df <- rbind(results_df, temp_df)
}

# factor order
results_df$Issue <- factor(results_df$Issue, levels = names(model_list))
results_df$Race <- factor(results_df$Race, levels = c("White", "Nonwhite"))

#  Plot
ggplot(results_df, aes(x = Issue, y = Estimate, fill = Race)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE),
                width = 0.2, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("White" = "#4C72B0", "Nonwhite" = "#ff474c")) +
  labs(title = "Effect of Issues by Race Group:Primary",
       x = "Issue",
       y = "Estimated Effect (with 95% CI)",
       fill = "Race") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

theme(legend.position = "none")



#other visualisation for raw estimates
library(metafor)
library(ggplot2)



# Extract estimates and vcov matrix
coefs <- coef(meta_fit_race_blm_interaction)
vcov_mat <- vcov(meta_fit_race_blm_interaction)

# Extract relevant estimates
est_issue <- coefs["issue_blm_race"]
est_inter <- coefs["issue_blm_race:race_binarywhite"]

# Extract relevant variances and covariance
var_issue <- vcov_mat["issue_blm_race", "issue_blm_race"]
var_inter <- vcov_mat["issue_blm_race:race_binarywhite", "issue_blm_race:race_binarywhite"]
covar <- vcov_mat["issue_blm_race", "issue_blm_race:race_binarywhite"]

# Calculate SEs
se_issue <- sqrt(var_issue)
se_white <- sqrt(var_issue + var_inter + 2 * covar)

# Create data frame with effects and SEs
df <- data.frame(
  race = c("Black", "White"),
  effect = c(est_issue, est_issue + est_inter),
  se = c(se_issue, se_white)
)

# Calculate 95% CI
df$ci.lb <- df$effect - 1.96 * df$se
df$ci.ub <- df$effect + 1.96 * df$se

# Plot
ggplot(df, aes(x = race, y = effect, fill = race)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.2) +
  labs(title = "Effect of BLM/Race Issue by Race",
       x = "Race",
       y = "Moderator Effect (Issue: BLM/Race)") +
  theme_minimal() +
  theme(legend.position = "none")






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
                            "Intercept (Nonwhite, Non-Immigration)",
                            "Immigration Issue (Effect for Nonwhite)",
                            "White (Effect on Non-Immigration)",
                            "Interaction: White × Immigration"
                          ))

# Plot raw coefficients
ggplot(plot_data, aes(x = Label, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI.lb, ymax = CI.ub), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Raw Meta-Regression Coefficients",
    x = "Model Term",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






######further check looking at black vs white only
df_responses2020 <- df_responses2020 %>%
  filter(ethnicity %in% c("white", "black")) %>%
  mutate(
    race_binary_b_w = recode(ethnicity, "white" = 1, "black" = 0)
  )

# 3. Combine favorability and vote choice
df_responses2020 <- df_responses2020 %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,
      !is.na(favorability) ~ favorability,
      !is.na(votechoice) ~ votechoice,
      TRUE ~ NA_real_
    )
  )

# Split into black and white groups
list_race_groups <- df_responses2020 %>%
  filter(!is.na(combined_outcome)) %>%
  group_split(race_binary_b_w)

# Define function to fit models
fit_models_by_study <- function(df) {
  list_study_ids <- unique(df$study_id)
  
  out_loop <- map(list_study_ids, function(.x) {
    df_filtered <- df %>% filter(study_id == .x)
    
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  compact(out_loop)
}

#  Fit models for each group
out_loop_black <- fit_models_by_study(list_race_groups[[1]]) # black
out_loop_white <- fit_models_by_study(list_race_groups[[2]]) # white

#  Combine estimates and variances
tidied_estimates_black <- map_dfr(out_loop_black, function(x) x$tidied_estimates) %>%
  mutate(race_binary_b_w = "black")

tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates) %>%
  mutate(race_binary_b_w = "white")

giant_vcov_matrix_black <- map(out_loop_black, ~ .x$vcov_matrix) %>% bdiag()
giant_vcov_matrix_white <- map(out_loop_white, ~ .x$vcov_matrix) %>% bdiag()

# Final data combination
lm_estimates_race <- bind_rows(tidied_estimates_black, tidied_estimates_white) %>%
  mutate(
    content_id = gsub("factor\\(content_id\\)", "", term),
    race_binary = factor(race_binary_b_w, levels = c("black", "white"))
  ) %>%
  left_join(df_tags2020, by = "content_id")

final_giant_vcov_matrix_b_w <- bdiag(giant_vcov_matrix_black, giant_vcov_matrix_white)

#  checks
stopifnot(isSymmetric(final_giant_vcov_matrix_b_w))
stopifnot(identical(
  diag(final_giant_vcov_matrix_b_w) %>% sqrt() %>% unname() %>% round(10),
  c(tidied_estimates_black$std.error, tidied_estimates_white$std.error) %>% round(10)
))

#  Meta-regressions

# Immigration
meta_fit_race_immigration_interaction <- rma.mv(
  yi = estimate,
  V = final_giant_vcov_matrix_b_w,
  mods = ~ issue_immigrant * race_binary,
  data = lm_estimates_race
)
summary(meta_fit_race_immigration_interaction)

# BLM
meta_fit_race_blm_interaction <- rma.mv(
  yi = estimate,
  V = final_giant_vcov_matrix_b_w,
  mods = ~ issue_blm_race * race_binary,
  data = lm_estimates_race
)
summary(meta_fit_race_blm_interaction)

# Foreign policy
meta_fit_race_foreignp_interaction <- rma.mv(
  yi = estimate,
  V = final_giant_vcov_matrix_b_w,
  mods = ~ issue_foreign_p * race_binary,
  data = lm_estimates_race
)
summary(meta_fit_race_foreignp_interaction)

# Decency
meta_fit_race_dec_interaction <- rma.mv(
  yi = estimate,
  V = final_giant_vcov_matrix_b_w,
  mods = ~ issue_decency * race_binary,
  data = lm_estimates_race
)
summary(meta_fit_race_dec_interaction)



meta_fit_disgust <- rma.mv(yi = estimate,   
                           V = final_giant_vcov_matrix_b_w,      
                           mods = ~ emotion_disgust*race_binary,  
                           data = lm_estimates_race)
summary(meta_fit_disgust)



meta_fit_fear <- rma.mv(yi = estimate,   
                        V = final_giant_vcov_matrix_b_w,      
                        mods = ~ emotion_fear*race_binary,  
                        data = lm_estimates_race)
summary(meta_fit_fear)

lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant

meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                              V = final_giant_vcov_matrix_b_w,      
                                              mods = ~ immigrationissue_combined*race_binary,  
                                              data = lm_estimates_race)
summary(meta_fit_combined_immigration_issue)


#immigration and foreign p
lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p

meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                 V = final_giant_vcov_matrix_b_w,      
                                                 mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                 data = lm_estimates_race)
summary(meta_fit_combined_immigration_foreignp)




#visualisations 
# Load required packages
library(metafor)
library(ggplot2)
library(dplyr)


model_list <- list(
  Immigration = meta_fit_race_immigration_interaction,
  BLM = meta_fit_race_blm_interaction,
  ForeignPolicy = meta_fit_race_foreignp_interaction,
  Decency = meta_fit_race_dec_interaction,
  Disgust = meta_fit_disgust,
  Fear = meta_fit_fear,
  CombinedImmigration = meta_fit_combined_immigration_issue,
  CombinedImmigrationForeign = meta_fit_combined_immigration_foreignp
)

#  empty dataframe for results
results_df <- data.frame()

# loop
for (issue_name in names(model_list)) {
  
  model <- model_list[[issue_name]]
  
  
  coef_est <- coef(model)
  vcov_mat <- vcov(model)
  
  
  c_white <- c(0, 0, 0, 1)      # Intercept + immigrant + race_binary + interaction
  c_black <- c(0, 1, 0, 0)   # Intercept + immigrant only (race_binary = 0)

  
  
  est_white <- sum(c_white * coef_est)
  est_black <- sum(c_black * coef_est)
  
  se_white <- sqrt(t(c_white) %*% vcov_mat %*% c_white)
  se_black <- sqrt(t(c_black) %*% vcov_mat %*% c_black)
  
  # Bind results into 1dataframe
  temp_df <- data.frame(
    Issue = rep(issue_name, 2),
    Race = c("White", "Black"),
    Estimate = c(est_white, est_black),
    SE = c(se_white, se_black)
  )
  
  results_df <- rbind(results_df, temp_df)
}

# Step 4: Clean factor order
results_df$Issue <- factor(results_df$Issue, levels = names(model_list))
results_df$Race <- factor(results_df$Race, levels = c("White", "Black"))

# Step 5: Plot
ggplot(results_df, aes(x = Issue, y = Estimate, fill = Race)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE),
                width = 0.2, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("White" = "#4C72B0", "Black" = "#DD8452")) +
  labs(title = "Effect of Issues by Race Group",
       x = "Issue",
       y = "Estimated Effect (with 95% CI)",
       fill = "Race") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  theme(legend.position = "none")
  
  
  
  
  
  
  
  
  
  
  
  
  
  #model 3 check looking at black vs nonblack 
  
  
  #1. race subset, black or none black  
  df_responses2020$race_binary <- recode(df_responses2020$ethnicity,
                                         "black" = 1,
                                         .default = 0)
  
  df_filtered <- df_responses2020 %>%
    filter(!is.na(race_binary))  # Removes NA values 
  # Create two subsets
  df_black <- df_filtered %>%
    filter(race_binary == 1)
  
  df_nonblack <- df_filtered %>%
    filter(race_binary == 0)
  table(df_nonblack$ethnicity)
  
  #black group
  list_study_ids_black <- df_black$study_id %>% unique
  
  # Get studies in to df
  df_sample_black <-
    df_black %>% 
    filter(study_id %in% list_study_ids_black) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  # 1. Fit linear model comparing each content_id to the control group 
  df_sample_combined_fav_choice_black <- df_sample_black %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  # Set NA if both are missing
      )
    )
  
  
  # Fitting the model using the correct filtered data 
  out_loop_black <- map(list_study_ids_black, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_black %>% filter(study_id == .x)
    
    # Skip if no data exists for this study_id
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    # Fit the linear model using the correct filtered data
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    
    # Tidying up the estimates
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    
    # Extract the variance-covariance matrix
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    # Return the tidied estimates and covariance matrix
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  # Remove NULLs if any were skipped
  out_loop_black <- compact(out_loop_black)
  tidied_estimates_black <- map_dfr(out_loop_black, function(x) x$tidied_estimates)
  giant_vcov_matrix_black <- map(out_loop_black, function(.x) .x$vcov_matrix) %>% bdiag()
  stopifnot(isSymmetric(giant_vcov_matrix_black))
  
  # Check if the standard errors from the model match those from the tidy results
  stopifnot(identical(giant_vcov_matrix_black %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_black$std.error %>% round(10)))
  
  # check for 1 study id
  out_loop_black[[1]]$tidied_estimates
  out_loop_black[[1]]$vcov_matrix
  
  tidied_estimates_black <- map_dfr(out_loop_black, function(x) x$tidied_estimates)
  # Then we can loop over the covariance matrics and bdiag them all together at the end
  
  
  #################################
  #2. subset for nonwhite group
  list_study_ids_nonblack <- df_nonblack$study_id %>% unique
  
  # Get studies in to df
  df_sample_nonblack <-
    df_nonblack %>% 
    filter(study_id %in% list_study_ids_nonblack) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  # 1. Fit linear model comparing each content_id to the control group 
  df_sample_combined_fav_choice_nonblack <- df_sample_nonblack %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  # Set NA if both are missing
      )
    )
  
  
  # Fitting the model using the correct filtered data 
  out_loop_nonblack<- map(list_study_ids_nonblack, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_nonblack %>% filter(study_id == .x)
    
    # Skip if no data exists for this study_id
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    # Fit the linear model using the correct filtered data
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    
    # Tidying up the estimates
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    
    # Extract the variance-covariance matrix
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    # Return the tidied estimates and covariance matrix
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  # Remove NULLs if any were skipped
  out_loop_nonblack <- compact(out_loop_nonblack)
  
  
  out_loop_nonblack[[1]]$tidied_estimates
  out_loop_nonblack[[1]]$vcov_matrix
  
  tidied_estimates_nonblack <- map_dfr(out_loop_nonblack, function(x) x$tidied_estimates)
  
  giant_vcov_matrix_nonblack <-
    map(out_loop_nonblack,
        function(.x) {
          .x$vcov_matrix
        }) %>% 
    bdiag()
  
  stopifnot(isSymmetric(giant_vcov_matrix_nonblack))
  stopifnot(identical(giant_vcov_matrix_nonblack %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_nonblack$std.error %>% round(10)))
  
  

  final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonblack, giant_vcov_matrix_black)
  stopifnot(isSymmetric(final_giant_vcov_matrix))
  # Extract standard errors from covariance matrix
  computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
  # Combine and chcekc
  expected_se <- c(tidied_estimates_nonblack$std.error, tidied_estimates_black$std.error) %>% round(10)
  stopifnot(identical(computed_se, expected_se))
  
  #META reg by race
  tidied_estimates_black <- tidied_estimates_black %>%
    mutate(race_binary = "black")
  
  tidied_estimates_nonblack <- tidied_estimates_nonblack %>%
    mutate(race_binary = "nonblack")
  
  # Combine estimates from both racial groups
  lm_estimates_race <- bind_rows(tidied_estimates_nonblack,tidied_estimates_black) %>%
    mutate(
      content_id = gsub("factor\\(content_id\\)", "", term),  # Clean term names
      race_binary = factor(race_binary, levels = c("nonblack", "black"))  # Set nonwhite as reference
    ) %>%
    left_join(df_tags2020, by = "content_id")  # Merge with tags dataset
  
  # Meta-regressions model with race level as a moderator
  meta_fit_race_immigration_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_immigrant*race_binary,  
    data = lm_estimates_race
  )
  summary(meta_fit_race_immigration_interaction)
  
  meta_fit_race_blm_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_blm_race*race_binary,  
    data = lm_estimates_race
  )
  summary(meta_fit_race_blm_interaction)
  
  
  
  meta_fit_race_forignp_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_foreign_p*race_binary, 
    data = lm_estimates_race
  )
  summary(meta_fit_race_forignp_interaction)
  
  
  meta_fit_race_dec_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_decency*race_binary,  
    data = lm_estimates_race
  )
  summary(meta_fit_race_dec_interaction)
 
  
  meta_fit_disgust <- rma.mv(yi = estimate,   
                             V = final_giant_vcov_matrix,      
                             mods = ~ emotion_disgust*race_binary, 
                             data = lm_estimates_race)
  summary(meta_fit_disgust)
  
  
  
  meta_fit_fear <- rma.mv(yi = estimate,   
                          V = final_giant_vcov_matrix,      
                          mods = ~ emotion_fear*race_binary,  
                          data = lm_estimates_race)
  summary(meta_fit_fear)
  
  lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant
  
  meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                                V = final_giant_vcov_matrix,      
                                                mods = ~ immigrationissue_combined*race_binary,  
                                                data = lm_estimates_race)
  summary(meta_fit_combined_immigration_issue)
  
  
  #immigration and foreign p
  lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p
  
  meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                   V = final_giant_vcov_matrix,      
                                                   mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                   data = lm_estimates_race)
  summary(meta_fit_combined_immigration_foreignp)
  
  model_list <- list(
    Immigration = meta_fit_race_immigration_interaction,
    BLM = meta_fit_race_blm_interaction,
    ForeignPolicy = meta_fit_race_forignp_interaction,
    Decency = meta_fit_race_dec_interaction,
    Disgust = meta_fit_disgust,
    Fear = meta_fit_fear,
    CombinedImmigration = meta_fit_combined_immigration_issue,
    CombinedImmigrationForeign = meta_fit_combined_immigration_foreignp
  )
  
  #  empty dataframe for results
  results_df <- data.frame()
  
  # loop
  for (issue_name in names(model_list)) {
    
    model <- model_list[[issue_name]]
    
    # Get coefficients and vcov
    coef_est <- coef(model)
    vcov_mat <- vcov(model)
    
  
    c_black <- c(0, 0, 0, 1)      
    c_nonblack <- c(0, 1, 0, 0)   
    
    

    est_black <- sum(c_black * coef_est)
    est_nonblack <- sum(c_nonblack * coef_est)
    

    se_black <- sqrt(t(c_black) %*% vcov_mat %*% c_black)
    se_nonblack <- sqrt(t(c_nonblack) %*% vcov_mat %*% c_nonblack)
    
    # Bind results into 1 dataframe
    temp_df <- data.frame(
      Issue = rep(issue_name, 2),
      Race = c("Black", "Nonblack"),
      Estimate = c(est_black, est_nonblack),
      SE = c(se_black, se_nonblack)
    )
    
    results_df <- rbind(results_df, temp_df)
  }
  
 
  results_df$Issue <- factor(results_df$Issue, levels = names(model_list))
  results_df$Race <- factor(results_df$Race, levels = c("Black", "Nonblack"))
  
  #  Plot
  ggplot(results_df, aes(x = Issue, y = Estimate, fill = Race)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = c("Black" = "#4C72B0", "Nonblack" = "#DD8452")) +
    labs(title = "Effect of Issues by Race Group:Primary",
         x = "Issue",
         y = "Estimated Effect (with 95% CI)",
         fill = "Race") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  theme(legend.position = "none")
  
  
  
  
  #########4. latino vs white
  
  df_responses2020 <- df_responses2020 %>%
    filter(ethnicity %in% c("white", "hispanic-latino", "hispanic-or-latino")) %>%
    mutate(
      race_binary_his_w = recode(ethnicity, 
                                 "white" = 1, 
                                 "hispanic-latino" = 0,
                                 "hispanic-or-latino" = 0)
    )
  
  # 3. Combine favorability and vote choice
  df_responses2020 <- df_responses2020 %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,
        !is.na(favorability) ~ favorability,
        !is.na(votechoice) ~ votechoice,
        TRUE ~ NA_real_
      )
    )
  
  # Split into hispanic and white groups
  list_race_groups <- df_responses2020 %>%
    filter(!is.na(combined_outcome)) %>%
    group_split(race_binary_his_w)
  
  # Define function to fit models
  fit_models_by_study <- function(df) {
    list_study_ids <- unique(df$study_id)
    
    out_loop <- map(list_study_ids, function(.x) {
      df_filtered <- df %>% filter(study_id == .x)
      
      if (nrow(df_filtered) == 0) {
        return(NULL)
      }
      
      lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
      tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
      
      vcov_matrix <- vcov(lm_fit)
      vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
      
      list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
    })
    
    compact(out_loop)
  }
  
  #  Fit models
  out_loop_white <- fit_models_by_study(list_race_groups[[1]]) # white
  out_loop_hispanic <- fit_models_by_study(list_race_groups[[2]]) # hispanic
  
 
  tidied_estimates_hispanic <- map_dfr(out_loop_hispanic, function(x) x$tidied_estimates) %>%
    mutate(race_binary_his_w = "hispanic-latino")
  
  tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates) %>%
    mutate(race_binary_his_w = "white")
  
  giant_vcov_matrix_hispanic <- map(out_loop_hispanic, ~ .x$vcov_matrix) %>% bdiag()
  giant_vcov_matrix_white <- map(out_loop_white, ~ .x$vcov_matrix) %>% bdiag()
  
  # Final data combination
  lm_estimates_race <- bind_rows(tidied_estimates_hispanic, tidied_estimates_white) %>%
    mutate(
      content_id = gsub("factor\\(content_id\\)", "", term),
      race_binary = factor(race_binary_his_w, levels = c("hispanic-latino", "white"))
    ) %>%
    left_join(df_tags2020, by = "content_id")
  
  final_giant_vcov_matrix_his_w <- bdiag(giant_vcov_matrix_hispanic, giant_vcov_matrix_white)
  
  #  checks
  stopifnot(isSymmetric(final_giant_vcov_matrix_his_w))
  stopifnot(identical(
    diag(final_giant_vcov_matrix_his_w) %>% sqrt() %>% unname() %>% round(10),
    c(tidied_estimates_hispanic$std.error, tidied_estimates_white$std.error) %>% round(10)
  ))
  
  #  Meta-regressions
  
  # Immigration
  meta_fit_race_immigration_interaction <- rma.mv(
    yi = estimate,
    V = final_giant_vcov_matrix_his_w,
    mods = ~ issue_immigrant * race_binary,
    data = lm_estimates_race
  )
  summary(meta_fit_race_immigration_interaction)
  
  # BLM
  meta_fit_race_blm_interaction <- rma.mv(
    yi = estimate,
    V = final_giant_vcov_matrix_his_w,
    mods = ~ issue_blm_race * race_binary,
    data = lm_estimates_race
  )
  summary(meta_fit_race_blm_interaction)
  
  # Foreign policy
  meta_fit_race_foreignp_interaction <- rma.mv(
    yi = estimate,
    V = final_giant_vcov_matrix_his_w,
    mods = ~ issue_foreign_p * race_binary,
    data = lm_estimates_race
  )
  summary(meta_fit_race_foreignp_interaction)
  
  # Decency
  meta_fit_race_dec_interaction <- rma.mv(
    yi = estimate,
    V = final_giant_vcov_matrix_his_w,
    mods = ~ issue_decency * race_binary,
    data = lm_estimates_race
  )
  summary(meta_fit_race_dec_interaction)
  
  
  
  meta_fit_disgust <- rma.mv(yi = estimate,   
                             V = final_giant_vcov_matrix_his_w,      
                             mods = ~ emotion_disgust*race_binary,  
                             data = lm_estimates_race)
  summary(meta_fit_disgust)
  
  
  
  meta_fit_fear <- rma.mv(yi = estimate,   
                          V = final_giant_vcov_matrix_his_w,      
                          mods = ~ emotion_fear*race_binary,  
                          data = lm_estimates_race)
  summary(meta_fit_fear)
  
  lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant
  
  meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                                V = final_giant_vcov_matrix_his_w,      
                                                mods = ~ immigrationissue_combined*race_binary,  
                                                data = lm_estimates_race)
  summary(meta_fit_combined_immigration_issue)
  
  
  #immigration and foreign p
  lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p
  
  meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                   V = final_giant_vcov_matrix_his_w,      
                                                   mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                   data = lm_estimates_race)
  summary(meta_fit_combined_immigration_foreignp)
  
  
  
  
  #visualisations 

  model_list <- list(
    Immigration = meta_fit_race_immigration_interaction,
    BLM = meta_fit_race_blm_interaction,
    ForeignPolicy = meta_fit_race_foreignp_interaction,
    Decency = meta_fit_race_dec_interaction,
    Disgust = meta_fit_disgust,
    Fear = meta_fit_fear,
    CombinedImmigration = meta_fit_combined_immigration_issue,
    CombinedImmigrationForeign = meta_fit_combined_immigration_foreignp
  )
  
  #  empty dataframe for results
  results_df <- data.frame()
  
  # loop
  for (issue_name in names(model_list)) {
    
    model <- model_list[[issue_name]]
    
    
    coef_est <- coef(model)
    vcov_mat <- vcov(model)
    
    # Contrast vectors
    c_white <- c(0, 0, 0, 1)      # Intercept + immigrant + race_binary + interaction
    c_hispanic <- c(0, 1, 0, 0)   # Intercept + immigrant only (race_binary = 0)
    
    
    # extract Coeff Estimates
    est_white <- sum(c_white * coef_est)
    est_hispanic <- sum(c_hispanic * coef_est)
    
    # extract SE
    se_white <- sqrt(t(c_white) %*% vcov_mat %*% c_white)
    se_hispanic <- sqrt(t(c_hispanic) %*% vcov_mat %*% c_hispanic)
    
    # Bind results into 1dataframe
    temp_df <- data.frame(
      Issue = rep(issue_name, 2),
      Race = c("White", "hispanic"),
      Estimate = c(est_white, est_hispanic),
      SE = c(se_white, se_hispanic)
    )
    
    results_df <- rbind(results_df, temp_df)
  }
  
  # Step 4: Clean factor order
  results_df$Issue <- factor(results_df$Issue, levels = names(model_list))
  results_df$Race <- factor(results_df$Race, levels = c("White", "hispanic"))
  
  # Step 5: Plot
  ggplot(results_df, aes(x = Issue, y = Estimate, fill = Race)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = c("White" = "#4C72B0", "hispanic" = "#DD8452")) +
    labs(title = "Effect of Issues by Race Group",
         x = "Issue",
         y = "Estimated Effect (with 95% CI)",
         fill = "Race") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  theme(legend.position = "none")
  
  
  #model 5 check looking at hispanic vs nonhispanic 
  
  
  #1. race subset, hispanic or nonhispanic 
  df_responses2020$race_binary <- case_when(
    df_responses2020$ethnicity %in% c("hispanic-latino", "hispanic-or-latino") ~ 1,
    TRUE ~ 0
  )
  
  df_filtered <- df_responses2020 %>%
    filter(!is.na(race_binary))  # Removes NA values 
  # Create two subsets
  df_hispanic <- df_filtered %>%
    filter(race_binary == 1)
  
  df_nonhispanic <- df_filtered %>%
    filter(race_binary == 0)
  table(df_hispanic$ethnicity)
  
  #black group
  list_study_ids_hispanic <- df_hispanic$study_id %>% unique
  
  # Get studies in to df
  df_sample_hispanic <-
    df_hispanic %>% 
    filter(study_id %in% list_study_ids_hispanic) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  # 1. Fit linear model comparing each content_id to the control group 
  df_sample_combined_fav_choice_hispanic <- df_sample_hispanic %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  # Set NA if both are missing
      )
    )
  
  
  # Fitting the model using the correct filtered data 
  out_loop_hispanic <- map(list_study_ids_hispanic, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_hispanic %>% filter(study_id == .x)
    
    # Skip if no data exists for this study_id
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    # Fit the linear model using the correct filtered data
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    # Return the tidied estimates and covariance matrix
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  # Remove NULLs if any were skipped
  out_loop_hispanic <- compact(out_loop_hispanic)
  tidied_estimates_hispanic <- map_dfr(out_loop_hispanic, function(x) x$tidied_estimates)
  giant_vcov_matrix_hispanic <- map(out_loop_hispanic, function(.x) .x$vcov_matrix) %>% bdiag()
  stopifnot(isSymmetric(giant_vcov_matrix_hispanic))
  stopifnot(identical(giant_vcov_matrix_hispanic %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_hispanic$std.error %>% round(10)))
  
  # check for 1 study id
  out_loop_hispanic[[1]]$tidied_estimates
  out_loop_hispanic[[1]]$vcov_matrix
  
  tidied_estimates_hispanic <- map_dfr(out_loop_hispanic, function(x) x$tidied_estimates)
  # Then we can loop over the covariance matrics and bdiag them all together at the end
  
  
  #################################
  #2. subset for nonwhite group
  list_study_ids_nonhispanic <- df_nonhispanic$study_id %>% unique
  
  # Get studies in to df
  df_sample_nonhispanic <-
    df_nonhispanic %>% 
    filter(study_id %in% list_study_ids_hispanic) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  # 1. Fit linear model comparing each content_id to the control group 
  df_sample_combined_fav_choice_nonhispanic <- df_sample_nonhispanic %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  # Set NA if both are missing
      )
    )
  
  
  # Fitting the model using the correct filtered data 
  out_loop_nonhispanic<- map(list_study_ids_nonhispanic, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_nonhispanic %>% filter(study_id == .x)
    
    # Skip if no data exists for this study_id
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    # Fit the linear model using the correct filtered data
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    
    # Tidying up the estimates
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    
    # Extract the variance-covariance matrix
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    # Return the tidied estimates and covariance matrix
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  # Remove NULLs if any were skipped
  out_loop_nonhispanic <- compact(out_loop_nonhispanic)
  
  
  out_loop_nonhispanic[[1]]$tidied_estimates
  out_loop_nonhispanic[[1]]$vcov_matrix
  
  tidied_estimates_nonhispanic <- map_dfr(out_loop_nonhispanic, function(x) x$tidied_estimates)
  
  giant_vcov_matrix_nonhispanic <-
    map(out_loop_nonhispanic,
        function(.x) {
          .x$vcov_matrix
        }) %>% 
    bdiag()
  
  stopifnot(isSymmetric(giant_vcov_matrix_nonhispanic))
  stopifnot(identical(giant_vcov_matrix_nonhispanic %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_nonhispanic$std.error %>% round(10)))
  
  
  # Sanity checks
  final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonhispanic, giant_vcov_matrix_hispanic)
  stopifnot(isSymmetric(final_giant_vcov_matrix))
  # Extract standard errors from covariance matrix
  computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
  # Combine and chcekc
  expected_se <- c(tidied_estimates_nonhispanic$std.error, tidied_estimates_hispanic$std.error) %>% round(10)
  stopifnot(identical(computed_se, expected_se))
  
  #META reg by race
  tidied_estimates_hispanic <- tidied_estimates_hispanic %>%
    mutate(race_binary = "hispanic")
  
  tidied_estimates_nonhispanic <- tidied_estimates_nonhispanic %>%
    mutate(race_binary = "nonhispanic")
  
  # Combine estimates from both racial groups
  lm_estimates_race <- bind_rows(tidied_estimates_nonhispanic,tidied_estimates_hispanic) %>%
    mutate(
      content_id = gsub("factor\\(content_id\\)", "", term),  
      race_binary = factor(race_binary, levels = c("nonhispanic", "hispanic"))  
    ) %>%
    left_join(df_tags2020, by = "content_id")  
  
  # Meta-regressions model with race level as a moderator
  meta_fit_race_immigration_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_immigrant*race_binary,  
    data = lm_estimates_race
  )
  summary(meta_fit_race_immigration_interaction)
  
  meta_fit_race_blm_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_blm_race*race_binary,  
    data = lm_estimates_race
  )
  summary(meta_fit_race_blm_interaction)
  
  
  
  meta_fit_race_forignp_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_foreign_p*race_binary, 
    data = lm_estimates_race
  )
  summary(meta_fit_race_forignp_interaction)
  
  
  meta_fit_race_dec_interaction <- rma.mv(
    yi = estimate,         
    V = final_giant_vcov_matrix,  
    mods = ~ issue_decency*race_binary,  
    data = lm_estimates_race
  )
  summary(meta_fit_race_dec_interaction)
  
  
  meta_fit_disgust <- rma.mv(yi = estimate,   
                             V = final_giant_vcov_matrix,      
                             mods = ~ emotion_disgust*race_binary, 
                             data = lm_estimates_race)
  summary(meta_fit_disgust)
  
  
  
  meta_fit_fear <- rma.mv(yi = estimate,   
                          V = final_giant_vcov_matrix,      
                          mods = ~ emotion_fear*race_binary,  
                          data = lm_estimates_race)
  summary(meta_fit_fear)
  
  lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant
  
  meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                                V = final_giant_vcov_matrix,      
                                                mods = ~ immigrationissue_combined*race_binary,  
                                                data = lm_estimates_race)
  summary(meta_fit_combined_immigration_issue)
  
  
  #immigration and foreign p
  lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p
  
  meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                   V = final_giant_vcov_matrix,      
                                                   mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                   data = lm_estimates_race)
  summary(meta_fit_combined_immigration_foreignp)
  
  # visualising
  library(metafor)
  library(ggplot2)
  library(dplyr)
  
  #  Store model objects and labels in a list
  model_list <- list(
    Immigration = meta_fit_race_immigration_interaction,
    BLM = meta_fit_race_blm_interaction,
    ForeignPolicy = meta_fit_race_forignp_interaction,
    Decency = meta_fit_race_dec_interaction,
    Disgust = meta_fit_disgust,
    Fear = meta_fit_fear,
    Combined_Primary = meta_fit_combined_immigration_issue,
    Combined_National_Security = meta_fit_combined_immigration_foreignp
  )
  
  #  empty dataframe for results
  results_df <- data.frame()
  
  # loop
  for (issue_name in names(model_list)) {
    
    model <- model_list[[issue_name]]
    
    # Get coefficients and vcov
    coef_est <- coef(model)
    vcov_mat <- vcov(model)
    
    # Contrast vectors
    c_black <- c(0, 0, 0, 1)      
    c_nonblack <- c(0, 1, 0, 0)   
    
    
    # extract Coeff Estimates
    est_hispanic <- sum(c_hispanic * coef_est)
    est_nonhispanic <- sum(c_nonhispanic * coef_est)
    
    # extract SE
    se_hispanic <- sqrt(t(c_hispanic) %*% vcov_mat %*% c_black)
    se_nonhispanic <- sqrt(t(c_nonhispanic) %*% vcov_mat %*% c_nonhispanic)
    
    # Bind results into 1dataframe
    temp_df <- data.frame(
      Issue = rep(issue_name, 2),
      Race = c("Hispanic", "Nonhispanic"),
      Estimate = c(est_hispanic, est_nonhispanic),
      SE = c(se_hispanic, se_nonhispanic)
    )
    
    results_df <- rbind(results_df, temp_df)
  }
  
  # factor order
  results_df$Issue <- factor(results_df$Issue, levels = names(model_list))
  results_df$Race <- factor(results_df$Race, levels = c("Hispanic", "Nonhispanic"))
  
  #  Plot
  ggplot(results_df, aes(x = Issue, y = Estimate, fill = Race)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = c("Hispanic" = "#4C72B0", "Nonhispanic" = "#DD8452")) +
    labs(title = "Effect of Issues by Race Group:Primary",
         x = "Issue",
         y = "Estimated Effect (with 95% CI)",
         fill = "Race") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  theme(legend.position = "none")
  
  
  
  
  
  
  
  
