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
  filter(!is.na(race_binary))  # Removes NA values 
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


# Fitting the model using the correct filtered data 
out_loop_white <- map(list_study_ids_white, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_white %>% filter(study_id == .x)
  
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
out_loop_white <- compact(out_loop_white)

# Combine tidied estimates across all studies
tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates)

# Combine the variance-covariance matrices
giant_vcov_matrix_white <- map(out_loop_white, function(.x) .x$vcov_matrix) %>% bdiag()

# Check if the combined variance-covariance matrix is symmetric
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

# 1. Fit linear model comparing each content_id to the control group 
df_sample_combined_fav_choice_nonwhite <- df_sample_nonwhite %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )


# Fitting the model using the correct filtered data 
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
lm_estimates_race <- bind_rows(tidied_estimates_white, tidied_estimates_nonwhite) %>%
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
# Load required packages
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
  c_white <- c(0, 1, 1, 1)      # Intercept + immigrant + race_binary + interaction
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
  scale_fill_manual(values = c("White" = "#4C72B0", "Nonwhite" = "#DD8452")) +
  labs(title = "Effect of Issues by Race Group:Primary",
       x = "Issue",
       y = "Estimated Effect (with 95% CI)",
       fill = "Race") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

theme(legend.position = "none")












#further check looking at black vs white only
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

#  Store model objects and labels in a list
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
  
  # Get coefficients and vcov
  coef_est <- coef(model)
  vcov_mat <- vcov(model)
  
  # Contrast vectors
  c_white <- c(0, 1, 1, 1)      # Intercept + immigrant + race_binary + interaction
  c_black <- c(0, 1, 0, 0)   # Intercept + immigrant only (race_binary = 0)

  
  # extract Coeff Estimates
  est_white <- sum(c_white * coef_est)
  est_black <- sum(c_black * coef_est)
  
  # extract SE
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

                                  
