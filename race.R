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



#1. eductaion subset 
df_responses2020$race_binary <- recode(df_responses2020$ethnicity,
                                       "white" = 1,
                                       .default = 0)

df_filtered <- df_responses2020 %>%
  filter(!is.na(race_binary))  # Removes NA values in education

# Create two subsets: below 2 and at least 2
df_white <- df_filtered %>%
  filter(race_binary == 1)

df_nonwhite <- df_filtered %>%
  filter(race_binary == 0)

#white group
list_study_ids_white <- df_white$study_id %>% unique

# Get studies in to df
df_sample_white <-
  df_white %>% 
  filter(study_id %in% list_study_ids_white) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model comparing each content_id to the control group ----
df_sample_combined_fav_choice_white <- df_sample_white %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )

# (map loops over the first argument's elements and applies the function to each element)
# Fitting the model using the correct filtered data (df_filtered)
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

# Now for each study ID (element 1:113, you have estimates and covariance matrix) e.g., for the first study ID:
out_loop_white[[1]]$tidied_estimates
out_loop_white[[1]]$vcov_matrix

tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates)
# Then we can loop over the covariance matrices and bdiag them all together at the end


#################################
#nonwhite group
list_study_ids_nonwhite <- df_nonwhite$study_id %>% unique

# Get studies in to df
df_sample_nonwhite <-
  df_nonwhite %>% 
  filter(study_id %in% list_study_ids_nonwhite) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model comparing each content_id to the control group ----
df_sample_combined_fav_choice_nonwhite <- df_sample_nonwhite %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )

# (map loops over the first argument's elements and applies the function to each element)
# Fitting the model using the correct filtered data (df_filtered)
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

# Now for each study ID (element 1:113, you have estimates and covariance matrix) e.g., for the first study ID:
out_loop_nonwhite[[1]]$tidied_estimates
out_loop_nonwhite[[1]]$vcov_matrix

tidied_estimates_nonwhite <- map_dfr(out_loop_nonwhite, function(x) x$tidied_estimates)
# Then we can loop over the covariance matrices and bdiag them all together at the end

giant_vcov_matrix_nonwhite <-
  map(out_loop_nonwhite,
      function(.x) {
        .x$vcov_matrix
      }) %>% 
  bdiag()

stopifnot(isSymmetric(giant_vcov_matrix_nonwhite))
stopifnot(identical(giant_vcov_matrix_nonwhite %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_nonwhite$std.error %>% round(10)))


# Combine the covariance matrices
final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonwhite, giant_vcov_matrix_white)
# Ensure symmetry
stopifnot(isSymmetric(final_giant_vcov_matrix))
# Extract standard errors from covariance matrix
computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
# Combine expected standard errors from both subsets
expected_se <- c(tidied_estimates_nonwhite$std.error, tidied_estimates_white$std.error) %>% round(10)
# Check if they match
stopifnot(identical(computed_se, expected_se))


#META reg by edu 


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

# Meta-regressions model with education level as a moderator
meta_fit_race_immigration_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_immigrant*race_binary,  # Interaction term
  data = lm_estimates_race
)
summary(meta_fit_race_immigration_interaction)

meta_fit_race_blm_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_blm_race*race_binary,  # Interaction term
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
