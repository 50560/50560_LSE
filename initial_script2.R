
##########loop attempt########
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
#visualising
hist(df_tags2020$issue_blm_race)
mean(df_tags2020$issue_blm_race)
hist(df_tags2020$issue_foreign_p)
mean(df_tags2020$issue_foreign_p)
hist(df_tags2020$issue_immigrant)
mean(df_tags2020$issue_immigrant)


# Get studies in to df
df_sample <-
  df_responses2020 %>% 
  filter(study_id %in% list_study_ids) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model comparing each content_id to the control group ----
df_sample_combined_fav_choice <- df_sample %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )

ggplot(df_sample_combined_fav_choice, aes(x = combined_outcome)) +
       geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
       labs(title = "Histogram of Combined Outcome", x = "Combined Outcome", y = "Frequency") +
       theme_minimal()


#### Here you need to loop over study ID -- fitting a model for each study!

# (map loops over the first argument's elements and applies the function to each element)
out_loop <-  
  map(list_study_ids,  
      function(.x) {  
        
        lm_fit <- lm(combined_outcome ~ factor(content_id),  
                     data = df_sample_combined_fav_choice %>% filter(study_id == .x))  
        
        tidied_estimates <- tidy(lm_fit) %>%  
          filter(term != "(Intercept)")  # Remove intercept
        
        vcov_matrix <- vcov(lm_fit)  
        
        # Remove the intercept term from vcov_matrix  
        vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)",  
                                   colnames(vcov_matrix) != "(Intercept)"]  
        
        # Return the objects in a list  
        list("tidied_estimates" = tidied_estimates,  
             "vcov_matrix" = vcov_matrix)  
      })  



# Now for each study ID (element 1:113, you have estimates and covariance matrix) e.g., for the first study ID:
out_loop[[1]]$tidied_estimates
out_loop[[1]]$vcov_matrix

tidied_estimates <- map_dfr(out_loop, function(x) x$tidied_estimates)
# Then we can loop over the covariance matrices and bdiag them all together at the end
# Note i havent removed the intercept terms here so you'll need to do that beforehand!
giant_vcov_matrix <-
  map(out_loop,
      function(.x) {
        .x$vcov_matrix
      }) %>% 
  bdiag()

stopifnot(isSymmetric(giant_vcov_matrix))
stopifnot(identical(giant_vcov_matrix %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates$std.error %>% round(10)))


# 2. Fit meta-regression ----

lm_estimates <- map_dfr(out_loop, function(x) {
  x$tidied_estimates  # Extract estimates
}) %>%
  mutate(content_id = gsub("factor\\(content_id\\)", "", term)) %>%  # Clean term names
  left_join(df_tags2020, by = "content_id")  # Merge with tags dataset

meta_fit_all <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                                          V = giant_vcov_matrix,      # Variance-covariance matrix
                                          mods = ~ issue_immigrant + issue_foreign_p + issue_blm_race + issue_decency + emotion_fear + persuaded_libs + persuaded_blacks + persuaded_latinx,  # Moderator variable
                                          data = lm_estimates)
summary(meta_fit_all)

meta_fit_immigrant <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                   V = giant_vcov_matrix,      # Variance-covariance matrix
                   mods = ~ issue_immigrant,  # Moderator variable
                   data = lm_estimates)
summary(meta_fit_immigrant)


meta_fit_blm <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                             V = giant_vcov_matrix,      # Variance-covariance matrix
                             mods = ~ issue_blm_race,  # Moderator variable
                             data = lm_estimates)
summary(meta_fit_blm)

meta_fit_forpoli <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                       V = giant_vcov_matrix,      # Variance-covariance matrix
                       mods = ~ issue_foreign_p,  # Moderator variable
                       data = lm_estimates)
summary(meta_fit_forpoli)


meta_fit_decency <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                           V = giant_vcov_matrix,      # Variance-covariance matrix
                           mods = ~ issue_decency,  # Moderator variable
                           data = lm_estimates)
summary(meta_fit_decency)


meta_fit_fear <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                           V = giant_vcov_matrix,      # Variance-covariance matrix
                           mods = ~ emotion_fear,  # Moderator variable
                           data = lm_estimates)
summary(meta_fit_fear)

meta_fit_persuadedlib <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                        V = giant_vcov_matrix,      # Variance-covariance matrix
                        mods = ~ persuaded_libs,  # Moderator variable
                        data = lm_estimates)
summary(meta_fit_persuadedlib)

meta_fit_persuadedlatinx <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                                V = giant_vcov_matrix,      # Variance-covariance matrix
                                mods = ~ persuaded_latinx,  # Moderator variable
                                data = lm_estimates)
summary(meta_fit_persuadedlatinx)

meta_fit_persuadedblack <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                                   V = giant_vcov_matrix,      # Variance-covariance matrix
                                   mods = ~ persuaded_black,  # Moderator variable
                                   data = lm_estimates)
summary(meta_fit_persuadedblack)


#combined metric 
# all
lm_estimates$immigrationissue_combined <- lm_estimates$issue_blm_race + lm_estimates$issue_foreign_p + lm_estimates$issue_immigrant

meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                            V = giant_vcov_matrix,      
                            mods = ~ immigrationissue_combined,  
                            data = lm_estimates)
summary(meta_fit_combined_immigration_issue)

#blm and immigration
lm_estimates$immigrationissue_combined_blm_immi <- lm_estimates$issue_blm_race + lm_estimates$issue_immigrant

meta_fit_combined_immigration_blm <- rma.mv(yi = estimate,   
                                              V = giant_vcov_matrix,      
                                              mods = ~ immigrationissue_combined_blm_immi,  
                                              data = lm_estimates)
summary(meta_fit_combined_immigration_blm)


#immigration and foreign p
lm_estimates$immigrationissue_combined_foreign_immi <- lm_estimates$issue_immigrant +lm_estimates$issue_foreign_p

meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                            V = giant_vcov_matrix,      
                                            mods = ~ immigrationissue_combined_foreign_immi,  
                                            data = lm_estimates)
summary(meta_fit_combined_immigration_foreignp)

#who they are trying to persuade
lm_estimates$persuaded_combined <- lm_estimates$persuaded_black + lm_estimates$persuaded_latinx + lm_estimates$persuaded_libs
meta_fit_combined_persuaded <- rma.mv(yi = estimate,   
                                         V = giant_vcov_matrix,      
                                        mods = ~ persuaded_combined,  
                                         data = lm_estimates)
summary(meta_fit_combined_persuaded)


#interaction for issues?





#does this change by race and education


#1. eductaion subset 
df_responses2020$education_numeric <- recode(df_responses2020$education,
                                             "school" = 1, 
                                             "highschool" = 2, 
                                             "some-college" = 3, 
                                             "college" = 4, 
                                             "postgrad" = 5, 
                                             .default = NA_real_)

df_responses2020$education_binary <- ifelse(is.na(df_responses2020$education_numeric), NA, df_responses2020$education_numeric >= 2)


df_filtered <- df_responses2020 %>%
  filter(!is.na(education_numeric))  # Removes NA values in education

# Create two subsets: below 2 and at least 2
df_low_edu <- df_filtered %>%
  filter(education_numeric < 2)

df_high_edu <- df_filtered %>%
  filter(education_numeric >= 2)

#high edu group
list_study_ids_high_edu <- df_high_edu$study_id %>% unique

# Get studies in to df
df_sample_high_edu <-
  df_high_edu %>% 
  filter(study_id %in% list_study_ids_high_edu) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model comparing each content_id to the control group ----
df_sample_combined_fav_choice_high_edu <- df_sample_high_edu %>%
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
out_loop_high_edu <- map(list_study_ids_high_edu, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_high_edu %>% filter(study_id == .x)
  
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
out_loop_high_edu <- compact(out_loop_high_edu)

# Combine tidied estimates across all studies
tidied_estimates_high_edu <- map_dfr(out_loop_high_edu, function(x) x$tidied_estimates)

# Combine the variance-covariance matrices
giant_vcov_matrix_high_edu <- map(out_loop_high_edu, function(.x) .x$vcov_matrix) %>% bdiag()

# Check if the combined variance-covariance matrix is symmetric
stopifnot(isSymmetric(giant_vcov_matrix_high_edu))

# Check if the standard errors from the model match those from the tidy results
stopifnot(identical(giant_vcov_matrix_high_edu %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_high_edu$std.error %>% round(10)))

# Now for each study ID (element 1:113, you have estimates and covariance matrix) e.g., for the first study ID:
out_loop_high_edu[[1]]$tidied_estimates
out_loop_high_edu[[1]]$vcov_matrix

tidied_estimates_high_edu <- map_dfr(out_loop_high_edu, function(x) x$tidied_estimates)
# Then we can loop over the covariance matrices and bdiag them all together at the end
# Note i havent removed the intercept terms here so you'll need to do that beforehand!
giant_vcov_matrix_high_edu <-
  map(out_loop_high_edu,
      function(.x) {
        .x$vcov_matrix
      }) %>% 
  bdiag()

stopifnot(isSymmetric(giant_vcov_matrix_high_edu))
stopifnot(identical(giant_vcov_matrix_high_edu %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_high_edu$std.error %>% round(10)))


#################################
#low edu group
list_study_ids_low_edu <- df_low_edu$study_id %>% unique

# Get studies in to df
df_sample_low_edu <-
  df_low_edu %>% 
  filter(study_id %in% list_study_ids_low_edu) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. Fit linear model comparing each content_id to the control group ----
df_sample_combined_fav_choice_low_edu <- df_sample_low_edu %>%
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
out_loop_low_edu <- map(list_study_ids_low_edu, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_low_edu %>% filter(study_id == .x)
  
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
out_loop_low_edu <- compact(out_loop_low_edu)

# Now for each study ID (element 1:113, you have estimates and covariance matrix) e.g., for the first study ID:
out_loop_low_edu[[1]]$tidied_estimates
out_loop_low_edu[[1]]$vcov_matrix

tidied_estimates_low_edu <- map_dfr(out_loop_low_edu, function(x) x$tidied_estimates)
# Then we can loop over the covariance matrices and bdiag them all together at the end

giant_vcov_matrix_low_edu <-
  map(out_loop_low_edu,
      function(.x) {
        .x$vcov_matrix
      }) %>% 
  bdiag()

stopifnot(isSymmetric(giant_vcov_matrix_low_edu))
stopifnot(identical(giant_vcov_matrix_low_edu %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_low_edu$std.error %>% round(10)))


# Combine the covariance matrices
final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_low_edu, giant_vcov_matrix_high_edu)
# Ensure symmetry
stopifnot(isSymmetric(final_giant_vcov_matrix))
# Extract standard errors from covariance matrix
computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
# Combine expected standard errors from both subsets
expected_se <- c(tidied_estimates_low_edu$std.error, tidied_estimates_high_edu$std.error) %>% round(10)
# Check if they match
stopifnot(identical(computed_se, expected_se))


#META reg by edu 


tidied_estimates_high_edu <- tidied_estimates_high_edu %>%
  mutate(education_group = "high")

tidied_estimates_low_edu <- tidied_estimates_low_edu %>%
  mutate(education_group = "low")

# Combine the estimates from both education groups
lm_estimates_edu <- bind_rows(tidied_estimates_high_edu, tidied_estimates_low_edu) %>%
  mutate(content_id = gsub("factor\\(content_id\\)", "", term)) %>%  # Clean term names
  left_join(df_tags2020, by = "content_id")  # Merge with tags dataset

# Meta-regressions model with education level as a moderator
meta_fit_edu_immigration_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_immigrant:education_group,  # Interaction term
  data = lm_estimates_edu
)
summary(meta_fit_edu_immigration_interaction)

meta_fit_edu_blm_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_blm_race:education_group,  # Interaction term
  data = lm_estimates_edu
)
summary(meta_fit_edu_blm_interaction)




meta_fit_edu_forignp_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_foreign_p:education_group,  # Interaction term
  data = lm_estimates_edu
)
summary(meta_fit_edu_forignp_interaction)


meta_fit_edu_dec_interaction <- rma.mv(
  yi = estimate,         
  V = final_giant_vcov_matrix,  
  mods = ~ issue_decency:education_group,  # Interaction term
  data = lm_estimates_edu
)
summary(meta_fit_edu_dec_interaction)
