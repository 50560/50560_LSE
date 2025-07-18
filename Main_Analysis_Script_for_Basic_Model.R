# This script was used as the basic model and starting point for the other meta-regressions with moderators. it works out a regression by study Id based on treatment and control conditions (content id), merges this with tagged features, and then runs a meta-regression by primary and secondry features of interest. some basic visualisation code that is and is not included in the thesis is featured.

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
setwd("/Users/x/Desktop/LSE/diss/replication_archive/output/processed_data")
df_responses2020 <- readRDS("responses.rds") %>% filter(dataset_year == "2020")
df_tags2020 <- readRDS("tagging_2020.rds")

list_study_ids <- df_responses2020$study_id %>% unique
#visualising the primary issues of interest 
hist(df_tags2020$issue_blm_race)
mean(df_tags2020$issue_blm_race)
hist(df_tags2020$issue_foreign_p)
mean(df_tags2020$issue_foreign_p)
hist(df_tags2020$issue_immigrant)
mean(df_tags2020$issue_immigrant)


# Get studies in to dataframe
df_sample <-
  df_responses2020 %>% 
  filter(study_id %in% list_study_ids) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# 1. calculate the combined measure used as the DV
df_sample_combined_fav_choice <- df_sample %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  # Average if both exist
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  # Set NA if both are missing
    )
  )
#visualisation of the combined measure 
ggplot(df_sample_combined_fav_choice, aes(x = combined_outcome)) +
       geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
       labs(title = "Histogram of Combined Outcome", x = "Combined Outcome", y = "Frequency") +
       theme_minimal()




# running the first stage of analysis, looping over each study id
out_loop <-  
  map(list_study_ids,  
      function(.x) {  
        
        lm_fit <- lm(combined_outcome ~ factor(content_id),  
                     data = df_sample_combined_fav_choice %>% filter(study_id == .x))  
        
        tidied_estimates <- tidy(lm_fit) %>%  
          filter(term != "(Intercept)")  # Removes the intercept
        
        vcov_matrix <- vcov(lm_fit)  
        
        # Remove the intercept term from vcov_matrix  
        vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)",  
                                   colnames(vcov_matrix) != "(Intercept)"]  
        
        # Return the objects in a list  
        list("tidied_estimates" = tidied_estimates,  
             "vcov_matrix" = vcov_matrix)  
      })  



# check one of the studies
out_loop[[1]]$tidied_estimates
out_loop[[1]]$vcov_matrix

tidied_estimates <- map_dfr(out_loop, function(x) x$tidied_estimates)
# loop the covaience matrix and merge them using bdiag
giant_vcov_matrix <-
  map(out_loop,
      function(.x) {
        .x$vcov_matrix
      }) %>% 
  bdiag()
# check the merge was correct 
stopifnot(isSymmetric(giant_vcov_matrix))
stopifnot(identical(giant_vcov_matrix %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates$std.error %>% round(10)))


# 2. Fit the meta-regression by ad features

lm_estimates <- map_dfr(out_loop, function(x) {
  x$tidied_estimates  # Extract estimates
}) %>%
  mutate(content_id = gsub("factor\\(content_id\\)", "", term)) %>%  # Clean term names
  left_join(df_tags2020, by = "content_id")  # Merge with tags dataset

meta_fit_all <- rma.mv(yi = estimate,   # Effect size (from lm_estimates)
                                          V = giant_vcov_matrix,      
                                          mods = ~ issue_immigrant + issue_foreign_p + issue_blm_race + issue_decency + emotion_fear + persuaded_libs + persuaded_black + persuaded_latinx,  # Moderator variable
                                          data = lm_estimates)
summary(meta_fit_all)
#primary issues
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

#secondary issues
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

#additional secondary measures using other opperationalisations
# combined primary issues
lm_estimates$immigrationissue_combined <- lm_estimates$issue_blm_race + lm_estimates$issue_foreign_p + lm_estimates$issue_immigrant

meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                            V = giant_vcov_matrix,      
                            mods = ~ immigrationissue_combined,  
                            data = lm_estimates)
summary(meta_fit_combined_immigration_issue)

#immigration and foreign p - called national security 
lm_estimates$immigrationissue_combined_foreign_immi <- lm_estimates$issue_immigrant +lm_estimates$issue_foreign_p

meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                            V = giant_vcov_matrix,      
                                            mods = ~ immigrationissue_combined_foreign_immi,  
                                            data = lm_estimates)
summary(meta_fit_combined_immigration_foreignp)





             
