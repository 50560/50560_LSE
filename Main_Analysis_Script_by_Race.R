# This script was used to explore race as a moderator and runs a number of different opperationalisations of race (H2).

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

#1. white group
list_study_ids_white <- df_white$study_id %>% unique

# Get studies in to a dataframe
df_sample_white <-
  df_white %>% 
  filter(study_id %in% list_study_ids_white) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)

# Fit linear model comparing each content id to the control group 
df_sample_combined_fav_choice_white <- df_sample_white %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  
    )
  )


out_loop_white <- map(list_study_ids_white, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_white %>% filter(study_id == .x)
  
  
  if (nrow(df_filtered) == 0) {
    return(NULL)
  }
  

  lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)

  tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
  
  vcov_matrix <- vcov(lm_fit)
  vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
  
  list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
})

out_loop_white <- compact(out_loop_white)

# Combine tidied estimates across all studies and combine the matrices
tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates)
giant_vcov_matrix_white <- map(out_loop_white, function(.x) .x$vcov_matrix) %>% bdiag()

# Check merging
stopifnot(isSymmetric(giant_vcov_matrix_white))
stopifnot(identical(giant_vcov_matrix_white %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_white$std.error %>% round(10)))

# check for 1 study id
out_loop_white[[1]]$tidied_estimates
out_loop_white[[1]]$vcov_matrix

tidied_estimates_white <- map_dfr(out_loop_white, function(x) x$tidied_estimates)
# Then we can then do the same for the other group and then loop over the covariance matrices and bdiag combine them all together at the end

#2. subset for nonwhite group
list_study_ids_nonwhite <- df_nonwhite$study_id %>% unique

# Get studies in to their own dataframe
df_sample_nonwhite <-
  df_nonwhite %>% 
  filter(study_id %in% list_study_ids_nonwhite) %>% 
  select(study_id, dataset_year, treat, content_id, favorability, votechoice)


df_sample_combined_fav_choice_nonwhite <- df_sample_nonwhite %>%
  mutate(
    combined_outcome = case_when(
      !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  
      !is.na(favorability) ~ favorability,  
      !is.na(votechoice) ~ votechoice,  
      TRUE ~ NA_real_  
    )
  )


out_loop_nonwhite<- map(list_study_ids_nonwhite, function(.x) {
  df_filtered <- df_sample_combined_fav_choice_nonwhite %>% filter(study_id == .x)
  
  # Skip if no data exists for this study_id
  if (nrow(df_filtered) == 0) {
    return(NULL)
  }
  
  lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
  

  tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
  
  # Extract the variance-covariance matrix
  vcov_matrix <- vcov(lm_fit)
  vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
  
  # Return the tidied estimates and covariance matrix
  list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
})

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
# sanity checks after merging
stopifnot(isSymmetric(giant_vcov_matrix_nonwhite))
stopifnot(identical(giant_vcov_matrix_nonwhite %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_nonwhite$std.error %>% round(10)))


# merge both groups data and check
final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonwhite, giant_vcov_matrix_white)
stopifnot(isSymmetric(final_giant_vcov_matrix))
computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
expected_se <- c(tidied_estimates_nonwhite$std.error, tidied_estimates_white$std.error) %>% round(10)
stopifnot(identical(computed_se, expected_se))

#running the actual meta-regression by race 
tidied_estimates_white <- tidied_estimates_white %>%
  mutate(race_binary = "white")

tidied_estimates_nonwhite <- tidied_estimates_nonwhite %>%
  mutate(race_binary = "nonwhite")

# Combine estimates from both racial groups
lm_estimates_race <- bind_rows(tidied_estimates_nonwhite,tidied_estimates_white) %>%
  mutate(
    content_id = gsub("factor\\(content_id\\)", "", term),  # Clean term names
    race_binary = factor(race_binary, levels = c("nonwhite", "white"))  
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

# combined primary                                      
meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                              V = final_giant_vcov_matrix,      
                                              mods = ~ immigrationissue_combined*race_binary,  
                                              data = lm_estimates_race)
summary(meta_fit_combined_immigration_issue)


#immigration and foreign policy - national security 
lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p

meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                 V = final_giant_vcov_matrix,      
                                                 mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                 data = lm_estimates_race)
summary(meta_fit_combined_immigration_foreignp)


# Model 2: further check looking at black vs white only
df_responses2020 <- df_responses2020 %>%
  filter(ethnicity %in% c("white", "black")) %>%
  mutate(
    race_binary_b_w = recode(ethnicity, "white" = 1, "black" = 0)
  )

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

# fit models by race
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

out_loop_black <- fit_models_by_study(list_race_groups[[1]]) # black
out_loop_white <- fit_models_by_study(list_race_groups[[2]]) # white

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

#  checks for correct data merging
stopifnot(isSymmetric(final_giant_vcov_matrix_b_w))
stopifnot(identical(
  diag(final_giant_vcov_matrix_b_w) %>% sqrt() %>% unname() %>% round(10),
  c(tidied_estimates_black$std.error, tidied_estimates_white$std.error) %>% round(10)
))

#  Meta-regressions by issue 
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


#disgust
meta_fit_disgust <- rma.mv(yi = estimate,   
                           V = final_giant_vcov_matrix_b_w,      
                           mods = ~ emotion_disgust*race_binary,  
                           data = lm_estimates_race)
summary(meta_fit_disgust)


#fear
meta_fit_fear <- rma.mv(yi = estimate,   
                        V = final_giant_vcov_matrix_b_w,      
                        mods = ~ emotion_fear*race_binary,  
                        data = lm_estimates_race)
summary(meta_fit_fear)
#combined primary
lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant

meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                              V = final_giant_vcov_matrix_b_w,      
                                              mods = ~ immigrationissue_combined*race_binary,  
                                              data = lm_estimates_race)
summary(meta_fit_combined_immigration_issue)


#immigration and foreign policy - National security
lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p

meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                 V = final_giant_vcov_matrix_b_w,      
                                                 mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                 data = lm_estimates_race)
summary(meta_fit_combined_immigration_foreignp)


  

  
  # model 3. latino vs white
  
  df_responses2020 <- df_responses2020 %>%
    filter(ethnicity %in% c("white", "hispanic-latino", "hispanic-or-latino")) %>%
    mutate(
      race_binary_his_w = recode(ethnicity, 
                                 "white" = 1, 
                                 "hispanic-latino" = 0,
                                 "hispanic-or-latino" = 0)
    )
  
  
  df_responses2020 <- df_responses2020 %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,
        !is.na(favorability) ~ favorability,
        !is.na(votechoice) ~ votechoice,
        TRUE ~ NA_real_
      )
    )
  
  # Split into latino and white groups
  list_race_groups <- df_responses2020 %>%
    filter(!is.na(combined_outcome)) %>%
    group_split(race_binary_his_w)
  

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
  
  #  sanity checks
  stopifnot(isSymmetric(final_giant_vcov_matrix_his_w))
  stopifnot(identical(
    diag(final_giant_vcov_matrix_his_w) %>% sqrt() %>% unname() %>% round(10),
    c(tidied_estimates_hispanic$std.error, tidied_estimates_white$std.error) %>% round(10)
  ))
  
  #  Meta-regressions by race as moderator
  
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
  # combined primary
  lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant
  
  meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                                V = final_giant_vcov_matrix_his_w,      
                                                mods = ~ immigrationissue_combined*race_binary,  
                                                data = lm_estimates_race)
  summary(meta_fit_combined_immigration_issue)
  
  
  #immigration and foreign policy - national security
  lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p
  
  meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                   V = final_giant_vcov_matrix_his_w,      
                                                   mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                   data = lm_estimates_race)
  summary(meta_fit_combined_immigration_foreignp)
  
  
  #model 4 check looking at black vs non-black 
  
  
  #1. race subset, black or non-black  
  df_responses2020$race_binary <- recode(df_responses2020$ethnicity,
                                         "black" = 1,
                                         .default = 0)
  
  df_filtered <- df_responses2020 %>%
    filter(!is.na(race_binary)) 
  # Create two subsets
  df_black <- df_filtered %>%
    filter(race_binary == 1)
  
  df_nonblack <- df_filtered %>%
    filter(race_binary == 0)
  table(df_nonblack$ethnicity)
  
  #black group
  list_study_ids_black <- df_black$study_id %>% unique
  
  # Get studies in to a dataframe
  df_sample_black <-
    df_black %>% 
    filter(study_id %in% list_study_ids_black) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  # 1. Fit linear model  
  df_sample_combined_fav_choice_black <- df_sample_black %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  
      )
    )
  
  
  out_loop_black <- map(list_study_ids_black, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_black %>% filter(study_id == .x)
    
    # Skip if no data exists for this study_id
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
 
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
 
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
 
  out_loop_black <- compact(out_loop_black)
  tidied_estimates_black <- map_dfr(out_loop_black, function(x) x$tidied_estimates)
  giant_vcov_matrix_black <- map(out_loop_black, function(.x) .x$vcov_matrix) %>% bdiag()
# Checks if the standard errors from the model match those from the tidy results and the merge is correct
  stopifnot(isSymmetric(giant_vcov_matrix_black))
  stopifnot(identical(giant_vcov_matrix_black %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_black$std.error %>% round(10)))
  
  # check for 1 study id
  out_loop_black[[1]]$tidied_estimates
  out_loop_black[[1]]$vcov_matrix
  
  tidied_estimates_black <- map_dfr(out_loop_black, function(x) x$tidied_estimates)

  
  #2. subset for nonblack group
  list_study_ids_nonblack <- df_nonblack$study_id %>% unique
  

  df_sample_nonblack <-
    df_nonblack %>% 
    filter(study_id %in% list_study_ids_nonblack) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
 
  df_sample_combined_fav_choice_nonblack <- df_sample_nonblack %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  
      )
    )
  
  out_loop_nonblack<- map(list_study_ids_nonblack, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_nonblack %>% filter(study_id == .x)
    
    
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    
    
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  out_loop_nonblack <- compact(out_loop_nonblack)
  
  # check example 
  out_loop_nonblack[[1]]$tidied_estimates
  out_loop_nonblack[[1]]$vcov_matrix
  
  tidied_estimates_nonblack <- map_dfr(out_loop_nonblack, function(x) x$tidied_estimates)
  
  giant_vcov_matrix_nonblack <-
    map(out_loop_nonblack,
        function(.x) {
          .x$vcov_matrix
        }) %>% 
    bdiag()
  # checks for correct merge
  stopifnot(isSymmetric(giant_vcov_matrix_nonblack))
  stopifnot(identical(giant_vcov_matrix_nonblack %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_nonblack$std.error %>% round(10)))
  

  final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonblack, giant_vcov_matrix_black)
  stopifnot(isSymmetric(final_giant_vcov_matrix))

  computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
  # Combine and check again
  expected_se <- c(tidied_estimates_nonblack$std.error, tidied_estimates_black$std.error) %>% round(10)
  stopifnot(identical(computed_se, expected_se))
  
  #meta-regression by racial group as moderator 
  tidied_estimates_black <- tidied_estimates_black %>%
    mutate(race_binary = "black")
  
  tidied_estimates_nonblack <- tidied_estimates_nonblack %>%
    mutate(race_binary = "nonblack")
  
  # combine estimates from both racial groups
  lm_estimates_race <- bind_rows(tidied_estimates_nonblack,tidied_estimates_black) %>%
    mutate(
      content_id = gsub("factor\\(content_id\\)", "", term),  
      race_binary = factor(race_binary, levels = c("nonblack", "black"))  
    ) %>%
    left_join(df_tags2020, by = "content_id")  
  
  
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
  #primary combined issues
  lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant
  
  meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                                V = final_giant_vcov_matrix,      
                                                mods = ~ immigrationissue_combined*race_binary,  
                                                data = lm_estimates_race)
  summary(meta_fit_combined_immigration_issue)
  
  
  #immigration and foreign p - national security
  lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p
  
  meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                   V = final_giant_vcov_matrix,      
                                                   mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                   data = lm_estimates_race)
  summary(meta_fit_combined_immigration_foreignp)
  
  
  
  #model 5 check looking at latino vs nonlatino
  
  
  #1. race subset, latino or nonlatino
  df_responses2020$race_binary <- case_when(
    df_responses2020$ethnicity %in% c("hispanic-latino", "hispanic-or-latino") ~ 1,
    TRUE ~ 0
  )
  
  df_filtered <- df_responses2020 %>%
    filter(!is.na(race_binary))  
  # Create two subsets
  df_hispanic <- df_filtered %>%
    filter(race_binary == 1)
  
  df_nonhispanic <- df_filtered %>%
    filter(race_binary == 0)
  table(df_hispanic$ethnicity)
  
  #latino group
  list_study_ids_hispanic <- df_hispanic$study_id %>% unique
  

  df_sample_hispanic <-
    df_hispanic %>% 
    filter(study_id %in% list_study_ids_hispanic) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  # 1. Fit linear model
  df_sample_combined_fav_choice_hispanic <- df_sample_hispanic %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2, 
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  
      )
    )
  

  out_loop_hispanic <- map(list_study_ids_hispanic, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_hispanic %>% filter(study_id == .x)
    
    # Skip if no data exists for this study_id
    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
  
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")
    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  out_loop_hispanic <- compact(out_loop_hispanic)
  tidied_estimates_hispanic <- map_dfr(out_loop_hispanic, function(x) x$tidied_estimates)
  giant_vcov_matrix_hispanic <- map(out_loop_hispanic, function(.x) .x$vcov_matrix) %>% bdiag()
  #checks                                  
  stopifnot(isSymmetric(giant_vcov_matrix_hispanic))
  stopifnot(identical(giant_vcov_matrix_hispanic %>% diag %>% sqrt %>% unname %>% round(10), tidied_estimates_hispanic$std.error %>% round(10)))
  
  # check for 1 study id
  out_loop_hispanic[[1]]$tidied_estimates
  out_loop_hispanic[[1]]$vcov_matrix
  
  tidied_estimates_hispanic <- map_dfr(out_loop_hispanic, function(x) x$tidied_estimates)

  #2. subset for nonlatino group
  list_study_ids_nonhispanic <- df_nonhispanic$study_id %>% unique
 
  df_sample_nonhispanic <-
    df_nonhispanic %>% 
    filter(study_id %in% list_study_ids_hispanic) %>% 
    select(study_id, dataset_year, treat, content_id, favorability, votechoice)
  
  df_sample_combined_fav_choice_nonhispanic <- df_sample_nonhispanic %>%
    mutate(
      combined_outcome = case_when(
        !is.na(favorability) & !is.na(votechoice) ~ (favorability + votechoice) / 2,  
        !is.na(favorability) ~ favorability,  
        !is.na(votechoice) ~ votechoice,  
        TRUE ~ NA_real_  
      )
    )
  
  
  out_loop_nonhispanic<- map(list_study_ids_nonhispanic, function(.x) {
    df_filtered <- df_sample_combined_fav_choice_nonhispanic %>% filter(study_id == .x)
    

    if (nrow(df_filtered) == 0) {
      return(NULL)
    }
    
    lm_fit <- lm(combined_outcome ~ factor(content_id), data = df_filtered)
    tidied_estimates <- tidy(lm_fit) %>% filter(term != "(Intercept)")

    vcov_matrix <- vcov(lm_fit)
    vcov_matrix <- vcov_matrix[rownames(vcov_matrix) != "(Intercept)", colnames(vcov_matrix) != "(Intercept)"]
    
    # Return the tidied estimates and covariance matrix
    list("tidied_estimates" = tidied_estimates, "vcov_matrix" = vcov_matrix)
  })
  
  out_loop_nonhispanic <- compact(out_loop_nonhispanic)
  #check 1 study
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
  
  
  # final merging and checks 
  final_giant_vcov_matrix <- bdiag(giant_vcov_matrix_nonhispanic, giant_vcov_matrix_hispanic)
  stopifnot(isSymmetric(final_giant_vcov_matrix))
  computed_se <- final_giant_vcov_matrix %>% diag() %>% sqrt() %>% unname() %>% round(10)
  expected_se <- c(tidied_estimates_nonhispanic$std.error, tidied_estimates_hispanic$std.error) %>% round(10)
  stopifnot(identical(computed_se, expected_se))
  
  # meta-regression by race
  tidied_estimates_hispanic <- tidied_estimates_hispanic %>%
    mutate(race_binary = "hispanic")
  
  tidied_estimates_nonhispanic <- tidied_estimates_nonhispanic %>%
    mutate(race_binary = "nonhispanic")
  
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
  #combined primary
  lm_estimates_race$immigrationissue_combined <- lm_estimates_race$issue_blm_race + lm_estimates_race$issue_foreign_p + lm_estimates_race$issue_immigrant
  
  meta_fit_combined_immigration_issue <- rma.mv(yi = estimate,   
                                                V = final_giant_vcov_matrix,      
                                                mods = ~ immigrationissue_combined*race_binary,  
                                                data = lm_estimates_race)
  summary(meta_fit_combined_immigration_issue)
  
  
  #immigration and foreign p - national security
  lm_estimates_race$immigrationissue_combined_foreign_immi <- lm_estimates_race$issue_immigrant +lm_estimates_race$issue_foreign_p
  
  meta_fit_combined_immigration_foreignp <- rma.mv(yi = estimate,   
                                                   V = final_giant_vcov_matrix,      
                                                   mods = ~ immigrationissue_combined_foreign_immi*race_binary,  
                                                   data = lm_estimates_race)
  summary(meta_fit_combined_immigration_foreignp)
  
 
  
  
