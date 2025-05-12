library(metafor)

# Step 1: Prepare variance (needed for metafor)
lm_estimates <- lm_estimates %>%
  mutate(vi = std.error^2)

# Step 2: Forest plot - All adverts
res_all <- rma(yi = estimate, vi = vi, data = lm_estimates)
forest(res_all,
       slab = paste("Content", lm_estimates$content_id),
       xlab = "Effect Size (All Adverts)",
       main = "Forest Plot: All Adverts")

# Step 3: Forest plot - Immigration adverts (≥ 0.5)
res_immigration <- rma(yi = estimate, vi = vi,
                       data = lm_estimates %>% filter(issue_immigrant >= 0.5))
forest(res_immigration,
       slab = paste("Content", lm_estimates %>% filter(issue_immigrant >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Immigration)",
       main = "Forest Plot: Immigration Ads")

# Step 4: Forest plot - BLM/Race adverts (≥ 0.5)
res_blm <- rma(yi = estimate, vi = vi,
               data = lm_estimates %>% filter(issue_blm_race >= 0.5))
forest(res_blm,
       slab = paste("Content", lm_estimates %>% filter(issue_blm_race >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (BLM/Race)",
       main = "Forest Plot: BLM/Race Ads")

# Step 5: Forest plot - Foreign Policy adverts (≥ 0.5)
res_foreign <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(issue_foreign_p >= 0.5))
forest(res_foreign,
       slab = paste("Content", lm_estimates %>% filter(issue_foreign_p >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Foreign Policy)",
       main = "Forest Plot: Foreign Policy Ads")


# Step 6: Forest plot - decency adverts (≥ 0.5)
res_decency <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(issue_decency >= 0.5))
forest(res_decency,
       slab = paste("Content", lm_estimates %>% filter(issue_decency >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Decency)",
       main = "Forest Plot: Decency Ads")

# Step 6: Forest plot - disgust adverts (≥ 0.5)
res_disgust <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(emotion_disgust >= 0.5))
forest(res_disgust,
       slab = paste("Content", lm_estimates %>% filter(emotion_disgust >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Digust)",
       main = "Forest Plot: Disgust Ads")

# Step 7: Forest plot - fear adverts (≥ 0.5)
res_fear <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(emotion_fear >= 0.5))
forest(res_fear,
       slab = paste("Content", lm_estimates %>% filter(emotion_fear >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Fear)",
       main = "Forest Plot: Fear")


lm_estimates$immigrationissue_combined <- lm_estimates$issue_blm_race + lm_estimates$issue_foreign_p + lm_estimates$issue_immigrant
# Step 7: Forest plot - combined racial content (≥ 0.5)
res_combi <- rma(yi = estimate, vi = vi,
                data = lm_estimates %>% filter(immigrationissue_combined >= 0.5))
forest(res_combi,
       slab = paste("Content", lm_estimates %>% filter(immigrationissue_combined >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Combined racial content)",
       main = "Forest Plot: Combined racial content")