#load in plotting package 
library(metafor)

#add in varience
lm_estimates <- lm_estimates %>%
  mutate(vi = std.error^2)

#Forest plot - All adverts
res_all <- rma(yi = estimate, vi = vi, data = lm_estimates)
forest(res_all,
       slab = paste("Content", lm_estimates$content_id),
       xlab = "Effect Size (All Adverts)",
       main = "Forest Plot: All Adverts")

# Immigration adverts (≥ 0.5) ### using 0.5 as a resonable cut off of the average coders rating. This was chosen as a comparamise between including a large sample of the ads 
and also having a good level of certainty about the feature.
res_immigration <- rma(yi = estimate, vi = vi,
                       data = lm_estimates %>% filter(issue_immigrant >= 0.5))
forest(res_immigration,
       slab = paste("Content", lm_estimates %>% filter(issue_immigrant >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Immigration)",
       main = "Forest Plot: Immigration Ads")

#  BLM/Race adverts (≥ 0.5)
res_blm <- rma(yi = estimate, vi = vi,
               data = lm_estimates %>% filter(issue_blm_race >= 0.5))
forest(res_blm,
       slab = paste("Content", lm_estimates %>% filter(issue_blm_race >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (BLM/Race)",
       main = "Forest Plot: BLM/Race Ads")

#  Foreign Policy adverts (≥ 0.5)
res_foreign <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(issue_foreign_p >= 0.5))
forest(res_foreign,
       slab = paste("Content", lm_estimates %>% filter(issue_foreign_p >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Foreign Policy)",
       main = "Forest Plot: Foreign Policy Ads")


#  decency adverts (≥ 0.5)
res_decency <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(issue_decency >= 0.5))
forest(res_decency,
       slab = paste("Content", lm_estimates %>% filter(issue_decency >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Decency)",
       main = "Forest Plot: Decency Ads")

#  Forest plot - disgust adverts (≥ 0.5)
res_disgust <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(emotion_disgust >= 0.5))
forest(res_disgust,
       slab = paste("Content", lm_estimates %>% filter(emotion_disgust >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Digust)",
       main = "Forest Plot: Disgust Ads")

# Forest plot - fear adverts (≥ 0.5)
res_fear <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(emotion_fear >= 0.5))
forest(res_fear,
       slab = paste("Content", lm_estimates %>% filter(emotion_fear >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Fear)",
       main = "Forest Plot: Fear")

# combined primary issues  content (≥ 0.5)
lm_estimates$immigrationissue_combined <- lm_estimates$issue_blm_race + lm_estimates$issue_foreign_p + lm_estimates$issue_immigrant
# - combined racial content (≥ 0.5)
res_combi <- rma(yi = estimate, vi = vi,
                data = lm_estimates %>% filter(immigrationissue_combined >= 0.5))
forest(res_combi,
       slab = paste("Content", lm_estimates %>% filter(immigrationissue_combined >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (Combined racial content)",
       main = "Forest Plot: Combined racial content")

# combined national sec content (≥ 0.5)
lm_estimates$immigrationissue_combined_foreign_immi <- lm_estimates$issue_immigrant +lm_estimates$issue_foreign_p

res_nationals <- rma(yi = estimate, vi = vi,
                 data = lm_estimates %>% filter(immigrationissue_combined_foreign_immi >= 0.5))
forest(res_nationals,
       slab = paste("Content", lm_estimates %>% filter(immigrationissue_combined_foreign_immi >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (National Security content)",
       main = "Forest Plot: National Security content",  xlab.cex = 0.9,  
       cex.lab = 0.7,   
       cex.axis = 0.9)





#####additonal plot without the random effects model plot example: 
res_x <- rma(yi = estimate, vi = vi,
                   data = lm_estimates %>% filter(issue/emtoion >= 0.5))

forest(res_x,
       slab = paste("Content", lm_estimates %>% filter(issue/emotion >= 0.5) %>% pull(content_id)),
       xlab = "Effect Size (x)",
       main = "Forest Plot: x Ads",
       header = "Content ID",
       addfit = FALSE,     
       xlab.cex = 0.9,  
       cex.lab = 0.7,   
       cex.axis = 0.9)

