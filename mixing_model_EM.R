##### mixing model #######


library(readxl)

library(tidyverse)
library(ggplot2)


####Import data 
lake <- read_excel("~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/GitHub/mixing_model/data/EM_Hgiso_mm.xlsx")
endmembers <- read_excel("~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/GitHub/mixing_model/data/endmember.xlsx")



# -----------------------------------
# Split endmember data
# -----------------------------------

guano <- endmembers %>%
  filter(endmember == "guano")

Hg2 <- endmembers %>%
  filter(endmember == "rainfall Hg")

soil <- endmembers %>%
  filter(endmember == "soil_moss")

# -----------------------------------
# Function to generate random source proportions
# -----------------------------------

random_props <- function(n) {
  
  p <- matrix(rexp(n * 3), ncol = 3)
  p <- p / rowSums(p)
  
  colnames(p) <- c("guano", "Hg2", "soil_moss")
  
  return(p)
}

# -----------------------------------
# Monte Carlo settings
# -----------------------------------

n_iter <- 100000

results <- data.frame()

# -----------------------------------
# Run model for each lake sample
# -----------------------------------

for(i in 1:nrow(lake)) {
  
  obs199 <- lake$Delta199Hg[i]
  obs200 <- lake$Delta200Hg[i]
  
  # Random source proportions
  props <- random_props(n_iter)
  
  # Randomly sample source isotope values
  g <- guano[sample(nrow(guano), n_iter, replace = TRUE), ]
  h <- Hg2[sample(nrow(Hg2), n_iter, replace = TRUE), ]
  s <- soil[sample(nrow(soil), n_iter, replace = TRUE), ]
  
  # Predict isotope compositions
  pred199 <-
    props[, "guano"]     * g$Delta199Hg +
    props[, "Hg2"]       * h$Delta199Hg +
    props[, "soil_moss"] * s$Delta199Hg
  
  pred200 <-
    props[, "guano"]     * g$Delta200Hg +
    props[, "Hg2"]       * h$Delta200Hg +
    props[, "soil_moss"] * s$Delta200Hg
  
  # Misfit
  error <-
    (pred199 - obs199)^2 +
    (pred200 - obs200)^2
  
  # Retain best 1% of solutions
  best <- props[error < quantile(error, 0.01), ]
  
  # Summarise source contributions
  out <- data.frame(
    Lake = lake$Lake[i],
    Depth = lake$Depth[i],
    AD = lake$AD[i],
    
    guano_lo  = quantile(best[, "guano"], 0.025),
    guano_med = median(best[, "guano"]),
    guano_hi  = quantile(best[, "guano"], 0.975),
    
    Hg2_lo  = quantile(best[, "Hg2"], 0.025),
    Hg2_med = median(best[, "Hg2"]),
    Hg2_hi  = quantile(best[, "Hg2"], 0.975),
    
    soil_lo  = quantile(best[, "soil_moss"], 0.025),
    soil_med = median(best[, "soil_moss"]),
    soil_hi  = quantile(best[, "soil_moss"], 0.975)
  )
  
  results <- bind_rows(results, out)
  
  cat("Finished sample", i, "of", nrow(lake), "\n")
}

# -----------------------------------
# View results
# -----------------------------------

results
# Save results
write.csv(results,
          "~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/GitHub/mixing_model/EM_Hg_mixing_results.csv",
          row.names = FALSE)

#
p1<-ggplot(results,
       aes(AD, guano_med)) +
  geom_ribbon(
    aes(ymin = guano_lo,
        ymax = guano_hi),
    alpha = 0.3
  ) +
  geom_line() +
  theme_bw() +
  labs(
    y = "Estimated guano contribution",
    x = "Year AD"
  )
p1
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/GitHub/mixing_model/guanothroughtime_EM.pdf",
  plot = p1,
  width = 8,
  height = 5
)


p2<-ggplot() +
  geom_point(
    data = endmembers,
    aes(Delta199Hg,
        Delta200Hg,
        colour = endmember)
  ) +
  geom_path(
    data = lake,
    aes(Delta199Hg,
        Delta200Hg)
  ) +
  geom_point(
    data = lake,
    aes(Delta199Hg,
        Delta200Hg),
    size = 3
  ) +
  theme_bw()

p2
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/GitHub/mixing_model/D199D200_EM.pdf",
  plot = p2,
  width = 8,
  height = 5
)

####Plot

results2 <- results %>%
  mutate(period = ifelse(AD < 1850, "Pre-1850", "Post-1850"))

summary_period <- results2 %>%
  group_by(period) %>%
  summarise(
    guano = mean(guano_med),
    Hg2 = mean(Hg2_med),
    soil_moss = mean(soil_med)
  ) %>%
  pivot_longer(cols = -period,
               names_to = "source",
               values_to = "proportion")

summary_period$period <- factor(summary_period$period,
                                levels = c("Pre-1850", "Post-1850"))

p<-ggplot(summary_period,
          aes(x = period,
              y = proportion,
              fill = source)) +
  geom_bar(stat = "identity",
           position = "stack") +
  theme_bw() +
  labs(
    x = "",
    y = "Relative contribution",
    fill = "Source"
  )
p
ggsave(
  filename = "~/Library/CloudStorage/OneDrive-AustralianNationalUniversity/GitHub/mixing_model/Hg_source_pre_post1850_EM.pdf",
  plot = p,
  width = 8,
  height = 5
)

