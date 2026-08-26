
library(readxl)

library(tidyverse)
library(ggplot2)
library(gt)
### read endmembers
endmembers <- read_excel("data/endmember.xlsx")

## import model outputs 
LK1<-read_csv("lk1_Hg_mixing_results.csv")
EM<-read_csv("EM_Hg_mixing_results.csv")
CT<-read_csv("CT_Hg_mixing_results.csv")
GG<-read_csv("GG_Hg_mixing_results.csv")
DP2<-read_csv("DP2_Hg_mixing_results.csv")


####Merge 
combined<- bind_rows(LK1, EM, CT, GG, DP2)


library(dplyr)
library(gt)

mix_summary_clean <- combined %>%
  
  # Define pre/post 1850
  mutate(
    Period = case_when(
      AD < 1850 ~ "Pre-1850",
      AD >= 1850 ~ "Post-1850",
      TRUE ~ NA_character_
    )
  ) %>%
  
  group_by(Lake, Period) %>%
  
  summarise(
    
    Guano = sprintf(
      "%.1f (%.1f–%.1f)",
      median(guano_med, na.rm = TRUE) * 100,
      median(guano_lo, na.rm = TRUE) * 100,
      median(guano_hi, na.rm = TRUE) * 100
    ),
    
    `Atmospheric Hg²⁺` = sprintf(
      "%.1f (%.1f–%.1f)",
      median(Hg2_med, na.rm = TRUE) * 100,
      median(Hg2_lo, na.rm = TRUE) * 100,
      median(Hg2_hi, na.rm = TRUE) * 100
    ),
    
    `Soil/moss` = sprintf(
      "%.1f (%.1f–%.1f)",
      median(soil_med, na.rm = TRUE) * 100,
      median(soil_lo, na.rm = TRUE) * 100,
      median(soil_hi, na.rm = TRUE) * 100
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    Lake = factor(
      Lake,
      levels = c("Lake_1", "Emerald", "Cumberland_Tarn", "Green_Gorge", "DP2")
    ),
    Period = factor(
      Period,
      levels = c("Pre-1850", "Post-1850")
    )
  ) %>%
  
  arrange(Lake, Period)


# Create publication-style table
mix_summary_clean %>%
  gt(groupname_col = "Lake") %>%
  
  tab_header(
    title = md("**Estimated Hg source contributions**"),
    subtitle = "Median contribution (%) with 95% credible intervals"
  ) %>%
  
  cols_label(
    Period = "Period",
    Guano = "Guano",
    `Atmospheric Hg²⁺` = "Atmospheric Hg²⁺",
    `Soil/moss` = "Soil/moss"
  ) %>%
  
  cols_align(
    align = "center",
    columns = c(Period, Guano, `Atmospheric Hg²⁺`, `Soil/moss`)
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  tab_source_note(
    source_note = md(
      "**Note:** Values are median estimated source contributions, with the 2.5th–97.5th percentile credible interval in parentheses."
    )
  ) %>%
  
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    data_row.padding = px(6)
  )




#### import lake data for a combo figure
lake_lk1<- read_excel("data/lk1_Hgiso_mm.xlsx")
lake_CT <- read_excel("data/CT_Hgiso_mm.xlsx")
lake_EM <- read_excel("data/EM_Hgiso_mm.xlsx")
lake_GG <- read_excel("data/GG_Hgiso_mm.xlsx")
lake_DP2<- read_excel("data/DP2_Hgiso_mm.xlsx")

combo_lakes<-combined<- bind_rows(lake_lk1, lake_CT, lake_EM, lake_GG, lake_DP2)

lake_iso <- combo_lakes %>%
  mutate(
    period = if_else(AD < 1850, "Pre-1850", "Post-1850")
  ) %>%
  arrange(Lake, AD)

lake_colors_2 <- c(
  "Emerald" = "#713e5a",
  "Lake_1" = "#FF8552",
  "Cumberland_Tarn" = "#63a375",
  "DP2" = "#ca6680", 
  "Green_Gorge" = "#6C809A"
)

p2 <- ggplot() +
  
  # --------------------------------------------------
# 95% confidence clouds for each endmember
# --------------------------------------------------
stat_ellipse(
  data = endmembers,
  aes(
    x = Delta199Hg,
    y = Delta200Hg,
    fill = endmember,
    colour = endmember
  ),
  type = "norm",
  level = 0.67,
  geom = "polygon",
  alpha = 0.15,
  linewidth = 0.1
) +
  
  # --------------------------------------------------
# Endmember mean values
# --------------------------------------------------
#stat_summary(
#  data = endmembers,
 # aes(
 #   x = Delta199Hg,
#    y = Delta200Hg,
#    colour = endmember
#  ),
 # fun = mean,
 # geom = "point",
 # size = 4
#) +
  
  # --------------------------------------------------
# Lake trajectories
# --------------------------------------------------
#geom_path(
#  data = lake_iso,
#  aes(
#    x = Delta199Hg,
 #   y = Delta200Hg,
#    group = Lake,
#    colour = Lake
#  ),
#  linewidth = 0.8,
#  alpha = 0.7
#) +
  
  # --------------------------------------------------
# Lake samples
# --------------------------------------------------
geom_point(
  data = lake_iso,
  aes(
    x = Delta199Hg,
    y = Delta200Hg,
    colour = Lake,
    alpha = period
  ),
  size = 3, 
  shape = 17
) +
  scale_color_manual(values = lake_colors_2) +
  scale_alpha_manual(
    values = c(
      "Pre-1850" = 0.65,
      "Post-1850" = 1
    )
  ) +
  
  labs(
    x = expression(Delta^{199}*Hg~("permille")),
    y = expression(Delta^{200}*Hg~("permille")),
    colour = "Lake",
    fill = "Endmember",
    alpha = "Period"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

p2
ggsave(
  filename = "endmembers_withlake.pdf",
  plot = p2,
  width = 8,
  height = 5
)


############ Combined mass balance ############################
library(dplyr)

######### With SD
# ============================================================
# Split atmospheric endmember data
# ============================================================

Hg0 <- endmembers %>%
  filter(endmember == "gaesous Hg0")

Hg2 <- endmembers %>%
  filter(endmember == "rainfall Hg")


# ============================================================
# Monte Carlo settings
# ============================================================

n_iter <- 100000

results_D200 <- data.frame()

combo_lakes <- combo_lakes %>%
  mutate(period = ifelse(AD < 1850, "Pre-1850", "Post-1850"))


# ============================================================
# Run model for every lake sample
# ============================================================

for(i in 1:nrow(combo_lakes)) {
  
  # Observed Delta200Hg
  obs200 <- combo_lakes$Delta200Hg[i]
  
  
  # ----------------------------------------------------------
  # Randomly sample Hg0 and Hg2+ endmember values
  # ----------------------------------------------------------
  
  Hg0_sample <- Hg0$Delta200Hg[
    sample(nrow(Hg0), n_iter, replace = TRUE)
  ]
  
  Hg2_sample <- Hg2$Delta200Hg[
    sample(nrow(Hg2), n_iter, replace = TRUE)
  ]
  
  
  # ----------------------------------------------------------
  # Mass balance
  #
  # obs = f_Hg0 * Hg0 + f_Hg2 * Hg2
  #
  # f_Hg2 = 1 - f_Hg0
  # ----------------------------------------------------------
  
  f_Hg0 <- (
    obs200 - Hg2_sample
  ) / (
    Hg0_sample - Hg2_sample
  )
  
  f_Hg2 <- 1 - f_Hg0
  
  
  # ----------------------------------------------------------
  # Remove physically impossible solutions
  # ----------------------------------------------------------
  
  valid <- (
    f_Hg0 >= 0 &
      f_Hg0 <= 1 &
      f_Hg2 >= 0 &
      f_Hg2 <= 1
  )
  
  f_Hg0 <- f_Hg0[valid]
  f_Hg2 <- f_Hg2[valid]
  
  
  # ----------------------------------------------------------
  # Summarise source contributions as mean ± SD
  # ----------------------------------------------------------
  
  out <- data.frame(
    
    Lake = combo_lakes$Lake[i],
    Depth = combo_lakes$Depth[i],
    AD = combo_lakes$AD[i],
    Period = combo_lakes$period[i],
    
    Hg0_mean = mean(f_Hg0, na.rm = TRUE),
    Hg0_sd = sd(f_Hg0, na.rm = TRUE),
    
    Hg2_mean = mean(f_Hg2, na.rm = TRUE),
    Hg2_sd = sd(f_Hg2, na.rm = TRUE)
  )
  
  
  # Add results
  results_D200 <- bind_rows(
    results_D200,
    out
  )
  
  
  cat(
    "Finished sample",
    i,
    "of",
    nrow(combo_lakes),
    "\n"
  )
}


# ============================================================
# View results
# ============================================================

results_D200

########## Summary table: mean ± SD

mix_summary_clean2 <- results_D200 %>%
  
  group_by(Lake) %>%
  
  summarise(
    
    `Atmospheric Hg²⁺` = sprintf(
      "%.1f ± %.1f",
      mean(Hg2_mean, na.rm = TRUE) * 100,
      mean(Hg2_sd, na.rm = TRUE) * 100
    ),
    
    `gaesous Hg0` = sprintf(
      "%.1f ± %.1f",
      mean(Hg0_mean, na.rm = TRUE) * 100,
      mean(Hg0_sd, na.rm = TRUE) * 100
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    Lake = factor(
      Lake,
      levels = c(
        "Lake_1",
        "Emerald",
        "Cumberland_Tarn",
        "Green_Gorge",
        "DP2"
      )
    )
  ) %>%
  
  arrange(Lake)


# Create publication-style table

mix_summary_clean2 %>%
  gt(groupname_col = "Lake") %>%
  
  tab_header(
    title = md("**Estimated Hg source Δ200Hg**"),
    subtitle = "Mean contribution (%) ± SD"
  ) %>%
  
  cols_label(
    `Atmospheric Hg²⁺` = "Atmospheric Hg²⁺",
    `gaesous Hg0` = "gaseous Hg0"
  ) %>%
  
  cols_align(
    align = "center",
    columns = c(
      `gaesous Hg0`,
      `Atmospheric Hg²⁺`
    )
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  tab_source_note(
    source_note = md(
      "**Note:** Values are mean estimated source contributions, with standard deviation (SD) shown in parentheses."
    )
  ) %>%
  
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    data_row.padding = px(6)
  )


##### Overall average####
overall_summary <- results_D200 %>%
  summarise(
    
    `Atmospheric Hg²⁺` = sprintf(
      "%.1f ± %.1f",
      mean(Hg2_mean, na.rm = TRUE) * 100,
      sd(Hg2_mean, na.rm = TRUE) * 100
    ),
    
    `gaseous Hg0` = sprintf(
      "%.1f ± %.1f",
      mean(Hg0_mean, na.rm = TRUE) * 100,
      sd(Hg0_mean, na.rm = TRUE) * 100
    )
  )

overall_summary
