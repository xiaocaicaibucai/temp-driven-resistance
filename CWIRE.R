# Climate warming and insecticide resistance evolution analysis script
# Author: Tingwei Cai
# Last updated: 2025-07-16
# Purpose: Visualize and quantify the impact of temperature on insecticide resistance evolution in Nilaparvata lugens

# === Load Required Packages ===
library(data.table)
library(readr)
library(patchwork)
library(ggpubr)
library(ggrepel)
library(ggplot2)
library(ggsci)
library(tidyverse)
library(ggmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(broom)
library(dplyr)

# === Import Raw Data and Prepare Coordinates ===
df <- read_csv("RR.csv", col_types = cols())

pts <- df %>%
  select(Population, Year, Longitude, Latitude) %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Add jitter to coordinates to avoid overplotting
pts_jittered <- pts %>%
  mutate(
    Lon_jitter = jitter(Longitude, amount = 0.15),
    Lat_jitter = jitter(Latitude, amount = 0.15)
  )

pts_sf_jittered <- st_as_sf(pts_jittered, coords = c("Lon_jitter", "Lat_jitter"), crs = 4326)

# Create year groups for coloring
pts_sf_jittered <- pts_sf_jittered %>%
  mutate(
    YearGroup = cut(
      Year,
      breaks = c(2005, 2009, 2012, 2016, 2020, 2024),
      labels = c("2006–2008","2009–2012","2013–2016","2017–2020","2021–2024")
    )
  )

# === Satellite Map of Sampling Sites ===
register_google(key = "GOOGLE_MAP_KEY")
map <- get_map(location = c(lon = 114, lat = 30), zoom = 5, maptype = "satellite")

# Plot map
Fig1A <- ggmap(map) +
  geom_point(data = pts_sf_jittered, aes(x = Longitude, y = Latitude, color = Year),
             size = 3, alpha = 0.85) +
  scale_color_viridis_c(
    option = "C",
    name = "Year",
    breaks = seq(min(pts_sf_jittered$Year), max(pts_sf_jittered$Year), by = 1),
    guide = guide_colorbar(barheight = 11, barwidth = 0.8)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.9, 0.4),
        legend.background = element_rect(fill = alpha("white", 0.6)),
        panel.grid = element_blank())
# Print the figure
Fig1A

# === Resistance Ratio (RR) Trends ===
lc_cols <- df %>% select(starts_with("X")) %>% names()
df_long <- df %>%
  select(Year, all_of(lc_cols)) %>%
  pivot_longer(cols = -Year, names_to = "Pesticide", values_to = "RR") %>%
  filter(!is.na(RR))

annual_mean <- df_long %>%
  group_by(Year, Pesticide) %>%
  summarise(Mean_RR = mean(RR), .groups = "drop")

# Linear fit for each pesticide
fit_df <- df_long %>%
  group_by(Pesticide) %>%
  do({
    model <- lm(RR ~ Year, data = .)
    year_seq <- seq(min(.$Year), max(.$Year), length.out = 100)
    pred <- predict(model, newdata = data.frame(Year = year_seq))
    data.frame(Pesticide = unique(.$Pesticide), Year = year_seq, RR = pred)
  })

# Significance annotations
sig_df <- df_long %>%
  group_by(Pesticide) %>%
  do({
    model <- lm(RR ~ Year, data = .)
    tidy_model <- broom::tidy(model)
    p_val <- tidy_model %>% filter(term == "Year") %>% pull(p.value)
    sig <- case_when(p_val < 0.001 ~ "***", p_val < 0.01 ~ "**", p_val < 0.05 ~ "*", TRUE ~ "")
    data.frame(Pesticide = unique(.$Pesticide), sig = sig)
  })

# Combine for labeling
label_df <- fit_df %>%
  group_by(Pesticide) %>%
  slice_tail(n = 1) %>%
  left_join(sig_df, by = "Pesticide") %>%
  mutate(label = paste0(Pesticide, " ", sig), Year = Year + 0.3)

# Define color scheme for each pesticide
pesticide_colors <- c(
  "X1A_Isoprocarb"       = "#F781BF",
  "X1B_Chlorpyrifos"     = "#377EB8",
  "X3A_Etofenprox"       = "#4DAF4A",
  "X4A_Clothianidin"     = "#984EA3",
  "X4A_Dinotefuran"      = "#FF7F00",
  "X4A_Imidacloprid"     = "#A65628",
  "X4A_Nitenpyram"       = "#E41A1C",
  "X4A_Thiamethoxam"     = "#66C2A5",
  "X4C_Sulfoxaflor"      = "#999999",
  "X4E_Triflumezopyrim"  = "#FFD92F",
  "XZ16_Buprofezin"      = "#8DA0CB"
)

# Define background color bands for resistance categories
resistance_bands <- data.frame(
  ymin  = c(0, 5, 10, 100),
  ymax  = c(5, 10, 100, Inf),
  fill  = c("white", "#d0e3f0", "#D9D9D9", "#B3B3B3"),
  alpha = c(0.01, 0.01, 0.05, 0.01)
)

# Plot resistance ratio trends
Fig1B <- ggplot(df_long, aes(x = Year, y = RR, color = Pesticide)) +
  
  # Add background shaded rectangles indicating resistance levels
  geom_rect(data = resistance_bands,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = I(fill), alpha = I(alpha)),
            inherit.aes = FALSE) +
  
  # Add resistance thresholds as dashed lines
  geom_hline(yintercept = c(5, 10, 100), linetype = "dashed", color = "black", size = 0.5) +
  
  # Add linear regression line per pesticide
  geom_smooth(method = "lm", se = TRUE, size = 0.8, linetype = "solid", alpha = 0.2) +
  
  # Add jittered data points
  geom_point(size = 2, alpha = 0.5, position = position_jitter(width = 0.2)) +
  
  # Add regression significance labels at the line ends
  geom_text_repel(data = label_df, aes(label = label), 
                  nudge_x = 0.5, direction = "y", hjust = 0, size = 4, show.legend = FALSE) +
  
  # Set x-axis ticks
  scale_x_continuous(breaks = seq(min(df_long$Year), max(df_long$Year), by = 1)) +
  
  # Log-scaled y-axis with labeled breaks
  scale_y_log10(
    breaks = c(1, 5, 10, 100, 1000),
    labels = c("1", "5", "10", "100", "1000")
  ) +
  
  # Apply manual color scheme
  scale_color_manual(values = pesticide_colors) +
  scale_fill_identity() +  # use fill colors defined in resistance_bands
  scale_alpha_identity() + # use alpha transparency values
  
  # Annotate resistance categories
  annotate("text", x = min(annual_mean$Year), y = 2.5, label = "Susceptible", hjust = 0, vjust = 6, size = 4) +
  annotate("text", x = min(annual_mean$Year), y = 7, label = "Low resistance", hjust = 0, vjust = 0.5, size = 4) +
  annotate("text", x = min(annual_mean$Year), y = 30, label = "Medium resistance", hjust = 0, vjust = 0.5, size = 4) +
  annotate("text", x = min(annual_mean$Year), y = 300, label = "High resistance", hjust = 0, vjust = -10, size = 4) +
  
  # Axis labels and theme settings
  labs(x = "Year", y = "Resistance Ratio (log scale)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid        = element_blank(),
    panel.background  = element_blank(),
    legend.position   = "none",
    axis.text.x       = element_text(angle = 45, hjust = 1)
  )

# Print the figure
Fig1B

# === LC50 Trends ===
df2 <- read_csv("LC50.csv", col_types = cols())

lc_cols <- df2 %>% select(starts_with("X")) %>% names()
df_long2 <- df2 %>%
  select(Year, all_of(lc_cols)) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Pesticide",
    values_to = "LC50"
  )
df_long2 <- df_long2 %>% filter(!is.na(LC50))
FigS1 <- ggplot(df_long2, aes(x = Year, y = LC50)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), linetype = "dashed") +
  facet_wrap(~ Pesticide, scales = "free_y", ncol = 4) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           method = "pearson", label.x.npc = "left", label.y.npc = "top",
           size = 3, color = "black") +
  scale_color_aaas(name = "Pesticide", palette = "default") +scale_x_continuous(
    breaks = seq(min(df_long2$Year), max(df_long2$Year), by = 1),  # 年份间隔为3年
    limits = c(min(df_long2$Year), max(df_long2$Year))             # 强制完整显示年份
  ) +
  labs(
    x     = "Year",
    y     = "LC50 (mg/L)"
  ) +
  theme_bw() +
  theme(
    strip.text       = element_text(size = 10),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
# Print the figure
FigS1

# === Resistance Change Rate (RCR) and Correlation Analysis ===
dt_LC50  <- as.data.table(df_long2)
dt_LC501 <- copy(dt_LC50)
setnames(dt_LC501, old = c("Year", "LC50"), new = c("Year_LC501", "LC501"))

# Calculate inter-annual relative change rate (RCR)
result <- dt_LC501[dt_LC50, on = .(Pesticide), allow.cartesian = TRUE
][Year_LC501 - Year == 1][, RCR := (LC501 - LC50) / LC50]

result2 <- result[, .(Year = Year_LC501, Pesticide = Pesticide, LC50_rate = RCR)]

# Merge with temperature data
temp_df <- df %>% select(Year, `Annual Mean Temperature (AMT)`) %>% distinct()
colnames(temp_df)[2] <- "Temperature"
df_merge <- result2 %>% left_join(temp_df, by = "Year") %>% filter(!is.na(LC50_rate))

# Correlation between resistance rate and temperature
cor_results <- df_merge %>%
  group_by(Pesticide) %>%
  summarize(
    n = n(),
    cor = cor(LC50_rate, Temperature, use = "complete.obs", method = "pearson"),
    p_value = cor.test(LC50_rate, Temperature)$p.value
  ) %>% ungroup() %>%
  mutate(significance = case_when(p_value < 0.001 ~ "***", p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", TRUE ~ ""))
cor_results
# Correlation between resistance rate and year
cor_results2 <- df_merge %>%
  summarize(
    n = n(),
    cor = cor(LC50_rate, Year, use = "complete.obs", method = "pearson"),
    p_value = cor.test(LC50_rate, Year)$p.value
  )
cor_results2

# === Visualization: LC50 Change Rate vs. Year ===
p_total <- ggplot(df_merge, aes(x = Year, y = LC50_rate)) +
  geom_point(alpha = 0.6, size = 2, color = "#0D6CA6") +
  geom_smooth(method = "lm", se = TRUE, color = "#0D6CA6", fill = "#0D6CA6") +
  scale_x_continuous(breaks = seq(min(df_merge$Year), max(df_merge$Year), by = 1), limits = c(min(df_merge$Year), max(df_merge$Year))) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), size = 3) +
  theme_bw() +
  labs(title = "Overall", x = "Year", y = "Resistance Change Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_line(color = "grey90"), panel.grid.minor = element_blank())

# Generate individual plots by pesticide (vs. year)
p_list <- df_merge %>% split(.$Pesticide) %>% lapply(function(df_sub) {
  ggplot(df_sub, aes(x = Year, y = LC50_rate)) +
    geom_point(alpha = 0.6, size = 2, color = "#0D6CA6") +
    geom_smooth(method = "lm", se = TRUE, color = "#0D6CA6", fill = "#0D6CA6") +
    scale_x_continuous(breaks = seq(min(df_merge$Year), max(df_merge$Year), by = 1)) +
    stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), size = 3) +
    theme_bw() +
    labs(title = unique(df_sub$Pesticide), x = "Year", y = "Resistance Change Rate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_line(color = "grey90"), panel.grid.minor = element_blank())
})

Fig1C <- wrap_plots(c(list(p_total), p_list), ncol = 6)
# Print the figure
Fig1C

# === Visualization: LC50 Change Rate vs. Temperature ===
p_total2 <- ggplot(df_merge, aes(x = Temperature, y = LC50_rate)) +
  geom_point(alpha = 0.6, size = 2, color = "#D55E00") +
  geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00") +
  stat_cor(method = "pearson", aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top", size = 3) +
  theme_bw() +
  labs(title = "Overall", x = "Temperature (°C)", y = "Resistance Change Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_line(color = "grey90"), panel.grid.minor = element_blank())

# Generate individual plots by pesticide (vs. temperature)
p_list2 <- df_merge %>% split(.$Pesticide) %>% lapply(function(df_sub) {
  ggplot(df_sub, aes(x = Temperature, y = LC50_rate)) +
    geom_point(alpha = 0.6, size = 2, color = "#D55E00") +
    geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00") +
    stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), size = 3) +
    theme_bw() +
    labs(title = unique(df_sub$Pesticide), x = "Temperature (°C)", y = "Resistance Change Rate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_line(color = "grey90"), panel.grid.minor = element_blank())
})

Fig1D <- wrap_plots(c(list(p_total2), p_list2), ncol = 6)
# Print the figure
Fig1D

# === End of Script ===

