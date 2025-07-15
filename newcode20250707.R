library(data.table)
library(readr)
library(patchwork)
library(ggpubr)
library(ggrepel)
library(ggplot2)
library(ggsci)
library(tidyverse)
library("ggmap")
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggsci) 
library(broom)  # for tidy()
library(dplyr)


#卫星图
# 1. 读入原始数据，并保留 Year、Longitude、Latitude
df <- read_csv(file.choose(), col_types = cols())

pts <- df %>%
  select(Population, Year, Longitude, Latitude) %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# 2. jitter 坐标：添加轻微扰动避免重叠
pts_jittered <- pts %>%
  mutate(
    Lon_jitter = jitter(Longitude, amount = 0.15),
    Lat_jitter = jitter(Latitude, amount = 0.15)
  )

# 3. 转为 sf 对象
pts_sf_jittered <- st_as_sf(pts_jittered, coords = c("Lon_jitter", "Lat_jitter"), crs = 4326)

pts_sf_jittered <- pts_sf_jittered %>%
  mutate(
    YearGroup = cut(
      Year,
      breaks = c(2005, 2009, 2012, 2016, 2020, 2024),
      labels = c("2006–2008","2009–2012","2013–2016","2017–2020","2021–2024")
    )
  )

register_google(key = "Your google map key")
map <- get_map(location = c(lon = 114, lat = 30), zoom = 5, maptype = "satellite")
get_map
ggmap(map) +
  geom_point(data = pts_sf_jittered, aes(x = Longitude, y = Latitude, color = Year),
             size = 3, alpha = 0.85) +
  scale_color_viridis_c(
    option = "C",
    name = "Year",
    breaks = seq(min(pts_sf_jittered$Year), max(pts_sf_jittered$Year), by = 1),
    guide = guide_colorbar(
      barheight = 11,  # 调整竖直方向长度
      barwidth = 0.8   # 控制横向厚度
    )) +
  labs(
    x        = "Longitude",
    y        = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = c(0.9, 0.4),
    legend.background = element_rect(fill = alpha("white", 0.6)),
    panel.grid        = element_blank()
  )


#RR-trends
lc_cols <- df %>% select(starts_with("X")) %>% names()
df_long <- df %>%
  select(Year, all_of(lc_cols)) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Pesticide",
    values_to = "RR"
  )
df_long <- df_long %>% filter(!is.na(RR))

# 4. 计算年度平均 RR
annual_mean <- df_long %>%
  group_by(Year, Pesticide) %>%
  summarise(Mean_RR = mean(RR), .groups = "drop")

fit_df <- df_long %>%
  group_by(Pesticide) %>%
  do({
    model <- lm(RR ~ Year, data = .)
    year_seq <- seq(min(.$Year), max(.$Year), length.out = 100)
    pred <- predict(model, newdata = data.frame(Year = year_seq))
    data.frame(Year = year_seq, RR = pred)
  })



sig_df <- df_long %>%
  group_by(Pesticide) %>%
  do({
    model <- lm(RR ~ Year, data = .)
    tidy_model <- broom::tidy(model)
    p_val <- tidy_model %>% filter(term == "Year") %>% pull(p.value)
    
    sig <- case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      TRUE          ~ ""
    )
    
    data.frame(Pesticide = unique(.$Pesticide),
               sig = sig)
  })

fit_df <- df_long %>%
  group_by(Pesticide) %>%
  do({
    model <- lm(RR ~ Year, data = .)
    year_seq <- seq(min(.$Year), max(.$Year), length.out = 100)
    pred <- predict(model, newdata = data.frame(Year = year_seq))
    data.frame(Pesticide = unique(.$Pesticide),
               Year = year_seq,
               RR = pred)
  })

label_df <- fit_df %>%
  group_by(Pesticide) %>%
  slice_tail(n = 1)  # 取回归线尾部

label_df <- label_df %>%
  left_join(sig_df, by = "Pesticide") %>%
  mutate(label = paste0(Pesticide, " ", sig),
         Year = Year + 0.3)  # 稍右移美观

Fig1B <- ggplot(df_long, aes(x = Year, y = RR, color = Pesticide, fill = Pesticide))  +
  geom_hline(yintercept = c(5, 10, 100), linetype = "dashed", color = "black", size = 0.5)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 5),
            fill = "white", alpha = 0.01, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 5, ymax = 10),
            fill = "#d0e3f0", alpha = 0.01, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 100),
            fill = "#D9D9D9", alpha = 0.05, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 100, ymax = Inf),
            fill = "#B3B3B3", alpha = 0.01, inherit.aes = FALSE) +
  geom_smooth(method = "lm", se = TRUE, size = 0.8, linetype = "solid", alpha = 0.2) +
  geom_point(size = 2,alpha = 0.5, position = position_jitter(width = 0.2))+
  scale_x_continuous(breaks = seq(min(df_long$Year), max(df_long$Year), by = 1)) +
  scale_y_log10(
    breaks = c(1, 5, 10, 100, 1000),
    labels = c("1", "5", "10", "100", "1000")
  ) +
  scale_color_manual(values = c(
    "X1A_Isoprocarb" = "#F781BF",
    "X1B_Chlorpyrifos" = "#377EB8",
    "X3A_Etofenprox" = "#4DAF4A",
    "X4A_Clothianidin" = "#984EA3",
    "X4A_Dinotefuran" = "#FF7F00",
    "X4A_Imidacloprid" = "#A65628",
    "X4A_Nitenpyram" = "#E41A1C",
    "X4A_Thiamethoxam" = "#66C2A5",
    "X4C_Sulfoxaflor" = "#999999",
    "X4E_Triflumezopyrim" = "#FFD92F",
    "XZ16_Buprofezin" = "#8DA0CB"
  ))+scale_fill_manual(values = c(
    "X1A_Isoprocarb" = "#F781BF",
    "X1B_Chlorpyrifos" = "#377EB8",
    "X3A_Etofenprox" = "#4DAF4A",
    "X4A_Clothianidin" = "#984EA3",
    "X4A_Dinotefuran" = "#FF7F00",
    "X4A_Imidacloprid" = "#A65628",
    "X4A_Nitenpyram" = "#E41A1C",
    "X4A_Thiamethoxam" = "#66C2A5",
    "X4C_Sulfoxaflor" = "#999999",
    "X4E_Triflumezopyrim" = "#FFD92F",
    "XZ16_Buprofezin" = "#8DA0CB"
  ))+
  annotate("text", x = min(annual_mean$Year), y = 2.5, label = "Susceptible", hjust = 0, vjust = 6, size = 4) +
  annotate("text", x = min(annual_mean$Year), y = 7, label = "Low resistance", hjust = 0, vjust = 0.5, size = 4) +
  annotate("text", x = min(annual_mean$Year), y = 30, label = "Medium resistance", hjust = 0, vjust = 0.5, size = 4) +
  annotate("text", x = min(annual_mean$Year), y = 300, label = "High resistance", hjust = 0, vjust = -10, size = 4) +
  
  labs(
    x = "Year",
    y = "Resistance Ratio (log scale)"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),  # 去掉主网格线
        panel.grid.minor = element_blank(),  # 去掉次网格线
        panel.background = element_blank(),  # 去掉背景色（保持透明）
        text = element_text(size = 12),legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
  )+ geom_text_repel(
    data = label_df,
    aes(label = label),
    nudge_x = 0.5,       # 横向微移以避免遮挡
    direction = "y",     # 标签竖直方向排列
    hjust = 0,
    size = 4,
    show.legend = FALSE
  )

Fig1B


#LC50-trends
df2 <- read_csv(file.choose(), col_types = cols())
lc_cols <- df2 %>% select(starts_with("X")) %>% names()
df_long2 <- df2 %>%
  select(Year, all_of(lc_cols)) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Pesticide",
    values_to = "LC50"
  )
df_long2 <- df_long2 %>% filter(!is.na(LC50))
p_raw <- ggplot(df_long2, aes(x = Year, y = LC50)) +
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

print(p_raw)



dt_LC50  <- as.data.table(df_long2)
dt_LC501 <- copy(dt_LC50)
setnames(dt_LC501, old = c("Year", "LC50"), new = c("Year_LC501", "LC501"))

# 4. 执行笛卡尔积并添加筛选条件：
result <- dt_LC501[dt_LC50, on = .(Pesticide), allow.cartesian = TRUE
][Year_LC501 - Year == 1  # 只保留相邻年份
][, RCR := (LC501 - LC50) / LC50]  # 计算相对变化率
head(result)
result2 <- result[, .(Year = Year_LC501, 
                      Pesticide = Pesticide, 
                      LC50_rate = RCR)]

temp_df <- data.frame(
  Year = df$Year,
  Temperature = df$`Annual Mean Temperature (AMT)`
)
temp_df <- temp_df %>% distinct()


df_merge <- result2 %>%
  left_join(temp_df, by = "Year") %>%
  filter(!is.na(LC50_rate))  # 去除NA避免影响相关性计算


cor_results <- df_merge %>%
  group_by(Pesticide) %>%
  summarize(
    n = n(),
    cor = cor(LC50_rate, Temperature, use = "complete.obs", method = "pearson"),
    p_value = cor.test(LC50_rate, Temperature)$p.value
  ) %>%
  ungroup()
print(cor_results)

cor_results <- cor_results %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

print(cor_results)


cor_results2 <- df_merge %>%
  summarize(
    n = n(),
    cor = cor(LC50_rate, Year, use = "complete.obs", method = "pearson"),
    p_value = cor.test(LC50_rate, Year)$p.value
  ) %>%
  ungroup()
print(cor_results2)



p_total <- ggplot(df_merge, aes(x = Year, y = LC50_rate)) +
  geom_point(alpha = 0.6, size = 2, color = "#0D6CA6") +
  geom_smooth(method = "lm", se = TRUE, color = "#0D6CA6", fill = "#0D6CA6") +
  scale_x_continuous(
    breaks = seq(min(df_merge$Year), max(df_merge$Year), by = 1),  # 年份间隔为3年
    limits = c(min(df_merge$Year), max(df_merge$Year))             # 强制完整显示年份
  ) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", 
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           size = 3) +
  theme_bw() +
  labs(title = "Overall", x = "Year", y = "Resistance Change Rate") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
# 2. 创建每个 Pesticide 的子图列表
p_list <- df_merge %>%
  split(.$Pesticide) %>%
  lapply(function(df_sub) {
    ggplot(df_sub, aes(x = Year, y = LC50_rate)) +
      geom_point(alpha = 0.6, size = 2, color = "#0D6CA6") +
      geom_smooth(method = "lm", se = TRUE, color = "#0D6CA6", fill = "#0D6CA6") +
      scale_x_continuous(
        breaks =seq(min(df_merge$Year), max(df_merge$Year), by = 1),
      ) +
      stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", 
               aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
               size = 3) +theme_bw() +
      labs(title = unique(df_sub$Pesticide), x = "Year", y = "Resistance Change Rate") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      )
  })
Fig1C<-wrap_plots(c(list(p_total), p_list), ncol = 6)
Fig1C


# 1. 创建总览图
p_total2 <- ggplot(df_merge, aes(x = Temperature, y = LC50_rate)) +
  geom_point(alpha = 0.6, size = 2, color = "#D55E00") +
  geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00") +
  stat_cor(method = "pearson", 
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left", label.y.npc = "top", 
           size = 3) +
  theme_bw() +
  labs(title = "Overall", 
       x = "Temperature (°C)", 
       y = "Resistance Change Rate") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
# 2. 创建每个 Pesticide 的子图列表
p_list2 <- df_merge %>%
  split(.$Pesticide) %>%
  lapply(function(df_sub) {
    ggplot(df_sub, aes(x = Temperature, y = LC50_rate)) +
      geom_point(alpha = 0.6, size = 2, color = "#D55E00") +
      geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00") +
      stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", 
               aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
               size = 3) +  # r 和 p 一起展示
      theme_bw() +
      labs(title = unique(df_sub$Pesticide), 
           x = "Temperature (°C)", 
           y = "Resistance Change Rate") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      )
  })
p_list2
# 3. 拼图：总图 + 11个子图，组成两排6列
Fig1D<-wrap_plots(c(list(p_total2), p_list2), ncol = 6)
Fig1D
