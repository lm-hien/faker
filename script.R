library(tidyverse)
library(lubridate)
library(MASS)

gains <- read_csv("events.csv") %>%
  as_tibble() %>%
  mutate(date = ymd(date), year = year(date), year_c = year - 2023)

# ---- Expected 2026 season total (events x gain per event) ----
season    <- gains %>% count(year, year_c)
fit_n     <- glm(n ~ year_c, data = season, family = poisson)
fit_nb    <- glm.nb(value ~ year_c, data = gains)
n_2026    <- predict(fit_n,  tibble(year_c = 3), type = "response")
gain_2026 <- predict(fit_nb, tibble(year_c = 3), type = "response")
total_2026 <- as.numeric(n_2026 * gain_2026)      # ~148

# ---- Add the predicted event and compute cumulative sum ----
start <- 291

plot_df <- gains %>%
  dplyr::select(date, value) %>%
  bind_rows(tibble(date = ymd("2026-11-14"), value = total_2026)) %>%
  arrange(date) %>%
  mutate(year      = year(date),
         cum       = start + cumsum(value),
         predicted = year == 2026)

# Add the starting point so the line begins at 291
start_pt <- tibble(date = ymd("2021-09-01"), cum = start, predicted = FALSE)

# Season-end labels (last event each year)
labels_df <- plot_df %>%
  group_by(year) %>%
  slice_max(date, n = 1) %>%
  ungroup()

# Segment from end of 2025 to the predicted 2026 point (highlight)
pred_seg <- bind_rows(
  labels_df %>% filter(year == 2025) %>% dplyr::select(date, cum),
  labels_df %>% filter(year == 2026) %>% dplyr::select(date, cum)
)

ggplot() +
  # shade the predicted season window
  annotate("rect", xmin = ymd("2026-09-01"), xmax = ymd("2026-11-30"),
           ymin = -Inf, ymax = Inf, fill = "tomato", alpha = .12) +
  # observed history
  geom_step(data = bind_rows(start_pt, filter(plot_df, !predicted)),
            aes(date, cum), linewidth = .9, colour = "grey20") +
  geom_point(data = filter(plot_df, !predicted),
             aes(date, cum), size = 1.2, colour = "grey20") +
  # predicted step (highlighted)
  geom_step(data = pred_seg, aes(date, cum),
            colour = "tomato", linewidth = 1.2, linetype = "dashed") +
  geom_point(data = filter(plot_df, predicted),
             aes(date, cum), size = 3.5, colour = "tomato") +
  # season-end labels
  geom_text(data = labels_df,
            aes(date, cum, label = round(cum), colour = predicted),
            vjust = -1, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "grey20", `TRUE` = "tomato")) +
 scale_x_date(breaks = labels_df$date,
             labels = format(labels_df$date, "%Y")) +
  labs(x = NULL, y = "Total kills",
       title = "Faker's Worlds Kills 2026 season forecast",
       subtitle = "Will Faker reach 700 career kills in 2026?")
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85"))

ggsave(
  "plot.png",
  width = 1200 / 150,   # 8 inches
  height = 675 / 150,   # 4.5 inches (16:9)
  dpi = 300,
  bg = "white"
)

