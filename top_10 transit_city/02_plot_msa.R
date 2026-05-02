# ──────────────────────────────────────────────────────────────────────
# 02_plot_msa.R
# Traffic fatality rate trajectories, 10 large U.S. transit MSAs,
# 2015–2024. NYT-style framing with Los Angeles emphasized.
# ──────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
})

here <- "top_10 transit_city"
msa <- read_csv(file.path(here, "msa_fatality_2015_2024.csv"),
                show_col_types = FALSE)

# Short labels for the right margin
msa <- msa |>
  mutate(
    short_name = str_extract(name, "^[^-,]+(?:[-,][^,]+)?") |>
      str_remove(", .*$") |>
      str_replace("^Washington.*",      "Washington, DC") |>
      str_replace("^New York.*",        "New York") |>
      str_replace("^Los Angeles.*",     "Los Angeles") |>
      str_replace("^Chicago.*",         "Chicago") |>
      str_replace("^Philadelphia.*",    "Philadelphia") |>
      str_replace("^Boston.*",          "Boston") |>
      str_replace("^San Francisco.*",   "San Francisco") |>
      str_replace("^Seattle.*",         "Seattle") |>
      str_replace("^Baltimore.*",       "Baltimore") |>
      str_replace("^Pittsburgh.*",      "Pittsburgh"),
    is_la = name == "Los Angeles-Long Beach-Anaheim, CA"
  )

end_pts <- msa |> filter(year == max(year))

la_color    <- "#c44536"   # focal red
other_color <- "#bfbfbf"   # muted gray for context lines

p <- ggplot(msa, aes(year, rate, group = name)) +
  # context lines
  geom_line(data = filter(msa, !is_la),
            color = other_color, linewidth = 0.55, alpha = 0.9) +
  geom_point(data = filter(msa, !is_la),
             color = other_color, size = 0.9, alpha = 0.8) +

  # focal line: Los Angeles
  geom_line(data = filter(msa, is_la),
            color = la_color, linewidth = 1.5) +
  geom_point(data = filter(msa, is_la),
             color = la_color, size = 2.1) +

  # right-margin labels (single repel layer so LA never collides)
  geom_text_repel(
    data = end_pts,
    aes(label = short_name,
        color = is_la, fontface = ifelse(is_la, "bold", "plain"),
        size  = is_la),
    family = "serif",
    direction = "y", hjust = 0,
    nudge_x = 0.18, segment.color = "#dddddd",
    segment.size = 0.25, box.padding = 0.25, point.padding = 0.15,
    min.segment.length = 0,
    xlim = c(max(msa$year) + 0.2, NA), seed = 7
  ) +
  scale_color_manual(values = c(`FALSE` = "#555555", `TRUE` = la_color),
                     guide = "none") +
  scale_size_manual( values = c(`FALSE` = 2.9,        `TRUE` = 3.5),
                     guide = "none") +

  scale_x_continuous(
    breaks = seq(2015, 2024, 1),
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  coord_cartesian(clip = "off") +

  labs(
    title    = "Traffic fatality rates across ten large U.S. metropolitan areas, 2015-2024",
    subtitle = "Annual deaths per 100,000 residents. Core-Based Statistical Areas.",
    x = NULL,
    y = "Fatalities per 100,000",
    caption = "Source: NHTSA FARS (person file, INJ_SEV = 4); U.S. Census Bureau ACS 1-year (2020 Decennial Census for 2020)."
  ) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, color = "#1a1a1a",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.5, color = "#555555",
                                 margin = margin(b = 14)),
    plot.caption  = element_text(size = 8, color = "#888888", hjust = 0,
                                 margin = margin(t = 12)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",

    axis.title.y  = element_text(size = 10, color = "#333333",
                                 margin = margin(r = 8)),
    axis.text     = element_text(size = 9, color = "#555555"),
    axis.ticks    = element_blank(),

    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "#ececec", linewidth = 0.4),

    plot.margin = margin(t = 18, r = 60, b = 12, l = 12)
  )

ggsave(file.path(here, "msa_fatality_2015_2024.png"),
       plot = p, width = 11, height = 5.8, dpi = 320, bg = "white")

print(p)
