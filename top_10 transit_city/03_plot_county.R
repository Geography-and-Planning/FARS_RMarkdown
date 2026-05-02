# ──────────────────────────────────────────────────────────────────────
# 03_plot_county.R
# Traffic fatality rate trajectories, 10 central counties of the
# largest U.S. transit MSAs, 2015–2024. Los Angeles County emphasized.
# ──────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
})

here <- "top_10 transit_city"
county <- read_csv(file.path(here, "county_fatality_2015_2024.csv"),
                   show_col_types = FALSE)

county <- county |>
  mutate(
    short_name = name |>
      str_replace(", .*$", "") |>
      str_replace("^District of Columbia$", "Washington, DC"),
    is_la = name == "Los Angeles County, CA"
  )

end_pts <- county |> filter(year == max(year))

la_color    <- "#c44536"
other_color <- "#bfbfbf"

p <- ggplot(county, aes(year, rate, group = name)) +
  geom_line(data = filter(county, !is_la),
            color = other_color, linewidth = 0.55, alpha = 0.9) +
  geom_point(data = filter(county, !is_la),
             color = other_color, size = 0.9, alpha = 0.8) +

  geom_line(data = filter(county, is_la),
            color = la_color, linewidth = 1.5) +
  geom_point(data = filter(county, is_la),
             color = la_color, size = 2.1) +

  geom_text_repel(
    data = end_pts |>
      mutate(label_text = if_else(is_la, "Los Angeles County", short_name)),
    aes(label = label_text,
        color = is_la, fontface = ifelse(is_la, "bold", "plain"),
        size  = is_la),
    family = "serif",
    direction = "y", hjust = 0,
    nudge_x = 0.18, segment.color = "#dddddd",
    segment.size = 0.25, box.padding = 0.25, point.padding = 0.15,
    min.segment.length = 0,
    xlim = c(max(county$year) + 0.2, NA), seed = 7
  ) +
  scale_color_manual(values = c(`FALSE` = "#555555", `TRUE` = la_color),
                     guide = "none") +
  scale_size_manual( values = c(`FALSE` = 2.9,        `TRUE` = 3.5),
                     guide = "none") +

  scale_x_continuous(
    breaks = seq(2015, 2024, 1),
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  coord_cartesian(clip = "off") +

  labs(
    title    = "Traffic fatality rates in ten central counties of major U.S. transit metros, 2015-2024",
    subtitle = "Annual deaths per 100,000 residents. Central county of each MSA.",
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

    plot.margin = margin(t = 18, r = 70, b = 12, l = 12)
  )

ggsave(file.path(here, "county_fatality_2015_2024.png"),
       plot = p, width = 11, height = 5.8, dpi = 320, bg = "white")

print(p)
