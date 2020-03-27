# pro srovnání...

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(zeme %in% c(
    "China", "Italy", "Japan", "Korea, South", "Czechia",
    "United Kingdom", "Slovakia", "Germany", "Spain", "US"
  ) & pocet > 0) %>%
  mutate(zeme = fct_relevel(as.factor(zeme), c("Czechia", "Slovakia"))) %>%
  group_by(zeme, datum) %>%
  summarise(pocet = sum(pocet)) %>%
  filter(pocet > 5) %>% # před pěti nemocnými je to hodně volatilní...
  group_by(zeme) %>%
  arrange(datum) %>%
  mutate(den = row_number())


ggplot(data = clean_data, aes(x = den, y = pocet, color = zeme, alpha = zeme)) +
  geom_line(aes(size = zeme)) +
  geom_text(
    data = slice(clean_data, which.max(den)),
    aes(x = den, y = pocet, label = pocet),
    hjust = -.5, show.legend = F
  ) +
  labs(
    title = "Trend šíření nákazy COVID-19 u nás a ve světě",
    color = "Počet potvrzených případů v zemi: ",
    x = "Dní od počátku vykazování",
    y = "Počet nakažených (log scale)",
    caption = paste("zdroj dat: Johns Hopkins, stav k", max(clean_data$datum) %>%
      format(format = "%d.%m.%Y"))
  ) +
  scale_x_continuous(limits = c(1, max(clean_data$den) + 3)) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_manual(
    values = c(
      "Czechia" = "firebrick",
      "Slovakia" = "goldenrod2",
      "China" = "gray45",
      "Italy" = "springgreen4",
      "Japan" = "cornflowerblue",
      "Korea, South" = "coral",
      "United Kingdom" = "red",
      "Germany" = "black",
      "US" = "yellowgreen",
      "Spain" = "yellow"
    ),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(size = 2)
    )
  ) +
  scale_alpha_manual(
    values = c(
      "Czechia" = 1,
      "Slovakia" = 1,
      "China" = .6,
      "Italy" = .6,
      "Japan" = .6,
      "Korea, South" = .6,
      "United Kingdom" = .6,
      "Germany" = .6,
      "Spain" = .6,
      "US" = .6
    ),
    guide = guide_none()
  ) +
  scale_size_manual(
    values = c(
      "Czechia" = 1.5,
      "Slovakia" = 1.5,
      "China" = .8,
      "Italy" = .8,
      "Japan" = .8,
      "Korea, South" = .8,
      "United Kingdom" = .8,
      "Germany" = .8,
      "Spain" = .8,
      "US" = .8
    ),
    guide = guide_none()
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25"),
    legend.position = "bottom"
  )

ggsave("./img/cizina.png", dpi = 300, units = "cm", width = 25, height = 16)
