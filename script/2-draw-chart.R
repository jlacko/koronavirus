# vytvořit a uložit graf jako obrázek

library(tidyverse)

pdf.options(encoding = "ISOLatin2") # aby nepadal pandoc na konverzi češtiny v nadpisech grafů

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(zeme == "Czechia" & pocet > 0) %>%
  mutate(den = lubridate::day(datum))

trend <- nls(pocet ~ a * (1 + r)^(den),
  data = subset(clean_data, den < 21),
  start = list(a = 1, r = .01)
)

budoucnost <- data.frame(den = 21:31)

predpoved <- data.frame(
  datum = c(as.Date("2020-03-21"):as.Date("2020-03-31")) %>% as.Date(origin = as.Date("1970-01-01")),
  pocet = predict(trend, newdata = budoucnost)
)

ggplot(data = clean_data, aes(x = datum, y = pocet)) +
  geom_text(
    data = predpoved, aes(
      x = datum, y = pocet,
      label = formatC(pocet, big.mark = " ", format = "f", digits = 0)
    ),
    hjust = 1.2,
    color = "gray50"
  ) +
  geom_smooth(data = filter(clean_data, datum >= as.Date("2020-03-21")),
              aes(color = "gray75"),
              method = "lm", size = .5, fullrange = T, se = F, linetype = "dashed") +
  geom_line(aes(color = "firebrick"), lwd = 1.2) +
  geom_point(data = predpoved, aes(
    x = datum, y = pocet), pch = 4) +
  geom_text(data = slice(clean_data, which.max(datum)), aes(
    x = datum, y = pocet,
    label = formatC(pocet, big.mark = " ", format = "f", digits = 0)
  ), hjust = -.5, color = "firebrick") +
  labs(
    title = "Trend šíření nákazy COVID-19 v ČR",
    color = "Počet nakažených",
    caption = paste("zdroj dat: Johns Hopkins, stav k", max(clean_data$datum) %>%
      format(format = "%d.%m.%Y"))
  ) +
  scale_x_date(
    date_breaks = "1 day",
    minor_breaks = NULL,
    labels = scales::date_format(format = "%d.%m."),
    limits = as.Date(c("2020-03-02", "2020-03-30"))
  ) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_identity(
    labels = c("skutečnost", "trend od 20.3.2020"),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5)
    ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25"),
    legend.position = "bottom"
  )


ggsave("./img/obrazek.pdf", dpi = 300, units = "cm", width = 25, height = 20)
ggsave("./img/obrazek.png", dpi = 300, units = "cm", width = 25, height = 20)
