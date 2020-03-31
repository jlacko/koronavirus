# vytvořit a uložit graf jako obrázek

library(tidyverse)

pdf.options(encoding = "ISOLatin2") # aby nepadal pandoc na konverzi češtiny v nadpisech grafů

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(zeme == "Czechia" & pocet > 0) %>%
  mutate(den = row_number())


# stará trend line - do 20. 3. včetně
old_trend <- nls(pocet ~ a * (1 + r)^(den),
  data = subset(clean_data, den <= 20),
  start = list(a = 1, r = .01)
)

# čas ve dnech pro zdvojnásobení počtu nakažených; starý trend
double_old <- log(2) / log(1 + coef(old_trend)[["r"]])

# predikce budoucnosti podle starého trendu
budoucnost <- data.frame(den = seq(from = 21, length.out = 11))

predpoved <- data.frame(
  datum = seq(from = as.Date("2020-03-21"), by = 1, length.out = 11),
  pocet = predict(old_trend, newdata = budoucnost)
)

# nová trend line - od 21. 3. dále
new_trend <- nls(pocet ~ a * (1 + r)^(den),
  data = subset(clean_data, den > 20) %>% mutate(den = den - 19),
  start = list(a = 1, r = .01)
)

# čas ve dnech pro zdvojnásobení počtu nakažených; nový trend
double_new <- log(2) / log(1 + coef(new_trend)[["r"]])

popisek_old <- paste("Trend před 20. březnem – zdvojnásobení počtu nakažených 1× za", str_replace(round(double_old, 2), "\\.", ","), "dní")
popisek_new <- paste("Trend po 20. březnu – zdvojnásobení počtu nakažených 1× za", str_replace(round(double_new, 2), "\\.", ","), "dní")

ggplot(data = clean_data, aes(x = datum, y = pocet)) +
  geom_text(
    data = predpoved, aes(
      x = datum, y = pocet,
      label = formatC(pocet, big.mark = " ", format = "f", digits = 0)
    ),
    hjust = 1.2,
    color = "gray50"
  ) +
  geom_smooth(
    data = filter(clean_data, datum >= as.Date("2020-03-21")),
    aes(color = "gray75"),
    method = "lm", size = .5, fullrange = T, se = F, linetype = "dashed"
  ) +
  geom_line(aes(color = "firebrick"), lwd = 1.2) +
  geom_point(data = predpoved, aes(
    x = datum, y = pocet
  ), pch = 4, show.legend = F) +
  geom_text(data = slice(clean_data, which.max(datum)), aes(
    x = datum, y = pocet,
    label = formatC(pocet, big.mark = " ", format = "f", digits = 0)
  ), hjust = -.5, color = "firebrick") +
  annotate("text", label = popisek_old, x = as.Date("2020-03-20"), y = 7, hjust = 0) +
  annotate("text", label = popisek_new, x = as.Date("2020-03-20"), y = 4.5, hjust = 0) +
  labs(
    title = "Trend šíření nákazy COVID-19 v ČR",
    color = "Počet nakažených",
    caption = paste("zdroj dat: https://onemocneni-aktualne.mzcr.cz, stav k", max(clean_data$datum) %>%
      format(format = "%d.%m.%Y"))
  ) +
  scale_x_date(
    breaks = seq(from = as.Date("2020-03-01"), by = 1, to = as.Date("2020-04-15")),
    minor_breaks = NULL,
    labels = scales::date_format(format = "%d.%m."),
    limits = as.Date(c("2020-03-02", "2020-04-14"))
  ) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_identity(
    labels = c("skutečnost", "trend po 20.3.2020"),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
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
