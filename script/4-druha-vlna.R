
library(tidyverse )

clean_data <- read_csv("./data/nakaza.csv") %>%
  filter(datum >= as.Date("2020-09-01")) %>%
  mutate(pocet = pocet_den,
         den = lubridate::day(datum))

trend <- nls(pocet ~ a * (1 + r)^(den),
             data = subset(clean_data, den < lubridate::day(Sys.Date())),
             start = list(a = 1, r = .01)
)

budoucnost <- data.frame(den = lubridate::day(Sys.Date()):30)

predpoved <- data.frame(
  datum = c(Sys.Date():as.Date("2020-09-30")) %>% as.Date(origin = as.Date("1970-01-01")),
  pocet = predict(trend, newdata = budoucnost)
)

# čas ve dnech pro zdvojnásobení
double_trend <- log(2) / log(1 + coef(trend)[["r"]])
popisek <- paste("Stávající trend – zdvojnásobení počtu nových případů 1× za", str_replace(round(double_trend, 2), "\\.", ","), "dní")

ggplot(data = clean_data, aes(x = datum, y = pocet)) +
  geom_text(
    data = predpoved, aes(
      x = datum, y = pocet,
      label = formatC(pocet, big.mark = " ", format = "f", digits = 0)
    ),
    hjust = 1.25,
    vjust = .5,
    angle = -45,
    color = "gray50"
  ) +
  geom_line(color = "firebrick", lwd = 1.2) +
  geom_point(data = predpoved, aes(
    x = datum, y = pocet), pch = 4) +
  geom_text(data = slice(clean_data, which.min(datum)), aes(
    x = datum, y = pocet,
    label = pocet
  ), hjust = 1.3, color = "firebrick") +
  geom_text(data = slice(clean_data, which.max(datum)), aes(
    x = datum, y = pocet,
    label = pocet
  ), hjust = -.15, vjust = 1.25, color = "firebrick") +
  labs(
    title = "Trend denního počtu nově diagnostikovaných případů nákazy COVID-19 za měsíc září",
    color = "Počet nakažených",
    caption = paste("zdroj dat: https://onemocneni-aktualne.mzcr.cz, stav k", max(clean_data$datum) %>%
                      format(format = "%d.%m.%Y"))
  ) +
  annotate("label", label = popisek, x = as.Date("2020-09-15"), y = 750, hjust = 0, 
           fill = "white") +
  scale_x_date(
    date_breaks = "1 day",
    minor_breaks = NULL,
    labels = scales::date_format(format = "%d.%m."),
    limits = as.Date(c("2020-09-01", "2020-09-31"))
  ) +
  scale_y_continuous(labels = scales::number_format(),
                     limits = c(0, 17500)) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25")
  )


ggsave("./img/druha-vlna.png", dpi = 300, units = "cm", width = 25, height = 16)
