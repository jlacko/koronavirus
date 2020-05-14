# vytvořit a uložit graf jako obrázek

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(zeme == "Czechia" & pocet > 0) %>%
  mutate(den = row_number())


# stará trend line - do 20. 3. včetně
march_12 <- nls(pocet ~ a * (1 + r)^(den),
  data = subset(clean_data, den < 20),
  start = list(a = 1, r = .01)
)

# čas ve dnech pro zdvojnásobení počtu nakažených; starý trend
double_march_12 <- log(2) / log(1 + coef(march_12)[["r"]])

# nová trend line - od 21. 3. do konce března
march_3 <- nls(pocet ~ a * (1 + r)^(den),
  data = subset(clean_data, den >= 20 & den < 32) %>% mutate(den = den - 19),
  start = list(a = 1, r = .01)
)

# čas ve dnech pro zdvojnásobení počtu nakažených; nový trend
double_march_3 <- log(2) / log(1 + coef(march_3)[["r"]])

# nová trend line - od 21. 3. do konce března
april_1 <- nls(pocet ~ a * (1 + r)^(den),
               data = subset(clean_data, den >= 32 & den < 42) %>% mutate(den = den - 31),
               start = list(a = 1, r = .01)
)

# čas ve dnech pro zdvojnásobení počtu nakažených; nový trend
double_april_1 <- log(2) / log(1 + coef(april_1)[["r"]])


# nová trend line - poslední data
april_2 <- nls(pocet ~ a * (1 + r)^(den),
               data = subset(clean_data, den >= 42 & den < 52) %>% mutate(den = den - 41),
               start = list(a = 1, r = .01)
)

# čas ve dnech pro zdvojnásobení počtu nakažených; nový trend
double_april_2 <- log(2) / log(1 + coef(april_2)[["r"]])

popisek_1 <- paste("Trend prvních dvou dekád března – zdvojnásobení počtu nakažených 1× za", str_replace(round(double_march_12, 2), "\\.", ","), "dní")
popisek_2 <- paste("Trend poslední březnové dekády – zdvojnásobení počtu nakažených 1× za", str_replace(round(double_march_3, 2), "\\.", ","), "dní")
popisek_3 <- paste("Trend první dubnové dekády – zdvojnásobení počtu nakažených 1× za", str_replace(round(double_april_1, 2), "\\.", ","), "dní")
popisek_4 <- paste("Trend druhé dubnové dekády – zdvojnásobení počtu nakažených 1× za", str_replace(round(double_april_2, 2), "\\.", ","), "dní")


ggplot(data = clean_data, aes(x = datum, y = pocet)) +
  geom_smooth(
    data = filter(clean_data, datum < as.Date("2020-03-20")),
    aes(color = "gray75"),
    method = "lm", size = .5, fullrange = T, se = F, linetype = "dashed"
  ) +
  geom_smooth(
    data = filter(clean_data, datum >= as.Date("2020-03-20") & datum < as.Date("2020-04-01")),
    aes(color = "gray75"),
    method = "lm", size = .5, fullrange = T, se = F, linetype = "dotdash"
  ) +
  geom_smooth(
    data = filter(clean_data, datum >= as.Date("2020-04-01")),
    aes(color = "gray75"),
    method = "lm", size = .5, fullrange = T, se = F, linetype = "dotted"
  ) +
  geom_line(aes(color = "firebrick"), lwd = 1.2) +
  geom_text(data = slice(clean_data, which.max(datum)), aes(
    x = datum, y = pocet,
    label = formatC(pocet, big.mark = " ", format = "f", digits = 0)
  ), hjust = -.5, color = "firebrick") +
  annotate("text", label = popisek_1, x = as.Date("2020-03-12"), y = 8, hjust = 0) +
  annotate("text", label = popisek_2, x = as.Date("2020-03-12"), y = 5.5, hjust = 0) +
  annotate("text", label = popisek_3, x = as.Date("2020-03-12"), y = 3.75, hjust = 0) +
  annotate("text", label = popisek_4, x = as.Date("2020-03-12"), y = 2.5, hjust = 0) +
  labs(
    title = "Trend šíření nákazy COVID-19 v ČR",
    color = "Počet nakažených",
    caption = paste("zdroj dat: https://onemocneni-aktualne.mzcr.cz, stav k", max(clean_data$datum) %>%
      format(format = "%d.%m.%Y"))
  ) +
  scale_x_date(
    date_breaks = "7 days",
    minor_breaks = NULL,
    labels = scales::date_format(format = "%d.%m."),
    limits = c(as.Date("2020-03-02"), max(clean_data$datum) + 5)
  ) +
  scale_y_log10(labels = scales::number_format(accuracy = 1),
                limits = c(1, 20000)) +
  scale_color_identity(
    labels = c("skutečnost", "zobecnění trendu"),
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

ggsave("./img/obrazek.png", dpi = 300, units = "cm", width = 25, height = 20)
