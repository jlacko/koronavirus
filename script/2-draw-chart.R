# vytvořit a uložit graf jako obrázek

library(tidyverse)

pdf.options(encoding = 'ISOLatin2') # aby nepadal pandoc na konverzi češtiny v nadpisech grafů

clean_data <- read_csv2("./data/raw_data.csv") %>% 
  filter(zeme == "Czechia" & pocet > 0)


ggplot(data = clean_data, aes(x = datum, y = pocet)) +
  geom_smooth(method = "lm", se = F, lwd = .5, fullrange = T, color = "gray75") +
  geom_line(color = "firebrick", lwd = 1.2) +
  geom_text(data = slice(clean_data, which.max(datum)), aes(x = datum, y = pocet, 
                                 label = pocet), hjust = -.5, color = "firebrick") +
  labs(title = "Trend šíření nákazy COVID-19 v ČR",
       color = "Počet nakažených",
       caption = paste("zdroj dat: John Hopkins, stav k", max(clean_data$datum) %>% 
                         format(format = "%d.%m.%Y"))) +
  scale_x_date(date_breaks = "1 day",
               minor_breaks = NULL,
               labels = scales::date_format(format = "%d.%m."),
               limits = as.Date(c("2020-03-02", "2020-04-10")) ) +
  scale_y_log10(labels = scales::number_format()) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title=element_blank(),
        panel.grid.major = element_line(color = "gray75"),
        plot.caption = element_text(color = "gray25"))
  
ggsave("obrazek.pdf", dpi = 300, units = "cm", width = 25, height = 16)
ggsave("obrazek.png", dpi = 300, units = "cm", width = 25, height = 16)