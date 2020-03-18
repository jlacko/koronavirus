# pro srovnání...

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>% 
  filter(zeme %in% c("China", "Italy", "Japan", "Korea, South") & pocet > 0) %>% 
  group_by(zeme, datum) %>% 
  summarise(pocet = sum(pocet)) %>% 
  filter(pocet > 10) # před deseti nemocnými je to hodně volatilní...

ggplot(data = clean_data, aes(x = datum, y = pocet, color = zeme)) +
  geom_line(lwd = 1.2) +
  geom_text(data = slice(clean_data, which.max(datum)), 
            aes(x = datum, y = pocet, label = pocet),
            hjust = -.5, show.legend = F) +
  labs(title = "Trend šíření nákazy COVID-19 ve světě",
       color = "Počet nakažených koronavirem: ",
       caption = paste("zdroj dat: John Hopkins, stav k", max(clean_data$datum) %>% 
                         format(format = "%d.%m.%Y"))) +
  scale_x_date(date_breaks = "1 day",
               minor_breaks = NULL,
               labels = scales::date_format(format = "%d.%m."),
               limits = c(as.Date("2020-01-20"), max(clean_data$datum)+2)) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_viridis_d(guide = guide_legend(title.position = "top",
                                             title.hjust = 0.5)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title=element_blank(),
        panel.grid.major = element_line(color = "gray75"),
        plot.caption = element_text(color = "gray25"),
        legend.position="bottom")

ggsave("cizina.png", dpi = 300, units = "cm", width = 25, height = 16)