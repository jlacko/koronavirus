# pro srovnání...

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>% 
  filter(zeme %in% c("China", "Italy", "Japan", "Korea, South", "Czechia") & pocet > 0) %>% 
  mutate(zeme = fct_relevel(as.factor(zeme), "Czechia")) %>% 
  group_by(zeme, datum) %>% 
  summarise(pocet = sum(pocet)) %>% 
  filter(pocet > 5) %>%  # před pěti nemocnými je to hodně volatilní...
  group_by(zeme) %>% 
  arrange(datum) %>% 
  mutate(den = row_number())
  

ggplot(data = clean_data, aes(x = den, y = pocet, color = zeme, alpha = zeme)) +
  geom_line(lwd = 1.2) +
  geom_text(data = slice(clean_data, which.max(den)), 
            aes(x = den, y = pocet, label = pocet),
            hjust = -.5, show.legend = F) +
  labs(title = "Trend šíření nákazy COVID-19 u nás a ve světě",
       color = "Počet nakažených koronavirem: ",
       x = "Dní od zjištění nákazy",
       y = "Počet nakažených (log scale)",
       caption = paste("zdroj dat: John Hopkins, stav k", max(clean_data$datum) %>% 
                         format(format = "%d.%m.%Y"))) +
  scale_x_continuous(limits = c(1, max(clean_data$den)+2)) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_manual(values = c("Czechia" = "firebrick",
                                "China" = "gray45",
                                "Italy" = "springgreen4",
                                "Japan" = "slategray",
                                "Korea, South" = "coral"),
                     guide = guide_legend(title.position = "top",
                                          title.hjust = 0.5)) +
  scale_alpha_manual(values = c("Czechia" = 1,
                                "China" = .7,
                                "Italy" = .7,
                                "Japan" = .7,
                                "Korea, South" = .7),
                     guide = guide_none()) + 
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y =element_blank(),
        panel.grid.major = element_line(color = "gray75"),
        plot.caption = element_text(color = "gray25"),
        legend.position="bottom")

ggsave("cizina.png", dpi = 300, units = "cm", width = 25, height = 16)