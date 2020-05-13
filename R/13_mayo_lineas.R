
# Dependencias ------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)

Sys.setlocale("LC_TIME", "es_ES.utf8")

# Lectura datos -----------------------------------------------------------
# http://geovision.uned.ac.cr/oges/
datos <- read_csv("data/covid_12_mayo_cr.csv",
                  col_types = cols(FECHA = col_date(format = "%d/%m/%Y"))) %>% 
  clean_names() %>% 
  mutate(semana = floor_date(fecha,"week")) %>% 
  drop_na(fecha)


ggplot(data = datos, mapping = aes(x = fecha)) +
  geom_line(mapping = aes(y = positivos, color = "1Total Casos"), size = 1) +
  geom_line(mapping = aes(y = activos, color = "2Activos"), size = 1) +
  geom_line(mapping = aes(y = recuperados, color = "3Recuperados"), size = 1) +
  scale_x_date(date_labels = "%B\n%d",date_breaks = "week") +
  labs(title = "Costa Rica: Casos positivos, activos y recuperados de Covid19",
       subtitle = "Datos actualizados al 12 de mayo del 2020",
       color = "",
       y = "Cantidad de casos",
       x = "") +
  scale_color_manual(
    values = c("#363636", "#eb4559","#639a67"),
    guide = guide_legend(
      title.position = "top",
      label.position = "left",
      label.vjust = 0.6
    ),
    labels = c("Total de casos","Activos","Recuperados")
  )  +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(t = -5, b = 5),
    plot.title = element_text(family = "Lato Black", size = 16),
    plot.subtitle = element_text(family = "Lato", size = 12)
  )

ggsave(
  here("plots/dia_2_lineas.png"),
  width = 10,
  height = 5,
  dpi = 300
)
