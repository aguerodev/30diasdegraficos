

# Dependencias ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(here)
library(haven)
library(scales)
library(glue)

# Lectura de datos --------------------------------------------------------
datos <- read_sav(here("data/ENAHO_2019.sav")) %>%
  filter(!is.na(ID_HOGAR)) %>%
  select(REGION, V19) %>%
  mutate(REGION = as_factor(REGION), V19 = as_factor(V19)) %>%
  rename(region = REGION, tiene_internet = V19) %>%
  count(region, tiene_internet) %>%
  group_by(region) %>%
  mutate(prop = round(n / sum(n), 4)) %>%
  ungroup()



# Grafico -----------------------------------------------------------------
ggplot(data = datos,
       mapping = aes(x = region, y = n,  fill = tiene_internet)) +
  geom_col(position = "fill") +
  geom_text(
    mapping = aes(y = prop, label = percent(prop, accuracy = 0.01)),
    position = position_fill(vjust = 0.5),
    family = "Lato Light",
    size = 4
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(
    values = c("#eb7070", "#a1dd70"),
    guide = guide_legend(
      title.position = "top",
      reverse = T,
      label.position = "left",
      keyheight = 0.5
    ),
    labels = c("No", "Sí")
  )  +
  labs(
    fill = "¿Tiene acceso a Internet en su hogar?",
    title = "Costa Rica: Porcentaje de hogares con acceso a Internet por región, 2019",
    caption = "Fuente: INEC, Encuesta Nacional de Hogares",
    x = "",
    y = ""
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(l = 15, t = 5, b = -5),
    plot.title = element_text(family = "Lato Black", size = 16)
  )

ggsave(
  here("plots/dia_1_barras.png"),
  width = 10,
  height = 5,
  dpi = 300
)
