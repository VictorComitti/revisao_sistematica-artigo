library(readxl)
library(dlyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)

# Figura 1 ---------------------------------------------------------------------

DT <- dados %>%
  select(Revista) %>%
  table() %>%
  prop.table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  setNames(c("Revistas", "Frequencia")) %>%
  data.table()

# Gráfico de barras
ggplot(DT, aes(x = reorder(Revistas, Frequencia), y = Frequencia * 100)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(Frequencia * 100, 2), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Frequência de Revistas Acadêmicas (%)",
       x = "Revistas", y = "Frequência (%)") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 10))

# Figura 2 ---------------------------------------------------------------------

df <- dados %>%
  select(País) %>%
  mutate(País = str_replace_all(País, " e ", ",")) %>%
  separate_rows(País, sep = ",") %>%
  mutate(País = str_trim(País)) %>%
  mutate(País = str_replace_all(País, "[;:.]", ""))

frequencia <- df %>%
  drop_na() %>%
  count(País, sort = TRUE)

ggplot(frequencia, aes(x = reorder(País, n), y = n)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  labs(title = "", x = "País", y = "Frequência") +
  theme_minimal()
