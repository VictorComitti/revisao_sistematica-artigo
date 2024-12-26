library(readxl)
library(dlyr)
library(ggplot2)
library(data.table)

# Figura 1 ---------------------------------------------------------------------

DT <- dados %>%
  select(Revista) %>%
  table() %>%
  prop.table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  setNames(c("Revistas", "Frequencia")) %>%
  data.table()

# Plotar gráfico
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

