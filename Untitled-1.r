# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load the data (update 'path_to_file' with the actual path)
data <- read_excel("path_to_file.xlsx")

# View the first few rows and get a summary of the data
head(data)
summary(data)
# Summary statistics by tipo_terapia and vpt
data %>%
  group_by(tipo_terapia, vpt) %>%
  summarise(mean_rr = mean(rr, na.rm = TRUE),
            sd_rr = sd(rr, na.rm = TRUE),
            n = n())
# Boxplot with ggplot2
ggplot(data, aes(x = tipo_terapia, y = rr, fill = as.factor(vpt))) +
  geom_boxplot() +
  labs(title = "Distribución de rr según tipo de terapia y vpt",
       x = "Tipo de Terapia",
       y = "rr") +
  scale_fill_discrete(name = "VPT")