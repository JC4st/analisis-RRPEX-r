# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Load the data (update 'path_to_file' with the actual path)
data <- read_excel("baseRRPEX.xlsx")

# View the first few rows and get a summary of the data
head(data)
summary(data)

data <- data %>%
  mutate(across(c(edad.anos, peso.kg, hto, volumen_plasmatico, vpt, vol_albumina, Qb, ff, 
                  igg_pre, igg_post, rr), as.numeric),
         across(c(genero, diagnostico, tipo_terapia, anticoagulacion, novedades_filtro, 
                  complicaciones), as.factor),
         vpt_factor = factor(vpt, levels = sort(unique(vpt)))
  )


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

# modelos RR

modelo <- lm(rr ~ tipo_terapia, data = data)
summary(modelo)

variables_candidatas <- c("tipo_terapia", "edad.anos", "peso.kg", "hto", "igg_pre", "vpt", 
                          "Qb")

# Paso 1: Verificar porcentaje de NA
na_summary <- data %>%
  summarise(across(all_of(variables_candidatas), ~mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "porcentaje_na")

# Paso 2: Seleccionar variables con <20% NA
variables_buenas <- na_summary %>%
  filter(porcentaje_na < 0.2) %>%
  pull(variable)

# Paso 3: Crear fórmula
fmla <- as.formula(
  paste("rr ~", paste(setdiff(variables_buenas, "rr"), collapse = " + "))
)

# Paso 4: Ajustar modelo
modelo_auto <- lm(fmla, data = data)
summary(modelo_auto)

