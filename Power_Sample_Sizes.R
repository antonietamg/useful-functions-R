##### Calculates the power for different sample sizes

# Cargar las bibliotecas necesarias
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(pwr)) install.packages("pwr")
library(pwr)

set.seed(123)  # Para reproducibilidad

# Función para calcular la potencia para diferentes tamaños de muestra
calculate_power <- function(n, effect_size, nsim = 1000) {
  power_t <- power_mw <- numeric(length(n))
  
  for (i in seq_along(n)) {
    p_values_t <- p_values_mw <- numeric(nsim)
    
    for (j in 1:nsim) {
      # Generar datos
      group1 <- rnorm(n[i], mean = 0, sd = 1)
      group2 <- rnorm(n[i], mean = effect_size, sd = 1)
      
      # Realizar t-test
      test_t <- t.test(group1, group2)
      p_values_t[j] <- test_t$p.value
      
      # Realizar Mann-Whitney test
      test_mw <- wilcox.test(group1, group2)
      p_values_mw[j] <- test_mw$p.value
    }
    
    # Calcular la potencia
    power_t[i] <- mean(p_values_t < 0.05)
    power_mw[i] <- mean(p_values_mw < 0.05)
  }
  
  return(data.frame(Size = n, Power_T = power_t, Power_MW = power_mw))
}

# Tamaños de muestra a probar
sample_sizes <- seq(10, 100, by = 10)

# Efecto moderado para observar diferencias claras
effect_size <- 0.5

# Calcular potencia
power_data <- calculate_power(sample_sizes, effect_size)

# Gráfico de potencia vs. tamaño de muestra
ggplot(power_data, aes(x = Size)) +
  geom_line(aes(y = Power_T, colour = "T-test"), size = 1.2) +
  geom_line(aes(y = Power_MW, colour = "Mann-Whitney"), size = 1.2) +
  labs(title = "Comparación de la Potencia entre T-test y Mann-Whitney",
       x = "Tamaño de Muestra",
       y = "Potencia",
       colour = "Test") +
  theme_minimal() +
  scale_colour_manual(values = c("blue", "red"))
