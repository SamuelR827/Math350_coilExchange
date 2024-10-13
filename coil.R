# Load necessary libraries
library(readr)
library(ggplot2)

# Read the dataset for further analysis
mental_health_data <- read.csv("mental-illnesses-prevalence-all.csv")

# Get only required countries
countries_of_interest <- c("USA")  # "Australia", "Cameroon", "Cambodia"
filtered_data <- subset(mental_health_data, Code %in% countries_of_interest)

# Perform linear regression analysis
linear_model <- lm(Schizo ~ Bipolar, data = filtered_data)
summary(linear_model)
linear_model_schizo <- lm(Schizo ~ Year, data = filtered_data)
linear_model_bipolar <- lm(Bipolar ~ Year, data = filtered_data)

# Perform more complex regression
filtered_data$Bipolar2 <- filtered_data$Bipolar^2
filtered_data$Bipolar3 <- filtered_data$Bipolar^3
filtered_data$Bipolar4 <- filtered_data$Bipolar^4
quadradic_model <-lm(Schizo ~ Bipolar + Bipolar2, data = filtered_data)
cubic_model <-lm(Schizo ~ Bipolar + Bipolar2 + Bipolar3, data = filtered_data)
quartic_model <-lm(Schizo ~ Bipolar + Bipolar2 + Bipolar3 + Bipolar4, data = filtered_data)
summary(quadradic_model)
summary(cubic_model)
summary(quartic_model)

# Time series scatter plot for Schizophrenia and Bipolar Disorder share over years for the USA
time_series_plot_combined <- function(){
plot(filtered_data$Year, filtered_data$Schizo, type = "b",
     ylim = c(0.3, 0.7), xlab = "Year", ylab = "Share of Population",
     col = "blue", pch = 16,
     main = "Schizophrenia vs Bipolar Disorder Share Over Years (USA)")
lines(filtered_data$Year, filtered_data$Bipolar, type = "b", col = "red", pch = 17)

legend("topright", legend = c("Schizophrenia Share", "Bipolar Share"),
       col = c("blue", "red"), pch = c(16, 17), bty = "n")

# Correlation test between Schizophrenia and Bipolar disorder in the USA
correlation <- cor.test(filtered_data$Schizo, filtered_data$Bipolar)
mtext(paste("Correlation:", round(correlation$estimate, 2), "\n", 
            "p-value:", signif(correlation$p.value, digits = 3)), 
      side = 1, line = 3, col = "darkgreen", adj = 0)
}
time_series_plot_combined()

# Loop through each country to perform correlation and regression analysis
for (country in countries_of_interest) {
  # Filter data for the current country
  country_data <- subset(filtered_data, Code == country)
  
  # Perform Pearson correlation test
  correlation_test <- cor.test(country_data$Bipolar, country_data$Schizo, method = "pearson")
  
  # Perform linear regression analysis
  linear_model_country <- lm(Schizo ~ Bipolar, data = country_data)
  
  # Print results for each country
  cat("\nResults for", country, ":\n")
  print(correlation_test)
  print(summary(linear_model_country))
  
  # Create scatter plot with regression line for each country
  ggplot(country_data, aes(x = Bipolar, y = Schizo)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Schizophrenia vs Bipolar in", country),
         x = "Bipolar Disorder Prevalence",
         y = "Schizophrenia Prevalence") +
    theme_minimal()
}

cor.test(filtered_data$Bipolar, filtered_data$Schizo, method = "pearson")
cor.test(filtered_data$Bipolar2, filtered_data$Schizo, method = "pearson")
# Quadradic Regression Plot
schizo_predict <- predict(quadradic_model,list(Bipolar=filtered_data$Bipolar, Bipolar2=filtered_data$Bipolar2))
plot(filtered_data$Bipolar, filtered_data$Schizo,
     main = "Quadradic Regression Visualized (USA)", 
     xlab = "Bipolar Disorder Prevalence", ylab = "Schizophrenia Prevalence"
     )
lines(filtered_data$Bipolar, schizopredict , col='blue')

# Time series analysis of Schizophrenia and Bipolar Disorder share over years for all countries
ggplot(filtered_data, aes(x = Year, y = Schizo, group = Code, color = Code)) +
  geom_line() +
  geom_point() +
  labs(title = "Schizophrenia Share Over Time",
       x = "Year",
       y = "Schizophrenia Share of Population") +
  theme_minimal()

ggplot(filtered_data, aes(x = Year, y = Bipolar, group = Code, color = Code)) +
  geom_line() +
  geom_point() +
  labs(title = "Bipolar Disorder Share Over Time",
       x = "Year",
       y = "Bipolar Disorder Share of Population") +
  theme_minimal()

# Quartile table statistics
summary(filtered_data$Schizo)
sd(filtered_data$Schizo)
summary(filtered_data$Bipolar)
sd(filtered_data$Bipolar)

# Box plots for Schizophrenia and Bipolar Disorder prevalence across countries
par(mfrow = c(1, 2))
boxplot(filtered_data$Schizo ~ filtered_data$Code, main = "Schizophrenia Prevalence (USA)",
        xlab = "Prevalence", ylab = "USA", horizontal = TRUE)
boxplot(filtered_data$Bipolar ~ filtered_data$Code, main = "Bipolar Disorder Prevalence (USA)",
        xlab = "Prevalence", ylab = "USA", horizontal = TRUE)
par(mfrow = c(1, 1))

# Histograms for each disorder across countries
par(mfrow = c(1, 2))
hist(filtered_data$Schizo, main = "Distribution of Schizophrenia Prevalence (USA)",
     xlab = "Percentage", ylab = "Frequency", col = "lightblue")
hist(filtered_data$Bipolar, main = "Distribution of Bipolar Disorder Prevalence (USA)",
     xlab = "Percentage", ylab = "Frequency", col = "lightcoral")
par(mfrow = c(1, 1))

# QQ Norm Plots - Included in next section
qqnorm(filtered_data$Schizo, pch = 1, frame = FALSE,
       main = "Normal Q-Q Plot for Schizophrenia Disorder Prevalence (USA)")
qqline(filtered_data$Schizo, col = "steelblue", lwd = 2)

qqnorm(filtered_data$Bipolar, pch = 1, frame = FALSE,
       main = "Normal Q-Q Plot for Bipolar Disorder Prevalence (USA)")
qqline(filtered_data$Bipolar, col = "steelblue", lwd = 2)

# Diagnostics Panel that is required for the presentation
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0))
# Cannot name these with "main=..." because it names all 4 the same thing
plot(linear_model_schizo, main = "")
title("\nDiagnostics panel for Schizophrenia Disorder Prevalence (USA)", outer = TRUE)
# mtext("Diagnostics Panel for Presentation", outer = TRUE, cex = 1.5)
plot(linear_model_bipolar)
title("\nDiagnostics panel for Bipolar Disorder Prevalence (USA)", outer = TRUE)
par(mfrow = c(1,1))

