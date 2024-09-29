# Load necessary libraries
library(readr)
library(ggplot2)

# Load the original dataset
MENTAL <- read_csv("MentalIllness-MATH350.csv")
# View(MENTAL)

# Read the cleaned dataset for further analysis
mental_health_data <- read.csv("mental-illnesses-prevalence-cleaned.csv")

# Define the countries of interest
countries_of_interest <- c("USA", "Australia", "Cameroon", "Cambodia")

# Filter dataset for those countries
filtered_data_multi <- subset(mental_health_data, Code %in% countries_of_interest)

# Loop through each country to perform correlation and regression analysis
for (country in countries_of_interest) {
  # Filter data for the current country
  country_data <- subset(filtered_data_multi, Code == country)
  
  # Perform Pearson correlation test
  correlation_test <- cor.test(country_data$Schizo, country_data$Bipolar, method = "pearson")
  
  # Perform linear regression analysis
  regression_model_country <- lm(Schizo ~ Bipolar, data = country_data)
  
  # Print results for each country
  cat("\nResults for", country, ":\n")
  print(correlation_test)
  print(summary(regression_model_country))
  
  # Create scatter plot with regression line for each country
  ggplot(country_data, aes(x = Bipolar, y = Schizo)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Schizophrenia vs Bipolar in", country),
         x = "Bipolar Disorder Prevalence",
         y = "Schizophrenia Prevalence") +
    theme_minimal()
}

# Time series analysis of Schizophrenia and Bipolar Disorder share over years for all countries
ggplot(filtered_data_multi, aes(x = Year, y = Schizo, group = Code, color = Code)) +
  geom_line() +
  geom_point() +
  labs(title = "Schizophrenia Share Over Time in Selected Countries",
       x = "Year",
       y = "Schizophrenia Share of Population") +
  theme_minimal()

ggplot(filtered_data_multi, aes(x = Year, y = Bipolar, group = Code, color = Code)) +
  geom_line() +
  geom_point() +
  labs(title = "Bipolar Disorder Share Over Time in Selected Countries",
       x = "Year",
       y = "Bipolar Disorder Share of Population") +
  theme_minimal()

# Box plots for Schizophrenia and Bipolar Disorder prevalence across countries
par(mfrow = c(1, 2))
boxplot(filtered_data_multi$Schizo ~ filtered_data_multi$Code, main = "Schizophrenia Prevalence",
        ylab = "Prevalence", xlab = "Country")
boxplot(filtered_data_multi$Bipolar ~ filtered_data_multi$Code, main = "Bipolar Disorder Prevalence",
        ylab = "Prevalence", xlab = "Country")
par(mfrow = c(1, 1))

# Histograms for each disorder across countries
par(mfrow = c(1, 2))
hist(filtered_data_multi$Schizo, main = "Distribution of Schizophrenia Prevalence",
     xlab = "Percentage", ylab = "Frequency", col = "lightblue")
hist(filtered_data_multi$Bipolar, main = "Distribution of Bipolar Disorder Prevalence",
     xlab = "Percentage", ylab = "Frequency", col = "lightcoral")
par(mfrow = c(1, 1))
