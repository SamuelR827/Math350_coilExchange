library(readr)
MENTAL <- read_csv("MentalIllness-MATH350.csv")
View(MENTAL)

# Supremely clever ChatGPT code /s
# Load necessary library
library(ggplot2)  # For visualization (optional)

# Read the dataset from a CSV file
mental_health_data <- read.csv("mental-illnesses-prevalence-cleaned.csv")

# Defining what country to filter for
Country <- 'USA'

# Filtering the dataset for country
filtered_data <- subset(mental_health_data, Code == Country)

# Check the structure of the data (optional)
str(mental_health_data)

# Perform a linear regression analysis
regression_model <- lm(Schizo ~ Bipolar, data = mental_health_data)

# View the summary of the regression model
summary(regression_model)

# Optional: Visualize the regression
ggplot(mental_health_data, aes(x = Bipolar, y = Schizo)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regression Analysis: Schizophrenia vs. Bipolar Disorder",
       x = "Bipolar Disorder Prevalence",
       y = "Schizophrenia Prevalence")

# Scatter Plot
plot(filtered_data$Year, filtered_data$Schizo, type = "b",
     ylim = c(0.3, 0.7), xlab = "Year", ylab = "Share of Population",
     col = "blue", pch = 16,
     main = "Schizophrenia vs Bipolar Disorder Share Over Years")
lines(filtered_data$Year, filtered_data$Bipolar, type = "b", col = "red", pch = 17)

legend("topright", legend = c("Schizophrenia Share", "Bipolar Share"),
       col = c("blue", "red"), pch = c(16,17), bty = "n")

correlation <- cor.test(filtered_data$Schizo, filtered_data$Bipolar)
mtext(paste("Correlation:", round(correlation$estimate, 2), "\n", 
            "p-value:", signif(correlation$p.value, digits = 3)), 
      side = 1, line = 3, col = "darkgreen", adj = 0)



