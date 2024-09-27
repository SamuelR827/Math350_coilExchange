library(readr)
MENTAL <- read_csv("MentalIllness-MATH350.csv")
View(MENTAL)

# Supremely clever ChatGPT code /s
# Load necessary library
library(ggplot2)  # For visualization (optional)

# Read the dataset from a CSV file
mental_health_data <- read.csv("MentalIllness-MATH350.csv")

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

