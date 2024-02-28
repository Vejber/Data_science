library(tidyverse)

# Load the data
school_earnings <- read.csv("~/Downloads/school_earnings.csv")

# View the dataset
# View(school_earnings)

# Numerical description of the data
summary(school_earnings)

# Using dplyr to get summary for each gender
school_earnings %>%
  summarise(
    Mean_Women = mean(Women, na.rm = TRUE),
    Median_Women = median(Women, na.rm = TRUE),
    Min_Women = min(Women, na.rm = TRUE),
    Max_Women = max(Women, na.rm = TRUE),
    Mean_Men = mean(Men, na.rm = TRUE),
    Median_Men = median(Men, na.rm = TRUE),
    Min_Men = min(Men, na.rm = TRUE),
    Max_Men = max(Men, na.rm = TRUE)
  )

# Reshape the data to long format for plotting
long_data <- school_earnings %>%
  pivot_longer(cols = Women:Men, names_to = "Gender", values_to = "Salary")

# Boxplot with ggplot2
ggplot(long_data, aes(x = School, y = Salary, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Распределение зарплат по полу и учебному заведению", 
       x = "Учебное заведение", y = "Зарплата") +
  scale_fill_brewer(palette = "Set1") +
  theme_light(base_size = 16) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


