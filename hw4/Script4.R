# Загрузка необходимых библиотек
library(tidyverse)
library(ggplot2)
library(readr)
library(corrplot)
library(tidyr)
library(GGally)
library(plotly)
library(stats)
library(reshape2)

# Загрузка данных и создание гистограмм
data <- read_csv("~/Downloads/insurance 2.csv")
head(data)

summary(data)

data$logged_charges <- log(data$charges)

# Создаем формулу, исключая исходный столбец с размерами страховых выплат
formula <- as.formula(paste("logged_charges ~", paste(names(data)[!names(data) %in% c("charges", "logged_charges")], collapse = " + ")))

# Построение модели линейной регрессии
model <- lm(formula, data = data)

# Отображение диагностических графиков для модели
par(mfrow = c(2, 2))  # Разделение графического окна на 2x2 матрицу для отображения нескольких графиков
plot(model)            
# Выводит четыре диагностических графика:
# 1. График остатков против подогнанных значений
# 2. Q-Q plot для остатков
# 3. График Scale-Location
# 4. График остатков против рычагов (leverage)

# Вывод результатов
summary(model)

#График остатков против предсказанных значений (фитов)
#plot(model$fitted.values, residuals(model),
#     xlab = "Fitted values", ylab = "Residuals",
#     main = "Residuals vs Fitted")
#abline(h = 0, col = "red")

#График Q-Q (квантиль-квантиль) остатков
#qqnorm(residuals(model))
#qqline(residuals(model), col = "red")

#График Scale-Location (или Spread-Location)
#plot(model$fitted.values, sqrt(abs(residuals(model))),
#     xlab = "Fitted values", ylab = "Sqrt(|Residuals|)",
#     main = "Scale-Location")

#График Residuals vs Leverage
#plot(model, which = 5)

