# Загрузка необходимых библиотек
library(tidyverse)
library(ggplot2)
library(readr)
library(corrplot)

# Шаг 1: Загрузка данных и создание гистограмм
data <- read_csv("~/Downloads/mexpenditures.csv")
head(data)

# Создание гистограмм для возраста, BMI и расходов
ggplot(data, aes(x=age)) +
  geom_histogram(bins=30, fill='blue', color='black') +
  ggtitle("Гистограмма возраста") +
  theme_minimal()

ggplot(data, aes(x=bmi)) +
  geom_histogram(bins=30, fill='green', color='black') +
  ggtitle("Гистограмма BMI") +
  theme_minimal()

ggplot(data, aes(x=charges)) +
  geom_histogram(bins=30, fill='red', color='black') +
  ggtitle("Гистограмма медицинских расходов") +
  theme_minimal()

# Подробнее про выбросы

# Функция boxplot возвращает список, одним из элементов которого является $out, содержащий выбросы
get_boxplot_outliers <- function(variable) {
  bp <- boxplot(variable, plot = FALSE)  # plot=FALSE чтобы не создавать график
  return(bp$out)
}

# Получаем выбросы для 'age', 'bmi', 'charges'
outliers_age <- get_boxplot_outliers(data$age)
outliers_bmi <- get_boxplot_outliers(data$bmi)
outliers_charges <- get_boxplot_outliers(data$charges)

# Выводим количество выбросов
length(outliers_age)
length(outliers_bmi)
length(outliers_charges)

# Визуализация выбросов с помощью boxplot для каждой переменной
par(mfrow=c(1,3))  # Определяем макет для 3 графиков в одной строке

# Возраст
boxplot(data$age, main="Boxplot возраста", horizontal = TRUE, col = "lightblue")

# BMI
boxplot(data$bmi, main="Boxplot BMI", horizontal = TRUE, col = "lightgreen")

# Медицинские расходы
boxplot(data$charges, main="Boxplot медицинских расходов", horizontal = TRUE, col = "lightcoral")

# Восстановление стандартного макета графиков
par(mfrow=c(1,1))

# Шаг 2: Сравнение групп мужчин и женщин
# Нужно использовать t-тест или непараметрический тест в зависимости от нормальности распределения

# Функция для выполнения статистического теста в зависимости от нормальности распределения
perform_statistical_test <- function(data, variable) {
  # Проверка на нормальность для мужчин и женщин
  shapiro_test_women <- shapiro.test(data[data$sex == 'female', ][[variable]])
  shapiro_test_men <- shapiro.test(data[data$sex == 'male', ][[variable]])
  
  # Выбор и выполнение теста
  if (shapiro_test_women$p.value > 0.05 && shapiro_test_men$p.value > 0.05) {
    # Если распределения нормальные, применяем t-тест
    test_result <- t.test(data[[variable]] ~ data$sex)
  } else {
    # Иначе применяем непараметрический тест Манна-Уитни
    test_result <- wilcox.test(data[[variable]] ~ data$sex)
  }
  
  # Возвращаем результат
  return(test_result)
}

# Применяем функцию к переменным 'age', 'bmi', 'charges'
test_result_age <- perform_statistical_test(data, 'age')
test_result_bmi <- perform_statistical_test(data, 'bmi')
test_result_charges <- perform_statistical_test(data, 'charges')

# Выводим результаты тестов
print("Результаты теста для возраста:")
print(test_result_age)

print("Результаты теста для BMI:")
print(test_result_bmi)

print("Результаты теста для медицинских расходов:")
print(test_result_charges)

# Шаг 3: Корреляционный анализ
cor_matrix <- cor(data[,c('age', 'bmi', 'charges')])

# Визуализация корреляционной матрицы
corrplot::corrplot(cor_matrix, method = "circle")
print(cor_matrix)
