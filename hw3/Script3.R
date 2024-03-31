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
data <- read_csv("~/Downloads/tripadvisor_review.csv")
head(data)

summary(data)

# Изменение названий столбцов
names(data) <- c("user id", "art galleries", "dance clubs", "juice bars", "restaurants", "museums", "resorts", "parks/picnic spots", "beaches", "theatres", "religious institutions")
names(data)

# Создание гистограмм
# Преобразуем данные из широкого формата в длинный для использования с ggplot
long_data <- data %>%
  pivot_longer(cols = "art galleries":"religious institutions", names_to = "Feature", values_to = "Rating")

# Boxplot с ggplot2
ggplot(long_data, aes(x = Rating, y = Feature, fill = Rating)) +
  geom_boxplot() +
  labs(title = "Распределение рейтингов", 
       x = "Рейтинг", y = "Место") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16)

#Корреляционный анализ
cor_matrix <- cor(data[,c("art galleries", "dance clubs", "juice bars", "restaurants", "museums", "resorts", "parks/picnic spots", "beaches", "theatres", "religious institutions")])

# Визуализация корреляционной матрицы
corrplot::corrplot(cor_matrix, method = "circle")
print(cor_matrix)

# Шкалирование
scaled_data <- scale(data[,c(2:6,8:10)])
scaled_data <- as.data.frame(scaled_data)

# Проверка средних значений (должны быть близки к 0)
colMeans(scaled_data)

# Проверка стандартных отклонений (должны быть близки к 1)
apply(scaled_data, 2, sd)

# Вычисление и анализ матрицы корреляции масштабированных данных
cor_matrix_scaled <- cor(scaled_data)
corrplot::corrplot(cor_matrix_scaled, method = "circle")
print(cor_matrix_scaled)

# Использование функции kmeans для разделения данных на 5 кластеров
set.seed(42) # Устанавливаем seed для воспроизводимости k-средних
kmeans_result <- kmeans(scaled_data, centers = 5)

# Посмотрим результаты кластеризации
print(kmeans_result)

library(FactoMineR)
data(iris)
res.pca <- PCA(iris[,1:4], graph=FALSE)

# Смотрим долю объясненной вариативности
print(res.pca$eig)

# Строим график каменистой осыпи
plot(res.pca$eig[,1], type="b", xlab="Номер компоненты", ylab="Процент объясненной вариативности",
     main="График каменистой осыпи")

#график каменистой осыпи указывает на оптимальное число главных компонент для использования(3)

cluster_labels <- kmeans_result$cluster
# Выполнение PCA для сокращения данных до 3 измерений
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
principal_components <- pca_result$x[, 1:3]

# Создание нового датафрейма для визуализации
pca_df <- data.frame(principal_components)
pca_df$Cluster <- as.factor(cluster_labels) # Преобразование меток кластера в фактор для цветовой кодировки

# Преобразование фактора в числовой формат перед вычислением максимального значения
num_clusters <- as.numeric(levels(pca_df$Cluster))[pca_df$Cluster]

# Создание 3D графика с использованием plotly
fig <- plot_ly(data = pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, colors = RColorBrewer::brewer.pal(max(num_clusters), "Set1"), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "PC1"),
                      yaxis = list(title = "PC2"),
                      zaxis = list(title = "PC3")),
         margin = list(l = 0, r = 0, b = 0, t = 0))

# Показ графика
fig

print(pca_df)

# Теперь посчитаем средние оценки для каждого типа локации
# scaled_data - это масштабированный датафрейм, а cluster_labels - вектор меток кластера
# добавим к scaled_data метки кластера
scaled_data$Cluster <- cluster_labels
head(scaled_data)

#scaled_data <- scaled_data[, -which(names(scaled_data) == "cluster")]

# Вычисление средних значений для каждого кластера по всем столбцам
library(dplyr)
cluster_means <- scaled_data %>%
  group_by(Cluster) %>%
  summarise_all(mean)

# Выводим результат
print(cluster_means)
dplyr::glimpse(cluster_means)
#=
#print(cluster_means, width=Inf, n=10)

# Построение ящиков с усами
# Преобразование данных из широкого формата в длинный
df_long <- gather(scaled_data, key = "Category", value = "Score", -Cluster)
ggplot(df_long, aes(x = Cluster, y = Score, fill = factor(Cluster))) +
  geom_boxplot() +
  facet_wrap(~ Category, scales = "free") + # Разбивка по категориям
  theme_light() +
  labs(y = "Оценки", fill = "Кластер") +
  scale_fill_brewer(palette = "Set3") # Использование палитры 'Set3' для цветов

# Статистический вывод между "art galleries" и "beaches"
cor_test_art_beaches <- cor.test(scaled_data$"art galleries", scaled_data$beaches)
print("Корреляция между 'art galleries' и 'beaches':")
print(cor_test_art_beaches)

# Статистический вывод между "art galleries" и "beaches" с использованием попарного t-критерия
t_test_result <- t.test(scaled_data$"art galleries", scaled_data$beaches, paired = TRUE)

# Вывод результатов теста
print(t_test_result)
