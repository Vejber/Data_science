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
data <- read_csv("~/Downloads/heart_disease_uci.csv")
head(data)

summary(data)

#Просмотр пропущенных значений
colSums(is.na(data))

#Заполнение пропущенных значений в числовых колонках, используя медианную импутацию
numeric_columns <- c("trestbps", "chol", "thalch", "oldpeak")
data[numeric_columns] <- lapply(data[numeric_columns], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

#Заполнение пропущенных значений в категориальных колонках, используя импутацию моды

# нахождение моды
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

categorical_columns <- c("fbs", "restecg", "exang", "slope", "thal")
data[categorical_columns] <- lapply(data[categorical_columns], function(x) ifelse(is.na(x), getMode(x[!is.na(x)]), x))

colSums(is.na(data))

#исключение колонки с большим количеством пропущенных значений и колонок id, dataset
data$ca <- NULL
data$id <- NULL
data$dataset <- NULL
head(data)



names = c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalch", "exang", "oldpeak", "slope", "thal", "num")
colnames(data) <- names

data <- data %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(cp = as.factor(cp)) %>%
  mutate(fbs = as.factor(fbs)) %>%
  mutate(restecg = as.factor(restecg)) %>%
  mutate(slope = as.factor(slope)) %>%
  mutate(thal = as.factor(thal)) %>%
  mutate(exang = as.factor(exang))

glimpse(data)
head(data)


#Распределение возраста пациентов с и без болезней сердца
ggplot(data, aes(x = age, fill = factor(num))) +
  geom_histogram(binwidth = 5, position = "stack") +
  labs(title = 'Распределение возраста пациентов и болезней сердца',
       x = 'Age',
       y = 'Count') +
  theme_minimal()

#Тип болей в груди в зависимости от наличия сердечных заболеваний
ggplot(data, aes(x = factor(cp), fill = factor(num))) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1", name = "Heart Disease", labels = c('0', '1', '2', '3', '4')) +
  labs(title = 'Тип болей в груди в зависимости от наличия сердечных заболеваний',
       x = 'Тип болей в груди',
       y = 'Count') +
  theme_minimal() +
  theme(legend.title = element_text(size = 12))

#Распределение болезней сердца по полу
ggplot(data, aes(x = sex, fill = factor(num))) +
  geom_bar(position = "dodge") +
  labs(title = 'Распределение болезней сердца по полу',
       x = 'Sex',
       y = 'Count') +
  scale_fill_discrete(name = "Heart Disease", labels = c('0', '1', '2', '3', '4')) +
  theme_minimal()

#Уровень сахара в крови натощак в зависимости от наличия сердечных заболеваний
ggplot(data, aes(x = factor(fbs), fill = factor(num))) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1", name = "Heart Disease", labels = c('0', '1', '2', '3', '4')) +
  scale_x_discrete(labels = c('No', 'Yes')) +
  labs(title = 'Уровень сахара в крови натощак > 120 mg/dl в зависимости от наличия сердечных заболеваний',
       x = 'Уровень сахара в крови натощак > 120 mg/dl',
       y = 'Count') +
  theme_minimal() +
  theme(legend.title = element_text(size = 12))

#Влияние стенокардии, вызванной физ.нагрузкой, на заболевания сердца
ggplot(data, aes(x = factor(exang), fill = factor(num))) +
  geom_bar(position = "dodge") +
  labs(title = 'Наличие стенокардии, вызванной физ.нагрузкой и заболевания сердца',
       x = 'Стенокардия, вызванной физ.нагрузкой',
       y = 'Count') +
  scale_fill_discrete(name = 'Heart Disease', labels = c('0', '1', '2', '3', '4')) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12))

#Уровень холестерина vs. Заболевание сердца
ggplot(data, aes(x = factor(num), y = chol)) +
  geom_boxplot() +
  labs(title = 'Уровень холестерина vs. Заболевание сердца',
       x = 'Заболевание сердца',
       y = 'Уровень холестерина (mg/dl)') +
  scale_x_discrete(labels = c('0', '1', '2', '3', '4')) +
  theme_minimal()

#Максимальная частота сердечного ритма с возрастом и статус заболевания сердца
ggplot(data, aes(x = age, y = thalch, color = factor(num))) +
  geom_point() +
  labs(title = 'Максимальная частота сердечного ритма с возрастом и статус заболевания сердца',
       x = 'Age',
       y = 'Max Heart Rate (thalch)',
       color = 'Heart Disease') +
  scale_color_discrete(labels = c('0', '1', '2', '3', '4')) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12))

#Среднее значение артериального давления в покое в зависимости от наличия сердечных заболеваний
ggplot(data, aes(x = factor(num), y = trestbps)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(), color = "blue", fill = "lightblue") +
  labs(title = 'Среднее значение артериального давления в покое в зависимости от наличия сердечных заболеваний',
       x = 'Heart Disease',
       y = 'Среднее значение артериального давления в покое (mm Hg)') +
  scale_x_discrete(labels = c('0', '1', '2', '3', '4')) +
  theme_minimal()

#Распространенность сердечных заболеваний в зависимости от результатов электрокардиографии в покое
ggplot(data, aes(x = factor(restecg), fill = factor(num))) +
  geom_bar(position = "dodge") +
  labs(title = "Распространенность сердечных заболеваний в зависимости от результатов электрокардиографии в покое",
       x = "Результаты электрокардиографии в покое",
       y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("0", "1", "2", "3", "4")) +
  theme_minimal()

#Преобразование категориальных переменных в численное представление для анализа корреляций
cor_data <- data
head(cor_data)

#sex Female = 0, Male = 1
unique(cor_data$sex)
cor_data$sex <- factor(cor_data$sex, levels = c("Female", "Male"))
# Преобразуем фактор в численное значение
cor_data$sex_numeric <- as.numeric(cor_data$sex) - 1
#Исключаем категориальную колонку `sex`
cor_data$sex <- NULL

#cp "typical angina" = 1, "asymptomatic" = 2, "non-anginal" = 3, "atypical angina" = 4
unique(cor_data$cp)
cor_data$cp
cor_data$cp <- factor(cor_data$cp, levels = c("typical angina", "asymptomatic", "non-anginal", "atypical angina"))
# Преобразуем фактор в численное значение
cor_data$cp_numeric <- as.numeric(cor_data$cp)
#Исключаем категориальную колонку `cp`
cor_data$cp <- NULL

#fbs "TRUE" = 1, "FALSE" = 0
unique(cor_data$fbs)
cor_data$fbs
cor_data$fbs <- factor(cor_data$fbs, levels = c("FALSE", "TRUE"))
# Преобразуем фактор в численное значение
cor_data$fbs_numeric <- as.numeric(cor_data$fbs) - 1
#Исключаем категориальную колонку `fbs`
cor_data$fbs <- NULL

#restecg "lv hypertrophy" = 1, "normal" = 2, "st-t abnormality" = 3
unique(cor_data$restecg)
cor_data$restecg
cor_data$restecg <- factor(cor_data$restecg, levels = c("lv hypertrophy", "normal", "st-t abnormality"))
# Преобразуем фактор в численное значение
cor_data$restecg_numeric <- as.numeric(cor_data$restecg)
#Исключаем категориальную колонку `fbs`
cor_data$restecg <- NULL

#exang "TRUE" = 1, "FALSE" = 0
unique(cor_data$exang)
cor_data$exang
cor_data$exang <- factor(cor_data$exang, levels = c("FALSE", "TRUE"))
# Преобразуем фактор в численное значение
cor_data$exang_numeric <- as.numeric(cor_data$exang) - 1
#Исключаем категориальную колонку `exang`
cor_data$exang <- NULL


#slope "downsloping" = 1, "flat" = 2, "upsloping" = 3
unique(cor_data$slope)
cor_data$slope
cor_data$slope <- factor(cor_data$slope, levels = c("downsloping", "flat", "upsloping"))
# Преобразуем фактор в численное значение
cor_data$slope_num <- as.numeric(cor_data$slope)
#Исключаем категориальную колонку `slope`
cor_data$slope <- NULL

#thal "fixed defect" = 1, "normal" = 2, "reversable defect" = 3
unique(cor_data$thal)
cor_data$thal
cor_data$thal <- factor(cor_data$thal, levels = c("fixed defect", "normal", "reversable defect"))
# Преобразуем фактор в численное значение
cor_data$thal_num <- as.numeric(cor_data$thal)
#Исключаем категориальную колонку `thal`
cor_data$thal <- NULL

# Выводим первые несколько строк для проверки
head(cor_data)


#Корреляционный анализ
#Корреляционный анализ с категориальными переменными
cor_matrix <- cor(cor_data)
corrplot::corrplot(cor_matrix, method = "circle")
print(cor_matrix)

#Корреляционный анализ без категориальных переменных
selected_columns <- data %>% select(age,trestbps, chol, thalch, oldpeak, num)
correlation_matrix <- cor(selected_columns) 
corrplot::corrplot(correlation_matrix, method = "circle")
print(correlation_matrix)

# Статистический вывод между num и age
cor_test_age_num<- cor.test(cor_data$age, cor_data$num)
print("Корреляция между возрастом и диагнозом:")
print(cor_test_age_num)

# Статистический вывод между num и chol
cor_test_chol_num<- cor.test(cor_data$chol, cor_data$num)
print("Корреляция между уровнем холестерина и диагнозом:")
print(cor_test_chol_num)

# Статистический вывод между num и thalch
cor_test_thalch_num<- cor.test(cor_data$thalch, cor_data$num)
print("Корреляция между максимальной ЧСС и диагнозом:")
print(cor_test_thalch_num)

# Статистический вывод между oldpeak и num
cor_test_oldpeak_num<- cor.test(cor_data$oldpeak, cor_data$num)
print("Корреляция между депрессией ST-сегмента и диагнозом:")
print(cor_test_oldpeak_num)

# Статистический вывод между sex_num и num
cor_test_sex_num_num<- cor.test(cor_data$sex_numeric, cor_data$num)
print("Корреляция между полом и диагнозом:")
print(cor_test_sex_num_num)

#Статистический вывод между cp_num и num
cor_test_exang_num_num<- cor.test(cor_data$cp_numeric, cor_data$num)
print("Корреляция между стенокардией при физ.нагрузке и диагнозом:")
print(cor_test_exang_num_num)

# Статистический вывод между exang_num и num
cor_test_exang_num_num<- cor.test(cor_data$exang_numeric, cor_data$num)
print("Корреляция между стенокардией при физ.нагрузке и диагнозом:")
print(cor_test_exang_num_num)

#Анализ групп
# Используем split() для разделения данных по группам по целевому признаку
unique(data$num)
groups <- split(data, data$num)
head(groups)

lapply(groups, summary)


# Построение модели с несколькими независимыми переменными

logged_data <- data %>%
  mutate(across(c(age, trestbps, chol, thalch, oldpeak), ~ log(. + 3)))

model <- lm(data = logged_data)
# Отображение диагностических графиков для модели
par(mfrow = c(2, 2))  # Разделение графического окна на 2x2 матрицу для отображения нескольких графиков
plot(model)
print(model)
summary(model)

#Кластерный анализ
#Шкалирование
scaled_data <- scale(cor_data)
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
#data(scaled_data)
res.pca <- PCA(scaled_data, graph=FALSE)
#data(iris)
#res.pca <- PCA(iris[,1:4], graph=FALSE)

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

# Теперь посчитаем средние значения
# scaled_data - это масштабированный датафрейм, а cluster_labels - вектор меток кластера
# добавим к scaled_data метки кластера
scaled_data$Cluster <- cluster_labels
head(scaled_data)

# Вычисление средних значений для каждого кластера по всем столбцам
library(dplyr)
cluster_means <- scaled_data %>%
  group_by(Cluster) %>%
  summarise_all(mean)

# Выводим результат
print(cluster_means)
dplyr::glimpse(cluster_means)

# Построение ящиков с усами
# Преобразование данных из широкого формата в длинный
df_long <- gather(scaled_data, key = "Category", value = "Score", -Cluster)
ggplot(df_long, aes(x = Cluster, y = Score, fill = factor(Cluster))) +
  geom_boxplot() +
  facet_wrap(~ Category, scales = "free") + # Разбивка по категориям
  theme_light() +
  labs(y = "Оценки", fill = "Кластер") +
  scale_fill_brewer(palette = "Set3") # Использование палитры 'Set3' для цветов
