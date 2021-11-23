# Cargamos las librerías
library(dplyr)
library(e1071)
library(caret)

# Fijamos semilla
set.seed(100)
# Leemos el dataset de https://archive.ics.uci.edu/ml/datasets/HCV+data que identifica donantes válidos frente a los que se les detecta hepatitis C
df_raw <- read.csv("hcvdat0.csv")

# Elimino el id del paciente (X)
df <- df_raw %>% select(-X) %>% na.omit()

# Etiqueto los de tipo categórico
df$Sex <- factor(df$Sex)
df$Category <- factor(df$Category)
# Consulto
head(df)

# Divido en prueba y entrenamiento
trainRow <- createDataPartition(df$Category, p=0.7, list=FALSE)
train <- df[trainRow, ]
test <- df[-trainRow, ]
# Obtengo las categorías correctas
test_categories <- test$Category
# Elimino categoría (por si acaso)
test <- select(test, -Category)

# Aplico SVM con modelo lineal y coste 1
modelo <- e1071::svm(
    formula = Category ~ .,
    data=train,
    type='C-classification',
    kernel="linear",
    C=1,
    scale=T
)

# Miro el número de referencias
# Predigo en predicted usando el modelo
predicted <- predict(modelo, newdata = test)

# Muestro la precisión
accuracy <- sum(predicted == test_categories)/length(predicted)
accuracy


# Ajusto parámetros (tuned) de lineal con coste de valores (0.1, 0.5, 1, 5, 10, 20, 100)
cvtuned <- e1071::tune(
    "svm",
    Category ~ .,
    data=df,
    kernel="linear",
    ranges = list(cost = c(0.1, 0.5, 1, 5, 10, 20, 100))
)

# Muestro el resumen

cvtuned

# Re-aprendo con todos los datos de entrenamiento y los mejores parámetros

bestModel <- e1071::svm(
    formula = Category ~ .,
    data=df,
    type='C-classification',
    kernel="linear",
    C=1,
    scale=T
)

bestModel

# Modelo radial con ajuste de parámetros

radialModel <- e1071::svm(
    formula = Category ~ .,
    data=train,
    type='C-classification',
    kernel="radial",
    C=1,
    gamma=2,
    scale=T
)

radialModel

# Aplico tuning indicando también el kernel como parámetro a ajustar
