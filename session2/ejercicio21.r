library(ISLR)
library(splines)
library(mgcv)

datos <-data.frame(
    y=as.numeric(I(iris$Sepal.Width<3)),
    x1=iris$Sepal.Length,
    x2=iris$Petal.Length,
    x3=iris$Petal.Width,
    x4=iris$Sepal.Width
)

model1 <- glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data=datos)
summary(model1)
Analisis(datos, model1)

model1Withx4 <- glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4)+ns(x4, 4), data=datos)
summary(model1Withx4)
Analisis(datos, model1Withx4)

# El mejor modelo es el que tiene la variable x4. Esto se debe a que le estamos
# poniendo la variable que se tiene en cuenta como etiqueta de clase.
