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

model1 <- glm(y~ns(x1,4)+ns(x2,4), data=datos)

model2 <- glm(y~ns(x1,4)+ns(x3,4), data=datos)

model3 <- glm(y~ns(x1,4)+ns(x4,4), data=datos)

model4 <- glm(y~ns(x2,4)+ns(x3,4), data=datos)

model5 <- glm(y~ns(x2,4)+ns(x4,4), data=datos)

model6 <- glm(y~ns(x3,4)+ns(x4,4), data=datos)

Analisis(datos, model1)
Analisis(datos, model2)
Analisis(datos, model3)
Analisis(datos, model4)
Analisis(datos, model5)
Analisis(datos, model6)

# El mejor modelo es aquel con las variables x3 y x4

