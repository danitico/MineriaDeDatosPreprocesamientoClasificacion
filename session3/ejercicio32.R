library(RWeka)
library(mgcv)
library(splines)




model <- gam(
    Species~ns(Sepal.Length,4)+ns(Sepal.Width,4)+ns(Petal.Length,4)+ns(Petal.Width,4),
    data=iris
)

setosaVsAll <- iris

levels(setosaVsAll$Species) <- c(levels(setosaVsAll$Species), "others")
setosaVsAll$Species[setosaVsAll$Species == "versicolor"| setosaVsAll$Species == "virginica"] <- "others"

model <- gam(
    Species~ns(Sepal.Length,4)+ns(Sepal.Width,4)+ns(Petal.Length,4)+ns(Petal.Width,4),
    family = "binomial",
    data=setosaVsAll
)

versicolorVsAll <- iris

levels(versicolorVsAll$Species) <- c(levels(versicolorVsAll$Species), "others")
setosaVsAll$Species[setosaVsAll$Species == "setosa"| setosaVsAll$Species == "virginica"] <- "others"

virginicaVsAll <- iris
