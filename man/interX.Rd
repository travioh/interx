\name{interX}
\alias{interX}
\title{interX}
\usage{
spotlight(x, w, y, spot = 1, center = TRUE, plot = FALSE, model = 1, data)
}
\description{
Conducts spotlight analysis on a simple categorical(x) x continuous (w) interaction
**REQUIRES library(tidyverse)

x => Categorical Variable (Recommended to be Binary 0: Control, 1: Treatment)
y => Dependent Variable
w => Continuous Variable
v => Covariate (Only allows 1 covariate for now)
spot => set to "1" for 1 SD below and above mean
center => centers w. default is to mean-center.
plot => provided ggplot of interaction
model => type of model (only model 1 available)
data => name of dataset

}
\examples{
group <- sample(0:1, 200, replace = TRUE)
dv <- sample(1:5, 200, replace = TRUE)
cont <- sample(1:5, 200, replace = TRUE)
cova <- sample(1:5, 200, replace = TRUE)
dataset <- data.frame(group, dv, cont, cova)

spotlight(x = group, y = dv, w = cont, data = dataset, plot = TRUE)
spotlight(x = group, y = dv, w = cont, v = cova, data = dataset, plot = TRUE)
}
