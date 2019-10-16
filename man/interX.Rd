\name{interX}
\alias{interX}
\title{interX}
\usage{
spotlight(x, w, y, spot = 1, center = TRUE, plot = FALSE, data)
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
data => name of dataset

}
\examples{
x <- sample(0:1, 200, replace = TRUE)
y <- sample(1:5, 200, replace = TRUE)
w <- sample(1:5, 200, replace = TRUE)
dataset <- data.frame(x, y, w, v)
spotlight(x = x, y = y, w = w, data = dataset)
spotlight(x = x, y = y, w = w, v = v, data = dataset, plot = TRUE)
}
