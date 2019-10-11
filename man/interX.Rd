\name{interX}
\alias{interX}
\title{interX}
\usage{
spotlight(x, w, y, spot = 1, center = FALSE, data)
}
\description{
Conducts spotlight analysis on a simple categorical(x) x continuous (w) interaction
**REQUIRES library(tidyverse)

x => Categorical Variable (Must be Binary 0,1)
y => Dependent Variable
w => Continuous Variable
spot => set to "1" for 1 SD below and above mean
center => centers x and w
data => name of dataset

}
\examples{
x <- sample(0:1, 50, replace = TRUE)
y <- sample(1:5, 50, replace = TRUE)
w <- sample(1:5, 50, replace = TRUE)
dataset <- data.frame(x, y, w)
spotlight(x = x, y = y, w = w, data = dataset)
}
