## ----style, echo=FALSE, results="asis", message=FALSE--------------------
knitr::opts_chunk$set(tidy = FALSE,
		   message = FALSE)

## ----echo=FALSE, results="hide", message=FALSE---------------------------
library("ggplot2")
library("ggimage")

## ----warning=FALSE-------------------------------------------------------
library("ggplot2")
library("ggimage")

set.seed(2017-02-21)
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("https://www.r-project.org/logo/Rlogo.png",
                                 "https://jeroenooms.github.io/images/frink.png"),
                               size=10, replace = TRUE)
                )

ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05)
ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05, by='height')

ggplot(d, aes(x, y)) + geom_image(image=d$image[1])
d$size=seq(.05, .15, length.out=10)
ggplot(d, aes(x, y)) + geom_image(aes(image=image, size=I(size)))

