##' geom layer for using emoji image
##'
##'
##' @title geom_emoji
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_emoji <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = emoji)
}

emoji <- function(emoji) {
    url <- paste0('https://twemoji.maxcdn.com/72x72/', emoji, ".png")

    check_url(url)
}

