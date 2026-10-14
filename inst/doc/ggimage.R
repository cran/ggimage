## ----setup--------------------------------------------------------------------
#| echo: false
#| results: hide
#| message: false
library("ggplot2")
library("ggimage")


## ----geom-image---------------------------------------------------------------
#| warning: false
set.seed(2017-02-21)
img <- list.files(system.file("extdata", package = "ggimage"),
                  pattern = "png", full.names = TRUE)
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(img, size = 10, replace = TRUE))

## one image, used by the sections below
logo <- system.file("extdata/Rlogo.png", package = "ggimage")

ggplot(d, aes(x, y)) + geom_image(aes(image = image), size = .05)
ggplot(d, aes(x, y)) + geom_image(aes(image = image), size = .05, by = "height")

ggplot(d, aes(x, y)) + geom_image(image = d$image[1])
d$size <- seq(.05, .15, length.out = 10)
ggplot(d, aes(x, y)) + geom_image(aes(image = image, size = I(size)))

ggplot(d, aes(x, y)) + geom_image(aes(image = image), color = "firebrick")


## ----geom-image-dimensions----------------------------------------------------
#| warning: false
d$w <- seq(.02, .1, length.out = 10)
ggplot(d, aes(x, y)) + geom_image(aes(image = image, width = w))

## width and height together stretch the image into the requested box
ggplot(d, aes(x, y)) + geom_image(aes(image = image, width = .12, height = .04))


## ----geom-image-colour--------------------------------------------------------
#| warning: false
d$fade <- seq(.2, 1, length.out = 10)
ggplot(d, aes(x, y)) + geom_image(aes(image = image), size = .08, alpha = .5) +
    scale_alpha_identity()

ggplot(d, aes(x, y)) + geom_image(aes(image = image, alpha = fade), size = .08) +
    scale_alpha_identity()


## ----geom-image-repel---------------------------------------------------------
#| warning: false
d2 <- data.frame(x = c(.50, .52, .51), y = c(.50, .51, .49))
p <- ggplot(d2, aes(x, y, image = logo)) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))

## as they are, the images overlap
p + geom_image(width = .12)

## ... and are pushed apart by geom_image_repel()
p + geom_image_repel(width = .12)


## ----geom-subview-------------------------------------------------------------
library(tibble)
dd <- data.frame(x = LETTERS[1:3], y = 1:3)
pie <- ggplot(dd, aes(x = 1, y, fill = x)) + geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() + theme(legend.position = "none") + theme_transparent()

df <- tibble(x = sample(2:9),
             y = sample(2:9),
             width = sample(seq(0.5, 3, length.out = length(x))),
             pie = list(pie))
p <- ggplot(data = data.frame(x = c(0, 10), y = c(0, 10)), aes(x, y)) + geom_blank()
p + geom_subview(aes(x = x, y = y, subview = pie, width = width, height = width), data = df)


## ----subview-base-------------------------------------------------------------
#| warning: false
#| fig-width: 7
#| fig-height: 4.8
set.seed(2026)
svd <- data.frame(x = rep(c(0, 2, 4), 2), y = rep(c(0, 2), each = 3))
svd$hist <- lapply(1:6, function(i) function() {
    hist(rnorm(300, mean = i), main = "", xlab = "", ylab = "",
         col = i, border = "white", axes = FALSE)
})
ggplot(svd, aes(x, y)) + geom_point(size = 2) +
    geom_subview(aes(x = x, y = y, subview = hist), data = svd,
                 width = 1.7, height = 1.7) +
    xlim(-1.2, 5.2) + ylim(-1.2, 3.2) + coord_fixed() + theme_bw()


## ----subview-systems----------------------------------------------------------
#| warning: false
#| fig-width: 7
#| fig-height: 5.6
svd <- data.frame(x = 1:3, y = 0)
svd$subview <- list(
    function() plot(cos(seq(0, 2 * pi, length.out = 50)), type = "l",
                    axes = FALSE, ann = FALSE),
    ggplot(mtcars, aes(wt, mpg)) + geom_point(),
    magick::image_read(system.file("extdata/Rlogo.png", package = "ggimage"))
)
labels <- c("base graphics", "ggplot", "magick")
names(svd$subview) <- labels

ggplot(svd, aes(x, y)) + geom_blank() +
    geom_subview(aes(x = x, y = y, subview = subview), data = svd,
                 width = .8, height = .8) +
    geom_text(aes(x, y, label = label),
              data = data.frame(x = 1:3, y = -.7, label = labels),
              vjust = 1, size = 3.2) +
    xlim(.5, 3.5) + ylim(-1.2, 1.2) + coord_fixed() + theme_void()


## ----geom-bgimage-------------------------------------------------------------
#| warning: false
ggplot(iris, aes(Petal.Length, Petal.Width)) +
    geom_bgimage(logo) + geom_point()


## ----geom-bgimage-anchor------------------------------------------------------
#| warning: false
info <- magick::image_info(magick::image_read(logo))

ggplot(iris, aes(Petal.Length, Petal.Width)) +
    geom_bgimage(logo, pixel = c(1, info$height), x = 0, y = 0,
                 scale = 2.5 / info$width) +
    geom_point() +
    scale_x_continuous(limits = c(-0.4, 7), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.3, 2.6), expand = c(0, 0)) +
    coord_fixed()


## ----ggbackground-------------------------------------------------------------
#| warning: false
ggbackground(ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(), logo)


## ----geom-pokemon-------------------------------------------------------------
#| warning: false
ggplot(d, aes(x, y)) + geom_pokemon(aes(image = ifelse(x > 0, "pikachu", "tauros")), size = .1)


## ----geom-emoji---------------------------------------------------------------
#| warning: false
set.seed(123)
iris2 <- iris[sample(1:nrow(iris), 30), ]
model <- lm(Petal.Length ~ Sepal.Length, data = iris2)
iris2$fitted <- predict(model)

ggplot(iris2, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_linerange(aes(ymin = fitted, ymax = Petal.Length),
                 colour = "purple") +
  geom_abline(intercept = model$coefficients[1],
              slope = model$coefficients[2]) +
  geom_emoji(aes(image = ifelse(abs(Petal.Length - fitted) > 0.5, "1f622", "1f600")))


## ----geom-icon----------------------------------------------------------------
#| warning: false
d$icon <- sample(c("power", "wifi", "pie-chart"), 10, replace = TRUE)
ggplot(d, aes(x, y)) + geom_icon(aes(image = icon), size = .1)


## ----geom-flag----------------------------------------------------------------
#| warning: false
f <- system.file("extdata/medals.txt", package = "ggimage")
medals <- read.table(f, header = TRUE)
p <- ggplot(medals, aes(Country, count)) + geom_col(aes(fill = medal), width = .8)

p + geom_flag(y = -2, aes(image = code)) +
    coord_flip() + expand_limits(y = -2) +
    scale_fill_manual(values = c("Gold" = "gold", "Bronze" = "#cd7f32", "Silver" = "#C0C0C0"))


## ----geom-worldcup2018--------------------------------------------------------
#| warning: false
wc <- expand.grid(x = 1:8, y = 1:4)
wc$country <- list.worldcup2018()

ggplot(wc, aes(x, y)) +
    geom_worldcup2018(aes(image = country), size = .1) +
    coord_fixed() + theme_void()


## ----geom-phylopic------------------------------------------------------------
#| warning: false
sp <- c("Canis lupus", "Ursus maritimus", "Homo sapiens", "Gallus gallus",
        "Bos taurus", "Panthera leo", "Loxodonta africana",
        "Drosophila melanogaster")
species <- data.frame(x = seq_along(sp), y = 1, species = sp)

ggplot(species, aes(x, y)) +
    geom_phylopic(aes(image = species), size = .12) +
    scale_x_continuous(breaks = seq_along(sp), labels = sp) +
    ylim(.7, 1.3) +
    theme_void() + theme(axis.text.x = element_text(size = 8, angle = 20, hjust = 1))


## ----phylopic-uid-------------------------------------------------------------
#| eval: false
# phylopic_uid(c("Canis lupus", "Homo sapiens"))


## ----geom-phylopic-uid--------------------------------------------------------
#| warning: false
uid <- "5036f260-5a0d-42c5-a0bd-9eb0729e54e0" # Canis lupus
ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_phylopic(aes(image = uid), size = .5) +
    xlim(.5, 1.5) + ylim(.5, 1.5) + theme_void()


## ----autocomplete-name--------------------------------------------------------
#| eval: false
# head(autocomplete_name("Canis"), 4)


## ----download-phylopic--------------------------------------------------------
#| eval: false
# dir <- tempfile()
# dir.create(dir)
# files <- download_phylopic(c("Canis lupus", "Gallus gallus"), destdir = dir)
# basename(files)


## ----list-images--------------------------------------------------------------
#| eval: false
# head(list.pokemon(), 3)
# head(list.icon(), 3)
# head(list.flag(), 3)


## ----geom-image-interactive---------------------------------------------------
#| warning: false
d$id <- seq_len(nrow(d))

ggplot(d, aes(x, y)) +
    geom_image_interactive(aes(image = image), size = .08) +
    theme_void()


## ----geom-image-interactive-girafe--------------------------------------------
#| eval: false
# library(ggiraph)
# 
# girafe(ggobj = ggplot(d, aes(x, y)) +
#     geom_image_interactive(aes(image = image, tooltip = round(x, 2), data_id = id),
#                            size = .08) +
#     theme_void())


## ----animate-materialise-frames-----------------------------------------------
#| eval: false
# library(magick)
# library(gganimate)
# 
# ## a small deterministic source animation: four solid-colour frames
# frames <- lapply(c("steelblue", "gold", "firebrick", "seagreen"), function(cl) {
#     image_blank(200, 200, color = cl) |> image_annotate(cl, size = 24)
# })
# gif <- image_join(frames)
# image_write(gif, "source.gif", format = "gif")   # a multi-frame file, not one PNG
# 
# ## ... and the same thing read back, one frame per file
# src <- image_read("source.gif")
# dir.create("frames", showWarnings = FALSE)
# paths <- sprintf("frames/frame%02d.png", seq_along(src))
# for (i in seq_along(src)) {
#     ## magick writes the format of the *object*, not of the file name, so an
#     ## explicit format is what keeps these files PNG rather than GIF bytes
#     image_write(src[i], paths[i], format = "png")
# }
# basename(paths)


## ----animate-frame-table------------------------------------------------------
#| eval: false
# ## one glyph that stays put, one that moves across the frames
# nf <- length(paths)
# d <- data.frame(
#     glyph = rep(c("fixed", "moving"), each = nf),
#     frame = rep(seq_len(nf), times = 2),
#     x = c(rep(1, nf), seq(1, 4, length.out = nf)),
#     y = rep(1, 2 * nf)
# )
# d$image <- rep(paths, times = 2)
# head(d, 3)


## ----animate-plot-------------------------------------------------------------
#| eval: false
# ## no frame column here, so this layer persists across states
# static <- data.frame(x = 1:4, y = 1)
# 
# p <- ggplot(d, aes(x, y)) +
#     geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0.6, ymax = 1.4),
#               data = data.frame(x = 1), fill = "grey92", inherit.aes = FALSE) +
#     geom_point(data = static, size = 1.5) +
#     geom_image(aes(image = image), data = d, size = .12) +
#     coord_fixed() +
#     theme_void()
# 
# ## the states come from the frame column, not from a ggimage aesthetic
# p <- p + transition_manual(frame)
# 
# animate(p, nframes = nf, fps = 4, renderer = magick_renderer())


## ----legend-key---------------------------------------------------------------
#| warning: false
d$group <- rep(c("up", "down"), each = 5)
key_plot <- ggplot(d, aes(x, y, image = image, colour = group)) +
    geom_image(size = .08) + theme_void()

options(ggimage.keytype = "rect")
key_plot

options(ggimage.keytype = "image")
key_plot + guides(colour = guide_legend(override.aes = list(image = img)))

options(ggimage.keytype = NULL)


## ----themes-transparent-------------------------------------------------------
#| warning: false
p <- ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point()

## the blue rectangle stands in for whatever is behind the plot
draw_on <- function(plot) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "steelblue", col = NA))
    print(plot, newpage = FALSE)
}

draw_on(p)

## the same plot with the theme applied
draw_on(p + theme_transparent())


## ----theme-nothing------------------------------------------------------------
#| warning: false
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point() + theme_nothing()


## ----image-read2--------------------------------------------------------------
#| warning: false
## put a white margin around the logo, as a scan or an exported figure has
bordered <- magick::image_border(magick::image_read(logo), "white", "40x40")
bf <- tempfile(fileext = ".png")
magick::image_write(bordered, bf)

rbind(before = magick::image_info(magick::image_read(bf))[, c("width", "height")],
      after  = magick::image_info(image_read2(bf))[, c("width", "height")])


## ----ggpreview----------------------------------------------------------------
#| warning: false
ggpreview(plot = ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(),
          width = 4, height = 3)


## ----image-cache-policy-------------------------------------------------------
set_image_cache_policy(capacity = 200, ttl = 60, eviction = "lru")
get_image_cache_policy()[c("base_capacity", "base_ttl", "eviction")]


## ----image-cache-stats--------------------------------------------------------
#| warning: false
reset_image_cache_stats()
ggplot(d, aes(x, y)) + geom_image(aes(image = image), size = .05)
get_image_cache_stats()$base


## ----image-cache-restore------------------------------------------------------
options(ggimage.image_cache_capacity = NULL,
        ggimage.image_cache_ttl = NULL,
        ggimage.image_cache_eviction = NULL)

