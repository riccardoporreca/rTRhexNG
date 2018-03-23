#####################
# !!!!!!!!!!!!!! TO BE REVIEWED
####################


# -----
mirai_dark <- "#333333"
magrittr_bg <- "#f9ecc6"
mirai_light <- "#5a5a5a"
mirai_blue <- "#008cc3"
mirai_blue_light <- "#ace4f3"

sq_cols_gb <- function(jump, split_s, split_p) {
  cols <- rep("#888888", len = 1000)
  cols[1 + jump] <- "#66AA66"
  cols[seq(jump + split_s, by = split_p, 1000)] <- "#6666AA"
             cols
}

rTRNGpng <- function(..., svg = "rTRNG.svg", dir = ".", view = TRUE) {
  png <- file.path(dir, paste0(paste("rTRNG", ..., sep = "-"), ".png"))
  message(svg, " -> ", png)
  rsvg::rsvg_png(svg, png, 1200*sqrt(3)/2, 1200)
  if (view) {
    grid::grid.newpage()
    grid::grid.raster(png::readPNG(png))
  }
  invisible(png)
}

# -----

do.call(
  rTRNG_stickR,
  within(list(), {
    n <- 9
    jump_size <- 3
    split_s <- 5 # based on the jump
    sq_cols <- sq_cols_gb(jump_size, split_s, n)
    full_col <- mirai_light
    jump_col <- "#228822"
    split_col <- "#222288"
    n_split <- 9
    n_jump <- 9
    text_size <- 9.5
    text_col <- mirai_blue
    bg_col <- mirai_blue_light
  })
)
rTRNGpng()



# 9_3_5_green_blue ----
fix_args <- within(list(), {
  n <- 9
  jump_size <- 3
  split_s <- 5 # based on the jump
  sq_cols <- sq_cols_gb(jump_size, split_s, n)
  full_col <- mirai_light
  jump_col <- "#228822"
  split_col <- "#222288"
  n_split <- 9
  n_jump <- 9
  text_size <- 9.5
})


# cols <- rep(heat.colors(5, 0.5), len = 1000)


rTRNG_9_3_5_green_blue <- function(
  bg_col,
  text_col,
  box_jump_col,
  box_split_col
) {
  cl <- as.call(c(rTRNG_stickR, fix_args))
  cl$bg_col <- bg_col
  cl$text_col <- text_col
  cl$box_jump_col <- box_jump_col
  cl$box_split_col <- box_split_col
  eval(cl)
}

png_dir <- paste("rTRNG_9_3_5_green_blue", Sys.Date(), sep = "-")
dir.create(png_dir)

txc <- c(mirai = mirai_dark, black = "black")
bgc <- c(magrittr = magrittr_bg, white = "white")
bxc <- c(col = "col", none = "none", black = "black")

txc <- c(mirai = mirai_dark, miraiblue = mirai_blue)
bgc <- c(magrittr = magrittr_bg, lightblue = mirai_blue_light)
bxc <- c(col = "col")

rTRNG_9_3_5_green_blue(
  text_col = txc[[1]],
  bg_col = bgc[[1]],
  box_jump_col = sub("col", fix_args$jump_col, bxc[[1]]),
  box_split_col = sub("col", fix_args$split_col, bxc[[1]])
)


if (FALSE) {
  for (bx in names(bxc)) {
    for (bg in names(bgc)) {
      for (tx in names(txc)) {
        rTRNG_9_3_5_green_blue(
          text_col = txc[[tx]],
          bg_col = bgc[[bg]],
          box_jump_col = sub("col", fix_args$jump_col, bxc[[bx]]),
          box_split_col = sub("col", fix_args$split_col, bxc[[bx]])
        )
        rTRNGpng(
          dir = png_dir,
          sprintf("T_%s", bx),
          sprintf("bg_%s", bg),
          sprintf("txt_%s", tx),
          view = FALSE
        )

      }
    }
  }
}
