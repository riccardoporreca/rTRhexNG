

source("rTRNGstickR.R")

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
  rTRNGstickR,
  within(list(), {
    n <- 9
    jump_size <- 3
    split_s <- 5 # based on the jump
    sq_cols <- sq_cols_gb(jump_size, split_s, n)
    sq_cols <- rep(cm.colors(n, 1), len = 1000)
    # sq_cols <- rep(hsv(seq(0, 1-1/(n+1), len = n), 0.75, 1), len = 1000)
    full_col <- mirai_light
    jump_col <- "#228822"
    split_col <- "#222288"
    n_split <- 9
    n_jump <- 9
    text_size <- 0.25
    text_col <- mirai_dark
    bg_col <- magrittr_bg
    inkscape <- FALSE
    text2path <- FALSE
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
  text_size <- 0.25
  inkscape <- FALSE
  text2path <- FALSE
})

rTRNG_9_3_5_green_blue <- function(
  bg_col,
  text_col,
  box_jump_col,
  box_split_col
) {
  cl <- as.call(c(rTRNGstickR, fix_args))
  cl$bg_col <- bg_col
  cl$text_col <- text_col
  cl$box_jump_col <- box_jump_col
  cl$box_split_col <- box_split_col
  eval(cl)
}

png_dir <- paste("rTRNG_9_3_5_green_blue", Sys.Date(), sep = "-")
dir.create(png_dir)

txc <- c(miraig = mirai_dark, miraib = mirai_blue)
bgc <- c(magrittr = magrittr_bg, white = "white", mirailb = mirai_blue_light)
bxc <- c(col = "col", none = "none", black = "black")


rTRNG_9_3_5_green_blue(
  text_col = txc[1],
  bg_col = bgc[1],
  box_jump_col = sub("col", fix_args$jump_col, "none"),
  box_split_col = sub("col", fix_args$split_col, "none")
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
