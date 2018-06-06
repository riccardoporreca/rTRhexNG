
source("rTRNGstickR.R")
source("colors.R")

# palette <- base
palette <- mesch_V2

# -----

sq_cols_gb <- function(jump, split_s, split_p) {
  cols <- rep(palette$full_fill, len = 1000)
  cols[1 + jump] <- palette$jump_fill
  cols[seq(jump + split_s, by = split_p, 1000)] <- palette$split_fill
  cols
}

rTRNGpng <- function(..., svg = "rTRNG.svg", dir = ".", view = TRUE) {
  png <- file.path(dir, paste0(paste("rTRNG", ..., sep = "-"), ".png"))
  message(svg, " -> ", png)
  poly_sector <- 360/n_poly
  poly_angles <- seq(-90, by = poly_sector, len = n_poly)
  xyratio <- if (circle) {
    1
  } else {
    diff(range(cospi(poly_angles/180))) /
      diff(range(sinpi(poly_angles/180)))
  }
  rsvg::rsvg_png(svg, png, 1200*xyratio, 1200)
  if (view) {
    grid::grid.newpage()
    grid::grid.raster(png::readPNG(png))
  }
  invisible(png)
}

# nominal sticker -----

n_poly <- 6
circle <- FALSE

do.call(
  rTRNGstickR,
  within(list(), {
    n_poly <- n_poly
    n <- 9
    jump_size <- 3
    split_s <- 5 # based on the jump
    n_split <- 9
    n_jump <- 9
    sq_cols <- sq_cols_gb(jump_size, split_s, n)
    # sq_cols <- rep(hsv(seq(0, 1-1/(n+1), len = n), 0.75, 1), len = 1000)
    # sq_cols <- function(x) hsv(x, 0.75, 1)
    full_col <- palette$full_stroke # mirai_light
    jump_col <- palette$jump_stroke # jumpbox_blue
    split_col <- palette$split_stroke # splitbox_green
    text_size <- 0.23 # 0.25
    text_col <- palette$txt # mirai_dark
    text_font <- "GothamBook"
    text_width <- 0.025
    bg_col <- palette$bg # magrittr_bg
    poly_pad <- 0.1 # tiny background border, OK for screen
    postprocess <- "inkscape-text2path" # preserves the the actual mm units
    circle <- circle
    guides <- FALSE
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
  jump_col <- jumpbox_blue
  split_col <- splitbox_green
  n_split <- 9
  n_jump <- 9
  text_size <- 0.23
  poly_pad <- 0.1
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

txc <- c(miraig = mirai_dark, miraib = mirai_blue)
bgc <- c(magrittr = magrittr_bg, white = "white", mirailb = mirai_blue_light)
bxc <- c(col = "col", none = "none", black = "black")

if (FALSE) {
  png_dir <- paste("rTRNG_9_3_5_green_blue", Sys.Date(), sep = "-")
  dir.create(png_dir)

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
