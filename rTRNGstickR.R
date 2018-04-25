rTRNGstickR <- function(
  file = "rTRNG.svg",
  n, # number of elements per side of the hexagon
  jump_size, # jump size
  split_s, # index of sub-sequence [1, n], after jump
  n_full = 5*n + 3, # nr of full sequence elements
  n_jump = 9, # nr of full sequence elements
  n_split = 9, # nr of full sequence elements
  n_path_ext = 2, # number of extra elements for the arrowed paths
  bg_col, # background color
  sq_cols, # color for the elements in the full sequence
  full_col, # color of the full sequqnce path
  jump_col, # color of the jump sequqnce path
  split_col, # color of the split sequqnce path
  box_jump_col = jump_col, # color of the jump box
  box_split_col = split_col, # color of the split box
  text_size, # rTRNG text size (fraction of hexagon inside)
  text_col, # rTRNG text color
  text_font = "GothamBook",
  text_width = 0.025, # fraction of text_size to make the font fatter
  hex_height = 50.8, # mm
  hex_pad = 0, # fraction of the square size
  postprocess = "rsvg2" # fast and reliable
) {

  # geometry and sizes ----

  hex_size <- list(x = hex_height*sqrt(3)/2, y = hex_height)
  hex_center <- lapply(hex_size, `/`, 2)

  # border size => spacing between squares
  sq_border_perc <- 0.12

  # size of squares
  sq_size <- hex_size$x / (
    2 + n * sqrt(3) +
      2 * (1 - sq_border_perc)*hex_pad # extra space = hex_pad * sq_size
  )

  # actual text size:
  text_size <- text_size * sq_size*n*sqrt(3)

  # border size => spacing between squares
  sq_border <- sq_size * sq_border_perc

  path_size <- sq_size * 0.2
  connector_size <- path_size*0.8


  # position of the top-left inside vertex
  tl_x <- hex_center$x - sq_size*n * sqrt(3)/2
  tl_y <- hex_center$y - sq_size*n / 2

  # top-left position of the jump&split T first element
  T_x <- tl_x + 2*sq_size
  T_y <- tl_y + 0.5*sq_size

  id <- 1

  # utils  ----
  .sub <- function(., ...) {
    subs <- data.frame(.s = ., ..., stringsAsFactors = FALSE)
    for (i in seq_len(nrow(subs))) {
      for (which in names(subs)) {
        subs$.s[i] <-
          gsub(sprintf("@%s@", which), subs[[which]][i], subs$.s[i])
      }
    }
    subs$.s
  }
  .df <- function(...) data.frame(..., stringsAsFactors = FALSE)

  # SVG base ----
  svg <- .sub(
    '
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" preserveAspectRatio="xMidYMid"
width="@w@mm"
height="@h@mm"
viewBox="0 0 @w@ @h@">

  <rect id="canvas" width="100%" height="100%" fill="@canvas@"/>

@content@

</svg>',
    h = hex_size$y,
    w = hex_size$x
  )


  # SVG utils ----

  rotate_square <- function(angle, x, y) {
    sprintf("rotate(%s, %s, %s)", angle, x, y)
  }

  square_center <- function(squares) {
    with(squares, data.frame(
      x = x + sq_size/2 * (cospi(angle/180) - sinpi(angle/180)),
      y = y + sq_size/2 * (sinpi(angle/180) + cospi(angle/180))
    ))
  }

  box <- function(squares) {
    with(squares, expand.grid(x = range(x) + c(0, sq_size), y = range(y) + c(0, sq_size)))
  }

  svg_squares <- function(x, y, fill, stroke = bg_col, angle = 0) {
    id <- id + seq(0, len = pmax(length(x), length(y)))
    svg <-
      .sub('
  <rect
    id="rect-@id@"
    width="@s@" height="@s@" x="@x@" y="@y@"
    style="fill:@fill@;stroke:@stroke@;fill-opacity:1;stroke-opacity:1;stroke-width:@border@"
    transform="@rotate@"
  />',
           id = id,
           s = sq_size, x = x, y = y,
           stroke = stroke, fill = fill, border = sq_border,
           rotate = rotate_square(angle, x, y)
      )
    id <<- max(id) + 1
    svg
  }

  svg_path <- function(points, stroke = "black", fill = "none",
                       width, close = FALSE) {
    svg <- .sub(
      '
      <path
      id="path-@id@"
      style="stroke: @stroke@; fill:@fill@; stroke-width:@width@;"
      d="@d@"
      />',
      id = id,
      stroke = stroke,
      fill = fill,
      width = width,
      d =  paste(c(
        paste0("M ", points$x[1], ",", points$y[1]),
        paste0("L ", points$x[-1], ",", points$y[-1]),
        "Z"[close]
      ), collapse = " " )

    )
    id <<- id + 1
    svg
  }

  svg_hex_bg <- function(color) {

    hex_v <- .df(
      x = hex_center$x + hex_height/2 * cospi(seq(-30, by = 60, len = 6) / 180),
      y = hex_center$y + hex_height/2 * sinpi(seq(-30, by = 60, len = 6) / 180)
    )
    svg_path(hex_v, stroke = "none", fill = color, width = 0, close = TRUE)
  }

  svg_squares_path <- function(squares, stroke) {
    centers <-
      cbind(
        square_center(squares),
        corner = c(FALSE, diff(squares$angle) != 0),
        r = sq_size * (1 + sqrt(2) * cospi(-1/4 + -30/180))
      )
    path <- with(
      centers,
      .sub('
  <defs>
    <marker id="arrow-@id@" markerWidth="5" markerHeight="3" refX="2.5" refY="1.5"
    orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 l5,1.5 l-5,1.5 l1,-1.5 Z" style="fill: @stroke@;"/>
    </marker>
    <marker id="circle-@id@" markerWidth="2.5" markerHeight="2.5" refX="1.25" refY="1.25"
    orient="auto" markerUnits="strokeWidth">
      <circle cx="1.25" cy="1.25" r="1.25" style="fill: @stroke@"/>
    </marker>
  </defs>
  <path
    id="path-@id@"
    style="stroke: @stroke@; fill:none; stroke-width:@size@;
    marker-start: url(#circle-@id@);
    marker-end: url(#arrow-@id@)"
    d="@d@"
  />',
           id = id,
           stroke = stroke,
           size = path_size,
           d =
             paste(c(
               paste0("M ", x[1], ",", y[1]),
               # paste0(ifelse(corner[-1], "M ", "L "),
               paste0(ifelse(corner[-1], paste0("A ", r[-1], ",", r[-1], " 0 0,1 "), "L "),
                      paste(x[-1], y[-1], sep = ","))
             ), collapse = " " ))
    )
    id <<- id + 1
    path
  }

  svg_connect_A <- function(from, to, r, stroke) {
    svg <-
      .sub('
  <path
    id="path-@id@"
    d="M@x1@,@y1@ A@rx@,@ry@ 0 0,@conc@ @x2@,@y2@"
    style="stroke:@stroke@; fill:none; stroke-width:@size@;
    stroke-dasharray:@da@,@da2@; stroke-dashoffset:@do@; stroke-linecap:round;"
  />',
           id = id,
           x1 = from$x, y1 = from$y, rx = abs(r[1]), ry = abs(tail(r, 1)), x2 = to$x, y2 = to$y,
           stroke = stroke, size = connector_size, conc = 1*(r[1] > 0),
           da = connector_size*2, da2 = connector_size*3, do = path_size*1.25
      )
    id <<- id + 1
    svg
  }

  svg_txt <- function(txt, x, y, size, anchor = "middle") {
    w <- size*0.025 * 1
    if (size > 0) {
      svg <-
        .sub('
  <text
    id="text-@id@"
    style="text-anchor:@anchor@;font-style:normal;font-variant:normal;
    font-weight:normal;font-stretch:semi-narrower;font-size:@size@;font-family:@font@;letter-spacing:0px;word-spacing:0px;
    fill:@color@;fill-opacity:1;stroke:@color@;stroke-width:@w@"
    x="@x@"
    y="@y@"
  >@txt@</text>',
             id = id, txt = txt, x = x, y = y - w/2, size = size, color = text_col, font = text_font,
             anchor = anchor,
             w = w
        )
      id <<- id + 1
      svg
    }
  }


  # full sequence ----
  seq_s <- seq(0, by = sq_size, length.out = n)
  full_seq_sq <- rbind(
    .df(x = tl_x + sqrt(3)/2 * seq_s, y = tl_y - 1/2 * seq_s, angle = -120),
    .df(x = tl_x + sqrt(3)/2 * (n*sq_size + sq_size + seq_s), y = tl_y - 1/2 * rev(seq_s), angle = -150),
    .df(x = tl_x + sqrt(3)/2 * 2*n*sq_size, y = tl_y + seq_s, angle = 0),
    .df(x = tl_x + sqrt(3)/2 * (2*n*sq_size - seq_s), y = tl_y + n*sq_size + 1/2 * seq_s, angle = 60),
    .df(x = tl_x + sqrt(3)/2 * rev(seq_s), y = tl_y + n*sq_size + 1/2 * rev(seq_s), angle = 30),
    .df(x = tl_x, y = tl_y + rev(seq_s), angle = 90),
    NULL
  )
  full_seq_sq$fill <- head(sq_cols, nrow(full_seq_sq))

  hex_sq_svg <- do.call(svg_squares, head(full_seq_sq, n_full))
  hex_path_svg <- svg_squares_path(head(full_seq_sq, n_full + n_path_ext), full_col)

  # jump sequence ----
  do_jump <- (jump_size > 0 && n_jump > 0)
  if (do_jump) {
    jump_seq_sq <- .df(
      x = T_x + seq(0, by = sq_size, len = nrow(full_seq_sq) - jump_size),
      y = T_y,
      fill = tail(full_seq_sq$fill, -jump_size),
      stroke = bg_col,
      angle = 0
    )
  }

  # split sequence ----
  do_split <- split_s > 0 && n_split > 0
  if (do_split) {
    split_seq_sq <- .df(
      x = T_x + (split_s - 1) * sq_size,
      y = T_y + seq(0, by = sq_size, len = n_split + n_path_ext),
      fill = full_seq_sq$fill[jump_size + split_s],
      stroke = bg_col,
      angle = 0
    )
  }

  # jump&split "T" ----
  T_sq_svg <- c(
    if (do_jump) {
      c(
        do.call(svg_squares, head(jump_seq_sq, n_jump)),
        svg_path(box(head(jump_seq_sq, n_jump))[c(2,1,3,4), ],
                 box_jump_col, "none", sq_border)
      )
    },
    if (do_split) {
      c(
        do.call(svg_squares, head(split_seq_sq, n_split)),
        NULL
      )
    }
  )
  T_path_svg <- c(
    if (do_jump) {
      c(
        svg_squares_path(head(jump_seq_sq, n_jump + n_path_ext), jump_col),
        NULL
      )

    },
    if (do_split) {
      c(
        svg_squares_path(head(split_seq_sq, n_split + n_path_ext), split_col),
        svg_path(box(head(split_seq_sq, n_split))[c(4,2,1,3), ],
                 box_split_col, "none", sq_border)
      )
    }
  )

  # rTRNG text ----
  txt_y <- T_y + sq_size + text_size
  txt_x <- T_x + sq_size*(split_s - 0.5)
  r_x <- txt_x - text_size/2 - sq_size/4
  R_x <- txt_x + text_size/1.8 + sq_size/4
  Dx <- (max(full_seq_sq$x) + sq_size/2 - (R_x - txt_x) - R_x) / 2 #text_size * 0.8 * sqrt(3)/2
  Dy <- (with(full_seq_sq, max(y[angle == 0])) + sq_size - txt_y) / 2 # Dx / sqrt(3) #text_size * 0.8 * 1/2

  rTRNG_svg <- c(
    svg_txt("r", r_x, txt_y, text_size, "middle"),
    svg_txt("R", R_x, txt_y, text_size, "middle"),
    svg_txt("N", R_x + Dx, txt_y + Dy, text_size, "middle"),
    svg_txt("G", R_x + 2*Dx, txt_y + 2*Dy, text_size, "middle"),
    # svg_path(points = .df(x = txt_x, y = c(txt_y, txt_y - text_size)), stroke = "red", width = 0.2),
    # svg_path(points = .df(x = c(R_x, R_x+2*Dx), y = c(txt_y, txt_y+2*Dy)), stroke = "red", width = 0.2),
    # svg_path(points = .df(x = c(r_x, R_x), y = c(txt_y, txt_y)), stroke = "red", width = 0.2),
    NULL
  )

  # write SVG ----
  writeLines(.sub(
    svg,
    canvas =
      if (bg_col %in% c("white", "#FFFFFF")) "lightgray" else "none",
    content =
      paste(c(
        svg_hex_bg(bg_col),
        hex_sq_svg,
        T_sq_svg,
        if (do_jump) {
          svg_connect_A(square_center(full_seq_sq[1, ]),
                        square_center(jump_seq_sq[1, ]),
                        r = -(T_x - tl_x + sq_size)*0.8,
                        jump_col)
        },
        if (do_split & !do_jump) {
          svg_connect_A(square_center(full_seq_sq[1, ]),
                        square_center(split_seq_sq[1, ]),
                        r = (T_x - tl_x + (split_s - 1)*sq_size),
                        split_col)
        },
        if (do_split & do_jump) {
          svg_connect_A(square_center(jump_seq_sq[1, ]),
                        square_center(split_seq_sq[1, ]),
                        r = sq_size*(split_s - 1)/2,
                        split_col)
        },
        hex_path_svg,
        T_path_svg,
        rTRNG_svg,
        NULL
      ), collapse = "\n")
  ), file)

  # post-rpocess SVG ----
  switch(
    postprocess,
    "rsvg" = {
      rsvg::rsvg_svg(file, file, hex_size$x*2.834645669, hex_size$y*2.834645669)
    },
    "rsvg2" = {
      rsvg::rsvg_svg(file, file, 100*hex_size$x*2.834645669, 100*hex_size$y*2.834645669)
      rsvg::rsvg_svg(file, file, hex_size$x*2.834645669, hex_size$y*2.834645669)
    },
    "inkscape-text2path" =
      if (system(paste("inkscape -z -l", file, "-T", file)) != 0) {
        stop("inkscape is not available!")
      },
    "inkscape" =
      if (system(paste("inkscape -z -l", file, file)) != 0) {
        stop("inkscape is not available!")
      }
  )

  invisible(file)
}
