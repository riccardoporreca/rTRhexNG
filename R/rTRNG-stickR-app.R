#' @import shiny
#' @importFrom colourpicker colourInput



# palette <- base
palette <- mesch_V2

# detect Inkscape
# inkscape <- system2("inkscape", "-V") == 0

# Define UI for app
ui <- function() {
  fluidPage(

    # App title ----
    titlePanel("R-generated hexagon sticker for rTRNG"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
        width = 3,

        sliderInput(inputId = "n",
                    label = "# of elements per side",
                    pre = "N=",
                    min = 5,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "n_full_n",
                    label = NULL, # "elements of full sequence",
                    pre = "n_full=",
                    post = "xN",
                    min = 0,
                    max = 6,
                    step = 0.1,
                    value = 5.3,
                    animate = animationOptions(
                      interval = 200,
                      loop = FALSE
                    )),

        sliderInput(inputId = "jump_size",
                    label = "jump ahead by number of steps",
                    pre = "jump(steps=",
                    post = ")",
                    min = 0,
                    max = 50,
                    value = 3),

        sliderInput(inputId = "n_jump",
                    label = NULL, # "# elements jump sequence",
                    pre = "n_jump=",
                    min = 0,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "split_p",
                    label = "split(p, s): s-th of p sub-sequences",
                    pre = "p=",
                    min = 0,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "split_s",
                    label = NULL, # "split sub-sequence [1, p]",
                    pre = "s=",
                    min = 0,
                    max = 50,
                    value = 5),

        sliderInput(inputId = "n_split",
                    label = NULL, # "# elements split sequence",
                    pre = "n_split=",
                    min = 0,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "text_size",
                    label = "text size",
                    step = 0.01,
                    min = 0,
                    max = 1,
                    value = 0.23),

        sliderInput(inputId = "poly_pad",
                    label = "pad",
                    step = 0.05,
                    min = 0,
                    max = 1,
                    value = 0.1),

        radioButtons(inputId = "poly_circle",
                     label = NULL, inline = TRUE,
                     choices = c("polygonal", "circular")),

        NULL
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        width = 8,

        plotOutput(outputId = "myImage", height = 550),

        fluidRow(

          column(2,
                 colourInput(inputId = "bg_col",
                             label = "background/text",
                             value = palette$bg)),
          column(2,
                 colourInput(inputId = "full_fill",
                             label = "full",
                             value = palette$full_fill)),
          column(2,
                 colourInput(inputId = "jump_fill",
                             label = "jump",
                             value = palette$jump_fill)),
          column(2,
                 colourInput(inputId = "split_fill",
                             label = "split",
                             value = palette$split_fill))

        ),
        fluidRow(
          column(2,
                 colourInput(inputId = "text_col",
                             label = NULL,
                             value = palette$txt)),
          column(2,
                 colourInput(inputId = "full_stroke",
                             label = NULL,
                             value = palette$full_stroke)),
          column(2,
                 colourInput(inputId = "jump_stroke",
                             label = NULL,
                             value = palette$jump_stroke)),
          column(2,
                 colourInput(inputId = "split_stroke",
                             label = NULL,
                             value = palette$split_stroke))

        ),

        radioButtons("svg_postprocess", label = NULL, inline = TRUE,
                     choices = c("as-is", "rsvg", "rsvg2", "inkscape", "inkscape-text2path"),
                     selected = "rsvg2"),

        downloadButton("save_png", "Save as PNG"),
        downloadButton("save_svg", "Save as SVG"),
        downloadButton("save_pdf", "Save as PDF"),
        downloadButton("save_R", "Download R code"),

        NULL

      )
    )
  )
}

# Define server logic----
server <- function(input, output) {

  split_ok <- reactive({
    input$split_p > 0 && input$split_s > 0 && input$split_s <= input$split_p
  })

  sq_cols_gb <- reactive({
    cols <- rep(input$full_fill, len = 1000)
    if (input$jump_size > 0) {
      cols[1 + input$jump_size] <- input$jump_fill
    }
    if (split_ok()) {
      cols[seq(input$jump_size + input$split_s, by = input$split_p, 1000)] <- input$split_fill
    }
    cols
  })

  stickR_args <- reactive({
    within(list(), {
      file <- tempfile("rTRNG-", fileext = ".svg")
      n <- input$n
      jump_size <- input$jump_size
      split_p <- input$split_p # based on the jump
      split_s <- input$split_s # based on the jump
      sq_cols <- sq_cols_gb()
      # sq_cols <- function(x) hsv(x, 0.75, 1)
      full_col <- input$full_stroke
      jump_col <- input$jump_stroke
      split_col <- input$split_stroke
      n_split <- if (split_ok()) input$n_split else 0
      n_jump <- input$n_jump
      text_size <- input$text_size
      text_col <- input$text_col
      bg_col <- input$bg_col
      n_full <- max(1L, ceiling(input$n * input$n_full_n))
      poly_pad <- input$poly_pad
      postprocess <- input$svg_postprocess
      text_font <- "GothamBook"
      circle <- input$poly_circle == "circular"
    })
  })

  create_sticker <- reactive({
    do.call(
      rTRNGstickR,
      stickR_args()
    )
  })

  output$save_png <- downloadHandler(
    filename = "rTRNG.png",
    content = function(file) {
      message(file)
      rsvg::rsvg_png(create_sticker(), file, 1200*sqrt(3)/2, 1200)
    })

  output$save_svg <- downloadHandler(
    filename = "rTRNG.svg",
    content = function(file) {
      message(file)
      file.copy(create_sticker(), file)
    })

  output$save_pdf <- downloadHandler(
    filename = "rTRNG.pdf",
    content = function(file) {
      message(file)
      rsvg::rsvg_pdf(create_sticker(), file)
    })

  output$save_R <- downloadHandler(
    filename = "rTRNG-stickR.R",
    content = function(file) {
      cl <- as.call(c(quote(rTRNGstickR), stickR_args()))
      cl$file <- "rTRNG.svg"
      out <- file(file, "w")
      on.exit(close(out))
      dput(bquote(rTRNGstickR <- .(rTRNGstickR)), out)
      dput(cl, out)
    })

  output$myImage <- renderImage({

    outfile <- create_sticker()
    # outfile <- tempfile("rTRNG-", fileext = ".png")
    # rsvg_png(create_sticker(), outfile, 1200*sqrt(3)/2, 1200)

    # Return a list containing the filename
    list(src = outfile,
         # contentType = 'image/svg+xml',
         height = 550,
         alt = "rTRNG")
  }, deleteFile = FALSE)
}

#' rTRhexNG Shiny App
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()]
#'
#' @export
rTRhexNG_app <- function(...) {
  shinyApp(ui = ui, server = server, ...)
}
