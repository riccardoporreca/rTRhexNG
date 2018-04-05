library(shiny)
library(colourpicker)
library(rsvg)
source("rTRNGstickR.R")

mirai_dark <- "#333333"
magrittr_bg <- "#f9ecc6"
mirai_light <- "#5a5a5a"
mirai_blue <- "#008cc3"
mirai_blue_light <- "#ace4f3"


# Define UI for app
ui <- fluidPage(

  # App title ----
  titlePanel("rTRNG stickR!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 4,

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
                  value = 0),

      sliderInput(inputId = "jump_size",
                  label = "jump ahead by number of steps",
                  pre = "jump(steps=",
                  post = ")",
                  min = 0,
                  max = 50,
                  value = 0),

      sliderInput(inputId = "n_jump",
                  label = NULL, # "# elements jump sequence",
                  pre = "n_jump=",
                  min = 0,
                  max = 50,
                  value = 0),

      sliderInput(inputId = "split_p",
                  label = "split(p, s): s-th of p sub-sequences",
                  pre = "p=",
                  min = 0,
                  max = 50,
                  value = 0),

      sliderInput(inputId = "split_s",
                  label = NULL, # "split sub-sequence [1, p]",
                  pre = "s=",
                  min = 0,
                  max = 50,
                  value = 0),

      sliderInput(inputId = "n_split",
                  label = NULL, # "# elements split sequence",
                  pre = "n_split=",
                  min = 0,
                  max = 50,
                  value = 0),

      sliderInput(inputId = "text_size",
                  label = "font size",
                  step = 0.01,
                  min = 0,
                  max = 1,
                  value = 0),

      NULL
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      width = 8,


      plotOutput(outputId = "myImage", height = 600),

      fluidRow(

        column(2,
               colourInput(inputId = "bg_col",
                           label = "background/text",
                           value = magrittr_bg)),
        column(2,
               colourInput(inputId = "full_fill",
                           label = "full",
                           value = "#888888")),
        column(2,
               colourInput(inputId = "jump_fill",
                           label = "jump",
                           value = "#66AA66")),
        column(2,
               colourInput(inputId = "split_fill",
                           label = "split",
                           value = "#6666AA"))

      ),
      fluidRow(
        column(2,
               colourInput(inputId = "text_col",
                           label = NULL,
                           value = mirai_dark)),
        column(2,
               colourInput(inputId = "full_stroke",
                           label = NULL,
                           value = mirai_light)),
        column(2,
               colourInput(inputId = "jump_stroke",
                           label = NULL,
                           value = "#228822")),
        column(2,
               colourInput(inputId = "split_stroke",
                           label = NULL,
                           value = "#222288"))

      ),

      fluidRow(
        column(2,
               selectInput("svg_postprocess", label = NULL, #width = "16%",
                           choices = c("as-is", "inkscape", "inkscape-text2path"))),
        column(6,
               downloadButton("save_png", "Save as PNG"),
               downloadButton("save_svg", "Save as SVG"),
               downloadButton("save_R", "Download R code"))
      ),

      NULL

    )
  )
)


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
      split_s <- input$split_s # based on the jump
      sq_cols <- sq_cols_gb()
      full_col <- input$full_stroke
      jump_col <- input$jump_stroke
      split_col <- input$split_stroke
      n_split <- if (split_ok()) input$n_split else 0
      n_jump <- input$n_jump
      text_size <- input$text_size
      text_col <- input$text_col
      bg_col <- input$bg_col
      n_full <- max(1L, ceiling(input$n * input$n_full_n))
      inkscape <- input$svg_postprocess == "inkscape"
      text2path <- input$svg_postprocess == "inkscape-text2path"
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
      rsvg_png(create_sticker(), file, 1200*sqrt(3)/2, 1200)
    })

  output$save_svg <- downloadHandler(
    filename = "rTRNG.svg",
    content = function(file) {
      message(file)
      file.copy(create_sticker(), file)
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

    outfile <- tempfile("rTRNG-", fileext = ".png")
    rsvg_png(create_sticker(), outfile, 1200*sqrt(3)/2, 1200)

    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 600*sqrt(3)/2,
         height = 600,
         alt = "rTRNG")
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)
