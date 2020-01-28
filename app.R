# ShinyApp

# add custom font
dir.create('~/.fonts', showWarnings = FALSE)
file.copy("fonts/GothamBookRegular/GothamBookRegular.otf", "~/.fonts")
system('fc-cache -f ~/.fonts')

if (file.access(.libPaths()[1], 2) < 0) {
  dir.create("~/.r-library", showWarnings = FALSE)
  .libPaths(c("~/.r-library", .libPaths()))
}
install.packages(".", repos = NULL)

rTRhexNG::rTRhexNG_app()
