
# add custom font
dir.create('~/.fonts', showWarnings = FALSE)
file.copy("fonts/GothamBookRegular/GothamBookRegular.otf", "~/.fonts")
system('fc-cache -f ~/.fonts')

pkgload::load_all() # required in order for rsconnect::deployApp() to get dependencies

rTRhexNG::rTRhexNG_app()
