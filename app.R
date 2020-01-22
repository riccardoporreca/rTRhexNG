
dir.create('~/.fonts')
file.copy("fonts/GothamBookRegular/GothamBookRegular.otf", "~/.fonts")
system('fc-cache -f ~/.fonts')
rTRhexNG::rTRhexNG_app()
