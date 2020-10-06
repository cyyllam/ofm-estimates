fluidPage(#theme = "stylesheet.css",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  navbarPage(
    title = "OFM Estimates",
    id = "navbar",
    selected = "home",
    # theme = "style.css",
    fluid = T,

# tabs --------------------------------------------------------------------


    home,
    explore
  )
)