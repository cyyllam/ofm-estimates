fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  navbarPage(
    title = "OFM Estimates for the Central Puget Sound Region",
    id = "navbar",
    selected = "home",
    fluid = T,

# tabs --------------------------------------------------------------------


    home,
    explore
  )
)