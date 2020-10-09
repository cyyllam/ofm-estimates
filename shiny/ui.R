fluidPage(#theme = "stylesheet.css",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100;300;400;500&family=Roboto&display=swap")
   
    # tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@200;300;400&display=swap")
    # tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;500&display=swap")
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