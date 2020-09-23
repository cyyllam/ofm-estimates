server <- function(input, output) {
  output$table <- renderReactable({
    reactable(df)
  })
}