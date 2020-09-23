
estimates <- tabPanel(
  title = "Estimates",
  value = "estimates",
  column(
    width = 12,
    reactableOutput("table")
  )
)