library(reactable)
library(sparkline)

reactable_col_def <- function(footer_name) {
  custom_colDef <- list(
    Jurisdiction = colDef(minWidth = 150, footer = footer_name),
    Trendline = colDef(
      cell = function(values) {
        sparkline(values, type = "bar")
      }
    )
  )
}
