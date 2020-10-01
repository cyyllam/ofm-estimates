library(reactable)
library(sparkline)

reactable_col_def <- function() {
  custom_colDef <- list(
    Jurisdiction = colDef(minWidth = 150),
    Trendline = colDef(
      cell = function(values) {
        sparkline(values, type = "bar")
      }
    )
  )
}
