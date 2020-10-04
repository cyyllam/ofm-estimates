library(reactable)
library(sparkline)

reactable_specific_col_def <- function() {
  custom_colDef <- list(
    Jurisdiction = colDef(minWidth = 150),
    Trendline = colDef(
      cell = function(values) {
        sparkline(values, type = "bar")
      }
    )
  )
}

default_col_def <- function(format_type) {
  if (format_type == "number") {
    return(colDef(format = colFormat(separators = T)))
  } else if (format_type == "percent") {
    return(colDef(format = colFormat(percent = TRUE, digits = 1)))
  }
  
}
