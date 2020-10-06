library(reactable)
library(sparkline)

rt_specific_col_def <- function() {
  custom_colDef <- list(
    Jurisdiction = colDef(minWidth = 150),
    Trendline = colDef(
      cell = function(values) {
        sparkline(values, type = "bar")
      }
    )
  )
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

top_ten_color <- make_color_pal(c("#ffe4b2","#ffc966", "#ffa500"), bias = 1.3)

rt_default_col_def <- function(table, format_type, add_style_top_ten = F) {
  if (format_type == "number") {
    frmt <- colFormat(separators = T)
  } else if (format_type == "percent") {
    frmt <- colFormat(percent = TRUE, digits = 1)
  }
  
  if (add_style_top_ten == T) {
    return(colDef(format = frmt,
                  cell = function(value, index, name) {
                    x <- head(sort(unlist(table[[name]]), decreasing = T), 10)
                    if (format_type == "number") {
                      frmt <- format(value, big.mark=",")
                    } else if (format_type == "percent") {
                      frmt <-  formatC(paste0(round(as.numeric(value) * 100, 1), "%"))
                    }
                    
                    if (is.numeric(value) && value != 0 && value %in% x) {
                      div(class = 'sub-cell', frmt)
                    } else if (is.numeric(value)){
                      frmt
                    } else {
                      value
                    }
                  }
                  # style = function(value, index, name) {
                  #   x <- head(sort(unlist(table[[name]]), decreasing = T), 10)
                  #   if (is.numeric(value) && value != 0 && value %in% x) {
                  #     normalized <- (value - min(x)) / (max(x) - min(x))
                  #     color <- top_ten_color(normalized)
                  #     list(fontWeight = "300",
                  #          background = color
                  #          # borderRadius = "100%"
                  #          )
                  #   }
                  # }
                  )
           )
  } else {
    return(colDef(format = frmt))
  }
}
