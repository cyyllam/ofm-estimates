server <- function(input, output) {
  filter_data <- reactive({
    years <- input$tablr_year
    id_cols <- c("Filter", "County", "Jurisdiction")
    
    d <- df %>% 
      arrange(year) %>% 
      filter(attr == input$tablr_attr,
             year %in% seq(min(years), max(years)))
    
    if (input$tablr_report_type == "Total") {
      t <- d %>% 
        pivot_wider(id_cols = all_of(id_cols),
                    names_from = year)
    } else {
      t <- d %>% 
        calc_delta() %>% 
        pivot_wider(id_cols = all_of(id_cols),
                    names_from = year,
                    values_from = delta) %>%
        ungroup()
    }
    
    return(t)
  })
  
  output$main_table <- renderReactable({
    t <- filter_data() %>% select(!Filter)
    
    cols <- str_subset(colnames(t), "\\d{4}")

    if (input$tablr_report_type == "Delta") {
      # re-name column headers
      cols_tail_full <- tail(cols, -1)
      cols_tail <- cols_tail_full %>% map(~ paste0("-", str_extract(.x, "\\d{2}$"))) %>% unlist
      new_cols_name <- head(cols, -1) %>% paste0(cols_tail)
      names(cols_tail_full) <- new_cols_name
      t <- t %>% rename(!!!cols_tail_full)
      cols <- c(cols[1], new_cols_name)
    }
    
    reactable(t,
              searchable = T,
              # defaultPageSize = ,
              columnGroups = list(
                colGroup(name = "Year", columns = cols)
                ),
              defaultColDef = colDef(format = colFormat(separators = T)),
              columns = list(
                Jurisdiction = colDef(minWidth = 260) 
              ),
              )

  })
}