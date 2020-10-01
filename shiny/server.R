server <- function(input, output, session) {
  observeEvent(!(input$tablr_juris %in% c(4,5)), {
    # clear county checkboxes if user clicks on the county related summaries
    updateCheckboxGroupInput(session, 
                             "tablr_county",  
                             choices = list("King" = "King",
                                            "Kitsap" = "Kitsap",
                                            "Pierce" = "Pierce",
                                            "Snohomish" = "Snohomish"),
                             selected = NULL)
  })
  
  filter_data <- reactive({
    years <- input$tablr_year
    id_cols <- c("Filter", "County", "Jurisdiction")

    d <- df %>% 
      arrange(year) %>% 
      filter(attr == input$tablr_attr,
             year %in% seq(min(years), max(years))) 

    if (!is.null(input$tablr_county)) { 
      cnty_filter <- input$tablr_county
      d <- d %>% filter(County %in% cnty_filter)
    } 
    
    if (input$tablr_juris %in% c(1:4)) {
      d <- d %>% filter(Filter == input$tablr_juris)
    }

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
    
    # create list column Trendline for sparkline htmlwidget
    t <- t %>% 
      mutate(Trendline = pmap(unname(.[str_subset(colnames(.), "\\d{4}")]), c)) %>% 
      select(all_of(id_cols), Trendline, everything())
    
    footer_name <- switch(input$tablr_juris, 
                          "1"= "Region", 
                          "2" = "Unincorporated Region",
                          "3" = "Incorporated Region"
    )
    
    if (!(input$tablr_juris %in% c(4, 5))) {
      # add total summary line if county related summary
      sum_cols <- str_subset(colnames(t), "\\d{4}")
      b <- t %>% 
        summarise(across(all_of(sum_cols), sum)) %>% 
        mutate(Trendline = pmap(unname(.[str_subset(colnames(.), "\\d{4}")]), c),
               Jurisdiction = footer_name)
      t <- bind_rows(t, b)
    }
    
    return(t)
  })
  
  output$ui_tablr_main_table <- renderUI({
    display_note <- "Estimates other than Total Population are not yet available for years after 2010."
    
    if ((all(as.integer(input$tablr_year) > 2010) ) & (input$tablr_attr != "Total Population")) {
      div(p(display_note))
    } else if (any(as.integer(input$tablr_year) > 2010) & (input$tablr_attr != "Total Population")) {
      div(
        div(p(display_note)),
        reactableOutput("tablr_main_table")
      )
    } else {
      reactableOutput("tablr_main_table")
    }
  })
  
  output$tablr_main_table <- renderReactable({
    t <- filter_data() %>% 
      select(!Filter)
    
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
    
    if (nrow(t) > 0) {
      reactable(t,
                searchable = T,
                defaultPageSize = 20,
                columnGroups = list(
                  colGroup(name = "Year", columns = cols)),
                defaultColDef = colDef(format = colFormat(separators = T)),
                columns = reactable_col_def()
      )
    }
  })
  
}