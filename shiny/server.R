server <- function(input, output) {
  filter_data <- reactive({
    years <- input$tablr_year
    id_cols <- c("Filter", "County", "Jurisdiction")
    
    df %>% 
      arrange(year) %>% 
      filter(attr == input$tablr_attr,
             year %in% seq(min(years), max(years))) %>% 
      pivot_wider(id_cols = all_of(id_cols),
                  names_from = year)
  })
  
  output$main_table <- renderReactable({
    t <- filter_data() %>% 
      select(-Filter)
    cols <- str_subset(colnames(t), "\\d{4}")

    reactable(t,
              searchable = T,
              # defaultPageSize = ,
              columnGroups = list(
                colGroup(name = "Year", columns = cols)
                ),
              defaultColDef = colDef(format = colFormat(separators = T)),
              columns = list(
                Jurisdiction = colDef(minWidth = 260)  # overrides the default
              ),
              )

  })
}