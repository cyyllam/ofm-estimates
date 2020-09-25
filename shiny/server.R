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
    # browser()
    cols <- str_subset(colnames(t), "\\d{4}")
    # coldef_args <-  lapply(cols, function(x) noquote(paste0("`",x,"`", " = colDef(format = colFormat(separators = T))")))

    reactable(t,
              searchable = T,
              # defaultPageSize = ,
              columnGroups = list(
                colGroup(name = "Year", columns = cols)
                ),
              columns = list(
                Jurisdiction = colDef(minWidth = 260)  # overrides the default
              ),
              )

  })
}