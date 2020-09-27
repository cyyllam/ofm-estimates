attr_list <- list(
  "Total Population" = "Total Population", 
  "Household Population" = "Household Population", 
  "Group Quarters Population" = "GQ Population",
  "Total Housing Units" = "Total Housing",
  "Households" = "Occupied Housing"
)
years <- unique(df$year)

tab_tabular <- tabPanel(
  title = "Tables",
  value = "tablr",
  fluidRow(
    column(width = 3,
           selectInput("tablr_attr", 
                       "Dataset", 
                       choices = attr_list,
                       selected = "Total Population"),
           radioButtons("tablr_report_type",
                        "Report Type",
                        choices = list("Total" = "Total", "Delta" = "Delta"), 
                        selected = "Total"),
           sliderInput("tablr_year", 
                       "Years",
                       min = as.numeric(min(years)), 
                       max = as.numeric(max(years)),
                       step = 1,
                       sep = "",
                       value = c(as.numeric(max(years))-3, as.numeric(max(years)))
                       )
    ),
    column(width = 9,
           uiOutput("ui_tablr_main_table")
    )
    ) # end fluidRow
)