library(tidyverse)
library(readxl)
library(here)

compile_postcensal_data <- function(filename) {
  # compile postcensal data (total pop only) into long format
  counties <- c("King", "Kitsap", "Pierce", "Snohomish")
  rdf <- read_excel(here("data", filename), sheet = "Population", skip = 4)
  
  est_cols <- str_subset(colnames(rdf), "^\\d+")
  
  df <- rdf %>% 
    select(-Line) %>% 
    filter(County %in% counties) %>% 
    pivot_longer(cols = all_of(est_cols)) %>% 
    separate(name, into = c("year", "attr"), sep = "(?<=[0-9])(\\s)") %>% 
    # extract(attr, "attr", "(^\\w+).*") %>%
    mutate(across(c(Filter, value), as.numeric), attr = "Total Population") %>% 
    filter(year != "2010")
}

clean_intercensal_sheet <- function(filename, sheetname) {
  # munge intercensal data from a single sheet
  # one sheet covers one attribute
  lookup <- read_excel(here("data", "lookup.xlsx")) %>%
    mutate(across("Filter", as.numeric))
    
  rdf <- read_excel(here("data", filename), sheet = sheetname)
  
  est_cols <- str_subset(colnames(rdf), "^\\d+")
  id_cols <- c("Filter", "County Name", "City Name", "Jurisdiction")
  id_cols2 <- c("Filter", "County", "Jurisdiction")
  counties <- c("King", "Kitsap", "Pierce", "Snohomish")
  
  df <- rdf %>% inner_join(lookup, by = c("Filter", "County Name" = "inter_county_name", 
                                          "Jurisdiction" = "inter_jurisdiction", 
                                          "City Name" = "inter_city_name")) %>% 
    select(!all_of(id_cols[2:4])) %>%
    rename(Jurisdiction = post_jurisdiction, County = post_county) %>% 
    filter(County %in% counties) %>% 
    select(all_of(c(id_cols2, est_cols))) %>% 
    pivot_longer(cols = all_of(est_cols)) %>% 
    extract(name, into = c("year"), "(^\\d+)") %>% 
    mutate(across(value, as.numeric), attr = sheetname)
}

compile_intercensal_data <- function(filename) {
  # compile select attributes of intercensal data across multiple sheets
  # pivot to long format
  sheets <- c("Total Population", "Household Population", "GQ Population", "Total Housing", "Occupied Housing")

  clean_data <- partial(clean_intercensal_sheet, filename = filename)
  all_data <- map(sheets, clean_data) %>% reduce(bind_rows)
}

bind_inter_post_censal_data <- function(post_censal_file, inter_censal_files) {
  bind_rows(
    compile_postcensal_data(post_censal_file), compile_intercensal_data(inter_censal_files)
  )
}


calc_delta <- function(table) {
  # calculate delta of estimate years from a compiled table
  group_cols <- c("Filter", "County", "Jurisdiction", "attr")
  df_delta <- table %>% 
    group_by(across(all_of(group_cols))) %>%
    mutate(delta = value - lag(value)) %>%
    mutate(delta = ifelse(is.na(delta), 0, delta))
}

# Test f(x) ---------------------------------------------------------------


# df <- compile_postcensal_data("ofm_april1_population_final.xlsx")
# df_delta <- calc_delta(df)

# df <- compile_intercensal_data('ofm_april1_intercensal_estimates_2000-2010.xlsx')
# df_delta <- calc_delta(df)
  
