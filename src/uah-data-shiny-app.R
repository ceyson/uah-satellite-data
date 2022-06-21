#### Inits
libs <- c('tidyverse','data.table','zoo','shiny','shinydashboard')
new.libs <- libs[!(libs %in% installed.packages()[,'Package'])]
if(length(new.libs)) install.packages(new.libs)
lapply(libs, require, character.only = TRUE)

### Read and preprocess UAH data
df_uah <- data.table::fread('https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt')
df_uah %>% dplyr::glimpse()

### Correct duplicate names
names <- c('year','month','global','global_land','global_sea',
           'north_hemisphere','north_hemisphere_land','north_hemisphere_sea',
           'south_hemisphere','south_hemisphere_land','south_hemisphere_sea',
           'tropics','tropics_land','tropics_sea',
           'north_ext','north_ext_land','north_ext_sea',
           'south_ext','south_ext_land','south_ext_sea',
           'north_pole','north_pole_land','north_pole_sea',
           'south_pole','south_pole_land','south_pole_sea',
           'usa48','usa49','aust')

### Assign column names
names(df_uah) <- names

### Drop last row, convert measures to numeric, update month feature
df_uah <- df_uah %>%
  utils::head(-1) %>%
  dplyr::mutate_if(base::is.character, base::as.numeric) %>%
  dplyr::mutate(month = paste(year,month,'01', sep = '-') %>% base::as.Date()) %>%
  dplyr::select(month, dplyr::everything())



### Interface elements
header <- shinydashboard::dashboardHeader()

sidebar <- shinydashboard::dashboardSidebar(
  p('Year window for trending'),
  shiny::sliderInput(
    inputId = 'input_year'
    , label = ''
    , c(min(df_uah$year),max(df_uah$year))
    , min = min(df_uah$year)
    , max = max(df_uah$year)
    , step = 1
    , dragRange = TRUE
    , sep = ''
  )
)

body <- shinydashboard::dashboardBody(
  plotOutput('plot_series')
)

### Assemble interface
ui <- shinydashboard::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

### Server 
server <- function(input, output, session) {
  
  ## Reactive data
  df_uah_react <- shiny::reactive({ 
    df_uah %>%
      dplyr::filter(between(year, input$input_year[1], input$input_year[2])) 
  })
  
  ## Linear fit
  lm_fit <- shiny::reactive({
    stats::lm(global ~ month, df_uah_react())
  })
  
  ## Linear trend / series chart
  output$plot_series <- renderPlot({
    df_uah_react() %>%
      ggplot2::ggplot(aes(month, global)) +
        ggplot2::geom_hline(yintercept = 0, color = 'gray', size = 1) +
        ggplot2::geom_point(color = 'blue') +
        ggplot2::geom_line(color = 'blue') +
        ggplot2::geom_smooth(method='lm', color = 'orange', fill = 'orange') +
        ggplot2::geom_line(aes(y = zoo::rollmean(global, 13, na.pad = TRUE)), color = 'red', size = 1.1) +
        ggplot2::xlab('') +
        ggplot2::ylab('T Departure from 1991-2020 Avg. (deg. C)') +
        ggplot2::ggtitle('UAH Satellite-Based Temperature of the Global Lower Atmosphere (Version 6.0)'
                         , subtitle = paste0("Slope = ", signif(lm_fit()$coef[[2]], 5))) +
        ggplot2::theme_minimal()
  })
  
}

## launch application
shinyApp(ui, server)