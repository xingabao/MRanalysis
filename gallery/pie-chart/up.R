up.ui <- function(id, width = 2) {
  
  e.palette = tags$div(
    title = '',
    shinyWidgets::virtualSelectInput(
      inputId = 'palette',
      label = h5("Select Palettes"),
      choices = default.colours,
      search = TRUE,
      multiple = TRUE
    )
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 3, e.palette)
    )
  )
  
  flu
}
