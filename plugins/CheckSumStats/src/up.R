up.ui <- function(width = 2) {

  e.pop = tags$div(
    id = 'e.pop',
    title = '',
    shinyWidgets::pickerInput(
      inputId = "population",
      label = "Choose Population", 
      choices = c("European MAF" = "EUR", "East Asian MAF" = "EAS", "African MAF" = "AFR", "American MAF" = "AMR", "South Asian MAF" = "SAS", "Global MAF" = "ALL"),
      multiple = TRUE,
      selected = c("EUR", "EAS", "AFR", "AMR", "SAS", "ALL"),
      choicesOpt = list(
        content = sprintf("<span class='label label-%s'>%s</span>", 
                          c("primary", 'default', "info", "success", "danger", "warning"), 
                          c("European MAF", "East Asian MAF", "African MAF", "American MAF", "South Asian MAF", "Global MAF")))
    )
  )
  
  e.ncol = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'ncol', label = 'Number of Columns in the Plot Grid', value = 3, step = 1, min = 1, max = 6)
  )
  
  e.fonts = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML(".dropdown-menu { min-width:360px; }"),
      HTML('#sw-drop-fonts { margin-left:12px; margin-top:24px; }')
    ),
    NULL,
    dropdown(
      inputId = 'fonts',
      label = "Set Font Size",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        shiny::sliderInput(inputId = 'title_size', label = 'Title Size', value = 28, step = 0.5, min = 10, max = 50),
        shiny::sliderInput(inputId = 'title_y_size', label = 'Title (Y) Size', value = 20, step = 0.5, min = 10, max = 50),
        shiny::sliderInput(inputId = 'axis_title_size', label = 'Axis Title Size', value = 16, step = 0.5, min = 10, max = 50),
        shiny::sliderInput(inputId = 'axis_text_size', label = 'Axis Text Size', value = 16, step = 0.5, min = 10, max = 50)
      ),
      width = '360px'
    )
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 2, e.pop),
           column(width * 2, e.ncol),
           column(width * 2, e.fonts)
    )
  )
  
  flu
}