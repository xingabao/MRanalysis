up.ui <- function(width = 2) {

  e.full = tags$div(
    tags$br(),
    title = 'When dealing with large datasets, plotting can be time-consuming. To address this, you can adjust the plotting parameters using a subset of the data. Once the parameters are finalized, you can then run the plot with the complete dataset.',
    shinyWidgets::switchInput(
      inputId = "full_data",
      label = "Use Full Data",
      labelWidth = "180px",
      handleWidth = '50px'
    )
  )
  
  e.point = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML(".dropdown-menu { min-width:360px; }"),
      HTML('#sw-drop-point { margin-left:12px; margin-top:24px; }')
    ),
    NULL,
    dropdown(
      inputId = 'point',
      label = "Set Point Style",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        shinyWidgets::colorPickr(inputId = 'point_col', label = 'Point Color', selected = '#0000FF', opacity = TRUE),
        shiny::sliderInput(inputId = 'point_size', label = 'Point Size', value = 3, step = 0.5, min = 0, max = 10)
      ),
      width = '360px'
    )
  )
  
  e.line = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML(".dropdown-menu { min-width:360px; }"),
      HTML('#sw-drop-line { margin-left:12px; margin-top:24px; }')
    ),
    NULL,
    dropdown(
      inputId = 'line',
      label = "Set Line Style",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        shinyWidgets::colorPickr(inputId = 'line_col', label = 'Line Color', selected = '#FF0000', opacity = TRUE),
        shiny::sliderInput(inputId = 'line_width', label = 'Line Width', value = 0.5, step = 0.25, min = 0, max = 5)
      ),
      width = '360px'
    )
  )
  
  e.font = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'font_size', label = 'Font Size', value = 16, step = 1, min = 1, max = 36)
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 2, e.full),
           column(width, e.point),
           column(width, e.line),
           column(width * 2, e.font)
    )
  )
  
  flu
}