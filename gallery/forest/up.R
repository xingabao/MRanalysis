up.ui <- function(dat, width = 2) {
  
  dat = as.data.frame(dat)
  
  e.shape = tags$div(
    title = '',
    shinyWidgets::colorPickr(inputId = 'shape_col', label = 'Shape Color', selected = '#FF0000', opacity = TRUE)
  )
  
  e.line = tags$div(
    title = '',
    shinyWidgets::colorPickr(inputId = 'line_col', label = 'Line Color', selected = '#00008B', opacity = TRUE)
  )
  
  e.font = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'font_size', label = 'Font Size', value = 1.4, step = 0.1, min = 1, max = 3)
  )
  
  e.digit = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'digit', label = 'Rounding of Numbers', value = 3, step = 1, min = 0, max = 5)
  )
  
  e.ratio = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'ratio', label = 'Layout Ratio', value = 4, step = 0.1, min = 0.1, max = 10)
  )
  
  e.vline = tags$div(
    title = '',
    shiny::numericInput(inputId = 'vline', label = 'Add Vertical Line', value = 1)
  )
  
  e.xlim = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML(".dropdown-menu { min-width:360px; }"),
      HTML('#sw-drop-xlim { margin-left:12px; margin-top:24px; }')
    ),
    NULL,
    dropdown(
      inputId = 'xlim',
      label = "Set scale limits",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      prettyToggle(
        value = TRUE,
        inputId = "xlim_mode",
        label_on = "Automatic Mode",
        icon_on = icon("check"),
        status_on = "info",
        status_off = "warning",
        label_off = "Custom Mode",
        icon_off = icon("check")
      ),
      conditionalPanel(
        condition = paste0('input.xlim_mode === false'),
        tags$div(
          sliderInput(inputId = 'xlim_custom', label = 'Set scale limits', min = round(round(min(c(dat[, 4], dat[, 5]))) - 2, 1), max = round(max(c(dat[, 4], dat[, 5])) + 2, 1), value = round(c(min(c(dat[, 4], dat[, 5])), max(c(dat[, 4], dat[, 5]))), 1), step = 0.1)
        )
      ),
      width = '320px'
    )
  )

  e.layout = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML(".dropdown-menu { min-width:360px; }"),
      HTML('#sw-drop-layout { margin-left:12px; margin-top:24px; }')
    ),
    NULL,
    dropdown(
      inputId = 'layout',
      label = "Set Plot Layout",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        shiny::numericInput(inputId = 'lmethod', label = 'Method', value = 0, step = 0.05),
        shiny::numericInput(inputId = 'lpVal', label = 'Pval', value = 6, step = 0.05),
        shiny::numericInput(inputId = 'lpValt', label = 'Pval Title', value = 6, step = 0.05),
        shiny::numericInput(inputId = 'lort', label = 'OR title', value = 9, step = 0.05),
        shiny::numericInput(inputId = 'lor', label = 'OR', value = 10, step = 0.05)
      ),
      width = '250px'
    )
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width, e.shape),
           column(width, e.line),
           column(width * 2, e.font),
           column(width * 2, e.digit),
           column(width * 2, e.ratio),
           column(width, e.vline),
           column(width, e.xlim),
           column(width, e.layout),
           
    )
  )
  
  flu
}