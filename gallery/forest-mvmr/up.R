up.ui <- function(id, dat, lc95, up95, width = 2) {
  
  e.vline = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'vline', label = 'vline', min = 0, max = 1, value = 1, step = 1)
  )
  
  e.xlim = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'xlim', label = 'XLIM', min = -5, max = 5, value = c(round(min(dat[, lc95]) * 0.8, 1), round(max(dat[, up95]) * 1.2, 1)), step = 0.1)
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 1, e.vline),
           column(width * 2.5, e.xlim)
    )
  )
  
  flu
}
