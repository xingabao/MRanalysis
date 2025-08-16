up.ui <- function(id, width = 2) {
  
  e.pval = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'pval', label = 'P-value Threshold (p.adjust)', min = 0.01, max = 1, value = 0.05, step = 0.01)
  )
  
  e.shownum = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'shownum', label = 'How many terms will be displayed?', min = 1, max = 10, value = 5, step = 1)
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 2.5, e.pval),
           column(width * 2.5, e.shownum)
    )
  )
  
  flu
}
