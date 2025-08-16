up.ui <- function(width = 2, dat) {
  
  dat = as.data.frame(dat)
  dat$SNPy = -log10(dat[3])
  ce = ceiling(max(dat$SNPy))
  
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

  e.hline = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'hline', label = 'Add Horizontal Line', value = 8, step = 1, min = 0, max = 10)
  )
  
  e.hlinec = tags$div(
    title = '',
    shinyWidgets::colorPickr(inputId = 'hline_col', label = 'Horizontal Line Color', selected = '#FF0000', opacity = TRUE)
  )
  
  e.palette = tags$div(
    title = '',
    shinyWidgets::virtualSelectInput(
      inputId = 'palette',
      label = h5("Select Palettes"),
      choices = default.colours,
      selected = 'pal_aaas',
      search = TRUE,
      multiple = FALSE
    )
  )
  
  e.font = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'font_size', label = 'Font Size', value = 12, step = 1, min = 1, max = 36)
  )
  
  e.ylim = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'ylim_custom', label = 'Set scale limits (Y)', min = 0, max = round(ce + 10, 1), value = round(ce, 1), step = 1)
  )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 2, e.full),
           column(width * 2, e.hline),
           column(width * 1.5, e.hlinec)
    ),
    column(12,
           class = 'up-col-sm-12',
           column(width * 2.5, e.palette),
           column(width * 1.5, e.font),
           column(width * 2, e.ylim)
    )
  )
  
  flu
}