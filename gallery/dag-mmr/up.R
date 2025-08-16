up.ui <- function(width = 2) {
  
  e.palette = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML('#sw-drop-paldrop { margin-left:12px; margin-top:24px; }')
    ),
    dropdown(
      inputId = 'paldrop',
      label = "Palettes",
      circle = FALSE,
      tags$div(
        title = '',
        prettyToggle(inputId = "self_palette", label_on = tags$strong("Use built-in color palette."), label_off = tags$strong("Customize each color."), icon_on = icon("check"), icon_off = icon("check"), value = FALSE)
      ),
      conditionalPanel(
        condition = paste0('input.', 'self_palette', ' === true'),
        tags$div(
          title = '',
          shinyWidgets::virtualSelectInput(
            inputId = 'palette_bin',
            label = h5("Select Palettes for Exposure/Mediator/Outcome"),
            choices = default.colours,
            search = TRUE,
            selected = 'pal_rgb',
            multiple = FALSE
          )
        ),
        tags$hr()
      ),
      conditionalPanel(
        condition = paste0('input.', 'self_palette', ' === false'),
        tags$h5("Set Palettes for Exposure/Mediator/Outcome"),
        tags$div(
          title = '',
          shinyWidgets::colorPickr(inputId = 'exposure_col', label = 'Exposure', selected = '#F8766D', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'mediator_col', label = 'Mediator', selected = '#7F7F7F', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'outcome_col', label = 'Outcome', selected = '#00BFC4', opacity = TRUE)
        )
      ),
      tags$br(),
      icon = icon("palette"),
      status = "danger", width = "360px"
    )
  )
  
  # e.xlim = tags$div(
  #   title = '',
  #   shiny::sliderInput(inputId = 'xlim', label = 'XLIM', min = -5, max = 5, value = c(round(min(dat[, lc95]) * 0.8, 1), round(max(dat[, up95]) * 1.2, 1)), step = 0.1)
  # )
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 2, e.palette)
    )
  )
  
  flu
}
