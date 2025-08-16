left.ui.pattle <- function() {
  tags$div(
    id = 'paldrop',
    title = 'Palettes',
    dropdown(
      tags$div(
        title = '',
        shinyWidgets::virtualSelectInput(
          inputId = 'palette',
          label = h5("Select Palettes for Inner Circle (Background Genes)"),
          choices = default.colours,
          search = TRUE,
          selected = 'pal_aaas',
          multiple = TRUE
        )
      ),
      tags$hr(),
      shinyWidgets::colorPickr(inputId = 'count_col', label = 'Count Color', selected = '#EE0000FF', opacity = TRUE),
      style = "material-circle", icon = icon("palette"),
      status = "danger", width = "480px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  )
}

left.ui.layout <-  function() {
  tags$div(
    id = 'layout',
    title = 'Layout',
    dropdown(
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'strip_text', label = 'Category Title', min = 0.1, max = 1, value = 0.5, step = 0.01)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'axis_title', label = 'Axis Title', min = 0.1, max = 1, value = 0.4, step = 0.01)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'axis_size', label = 'Axis Text', min = 0.1, max = 1, value = 0.36, step = 0.01)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'count', label = 'Count Number', min = 0.1, max = 1, value = 0.4, step = 0.01)
      ),
      tags$h5(tags$strong('Set major grid lines along the Y-axis')),
      tags$div(
        title = '',
        shinyWidgets::prettyToggle(
          value = TRUE,
          outline = TRUE,
          inputId = "panel.grid.major.y",
          label_on = 'Draw dotted line',
          icon_on = icon("check"),
          status_on = "info",
          status_off = "warning",
          label_off = 'None',
          icon_off = icon("check")
        ),
      ),
      tags$br(),
      style = "material-circle", icon = icon("hashtag"),
      status = "danger", width = "480px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  )
}
