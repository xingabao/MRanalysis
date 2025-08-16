left.ui.pattle <- function() {
  tags$div(
    id = 'paldrop',
    title = 'Palettes',
    dropdown(
      tags$div(
        title = '',
        prettyToggle(inputId = "self_palette", label_on = "Use built-in color palette.", label_off = "Customize each color.", icon_on = icon("check"), icon_off = icon("check"), value = FALSE)
      ),
      conditionalPanel(
        condition = paste0('input.', 'self_palette', ' === true'),
        tags$hr(),
        tags$div(
          title = '',
          shinyWidgets::virtualSelectInput(
            inputId = 'palette',
            label = h5("Select Palettes for Inner Circle (Background Genes)"),
            choices = default.colours,
            search = TRUE,
            selected = 'pal_yr',
            multiple = FALSE
          )
        ),
      ),
      conditionalPanel(
        condition = paste0('input.', 'self_palette', ' === false'),
        tags$h5("Set Palettes for Inner Circle (Background Genes)"),
        tags$div(
          title = '',
          shinyWidgets::colorPickr(inputId = 'low_col', label = 'Low Color', selected = '#FFFF00FF', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'high_col', label = 'High Color', selected = '#EE0000FF', opacity = TRUE)
        ),
      ),
      tags$br(),
      style = "material-circle", icon = icon("palette"),
      status = "danger", width = "320px",
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
        shinyWidgets::radioGroupButtons(
          inputId = "layout_type",
          label = "Layout Type", 
          choices = c("No Facet" = 'A', "Horizontal Facet" = 'B', "Vertical Facet" = 'C'),
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
        )
      ),
      tags$hr(),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'axis_title', label = 'Axis Title', min = 0.1, max = 1, value = 0.2, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'axis_size', label = 'Axis Text', min = 0.1, max = 1, value = 0.2, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'legend_title', label = 'Legend Title', min = 0.1, max = 1, value = 0.2, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'legend_text', label = 'Legend Text', min = 0.1, max = 1, value = 0.2, step = 0.05)
      ),
      conditionalPanel(
        condition = paste0('input.', 'layout_type', ' != "A"'),
        tags$div(
          title = '',
          shiny::sliderInput(inputId = 'strip_size', label = 'Strip Text', min = 0.1, max = 1, value = 0.2, step = 0.05)
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
