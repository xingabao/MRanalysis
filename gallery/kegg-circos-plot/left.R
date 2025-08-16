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
        tags$div(
          title = '',
          shinyWidgets::virtualSelectInput(
            inputId = 'palette_outer',
            label = h5("Select Palettes for Outer Circle (BP/CC/MF)"),
            choices = default.colours,
            search = TRUE,
            selected = 'pal_aaas',
            multiple = FALSE
          )
        ),
        tags$hr(),
        tags$div(
          title = '',
          shinyWidgets::virtualSelectInput(
            inputId = 'palette_inner',
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
        tags$h5("Set Palettes for Outer Circle"),
        tags$div(
          title = '',
          shinyWidgets::colorPickr(inputId = 'term_col', label = 'Term Color', selected = '#3B4992FF', opacity = TRUE),
        ),
        tags$hr(),
        tags$h5("Set Palettes for Inner Circle (Background Genes)"),
        tags$div(
          title = '',
          shinyWidgets::colorPickr(inputId = 'low_col', label = 'Low Color', selected = '#FFFF00FF', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'high_col', label = 'High Color', selected = '#EE0000FF', opacity = TRUE)
        ),
      ),
      tags$br(),
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
        shiny::sliderInput(inputId = 'margin_right', label = 'Margin Right', min = 4, max = 10, value = 5.5, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'left_right', label = 'Left or Right', min = 0.1, max = 1, value = 0.47, step = 0.01)
      ),
      tags$hr(),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'axis_size', label = 'Axis Size', min = 0.1, max = 1, value = 0.3, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'bg_gene_size', label = 'Background Gene Size', min = 0.1, max = 1, value = 0.4, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'enriched_gene_size', label = 'Enriched Gene Size', min = 0.1, max = 1, value = 0.4, step = 0.05)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'legend_size', label = 'Legend Size', min = 0.1, max = 1, value = 0.5, step = 0.05)
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
