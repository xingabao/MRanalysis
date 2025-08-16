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
            selected = 'pal_aaas',
            multiple = FALSE
          )
        ),
      ),
      conditionalPanel(
        condition = paste0('input.', 'self_palette', ' === false'),
        tags$h5("Set Palettes for GO Ontology (BP/CC/MF)"),
        tags$div(
          title = '',
          shinyWidgets::colorPickr(inputId = 'bp_col', label = 'BP Color', selected = '#3B4992FF', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'cc_col', label = 'CC Color', selected = '#EE0000FF', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'mf_col', label = 'MF Color', selected = '#008B45FF', opacity = TRUE)
        ),
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
        shinyWidgets::radioGroupButtons(
          inputId = "order",
          label = "Order or Not", 
          choices = c("Unsorted" = 'Unsorted', "Descending" = 'Descending', "Ascending" = 'Ascending'),
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
        )
      ),
      tags$hr(),
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
      tags$div(
        title = '',
        shiny::sliderInput(inputId = 'legend_text', label = 'Legend Text', min = 0.1, max = 1, value = 0.36, step = 0.01)
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
