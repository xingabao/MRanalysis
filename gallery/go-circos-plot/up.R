up.ui <- function(id, width = 2) {
  
  e.pval = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'pval', label = 'P-value Threshold', min = 0.01, max = 1, value = 0.05, step = 0.01)
  )
  
  e.shownum = tags$div(
    title = '',
    shiny::sliderInput(inputId = 'shownum', label = 'How many terms will be displayed?', min = 1, max = 10, value = 5, step = 1)
  )
  
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
        tags$h5("Set Palettes for Outer Circle (BP/CC/MF)"),
        tags$div(
          title = '',
          shinyWidgets::colorPickr(inputId = 'bp_col', label = 'BP Color', selected = '#3B4992FF', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'cc_col', label = 'CC Color', selected = '#EE0000FF', opacity = TRUE),
          shinyWidgets::colorPickr(inputId = 'mf_col', label = 'MF Color', selected = '#008B45FF', opacity = TRUE)
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
      icon = icon("palette"),
      status = "danger", width = "480px"
    )
  )
  
  e.layout = tags$div(
    tags$style(
      HTML(".col-sm-2, .col-sm-10, .col-sm-8 { padding:0px; margin:0px; }"),
      HTML('#sw-drop-layout { margin-left:12px; margin-top:24px; }')
    ),
    dropdown(
      inputId = 'layout',
      label = "Layout",
      circle = FALSE,
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
        shiny::sliderInput(inputId = 'term_size', label = 'Term Size', min = 0.1, max = 1, value = 0.4, step = 0.05)
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
      icon = icon("hashtag"),
      status = "danger", width = "480px"
    )
  )
  
  
  flu <- fluidPage(
    
    column(12,
           class = 'up-col-sm-12',
           column(width * 1, e.palette),
           column(width * 1, e.layout),
           column(width * 1.5, e.pval),
           column(width * 1.5, e.shownum)
    )
  )
  
  flu
}
