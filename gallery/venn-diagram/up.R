up.ui <- function(theme.default, id, width = 2) {
  
  my.palette <- c('#0000FFFF', '#FFFFFFFF', '#EE0000FF')
  
  e.box = tags$div(
    dropdownButton(
      inputId = 'box_controls',
      label = "Box Controls",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        title = 'Degree of opacity for the color(s) specified with zcolor (less opacity, more transparency).',
        sliderInput(inputId = 'opacity', label = 'Opacity', value = 0.3, step = 0.05, min = 0, max = 1)
      ),
      tags$hr(),
      tags$div(
        title = 'Logical, force the shape to an ellipse, where possible.',
        prettyToggle(inputId = 'ellipse', label_on = 'Force the box shape to an ellipse.', label_off = "Don't force the box shape to an ellipse.", icon_on = icon("check"), icon_off = icon("xmark"), value = FALSE)
      ),
      tags$hr(),
      tags$div(
        title = 'Logical: draw the outside square',
        prettyToggle(inputId = 'box', label_on = 'Draw the outside square.', label_off = "Don't draw the outside square.", icon_on = icon("check"), icon_off = icon("xmark"), value = FALSE)
      ),
      width = '480px'
    )
  )
  
  e.palette = tags$div(
    dropdownButton(
      inputId = 'palette',
      label = "Select Palettes",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        title = 'A vector of colors for the custom zones, or predefined colors if "style".',
        selectizeInput(inputId = 'zcolor', label = "zcolor", choices = c('style', 'bw', 'use palette'), selected = 'style'),
      ),
      conditionalPanel(
        condition = paste0('input.', 'zcolor', ' === "use palette"'),
        tags$div(
          title = '',
          shinyWidgets::virtualSelectInput(inputId = 'palette_default_single', label = "Select One Palette", choices = default.colours, search = TRUE, multiple = FALSE, selected = 'pal_aaas'),
        )
      ),
      width = '480px'
    )
  )
  
  e.font = tags$div(
    dropdownButton(
      inputId = 'font_controls',
      label = "Font Controls",
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      tags$div(
        title = 'Character expansion (in base plots) or size (in ggplots) for the intersection labels.',
        sliderInput(inputId = 'ilcs', label = 'Size for the intersection labels', value = 1, step = 0.1, min = 0.5, max = 5)
      ),
      tags$div(
        title = 'Character expansion (in base plots) or size (in ggplots) for the set names.',
        sliderInput(inputId = 'sncs', label = 'Size for the set names', value = 1.5, step = 0.1, min = 0.5, max = 5)
      ),
      width = '360px'
    )
  )
 
  flu <- fluidPage(
    column(12,
           class = 'up-col-sm-12',
           column(width, e.palette),
           column(width, e.box),
           column(width, e.font)
    )
  )
  
  flu
}