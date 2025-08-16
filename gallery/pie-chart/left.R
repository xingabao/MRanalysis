left.ui <- function(id = '') {
  tags$div(
    id = 'pie_chart_left_ui',
    title = 'Legend Setting (Color)',
    dropdown(
      tags$style( 
        HTML(".col-sm-2, .col-sm-10 .col-sm-6 { padding:0px; margin:0px; }"),
        HTML(".dropdown-menu { min-width:480px; }"),
      ),
      tags$div(
        title = '',
        prettyToggle(inputId = "text_or_label", label_on = "Use geom_text_repel for annotation.", label_off = "Use geom_label_repel for annotation.", icon_on = icon("check"), icon_off = icon("check"), value = FALSE)
      ),
      tags$div(
        title = '',
        prettyToggle(inputId = "arrange", label_on = "Perform a sort operation.", label_off = "In Original Order.", icon_on = icon("check"), icon_off = icon("check"), value = TRUE)
      ),
      tags$div(
        title = '',
        shiny::sliderInput(inputId = "label_position", label = 'Label Position', min = 0, max = 1, value = 0.55, step = 0.05)
      ),
      tags$div(
        title = '',
        prettyToggle(inputId = "label_line", label_on = "Add Label Line.", label_off = "Remove Label Line.", icon_on = icon("check"), icon_off = icon("check"), value = FALSE)
      ),
      
      conditionalPanel(
        condition = paste0('input.', 'text_or_label', ' === true'),
        tags$div(
          title = 'Radius of rounded corners, as unit or number. Defaults to 0.15.',
          shiny::sliderInput(inputId = "label_radius", label = 'Label Radius', min = 0, max = 0.5, value = 0.15)
        ),
      ),
      conditionalPanel(
        condition = paste0('input.', 'text_or_label', ' === true'),
        tags$div(
          title = 'Amount of padding around label, as unit or number. Defaults to 0.25.',
          shiny::sliderInput(inputId = "label_padding", label = 'Label Padding', min = 0, max = 1, value = 0.25)
        ),
      ),
      tags$div(
        title = 'Label Color.',
        colourpicker::colourInput(inputId = 'label_color', label = 'Label Color', value = '#000000', allowTransparent = TRUE)
      ),
      conditionalPanel(
        condition = paste0('input.', 'text_or_label', ' === true'),
        tags$div(
          title = 'Label Fill.',
          colourpicker::colourInput(inputId = 'label_fill', label = 'Label Fill', value = '#FFFFFF', allowTransparent = TRUE)
        ),
      ),
      tags$div(
        title = 'Label Font Size.',
        shiny::sliderInput(inputId = "label_size", label = 'Label Size', min = 0, max = 1, value = 0.4, step = 0.01)
      ),
      tags$br(),
      style = "material-circle", icon = icon("palette"),
      status = "danger", width = "360px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  )
}

# legend.background, legend.margin, legend.spacing.x, ...
left.ui.legend <-  function(theme.default, id = '', l.title = 'Item', legend.key = FALSE, legend.background = FALSE) {
  tags$div(
    id = 'dotplot_legend_left_ui',
    title = 'Legend Setting',
    dropdown(
      tags$style( 
        HTML(".col-sm-2, .col-sm-10 { padding:0px; margin:0px; }"),
        HTML(".dropdown-menu { min-width:480px; }"),
        HTML(paste0('#', 'legend_titlet_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'legend_titlet_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } ")),
        HTML(paste0('#', 'legend_text', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")),   
        HTML(paste0('#', 'legend_margin', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")),   
        HTML(paste0('#', 'legend_background', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")), 
        HTML(paste0('#', 'legend_placement', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")),
        HTML(paste0('#', 'legend_key', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")),
        HTML(paste0('#', 'legend_spacing', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")),
        HTML(paste0('#', 'legend_box', " { margin-left:4px; margin-top:4px; width:260px; height:35px; background-color:#FFFFFF; color:#000000; text-align:left; } ")),
      ),
      tags$div(
        dropdownButton(
          inputId = "legend_text",
          label = "Legend Text",
          icon = icon("sliders"),
          status = "primary",
          circle = FALSE,
          tooltip = TRUE,
          selectizeInput(inputId = 'legend_text_family', label = "Font Family", choices = font.families, selected = theme.default$legend.text$family),
          selectizeInput(inputId = 'legend_text_face', label = "Face", choices = faces, selected = theme.default$legend.text$face),
          colourInput(inputId = 'legend_text_colour', "Colour: ", value = theme.default$legend.text$colour, allowTransparent = TRUE),
          sliderInput(inputId = 'legend_text_size', label = 'Size (pt): ', min = 0, max = 50, value = ifelse(test = class(theme.default$legend.text$size) == 'rel', yes = as.numeric(theme.default$legend.text$size) * base_size, no = theme.default$legend.text$size), step = 1),
          sliderInput(inputId = 'legend_text_hjust', label = 'Horizontal Align: ', min = 0, max = 1, value = theme.default$legend.text$hjust, step = 0.1),
          sliderInput(inputId = 'legend_text_vjust', label = 'Vertical Align: ', min = 0, max = 1, value = theme.default$legend.text$vjust, step = 0.1),
          sliderInput(inputId = 'legend_text_angle', label = 'Angle: ', min = -180, max = 180, value = theme.default$legend.text$angle, step = 1),
          sliderInput(inputId = 'legend_text_lineheight', label = 'Line Height: ', value = theme.default$legend.text$lineheight, min = 0, max = 5, step = 0.25),
          fluidRow(
            column(12, strong("Margin: ")),
            column(3, numericInput(inputId = 'legend_text_margin_top', label = h6("Top:"), value = as.numeric(theme.default$legend.text$margin[1]), step = 0.5)),
            column(3, numericInput(inputId = 'legend_text_margin_right', label = h6("Right:"), value = as.numeric(theme.default$legend.text$margin[2]), step = 0.5)),
            column(3, numericInput(inputId = 'legend_text_margin_bottom', label = h6("Bottom:"), value = as.numeric(theme.default$legend.text$margin[3]), step = 0.5)),
            column(3, numericInput(inputId = 'legend_text_margin_left', label = h6("Left:"), value = as.numeric(theme.default$legend.text$margin[4]), step = 0.5))
          )
        ),
        dropdownButton(
          inputId = "legend_placement",
          label = "Legend Placement",
          icon = icon("sliders"),
          status = "primary",
          circle = FALSE,
          tooltip = TRUE,
          tags$style(
            HTML("hr { height:0.5px; background-color:#EC2D7A; border:none; }"),
          ),
          selectizeInput(inputId = 'legend_direction', label = "Direction", choices = c('vertical', 'horizontal'), selected = theme.default$legend.direction),
          tags$hr(),
          selectizeInput(inputId = 'legend_position', label = "Position", choices = c('use X-Y', 'none', 'left', 'right', 'bottom', 'top'), selected = ifelse(test = length(theme.default$legend.position) == 2, yes = 'use X-Y', no = theme.default$legend.position)),
          conditionalPanel(
            condition = paste0('input.', 'legend_position', ' === "use X-Y"'),
            h5("Or, you can enter a vector specifying Position (0 to 1)."),
            h5('Manual vector input takes precedence over the dropdown above.'),
            fluidRow(column(width = 6, sliderInput(inputId = 'legend_position_x', label = p("X"), value = ifelse(test = length(theme.default$legend.position) == 2, yes = theme.default$legend.position[1], no = 1),  min = 0, max = 1, step = 0.05)),
                     column(width = 6, sliderInput(inputId = 'legend_position_y', label = p("Y"), value = ifelse(test = length(theme.default$legend.position) == 2, yes = theme.default$legend.position[2], no = 0.5), min = 0, max = 1, step = 0.05))
            )
          ),
          tags$hr(),
          selectizeInput(inputId = 'legend_justification', label = "Justification", choices = c('use X-Y', 'left', 'center', 'right', 'top', 'bottom'), selected = theme.default$legend.justification),
          conditionalPanel(
            condition = paste0('input.', 'legend_justification', ' === "use X-Y"'),
            h5("Or, you can enter a vector specifying Justification (0 to 1)."),
            h5('Manual vector input takes precedence over the dropdown above.'),
            fluidRow(column(width = 6, sliderInput(inputId = 'legend_justification_x', label = p("X"), value = .5,  min = 0, max = 1, step = 0.05)),
                     column(width = 6, sliderInput(inputId = 'legend_justification_y', label = p("Y"), value = .5, min = 0, max = 1, step = 0.05))
            )
          )
        ),
        dropdownButton(
          inputId = "legend_key",
          label = "Legend Key",
          icon = icon("sliders"),
          status = "primary",
          circle = FALSE,
          tooltip = TRUE,
          tags$style(
            HTML("hr { height:0.5px; background-color:#EC2D7A; border:none; }"),
          ),
          sliderInput(inputId = 'legend_key_height', label = 'Key Height (pt): ', min = 0, max = 100, value = as.numeric(theme.default$legend.key.height), step = 0.5),
          sliderInput(inputId = 'legend_key_width', label = 'Key Width (pt): ', min = 0, max = 100, value = as.numeric(theme.default$legend.key.width), step = 0.5)
        ),
      ),
      tags$div(
        column(width = 10, textInput(inputId = "legend_title", label = h5("Legend Title"), value = l.title, placeholder = "Please input some words ...")),
        column(width = 2,
               dropdownButton(
                 inputId = "legend_titlet_dropdownbutton",
                 label = "Legend Title Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 selectizeInput(inputId = 'legend_title_family', label = "Font Family", choices = font.families, selected = theme.default$legend.title$family),
                 selectizeInput(inputId = 'legend_title_face', label = "Face", choices = faces, selected = theme.default$legend.title$face),
                 colourInput(inputId = 'legend_title_colour', "Colour: ", value = theme.default$legend.title$colour, allowTransparent = TRUE),
                 sliderInput(inputId = 'legend_title_size', label = 'Size (pt): ', min = 0, max = 50, value = ifelse(test = class(theme.default$legend.title$size) == 'rel', yes = as.numeric(theme.default$legend.title$size) * base_size, no = theme.default$legend.title$size), step = 1),
                 sliderInput(inputId = 'legend_title_hjust', label = 'Horizontal Align: ', min = 0, max = 1, value = theme.default$legend.title$hjust, step = 0.1),
                 sliderInput(inputId = 'legend_title_vjust', label = 'Vertical Align: ', min = 0, max = 1, value = theme.default$legend.title$vjust, step = 0.1),
                 sliderInput(inputId = 'legend_title_angle', label = 'Angle: ', min = -180, max = 180, value = theme.default$legend.title$angle, step = 1),
                 sliderInput(inputId = 'legend_title_lineheight', label = 'Line Height: ', value = theme.default$legend.title$lineheight, min = 0, max = 5, step = 0.25),
                 fluidRow(
                   column(12, strong("Margin: ")),
                   column(3, numericInput(inputId = 'legend_title_margin_top', label = h6("Top:"), value = as.numeric(theme.default$legend.title$margin[1]), step = 0.5)),
                   column(3, numericInput(inputId = 'legend_title_margin_right', label = h6("Right:"), value = as.numeric(theme.default$legend.title$margin[2]), step = 0.5)),
                   column(3, numericInput(inputId = 'legend_title_margin_bottom', label = h6("Bottom:"), value = as.numeric(theme.default$legend.title$margin[3]), step = 0.5)),
                   column(3, numericInput(inputId = 'legend_title_margin_left', label = h6("Left:"), value = as.numeric(theme.default$legend.title$margin[4]), step = 0.5))
                 )
               ),
        ),
      ),
      style = "material-circle", icon = icon("l"),
      status = "danger", width = "300px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  )
}
