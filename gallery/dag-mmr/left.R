left.ui.title <-  function(e.title = 'Exposure', m.title = 'Mediator', o.title = 'Outcome') {
  tags$div(
    id = 'beta_title',
    title = 'BETA',
    dropdown(
      tags$style( 
        HTML(".col-sm-2, .col-sm-10 { padding:0px; margin:0px; }"),
        HTML(".dropdown-menu { min-width:480px; }"),
        HTML(paste0('#', 'total_effect_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'total_effect_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } ")),   
        HTML(paste0('#', 'indirect_effect_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'indirect_effect_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } "))
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "exposure_text", label = "Exposure", value = e.title, placeholder = "Enter text for 'exposure' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "exposure_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 colourInput(inputId = 'exposure_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'exposure_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.5, step = 0.01),
                 sliderInput(inputId = 'exposure_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 0, step = 0.05),
                 sliderInput(inputId = 'exposure_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0, step = 0.05),
                 sliderInput(inputId = 'exposure_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'exposure_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "mediator_text", label = "Mediator", value = m.title, placeholder = "Enter text for 'mediator' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "mediator_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 colourInput(inputId = 'mediator_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'mediator_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.5, step = 0.01),
                 sliderInput(inputId = 'mediator_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 0.5, step = 0.05),
                 sliderInput(inputId = 'mediator_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0, step = 0.05),
                 sliderInput(inputId = 'mediator_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'mediator_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "outcome_text", label = "Outcome", value = o.title, placeholder = "Enter text for 'outcome' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "outcome_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 colourInput(inputId = 'outcome_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'outcome_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.5, step = 0.01),
                 sliderInput(inputId = 'outcome_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 1, step = 0.05),
                 sliderInput(inputId = 'outcome_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0, step = 0.05),
                 sliderInput(inputId = 'outcome_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'outcome_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      tags$div(column(width = 12, tags$p('.'))),
      style = "material-circle", icon = icon("align-left"),
      status = "danger", width = "300px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  )
}

left.ui.text <-  function(t.e.title = '', i.e.title = '', beta1.title = '', beta2.title = '') {
  tags$div(
    id = 'beta_text',
    title = 'BETA',
    dropdown(
      tags$style( 
        HTML(paste0('#', 'beta1_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'beta1_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } ")),   
        HTML(paste0('#', 'beta2_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'beta2_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } ")),   
        HTML(paste0('#', 'exposure_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'exposure_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } ")),   
        HTML(paste0('#', 'mediator_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'mediator_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } ")),   
        HTML(paste0('#', 'outcome_dropdownbutton_state', " { margin-left:4px; margin-top:30px; }")),
        HTML(paste0('#', 'outcome_dropdownbutton', " { width:29px; border-radius:19px; font-size:10px; height:29px; padding:0px; line-height:2; margin-top:8px; margin-left:8px; padding-top:0.5px; } "))  
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "beta1_text", label = "beta1", value = beta1.title, placeholder = "Enter text for 'beta1' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "beta1_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 up = FALSE,
                 colourInput(inputId = 'beta1_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'beta1_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.4, step = 0.01),
                 sliderInput(inputId = 'beta1_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 0.9, step = 0.05),
                 sliderInput(inputId = 'beta1_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0.3, step = 0.05),
                 sliderInput(inputId = 'beta1_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'beta1_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "beta2_text", label = "beta2", value = beta2.title, placeholder = "Enter text for 'beta2' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "beta2_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 up = FALSE,
                 colourInput(inputId = 'beta2_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'beta2_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.4, step = 0.01),
                 sliderInput(inputId = 'beta2_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 0.1, step = 0.05),
                 sliderInput(inputId = 'beta2_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0.3, step = 0.05),
                 sliderInput(inputId = 'beta2_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'beta2_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "indirect_effect_text", label = "Indirect Effect", value = i.e.title, placeholder = "Enter text for 'indirect effect' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "indirect_effect_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 up = FALSE,
                 colourInput(inputId = 'indirect_effect_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'indirect_effect_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.4, step = 0.01),
                 sliderInput(inputId = 'indirect_effect_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 0.5, step = 0.05),
                 sliderInput(inputId = 'indirect_effect_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0.8, step = 0.05),
                 sliderInput(inputId = 'indirect_effect_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'indirect_effect_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      tags$div(
        column(width = 10, textAreaInput(inputId = "total_effect_text", label = "Total Effect", value = t.e.title, placeholder = "Enter text for 'total effect' ...", rows = 2, resize = 'horizontal')),
        column(width = 2,
               dropdownButton(
                 inputId = "total_effect_dropdownbutton",
                 label = "Setting",
                 icon = icon("add"),
                 status = "primary",
                 circle = TRUE,
                 tooltip = TRUE,
                 up = FALSE,
                 colourInput(inputId = 'total_effect_colour', "Colour: ", value = '#000000', allowTransparent = TRUE),
                 sliderInput(inputId = 'total_effect_size', label = 'Size (pt): ', min = 0, max = 1, value = 0.4, step = 0.01),
                 sliderInput(inputId = 'total_effect_hjust', label = 'Horizontal Align: ', min = -1.5, max = 1.5, value = 0.5, step = 0.05),
                 sliderInput(inputId = 'total_effect_vjust', label = 'Vertical Align: ', min = -1.5, max = 1.5, value = 0, step = 0.05),
                 sliderInput(inputId = 'total_effect_angle', label = 'Angle: ', min = -180, max = 180, value = 0, step = 5),
                 sliderInput(inputId = 'total_effect_lineheight', label = 'Line Height: ', value = 1, min = 0, max = 3, step = 0.05)
               ))
      ),
      style = "material-circle", icon = icon("hashtag"),
      status = "danger", width = "480px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  )
}