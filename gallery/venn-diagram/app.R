#' @title
#'
#' @description
#' 
# Load Global
source('global.R')
source('up.R')
source('src/i18n.R')

# load R packages

suppressMessages(suppressWarnings(library(venn)))

options(shiny.maxRequestSize = 1 * 1024 ^ 2)

ui.gallery <- function() {
  
  www = 'www/data'
  venn_diagram_demo <<- read.table(glue('{www}/demo.tsv'), header = TRUE, sep = '\t', check.names = FALSE)
  
  fluidPage(
    useShinyjs(),
    web.statistic.baidu,
    web.statistic.google,
    tags$head(
      includeCSS("www/css/shiny-style.css"),
      HTML("<html lang='en'>"),
      tags$title('Venn Diagram'),
      tags$link(rel = "shortcut icon", href = "img/favicon.ico"),
    ),
    tags$style(
      HTML(".col-sm-1 { width:3.5%; !important }")
    ),
    wellPanel(
      column(width = 5,
             tags$style(
               HTML(paste0('#download_demo_data', " { background-color:#E3B4B8; border-color:#000000; }")),
               HTML(paste0('#help', " { background-color:#E3B4B8; border-color:#000000; min-width:6vh; padding:6px; margin-top:0px; color:#000000 }"))
             ),
             tags$div(
               style = "width:1200px",
               HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.1}</strong>")),
               HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
               shiny::downloadButton(outputId = 'download_demo_data', label = download_demo_data),
               shiny::actionButton(inputId = 'help', label = 'Help', icon = icon('question-circle'), class = "down"),
               tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "auto", height = "100", alt = "ZOOM IN")),
               HTML('</div>')
             ),
             
             import_ui_demo(
               id = 'show_data',
               from = c('env', 'file', 'copypaste', 'url'),
               file_extensions = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")
             ),
             
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.2}</strong></div>")),
             tags$div(id = 'variable-des',  
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i><span class='var-var'>This application uses a variety of input data to draw and display a Venn diagram with up to <strong>7</strong> sets.</span>")
             ),
             uiOutput(outputId = 'ui_aesthetics'),
             
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.3}</strong></div>")),
             uiOutput(outputId = 'is_add_plot_buttom'),
             
      ),
      column(width = 7, 
             column(width = 12, uiOutput(outputId = 'modify_plot_params')),
             tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
             column(width = 12,
                    mainPanel(
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:705px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
                    )),
             
             column(width = 12, id = 'introbox-download',  uiOutput(outputId = 'intro_box')),
             
      )
    )
  )
}


server.gallery <- function(input, output, session) {
  
  www = 'www/data'
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste('demo.zip') },
    content <- function(file){
      file.copy(glue("{www}/demo.zip"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(id = "show_data", choices = c('venn_diagram_demo'), return_class = "tbl_df", read_fns = list(tsv = function(file) { data.table::fread(fread) }))
  
  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    data = imported$data()
    if (!is.null(data)) {
      shinyWidgets::multiInput(
        inputId = "dragvars",
        label = NULL,
        choices = colnames(data),
        selected = NULL,
        options = list(
          enable_search = TRUE,
          non_selected_header = "Variable Pool:",
          selected_header = "Variable Selected:"
        )
      )
    }
  })
  
  # 组件显示状态
  display.status <- reactive({ 
    !is.null(input[["dragvars"]])
  })
  
  # 点击绘图按钮是否展示
  output[['is_add_plot_buttom']] <- renderUI({
    if (display.status()) {
      tags$div(
        tags$style(
          HTML(paste0("#plot_button", " { margin-top:3px; height:38px } ")),
          HTML(paste0("#plot_refresh", " { margin-top:3px; height:38px } ")),
          HTML(paste0("#col-plot_button", " { padding:0px; margin:0px; float:left; margin-left:-16px; margin-right:5px; } ")),
          HTML(paste0("#col-plot_interactively", " { padding:0px; margin:0px; float:left; margin-right:5px; } ")),
          HTML(paste0("#col-plot_refresh", " { padding:0px; margin:0px; float:left; margin-right:5px; } ")),
        ),
        column(width = 12,
               tags$div(id = 'col-plot_button', bs4Dash::actionButton(inputId = "plot_button", label = plot_button, class = "btn-danger")),
               tags$div(id = 'col-plot_interactively', shinyWidgets::switchInput(inputId = "plot_interactively", label = plot_interactively, labelWidth = "130px", handleWidth = "50px")),
               tags$div(id = 'col-plot_refresh', bs4Dash::actionButton(inputId = "plot_refresh", label = plot_refresh, class = "btn-warning")))
        ,
        tags$div(
          id = 'interactively-des',
          HTML(glue('<span class="help-block m-b-none des-interactively"><i class="fa fa-info-circle"></i> {interactively.a}</span>')))
        ,
        HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.4d}</strong></div>")),
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 800,  min = 100, max = 3000, step = 10)),
                 column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 700, min = 100, max = 3000, step = 10))
        )
      )
    } else {
      actionButton(inputId = "plot_button_null", label = HTML(glue("<span style='color:#EE3F4D;font-size:13px;margin-top:1px;margin-bottom:1px;'><i class=\"fa fa-info-circle\"></i> {plot_button_null}</strong></span>")))
    }
  })
  
  # 对应图的一些个性化设置
  output[['modify_plot_params']] <- renderUI({
    if (display.status()) {
      tags$div(
        HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.5b} </strong>")),
        tags$br(),
        column(width = 12, up.ui(width = 2)), tags$br(), tags$br(),
      )
    } else {
      shinyjs::hide('io-albert-gg')
    }
  })
  
  # introBox
  output[['intro_box']] <- renderUI({
    if (display.status()) {
      tags$div(
        tags$style(
          HTML('.panel-default { border-color:#FFFFFF }'),
          HTML('div.panel-heading.card-header { color:#FFFFFF; background-color:#FFFFFF; border-color:#FFFFFF }'),
          HTML('div.panel-body.card-body { display:none }'),
          HTML('div.modal-content { min-width:960px }')
        ),
        tags$h5(introbox.des),
        column(width = 12,
               class = 'up-col-sm-12-20',
               tags$div(
                 tags$style(
                   HTML('.down { min-width:12vh;}'),
                 )
               ),
               shiny::downloadButton(outputId = 'download_gg_tiff', label = download.tiff, class = "down", width = '750px'),
               shiny::downloadButton(outputId = 'download_gg_pdf', label = download.pdf, class = "down", width = '750px'),
               shiny::downloadButton(outputId = 'download_gg_svg', label = download.svg, class = "down", width = '750px'),
               shiny::actionButton(inputId = 'show_codes', label = show.plot.codes, icon = icon('code'), class = "down"),
               shiny::actionButton(inputId = 'show_sessioninfo', label = show.sessioninfo, icon  = icon('info'), class = "down"),
        ),
        tags$br(), tags$br(), 
        column(width = 12, verbatimTextOutput('show_sessioninfo_output'))
      )
    }
  })
  
  # 获取各个值
  i.status <- reactiveValues(status = TRUE)
  i.data <- reactive({
    imported$data() %>% dplyr::select(c(unlist(db.dragvars(), use.names = FALSE)))
  })
  
  # DEBOUNCE
  db.dragvars <- debounce(reactive({ input[['dragvars']] }), DEBOUNCE.P)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.P)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.P)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # Load data
    # ------------------------------------------------------------------------------------------------------------
    venn_dat <- i.data()
    if (ncol(venn_dat) > 7) {
      venn_dat <- venn_dat[, 1:7]
    }
    
    # Data processing, convert to list format
    # ------------------------------------------------------------------------------------------------------------
    venn_list <- lapply(1:ncol(venn_dat), function(i) na.omit(unlist(venn_dat[, colnames(venn_dat)[i]])))
    names(venn_list) <- colnames(venn_dat)
    
    # Set palette
    # ------------------------------------------------------------------------------------------------------------
    if (input[['zcolor']] %in% c('bw', 'style')) {
      zcolor = input[['zcolor']]
    } else {
      zcolor = c.palette(input[['palette_default_single']])
    }
    palette.(zcolor)
    
    message(glue('{Sys.time()} Plot Success'))
    
    # Draw venn diagram
    # ------------------------------------------------------------------------------------------------------------
    venn(
      venn_list,
      zcolor = zcolor,
      opacity = as.logical(input[['opacity']]),
      box = as.logical(input[['box']]),
      ellipse = as.logical(input[['ellipse']]),
      ilcs = input[['ilcs']],
      sncs = input[['sncs']],
      ilabels = TRUE
    )
  }

  # 同时支持自定义图片大小
  # ------------------------------------------------------------------------------------------------------------
  observeEvent(input[['show_data-confirm']], {
    
    shinyjs::hide('io-albert-gg')
    shinyjs::hide('introbox-download')
    shinyjs::hide('variable-des')
    shinyWidgets::updateSwitchInput(session = session, inputId = 'plot_interactively', value = FALSE)
    
    observeEvent(input[['plot_button']], {
      
      req(db.dragvars(), cancelOutput = TRUE)
      
      output[['io.albert.gg']] <- renderUI({
        plotOutput(outputId = 'io.albert.gg.o', height = db.height(), width = db.width()) %>% withSpinner(type = 1, color = '#EE3F4D', size = 1.2, color.background = '#2F2F35')
      })
      
      output[['io.albert.gg.o']] <- renderPlot({
        if (input[['plot_interactively']]) {
          spsComps::shinyCatch({ plot.reactive() }, trace_back = FALSE)
        } else {
          isolate({ spsComps::shinyCatch({ plot.reactive() }, trace_back = FALSE) })
        }
      }, res = 100)
      shinyjs::show('io-albert-gg')
      shinyjs::show('introbox-download')
    })
    
    observeEvent(input[['plot_refresh']], {
      try(dev.off(), silent = TRUE)
      shinyjs::refresh()
    })
  })
  

  # 下载图片
  # 下载图片 TIFF
  # ------------------------------------------------------------------------------------------------------------
  thisp <- 'venn_diagram'
  thist <- as.integer(as.numeric(Sys.time()))
  output[['download_gg_tiff']] <- downloadHandler(
    filename = glue('{thisp}.{thist}.tiff'),
    content = function(file) {
      spsComps::shinyCatch({
        tiff(file, height = input[['HEIGHT']]/100, width = input[['WIDTH']]/100, res = 300, compression = 'lzw', units = 'in')
        print(plot.reactive(), newpage = FALSE)
        dev.off()
        message(glue('{Sys.time()} Download Plot as TIFF Format'))
      }, trace_back = FALSE)
    })
  
  # 下载图片 PDF
  output[['download_gg_pdf']] <- downloadHandler(
    filename = glue('{thisp}.{thist}.pdf'),
    content = function(file) {
      spsComps::shinyCatch({ 
        pdf(file, height = input[['HEIGHT']]/100, width = input[['WIDTH']]/100)
        print(plot.reactive(), newpage = FALSE)
        dev.off()
        message(glue('{Sys.time()} Download Plot as PDF Format'))
      }, trace_back = FALSE)
    }) 
  
  # 下载图片 SVG
  output[['download_gg_svg']] <- downloadHandler(
    filename = glue('{thisp}.{thist}.svg'),
    content = function(file) {
      spsComps::shinyCatch({ 
        svg(filename = file, height = input[['HEIGHT']]/100, width = input[['WIDTH']]/100)
        print(plot.reactive(), newpage = FALSE)
        dev.off()
        message(glue('{Sys.time()} Download Plot as SVG Format'))
      }, trace_back = FALSE)
    })
  
  
  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {
    '# Load your own data. Here use "data.table::fread" and "demo.tsv" for example.'
    data <- data.table::fread("E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/venn-diagram/www/data/demo.tsv")
    # data <- data.table::fread('path/to/demo.tsv')
    venn_dat <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% 
      dplyr::select(..(c(unlist(db.dragvars(), use.names = FALSE))))
    '# This application uses a variety of input data to draw and display a Venn diagram with up to 7 sets.'
    if (ncol(venn_dat) > 7) {
      venn_dat <- venn_dat[, 1:7]
    }
    '# Data processing, convert to list format'
    venn_list <- lapply(1:ncol(venn_dat), function(i) na.omit(unlist(venn_dat[, colnames(venn_dat)[i]])))
    names(venn_list) <- colnames(venn_dat)
    '# Set palette'
    # ------------------------------------------------------------------------------------------------------------
    zcolor <- ..(palette.())
    '# Draw venn diagram'
    venn(
      venn_list,
      zcolor = zcolor,
      opacity = ..(as.logical(input[['opacity']])),
      box = ..(as.logical(input[['box']])),
      ellipse = ..(as.logical(input[['ellipse']])),
      ilcs = ..(input[['ilcs']]),
      sncs = ..(input[['sncs']]),
      ilabels = TRUE
    )
  })
  
  # Show Plot Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(venn)
      }),
      spsComps::shinyCatch({ output[['workflow_code']]() }, trace_back = TRUE)
    )
    
    displayCodeModal(
      code = formatR::tidy_source(text = as.character(code), args.newline = TRUE, output = FALSE)$text.tidy,
      title = "Code to reproduce data and plot",
      clip = NULL,
      size = 'l'
    )
  })
  #+++++++++++++++++++++++++++++++  Show Code End ++++++++++++++++++++++++++++++
  
  # Show sessionInfo()
  observeEvent(input[['show_sessioninfo']], {
    displayCodeModal(
      code = as.character(xfun::session_info(c('glue', 'dplyr', 'venn'))),
      title = "Collect Information About the Current R Session",
      clip = NULL,
      size = 'l'
    )
  })
  
  # HELP
  observeEvent(input[['help']], {
    showModal(help.modal)
  })
  
  help.modal <- modalDialog(
    title = "How to use this application.",
    tags$h3(attention),
    HTML(glue('<span class="help-block m-b-none des-interactively2"><i class="fa fa-info-circle"></i> {interactively.b}</span>')),
    HTML(glue('<span class="help-block m-b-none des-interactively2"><i class="fa fa-info-circle"></i> {interactively.c}</span>')),
    HTML(glue('<span class="help-block m-b-none des-interactively2"><i class="fa fa-info-circle"></i> {interactively.d}</span>')),
    tags$hr(),
    tags$h3('Click here for a demo:'),
    tags$a(href = glue("data/demo.gif"), target="_blank", tags$img(src = glue("data/demo.gif"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$hr(),
    tags$h3('What is venn diagram?'),
    tags$p(style = 'text-align:left', "A Venn diagram may also be called a set diagram or logic diagram. It is a diagram that shows all possible logical relations between a finite collection of different sets. These diagrams depict elements as points in the plane, and sets as regions inside closed curves."),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$hr(),
    tags$h3('When to use venn diagram?'),
    tags$p(style = 'text-align:left', "A Venn diagram illustrates the relationships between two or more data sets. Venn diagrams are especially useful for highlighting similarities and differences and are commonly used to compare and contrast the characteristics of different data sets."),
    tags$p(style = 'text-align:left', "In a Venn diagram, circles are used to represent each data set. Inside each circle is a list of characteristics that define that data. Characteristics shared between two or more data sets are listed in the area where the circles overlap. Simple Venn diagrams consist of two overlapping circles, but complex Venn diagrams may compare up to five or more data sets using up to five or more circles."),
    tags$p(style = 'text-align:left', "Venn diagrams are commonly used in business and education settings to visualize and explore how things relate. In a business context, Venn diagrams are commonly used in product development, marketing, management, and more."),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)












