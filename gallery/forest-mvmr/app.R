#' @title
#'
#' @description
#' 
# Load Global
source('global.R')
source('up.R')
source('left.R')
source('src/i18n.R')

# load R packages
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(forcats)))

options(shiny.maxRequestSize = 1 * 1024 ^ 2)

ui.gallery <- function() {
  
  www = 'www/data'
  forest_demo <<- data.table::fread(glue('{www}/demo.csv'))
  
  fluidPage(
    useShinyjs(),
    web.statistic.baidu,
    web.statistic.google,
    tags$head(
      includeCSS("www/css/shiny-style.css"),
      HTML("<html lang='en'>"),
      tags$title('Forest Plot'),
      tags$link(rel = "shortcut icon", href = "img/favicon.ico"),
    ),
    tags$style(
      HTML(".col-sm-1 { width:3.5%; !important }")
    ),
    wellPanel(
      column(width = 1, uiOutput(outputId = "left_edit")),
      column(width = 4,
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
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Exposure</span>, <span class='var-num'>one</span>. The risk factor (exposure) being studied.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Method</span>, <span class='var-num'>one</span>. List of MR methods.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>OR</span>, <span class='var-num'>one</span>. The odds ratio (OR) is a measure of the association between an exposure and an outcome.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>OR_lci95</span>, <span class='var-num'>one</span>. This is the lower bound of the 95% confidence interval for the odds ratio.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>OR_upi95</span>, <span class='var-num'>one</span>. This is the upper bound of the 95% confidence interval for the odds ratio.</span>")
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
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:720px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
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
    filename <- function(){ paste('mvmr_result.csv') },
    content <- function(file){
      file.copy(glue("{www}/demo.csv"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(
    id = "show_data", 
    choices = c('forest_demo'), 
    return_class = "tbl_df",
    read_fns = list(tsv = function(file) { 
      data.table::fread(fread) 
    }))
  
  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    aesthetics <- c('Exposure', 'Method', 'OR', 'OR_lci95', 'OR_upi95')
    data = imported$data()
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, "sf_column"))
      dragulaInput(
        inputId = "dragvars",
        sourceLabel = "Variables",
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        # selected = list(
        #   Exposure = find.first(var_choices, c('exposure')),
        #   Method = find.first(var_choices, c('method')),
        #   OR = find.first(var_choices, c('or')),
        #   OR_lci95 = find.first(var_choices, c('or_lci95')),
        #   OR_upi95 = find.first(var_choices, c('or_uci95'))
        # ),
        badge = TRUE,
        copySource = FALSE,
        width = "100%",
        height = "70px",
        replace = TRUE,
        ncolGrid = 3
      )
    } else {
      dragulaInput(
        inputId = "dragvars",
        sourceLabel = "Variables",
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = "",
        badge = TRUE,
        copySource = FALSE,
        width = "100%",
        height = "70px",
        replace = TRUE,
        ncolGrid = 3
      )
    }
  })
  
  # 组件显示状态
  display.status <- reactive({
    !is.null(input[["dragvars"]]$target$Exposure) &&
      !is.null(input[["dragvars"]]$target$Method) &&
      !is.null(input[["dragvars"]]$target$OR) &&
      !is.null(input[["dragvars"]]$target$OR_lci95) &&
      !is.null(input[["dragvars"]]$target$OR_upi95)
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
                 column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 600, min = 100, max = 3000, step = 10))
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
        column(width = 12, up.ui(dat = i.data(), lc95 = lc95(), up95 = up95(), width = 2)), tags$br(), tags$br(),
      )
    } else {
      shinyjs::hide('io-albert-gg')
    }
  })

  # left ui
  output[['left_edit']] <- renderUI({
    if (display.status()) {
      tags$div(
        tags$style(
          HTML(".sw-dropdown { margin-bottom:10px }")
        ),
        tags$br(),
        tags$br(),
        left.ui.pattle(),
        left.ui.layout(),
      )
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
  
  lc95 <- reactive({ input[['dragvars']]$target$OR_lci95 })
  up95 <- reactive({ input[['dragvars']]$target$OR_upi95 })
  
  # 获取各个值
  i.data <- reactive({
    try.c <- try({
      tmp <- imported$data()
      tmp %>% dplyr::select(
        input[['dragvars']]$target$Exposure,
        input[['dragvars']]$target$Method,
        input[['dragvars']]$target$OR,
        input[['dragvars']]$target$OR_lci95,
        input[['dragvars']]$target$OR_upi95
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  # DEBOUNCE
  db.strip_text <- debounce(reactive({ input[['strip_text']] * 28 }), DEBOUNCE.0)
  db.axis_size <- debounce(reactive({ input[['axis_size']] * 28 }), DEBOUNCE.0)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.A)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.A)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    # i.data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/forest-mvmr/www/data/demo.csv')
    i.data <- i.data()
    # i.data <- i.data[, c('exposure', 'method', 'or', 'or_lci95', 'or_uci95')]
    colnames(i.data) <- c('Exposure', 'Method', 'OR', 'OR_lci95', 'OR_upi95')
    
    # 接收参数
    # ------------------------------------------------------------------------------------------------------------
    min.x <- input[['xlim']][1]
    max.x <- input[['xlim']][2]
    palette. <- c.palette(input[['palette']])
    palette. <- rep(palette., 5)
    
    forest.mv <- function(dat) {
      gg = ggplot(data = dat, aes(x = Exposure, y = OR, ymin = OR_lci95, ymax = OR_upi95, colour = Exposure))
      gg = gg + geom_pointrange()
      gg = gg + geom_hline(yintercept = input[['vline']], lty = 2)
      gg = gg + facet_wrap(~Method, strip.position = "top", ncol = 1, scales = "free_y")
      gg = gg + coord_flip()
      gg = gg + ylab("")
      gg = gg + theme_bw()
      gg = gg + theme(legend.position = "null", axis.title.y = element_blank())
      gg = gg + scale_color_manual(values = palette.)
      gg = gg + theme(
        axis.text = element_text(size = db.axis_size(), color = '#000000'),
        strip.text = element_text(size = db.strip_text(), color = input[['strip_col']]),
        panel.grid = if (input[['panel.grid']]) { element_line(linewidth = .5, color = "lightgrey", linetype = "dotted") } else { element_blank() }
      )
      gg + ylim(min.x, max.x)
    }
    gg <- forest.mv(
      dat = i.data %>%
        na.omit()
    )
    message(glue('{Sys.time()} Plot Success'))
    gg
  }
  
  # 同时支持自定义图片大小
  # ------------------------------------------------------------------------------------------------------------
  observeEvent(input[['show_data-confirm']], {
    
    shinyjs::hide('io-albert-gg')
    shinyjs::hide('introbox-download')
    shinyjs::hide('variable-des')
    shinyWidgets::updateSwitchInput(session = session, inputId = 'plot_interactively', value = FALSE)
    
    observeEvent(input[['plot_button']], {
      
      # req(db.axis_title(), cancelOutput = TRUE)
      req(db.axis_size (), cancelOutput = TRUE)
      # req(db.count(), cancelOutput = TRUE)
      req(db.strip_text(), cancelOutput = TRUE)
      
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
  thisp <- 'forest_plot'
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
    '# Define a function for Forest Plot'
    forest.mv <- function(dat) {
      gg = ggplot(data = dat, aes(x = Exposure, y = OR, ymin = OR_lci95, ymax = OR_upi95, colour = Exposure))
      gg = gg + geom_pointrange()
      gg = gg + geom_hline(yintercept = ..(input[['vline']]), lty = 2)
      gg = gg + facet_wrap(~Method, strip.position = "top", ncol = 1, scales = "free_y")
      gg = gg + coord_flip()
      gg = gg + ylab("")
      gg = gg + theme_bw()
      gg = gg + theme(legend.position = "null", axis.title.y = element_blank())
      gg = gg + scale_color_manual(values = palette.)
      gg = gg + theme(
        axis.text = element_text(size = ..(db.axis_size()), color = '#000000'),
        strip.text = element_text(size = ..(db.strip_text()), color = ..(input[['strip_col']])),
        panel.grid = ..(if (input[['panel.grid']]) { quote(element_line(linewidth = .5, color = "lightgrey", linetype = "dotted")) } else { quote(element_blank()) })
      )
      gg + ylim(min.x, max.x)
    }
    '# Load your own data. Here use "data.table::fread" and "demo.csv" for example.'
    # data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/forest-mvmr/www/data/demo.csv')
    data <- data.table::fread('path/to/demo.csv')
    '# Prepare plotting data'
    i.data <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% dplyr::select(
      ..(input[['dragvars']]$target$Exposure),
      ..(input[['dragvars']]$target$Method),
      ..(input[['dragvars']]$target$OR),
      ..(input[['dragvars']]$target$OR_lci95),
      ..(input[['dragvars']]$target$OR_upi95)
    )
    colnames(i.data) <- c('Exposure', 'Method', 'OR', 'OR_lci95', 'OR_upi95')
    '# Set parameters'
    min.x <- ..(input[['xlim']][1])
    max.x <- ..(input[['xlim']][2])
    palette. <- ..(c.palette(input[['palette']]))
    palette. <- rep(palette., 5)
    '# Draw Plot'
    gg <- forest.mv(dat = i.data %>% na.omit())
    gg
  })
  
  # Show Plot Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(ggplot2)
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
      code = as.character(xfun::session_info(c('openxlsx', 'dplyr', 'glue', 'stringr', 'ggplot2', 'ggtext'))),
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
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "60%", height = "auto", alt = "ZOOM IN")),
    tags$hr(),
    tags$h3('What is a forest plot?'),
    tags$p(style = 'text-align:left', "Forest plots visually demonstrate the overall relationship studies have to each other in an meta-analysis and illustrate the consistency, precision, and heterogeneity of studies' individual results and the collective results of all trials included in a meta-analysis."),
    tags$p(style = 'text-align:left', "A forest plot is a graphical representation of a meta-analysis that visualizes the association of all studies included in the meta-analysis in relation to each other and demonstrates the pooled effect estimate and heterogeneity of all results."),
    tags$h3('The parts of a forest plot'),
    tags$a(href = glue("data/forestplot.avif"), target="_blank", tags$img(src = glue("data/forestplot.avif"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$h3('Advantages'),
    tags$p(style = 'text-align:left', "Forest plots are useful tools for showing the main features of meta-analysis results and comprehensively visualizing the direction and magnitude of the overall effect as well as the results and potential heterogeneity of individual studies."),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)