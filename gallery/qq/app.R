#' @title
#'
#' @description
#' 
# Load Global
source('global.R')
source('up.R')
source('src/i18n.R')

# load R packages
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(ggrepel)))
suppressMessages(suppressWarnings(library(ggplot2)))
options(shiny.maxRequestSize = 300 * 1024 ^ 2)


js <- HTML("
$(function() {
  $('.shiny-input-container .input-group-btn .btn').on('click', function() {
     const id = $(this).find('input[type=\"file\"]').attr('id');
     Shiny.setInputValue(id + '_click', Math.random());
  })    
})")

ui.gallery <- function() {
  
  www = 'www/data'
  
  fluidPage(
    titlePanel("Q-Q Plot"),
    useShinyjs(),
    web.statistic.baidu,
    web.statistic.google,
    tags$title('Q-Q Plot'),
    tags$head(
      includeCSS("www/css/shiny-style.css"),
      HTML("<html lang='en'>"),
      tags$link(rel = "shortcut icon", href = "img/favicon.ico"),
      tags$style(HTML("
      h2 {
        margin-left: 20px;
      }
      .btn-custom {
        font-size: 20px;
        margin: 10px;
        padding: 3px 30px;
        background-color: #007bff;
        color: white;
        border-radius: 5px;
        border: none;
      }
      .btn-custom:hover {
        background-color: #0056b3;
      }
      .loaderi {
        border: 16px solid #f3f3f3;
        border-top: 16px solid #3498db;
        border-radius: 50%;
        width: 120px;
        height: 120px;
        animation: spin 2s linear infinite;
        margin: 0 auto;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      .button-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        margin-top: 0px;
      }
    "))
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
             
             div(
               id = 'fileUploadButton',
               fileInput(inputId = "fileUpload", label = "Upload Data", accept = c(".csv", ".txt", ".tsv", ".csv.gz", ".txt.gz", ".tsv.gz"))
             ),
             
             uiOutput(outputId = 'is_add_import'),
             
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.2}</strong></div>")),
             tags$div(id = 'variable-des',
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>PVAL</span>, <span class='var-num'>one</span>. The p-value indicates the statistical significance of the observed effect.</span>")
             ),
             uiOutput(outputId = 'ui_aesthetics'),
             
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.3}</strong></div>")),
             uiOutput(outputId = 'is_add_button'),
             
      ),
      column(width = 7, 
             column(width = 12, uiOutput(outputId = 'modify_plot_params')),
             tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
             column(width = 12,
                    mainPanel(
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:630px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
                    )),
             
             column(width = 12, id = 'introbox-download',  uiOutput(outputId = 'intro_box')),
             
      )
    )
  )
}


server.gallery <- function(input, output, session) {
  
  www = 'www/data'
  i.data <- reactiveVal(NULL)
  s.data <- reactiveVal(NULL)
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste('GWAS.csv') },
    content <- function(file){
      file.copy(glue("{www}/demo.csv"), file)
    }, contentType = NULL)
  
  # 导入数据
  output[['is_add_import']] <- renderUI({
    req(input$fileUpload)
    fluidRow(
      div(class = "button-container", actionButton("btnImport", label = "Import", class = "btn-custom")),
    )
  })
  
  # 组件显示状态
  display.status <- reactive({
    !is.null(input[["dragvars"]]$target$PVAL)
  })
  
  # 点击绘图按钮是否展示
  output[['is_add_button']] <- renderUI({
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
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 650,  min = 100, max = 3000, step = 10)),
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
  
  # DEBOUNCE
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.P)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.P)
  db.point_size <- debounce(reactive({ input[['point_size']] }), DEBOUNCE.A)
  db.line_width <- debounce(reactive({ input[['line_width']] }), DEBOUNCE.A)
  db.font_size <- debounce(reactive({ input[['font_size']] }), DEBOUNCE.A)
  
  # 绘图部分
  plot.reactive <- function() {
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    i.data <- na.omit(s.data())
    
    # 数据整理
    # ------------------------------------------------------------------------------------------------------------
    if (as.logical(input[['full_data']])) {
      i.data <- i.data
    } else {
      if (as.integer(nrow(i.data) / 10) > 10000) {
        N = 10000
      } else {
        N = nrow(i.data)
      }
      set.seed(12306)
      try.c <- try({ i.data <- i.data %>% dplyr::sample_n(N) }, silent = QUITE)
      if(! "try-error" %in% class(try.c)) {
        i.data
      }
    }
    
    colnames(i.data) <- c('PVAL')
    i.data$PVAL <- as.numeric(i.data$PVAL)
    
    # 自定义函数
    # ------------------------------------------------------------------------------------------------------------
    QQ <- function(ps, ci = 0.95, shape = 2, size = 3, color = '#0000FF', title = '', ribbon = FALSE) {
      n  <- length(ps)
      DF <- data.frame(
        observed = -log10(sort(ps)),
        expected = -log10(ppoints(n)),
        clower   = -log10(qbeta(p = (1 - ci) / 2, shape1 = 1:n, shape2 = n:1)),
        cupper   = -log10(qbeta(p = (1 + ci) / 2, shape1 = 1:n, shape2 = n:1))
      )
      
      log10Pe = expression(Expected ~~ -log[10](P))
      log10Po = expression(Observed ~~ -log[10](P))
      
      gg <- ggplot(DF) +
        geom_point(aes(expected, observed), shape = shape, size = size, color = color) +
        geom_abline(intercept = 0, slope = 1, alpha = 0.5, color = input[['line_col']], linewidth = input[['line_width']]) +
        labs(x = log10Pe, y = log10Po, title = title)
      
      if (ribbon) { gg <- gg + geom_ribbon(mapping = aes(x = expected, ymin = clower, ymax = cupper), alpha = 0.1) }
      
      gg
    }
    
    # 开始画图
    # ------------------------------------------------------------------------------------------------------------
    gg <- QQ(ps = i.data$PVAL, shape = 20, size = db.point_size(), color = input[['point_col']], title = '', ribbon = TRUE) +
      theme_classic() + 
      theme(  
        axis.title = element_text(size = db.font_size()),
        axis.text = element_text(size = db.font_size()),
        axis.text.x = element_text(colour = '#000000'),
        axis.text.y = element_text(colour = '#000000'),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
      scale_x_continuous(expand = c(0,0), limits = c(0, NA))
    
    message(glue('{Sys.time()} Plot Success'))
    gg
  }
  
  # Upload Status
  IMDF <- reactive({
    req(input$fileUpload)
  })
  
  # 点击 Import 按钮
  observeEvent(input$btnImport, {
    
    shinyjs::hide('intro_box')
    shinyjs::hide('is_add_import')
    shinyjs::show('ui_aesthetics')
    
    if (is.null(IMDF())) {
      showModal(modalDialog(
        title = "Requirement",
        "The target data must be uploaded!",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    showModal(modalDialog(
      title = "Succeed",
      tags$div(
        style = "text-align: center",
        tags$p("Please wait while data is being processed."),
        tags$div(class = "loaderi")
      ),
      easyClose = FALSE,
      footer = NULL
    ))
    
    e.data <- tryCatch({
      data.table::fread(IMDF()$datapath)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to read the uploaded file. Please ensure it is a valid file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    })
    
    i.data(e.data)
    
    # Choose Variable
    output[['ui_aesthetics']] <- renderUI({
      aesthetics <- c('PVAL')
      data = as.data.frame(e.data[1:2, ])
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
          # selected = list(PVAL = 'PVAL'),
          badge = TRUE,
          copySource = FALSE,
          width = "100%",
          height = "70px",
          replace = TRUE
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
          replace = TRUE
        )
      }
    })
    
    shinyjs::show('is_add_button')
    removeModal()
  })
  
  
  # 同时支持自定义图片大小
  # ------------------------------------------------------------------------------------------------------------
  observeEvent(input[['plot_button']], {
    
    shinyjs::hide('intro_box')
    shinyjs::hide('variable-des')
    shinyjs::hide('introbox-download')
    shinyjs::show('io-albert-gg')
    shinyjs::show('modify_plot_params')
    
    e.data <- i.data() %>% dplyr::select(
      input[['dragvars']]$target$CHR,
      input[['dragvars']]$target$POS,
      input[['dragvars']]$target$PVAL
    )
    s.data(na.omit(e.data))
    
    shinyWidgets::updateSwitchInput(session = session, inputId = 'plot_interactively', value = FALSE)
    
    req(i.data(), cancelOutput = TRUE)
    req(db.point_size(), cancelOutput = TRUE)
    req(db.line_width(), cancelOutput = TRUE)
    req(db.font_size(), cancelOutput = TRUE)
    
    output[['io.albert.gg']] <- renderUI({
      plotOutput(outputId = 'io.albert.gg.o', height = db.height(), width = db.width()) %>% withSpinner(type = 7, color = '#EE3F4D', size = 1, color.background = '#2F2F35')
    })
    
    output[['io.albert.gg.o']] <- renderPlot({
      if (input[['plot_interactively']]) {
        spsComps::shinyCatch({ plot.reactive() }, trace_back = FALSE)
      } else {
        isolate({ spsComps::shinyCatch({ plot.reactive() }, trace_back = FALSE) })
      }
    }, res = 100)
    shinyjs::show('intro_box')
    shinyjs::show('variable-des')
    shinyjs::show('introbox-download')
  })
  
  observeEvent(input[['plot_refresh']], {
    try(dev.off(), silent = TRUE)
    shinyjs::refresh()
  })
  
  observeEvent(input[['full_data']], {
    if (as.logical(input[['full_data']])) {
      shinyWidgets::updateSwitchInput(session = session, inputId = "plot_interactively", value = FALSE)
    }
  })
  
  observeEvent(input[['plot_interactively']], {
    if (as.logical(input[['plot_interactively']])) {
      shinyWidgets::updateSwitchInput(session = session, inputId = "full_data", value = FALSE)
    }
  })
  
  observeEvent({
    input[['fileUpload']]
  }, {
    req(input$fileUpload)
    shinyjs::show('is_add_import')
    shinyjs::show('plot_button_null')
  })
  
  observeEvent({
    input[['fileUpload_click']]
  }, {
    shinyjs::show('variable-des')
    shinyjs::show('plot_button_null')
    shinyjs::hide('is_add_button')
    shinyjs::hide('ui_aesthetics')
    shinyjs::hide('is_add_import')
    shinyjs::hide('modify_plot_params')
    shinyjs::hide('intro_box')
    shinyjs::hide('io-albert-gg')
  })
  
  # 下载图片
  # 下载图片 TIFF
  # ------------------------------------------------------------------------------------------------------------
  thisp <- 'qq'
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
    '# Define a function for Q-Q plot'
    QQ <- function(ps, ci = 0.95, shape = 2, size = 3, color = '#0000FF', title = '', ribbon = FALSE) {
      n  <- length(ps)
      DF <- data.frame(
        observed = -log10(sort(ps)),
        expected = -log10(ppoints(n)),
        clower   = -log10(qbeta(p = (1 - ci) / 2, shape1 = 1:n, shape2 = n:1)),
        cupper   = -log10(qbeta(p = (1 + ci) / 2, shape1 = 1:n, shape2 = n:1))
      )
      
      log10Pe = expression(Expected ~~ -log[10](P))
      log10Po = expression(Observed ~~ -log[10](P))
      
      gg <- ggplot(DF) +
        geom_point(aes(expected, observed), shape = shape, size = size, color = color) +
        geom_abline(intercept = 0, slope = 1, alpha = 0.5, color = ..(input[['line_col']]), linewidth = ..(input[['line_width']])) +
        labs(x = log10Pe, y = log10Po, title = title)
      
      if (ribbon) { gg <- gg + geom_ribbon(mapping = aes(x = expected, ymin = clower, ymax = cupper), alpha = 0.1) }
      
      gg
    }
    '# Load your own data. Here use "data.table::fread" and "demo.csv" for example.'
    # i.data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/manhattan/www/data/demo.csv')
    i.data <- data.table::fread('path/to/demo.csv')
    i.data <- i.data %>%
      dplyr::select(..(input[['dragvars']]$target$CHR), ..(input[['dragvars']]$target$POS), ..(input[['dragvars']]$target$PVAL)) %>%
      na.omit()
    
    '# Data preparation'
    colnames(i.data) <- c('PVAL')
    i.data$PVAL <- as.numeric(i.data$PVAL)

    '# Draw Plot'
    QQ(ps = i.data$PVAL, shape = 20, size = ..(db.point_size()), color = ..(input[['point_col']]), title = '', ribbon = TRUE) +
      theme_classic() + 
      theme(  
        axis.title = element_text(size = ..(db.font_size())),
        axis.text = element_text(size = ..(db.font_size())),
        axis.text.x = element_text(colour = '#000000'),
        axis.text.y = element_text(colour = '#000000'),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
      scale_x_continuous(expand = c(0,0), limits = c(0, NA))
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
      code = as.character(xfun::session_info(c('glue', 'dplyr', 'ggplot2'))),
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
    tags$h3('What is a Q-Q plot?'),
    tags$p(style = 'text-align:left', "The QQ plot, or quantile-quantile plot, is a graphical tool to help us assess if a set of data plausibly came from some theoretical distribution such as a normal or exponential. For example, if we run a statistical analysis that assumes our residuals are normally distributed, we can use a normal QQ plot to check that assumption. It's just a visual check, not an air-tight proof, so it is somewhat subjective. But it allows us to see at-a-glance if our assumption is plausible, and if not, how the assumption is violated and what data points contribute to the violation."),
    tags$p(style = 'text-align:left', "A QQ plot is a scatterplot created by plotting two sets of quantiles against one another. If both sets of quantiles came from the same distribution, we should see the points forming a line that's roughly straight. Here's an example of a normal QQ plot when both sets of quantiles truly come from normal distributions."),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$h3('Advantages'),
    tags$p(style = 'text-align:left', "A QQ plot provides a powerful visual assessment, pinpointing deviations between distributions and identifying the data points responsible for them. When comparing a sample to a probability distribution, you’ll typically use this graph with a distribution test, such as a normality test, to verify statistical assumptions."),
    tags$p(style = 'text-align:left', "The most common use for a QQ plot is determining whether sample data follow a particular probability distribution."),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)












