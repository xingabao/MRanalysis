#' @title
#'
#' @description
#' 
# Load Global
source('global.R')
source('up.R')
source('src/i18n.R')

# load R packages
suppressMessages(suppressWarnings(library(ggrepel)))

options(shiny.maxRequestSize = 1 * 1024 ^ 2)

ui.gallery <- function() {
  
  www = 'www/data'
  forest_demo <<- read.table(glue('{www}/demo.txt'), header = TRUE, sep = '\t', stringsAsFactors = FALSE)
  
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
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>NAME</span>, <span class='var-num'>one</span>. This column lists the names or identifiers for the individual studies, genetic variants (e.g., SNPs), or methods involved in the analysis.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>PVAL</span>, <span class='var-num'>one</span>. The p-value indicates the statistical significance of the observed effect.</span>"),
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
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:540px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
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
    filename <- function(){ paste('mr_result.txt') },
    content <- function(file){
      file.copy(glue("{www}/demo.txt"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(
    id = "show_data", 
    choices = c('forest_demo'),
    return_class = "tbl_df",
    read_fns = list(
      tsv = function(file) { data.table::fread(file) },
      xls = function(file) { readxl::read_xls(file) },
      xlsx = function(file) { openxlsx::read.xlsx(file) }
    ))
  
  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    
    aesthetics <- c('NAME', 'PVAL', 'OR', 'OR_lci95', 'OR_upi95')
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
        # selected = list(NAME = 'method', PVAL = 'pval', OR = 'or', OR_lci95 = 'or_lci95', OR_upi95 = 'or_uci95'),
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
  
  # 组件显示状态
  display.status <- reactive({
    !is.null(input[["dragvars"]]$target$NAME) &&
      !is.null(input[["dragvars"]]$target$PVAL) &&
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
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 1000,  min = 100, max = 3000, step = 10)),
                 column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 480, min = 100, max = 3000, step = 10))
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
        column(width = 12, up.ui(width = 2, dat = i.data())), tags$br(), tags$br(),
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
  i.data <- reactive({
    try.c <- try({
      tmp <- imported$data()
      tmp %>% dplyr::select(
        input[['dragvars']]$target$NAME,
        input[['dragvars']]$target$PVAL,
        input[['dragvars']]$target$OR,
        input[['dragvars']]$target$OR_lci95,
        input[['dragvars']]$target$OR_upi95
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })

  # DEBOUNCE
  db.font_size <- debounce(reactive({ input[['font_size']] }), DEBOUNCE.A)
  db.digit <- debounce(reactive({ input[['digit']] }), DEBOUNCE.A)
  db.ratio <- debounce(reactive({ input[['ratio']] }), DEBOUNCE.A)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.P)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.P)
  db.vline <- debounce(reactive({ ifelse(test = is.na(input[['vline']]), yes = 1, no = input[['vline']]) }), DEBOUNCE.L)
  db.xlim_custom <- debounce(reactive({ input[['xlim_custom']] }), DEBOUNCE.A)
  db.lmethod <- debounce(reactive({ ifelse(test = is.na(input[['lmethod']]), yes = 0, no = input[['lmethod']]) }), DEBOUNCE.A)
  db.lpVal <- debounce(reactive({ ifelse(test = is.na(input[['lpVal']]), yes = 6, no = input[['lpVal']]) }), DEBOUNCE.A)
  db.lpValt <- debounce(reactive({ ifelse(test = is.na(input[['lpValt']]), yes = 6, no = input[['lpValt']]) }), DEBOUNCE.A)
  db.lort <- debounce(reactive({ ifelse(test = is.na(input[['lort']]), yes = 9, no = input[['lort']]) }), DEBOUNCE.A)
  db.lor <- debounce(reactive({ ifelse(test = is.na(input[['lor']]), yes = 10, no = input[['lor']]) }), DEBOUNCE.A)
  
  # 绘图部分
  plot.reactive <- function() {
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    i.data <- i.data()

    # 数据整理
    # ------------------------------------------------------------------------------------------------------------
    colnames(i.data) <- c('method', 'pval', 'or', 'or_lci95', 'or_uci95')
    
    # 自定义函数
    # ------------------------------------------------------------------------------------------------------------	
    forest.x <- function(dat, shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(5, 2), vline = 1, xrang = NULL, text.layout = c(0, 6, 6.8, 8.5, 10)) {
      method <- dat$method
      or <- sprintf(paste0('%.', digit, 'f'), dat$'or')
      orLow  <- sprintf(paste0('%.', digit, 'f'), dat$'or_lci95')
      orHigh <- sprintf(paste0('%.', digit, 'f'), dat$'or_uci95')
      OR <- paste0(or, ' (', orLow, '-', orHigh, ')')
      pVal <- ifelse(dat$pval < 0.001, ' < 0.001', sprintf(paste0('%.', digit, 'f'), dat$pval))
      
      n <- nrow(dat)
      nRow <- n + 1
      ylim <- c(1, nRow)
      layout(matrix(c(1, 2), nc = 2), width = width.ratio)
      
      xlim <- c(0, 10)
      par(mar = c(4, 2.5, 2, 1))
      plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, xlab = '', ylab = '')
      text(text.layout[1], n:1, method, adj = 0, cex = text.cex)
      text(text.layout[2], n:1, pVal, adj = 1, cex = text.cex)
      text(text.layout[3], n + 1, 'P-value', cex = text.cex, font = 2, adj = 1)
      text(text.layout[4], n + 1, 'OR', cex = text.cex, font = 2, adj = 1)
      text(text.layout[5], n:1, OR, adj = 1, cex = text.cex)
      
      par(mar = c(4, 1, 2, 1), mgp = c(2, 0.5, 0), cex.lab = text.cex, cex.axis = text.cex)
      if (is.null(xrang)) {
        xlim = c(min(as.numeric(orLow)*0.75, as.numeric(orHigh)*0.75, 0.5), max(as.numeric(orLow) * 1.25, as.numeric(orHigh) * 1.25, 1))
      } else {
        xlim = xrang
      }
      if (xlim[1] > vline) { xlim[1] = vline }
      if (xlim[2] < vline) { xlim[2] = vline }
      plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, ylab = '', xaxs = 'i', xlab = 'OR')
      arrows(as.numeric(orLow), n:1, as.numeric(orHigh), n:1, angle = 90, code = 3, length = 0.05, col = line.col, lwd = 3)
      abline(v = vline, col = 'black', lty = 2, lwd = 2)
      boxcolor = ifelse(as.numeric(or) > 1, shape.col, shape.col)
      points(as.numeric(or), n:1, pch = 15, col = boxcolor, cex = 2)
      axis(1)
    }
    
    message(glue('{Sys.time()} Plot Success'))
    
    # 森林图
    # ------------------------------------------------------------------------------------------------------------
    if (as.logical(input[['xlim_mode']])) {
      xrang = NULL
    } else {
      xrang = db.xlim_custom()
    }
    forest.x(
      dat = i.data, 
      shape.col = input[['shape_col']], 
      line.col = input[['line_col']], 
      text.cex = db.font_size(), 
      digit = db.digit(), 
      width.ratio = c(1, 1 / db.ratio()), 
      vline = db.vline(), 
      xrang = xrang, 
      text.layout = c(db.lmethod(), db.lpVal(), db.lpValt(), db.lort(), db.lor())
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
      
      req(db.digit(), cancelOutput = TRUE)
      req(db.font_size(), cancelOutput = TRUE)
      req(db.ratio(), cancelOutput = TRUE)
      req(db.vline(), cancelOutput = TRUE)
      req(db.xlim_custom(), cancelOutput = TRUE)
      req(db.lmethod(), cancelOutput = TRUE)
      req(db.lpVal(), cancelOutput = TRUE)
      req(db.lpValt(), cancelOutput = TRUE)
      req(db.lort(), cancelOutput = TRUE)
      req(db.lor(), cancelOutput = TRUE)

      shinyjs::show('io-albert-gg')
      shinyjs::show('introbox-download')
      
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
    })

    observeEvent(input[['plot_refresh']], {
      try(dev.off(), silent = TRUE)
      shinyjs::refresh()
    })
  })
  
  # 下载图片
  # 下载图片 TIFF
  # ------------------------------------------------------------------------------------------------------------
  thisp <- 'forest'
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
    forest.x <- function(dat, shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(5, 2), vline = 1, xrang = NULL, text.layout = c(0, 6, 6.8, 8.5, 10)) {
      method <- dat$method
      or <- sprintf(paste0('%.', digit, 'f'), dat$'or')
      orLow  <- sprintf(paste0('%.', digit, 'f'), dat$'or_lci95')
      orHigh <- sprintf(paste0('%.', digit, 'f'), dat$'or_uci95')
      OR <- paste0(or, ' (', orLow, '-', orHigh, ')')
      pVal <- ifelse(dat$pval < 0.001, ' < 0.001', sprintf(paste0('%.', digit, 'f'), dat$pval))
      
      n <- nrow(dat)
      nRow <- n + 1
      ylim <- c(1, nRow)
      layout(matrix(c(1, 2), nc = 2), width = width.ratio)
      
      xlim <- c(0, 10)
      par(mar = c(4, 2.5, 2, 1))
      plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, xlab = '', ylab = '')
      text(text.layout[1], n:1, method, adj = 0, cex = text.cex)
      text(text.layout[2], n:1, pVal, adj = 1, cex = text.cex)
      text(text.layout[3], n + 1, 'P-value', cex = text.cex, font = 2, adj = 1)
      text(text.layout[4], n + 1, 'OR', cex = text.cex, font = 2, adj = 1)
      text(text.layout[5], n:1, OR, adj = 1, cex = text.cex)
      
      par(mar = c(4, 1, 2, 1), mgp = c(2, 0.5, 0), cex.lab = text.cex, cex.axis = text.cex)
      if (is.null(xrang)) {
        xlim = c(min(as.numeric(orLow)*0.75, as.numeric(orHigh)*0.75, 0.5), max(as.numeric(orLow) * 1.25, as.numeric(orHigh) * 1.25, 1))
      } else {
        xlim = xrang
      }
      if (xlim[1] > vline) { xlim[1] = vline }
      if (xlim[2] < vline) { xlim[2] = vline }
      plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, ylab = '', xaxs = 'i', xlab = 'OR')
      arrows(as.numeric(orLow), n:1, as.numeric(orHigh), n:1, angle = 90, code = 3, length = 0.05, col = line.col, lwd = 3)
      abline(v = vline, col = 'black', lty = 2, lwd = 2)
      boxcolor = ifelse(as.numeric(or) > 1, shape.col, shape.col)
      points(as.numeric(or), n:1, pch = 15, col = boxcolor, cex = 2)
      axis(1)
    }
    '# Load your own data. Here use "data.table::fread" and "demo.txt" for example.'
    # data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/forest/www/data/demo.txt')
    data <- data.table::fread('path/to/demo.txt')
    '# Prepare plotting data'
    i.data <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% dplyr::select(
      ..(input[['dragvars']]$target$NAME),
      ..(input[['dragvars']]$target$PVAL),
      ..(input[['dragvars']]$target$OR),
      ..(input[['dragvars']]$target$OR_lci95),
      ..(input[['dragvars']]$target$OR_upi95)
    )
    colnames(i.data) <- c('method', 'pval', 'or', 'or_lci95', 'or_uci95')
    '# Draw Plot'
    forest.x(
      dat = i.data, 
      shape.col = ..(input[['shape_col']]), 
      line.col = ..(input[['line_col']]), 
      text.cex = ..(db.font_size()), 
      digit = ..(db.digit()), 
      width.ratio = c(1, 1 / ..(db.ratio())), 
      vline = ..(db.vline()), 
      xrang = ..(if (as.logical(input[['xlim_mode']])) { NULL } else { db.xlim_custom() }), 
      text.layout = ..(c(db.lmethod(), db.lpVal(), db.lpValt(), db.lort(), db.lor()))
    )
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












