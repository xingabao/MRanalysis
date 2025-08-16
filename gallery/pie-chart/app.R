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
suppressMessages(suppressWarnings(library(ggrepel)))

options(shiny.maxRequestSize = 1 * 1024 ^ 2)

ui.gallery <- function() {
  
  www = 'www/data'
  pie_chart_demo <<- read.csv(glue('{www}/demo.csv'))
  
  fluidPage(
    useShinyjs(),
    web.statistic.baidu,
    web.statistic.google,
    tags$head(
      includeCSS("www/css/shiny-style.css"),
      HTML("<html lang='en'>"),
      tags$title('Pie chart'),
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
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>labels</span>, <span class='var-num'>one</span>. Names of each category.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>n</span>, <span class='var-num'>one</span>. Specific values for each category.</span>"),
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
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:620px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
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
    filename <- function(){ paste('demo.csv') },
    content <- function(file){
      file.copy(glue("{www}/demo.csv"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(id = "show_data", choices = c('pie_chart_demo'), return_class = "tbl_df", read_fns = list(tsv = function(file) { data.table::fread(fread) }))
  
  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    
    aesthetics <- c('labels', 'n')
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
        # selected = list(labels = find.first(var_choices, c('pathway', 'name')), n = find.first(var_choices, c('Count'))),
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
    !is.null(input[["dragvars"]]$target$labels) &&
      !is.null(input[["dragvars"]]$target$n)
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
        column(width = 12, up.ui(width = 2)), tags$br(), tags$br(),
      )
    } else {
      shinyjs::hide('io-albert-gg')
    }
  })
  
  # left ui
  base_size = 11
  theme.default <- theme_default(base_size = base_size)
  if (TRUE) {
    theme.default$complete = FALSE
    theme.default$legend.title = element_text(family = NA, face = 'plain', colour = '#000000', size = 12, hjust = 0, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = 'pt'))
    theme.default$legend.text = element_text(family = NA, face = 'plain', colour = '#000000', size = 10, hjust = 0, vjust = 0.5, angle = 0.5, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'))
    theme.default$legend.position = "right"
    theme.default$legend.direction = 'vertical'
    theme.default$legend.justification = "center"
    theme.default$legend.key.height = grid::unit(18, units = 'pt')
    theme.default$legend.key.width = grid::unit(18, units = 'pt')
  }
  output[['left_edit']] <- renderUI({
    if (display.status()) {
      tags$div(
        tags$style(
          HTML(".sw-dropdown { margin-bottom:10px }")
        ),
        tags$br(),
        tags$br(),
        left.ui(id = id),
        left.ui.legend(theme.default = theme.default, id = id, l.title = input[['dragvars']]$target$labels),
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
  
  # 获取各个值
  i.data <- reactive({
    try.c <- try({
      tmp <- imported$data()
      tmp %>% dplyr::select(
        input[['dragvars']]$target$labels,
        input[['dragvars']]$target$n
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  # DEBOUNCE
  db.label_position <- debounce(reactive({ input[['label_position']] }), DEBOUNCE.L)
  db.label_radius <- debounce(reactive({ input[['label_radius']] }), DEBOUNCE.L)
  db.label_padding <- debounce(reactive({ input[['label_padding']] }), DEBOUNCE.L)
  db.label_size <- debounce(reactive({ input[['label_size']] * 10 }), DEBOUNCE.L)
  db.fill_title <- debounce(reactive({ input[['legend_title']] }), DEBOUNCE.M)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.P)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.P)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    i.data <- i.data()
    
    # 数据整理
    # ------------------------------------------------------------------------------------------------------------
    colnames(i.data) <- c('Group', 'Count')
    DF.PLOT <- i.data %>% 
      mutate(perc = `Count` / sum(`Count`)) %>% 
      mutate(label = scales::percent(perc))
    
    if (input[['arrange']]) { DF.PLOT <- DF.PLOT %>% arrange(perc) }
    
    DF.PLOT$Group <- factor(DF.PLOT$Group, levels = rev(DF.PLOT$Group))
    DF.PLOT <- DF.PLOT %>% mutate(text_y = cumsum(`Count`) - `Count`/2)
    
    # draw plot
    # ------------------------------------------------------------------------------------------------------------
    my.palette <- c(c.palette(input[['palette']], n = length(unique(i.data[['Group']]))), a.palettes)[1:length(unique(i.data$Group))]
    palette.(my.palette)
    gg <- ggplot(DF.PLOT, aes(x = "", y = Count, fill = Group)) +
      geom_col()
    if (input[['text_or_label']]) {
      gg <- gg + ggrepel::geom_label_repel(
        aes(label = label, y = text_y),
        color = input[['label_color']], 
        fill = input[['label_fill']],
        size = db.label_size(),
        nudge_x = db.label_position(),
        nudge_y = 0.6,
        show.legend = FALSE,
        label.r = grid::unit(db.label_radius(), units = 'line'),
        label.padding = grid::unit(db.label_padding(), units = 'line'),
        segment.color = ifelse(test = input[['label_line']], yes = '#000000', no = NA)
      )
    } else {
      gg <- gg + ggrepel::geom_text_repel(
        aes(label = label, y = text_y), 
        color = input[['label_color']], 
        size = db.label_size(),
        nudge_x = db.label_position(),
        nudge_y = 0.6,
        show.legend = FALSE,
        segment.color = ifelse(test = input[['label_line']], yes = '#000000', no = NA)
      )
    }
    gg <- gg + guides(fill = guide_legend(title = eval(parse(text = (ifelse(test = is.null(db.fill_title()) || db.fill_title() == '', yes = "input[['dragvars']]$target$labels", no = "db.fill_title()")))))) +
      scale_fill_manual(values = my.palette) +
      coord_polar(theta = "y") +
      theme_void()
    
    gg <- gg + theme(
      legend.title = element_text(family = c.family(input[['legend_title_family']]), face = input[['legend_title_face']], size = input[['legend_title_size']], colour = input[['legend_title_colour']], hjust = input[['legend_title_hjust']], vjust = input[['legend_title_vjust']], angle = input[['legend_title_angle']], lineheight = input[['legend_title_lineheight']], margin = margin(t = input[['legend_title_margin_top']], r = input[['legend_title_margin_right']], b = input[['legend_title_margin_bottom']], l = input[['legend_title_margin_left']], unit = "pt")),
      legend.text = element_text(family = c.family(input[['legend_text_family']]), face = input[['legend_text_face']], size = input[['legend_text_size']], colour = input[['legend_text_colour']], hjust = input[['legend_text_hjust']], vjust = input[['legend_text_vjust']], angle = input[['legend_text_angle']], lineheight = input[['legend_text_lineheight']], margin = margin(t = input[['legend_text_margin_top']], r = input[['legend_text_margin_right']], b = input[['legend_text_margin_bottom']], l = input[['legend_text_margin_left']], unit = "pt")),
      legend.direction = input[['legend_direction']],
      legend.position = text.x.y(input[['legend_position']], input[['legend_position_x']], input[['legend_position_y']], 'position'),
      legend.justification = text.x.y(input[['legend_justification']], input[['legend_justification_x']], input[['legend_justification_y']], 'justification'),
      legend.key.height = grid::unit(input[['legend_key_height']], "points"),
      legend.key.width = grid::unit(input[['legend_key_width']], "points"),
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
      
      req(db.label_position(), cancelOutput = TRUE)
      req(db.label_radius(), cancelOutput = TRUE)
      req(db.label_padding(), cancelOutput = TRUE)
      req(db.label_size(), cancelOutput = TRUE)
      
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
  thisp <- 'pie_chart'
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
    '# Load your own data. Here use "data.table::fread" and "demo.csv" for example.'
    # data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/pie-chart/www/data/demo.csv')
    data <- data.table::fread('path/to/demo.csv')
    '# Prepare plotting data'
    i.data <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% dplyr::select(
      ..(input[['dragvars']]$target$labels),
      ..(input[['dragvars']]$target$n)
    )
    '# Data preparation'
    colnames(i.data) <- c('Group', 'Count')
    DF.PLOT <- i.data %>% 
      mutate(perc = `Count` / sum(`Count`)) %>% 
      mutate(label = scales::percent(perc))
    if (..(input[['arrange']])) { DF.PLOT <- DF.PLOT %>% arrange(perc) }
    DF.PLOT$Group <- factor(DF.PLOT$Group, levels = rev(DF.PLOT$Group))
    DF.PLOT <- DF.PLOT %>% mutate(text_y = cumsum(`Count`) - `Count`/2)
    '# Set palette'
    my.palette <- ..(palette.())
    '# Draw plot'
    gg <- ggplot(DF.PLOT, aes(x = "", y = Count, fill = Group)) +
      geom_col()
    if (..(input[['text_or_label']])) {
      gg <- gg + ggrepel::geom_label_repel(
        aes(label = label, y = text_y),
        color = ..(input[['label_color']]),
        fill = ..(input[['label_fill']]),
        size = ..(db.label_size()),
        nudge_x = ..(db.label_position()),
        nudge_y = 0.6,
        show.legend = FALSE,
        label.r = grid::unit(..(db.label_radius()), units = 'line'),
        label.padding = grid::unit(..(db.label_padding()), units = 'line'),
        segment.color = ..(ifelse(test = input[['label_line']], yes = '#000000', no = NA))
      )
    } else {
      gg <- gg + ggrepel::geom_text_repel(
        aes(label = label, y = text_y),
        color = ..(input[['label_color']]),
        size = ..(db.label_size()),
        nudge_x = ..(db.label_position()),
        nudge_y = 0.6,
        show.legend = FALSE,
        segment.color = ..(ifelse(test = input[['label_line']], yes = '#000000', no = NA))
      )
    }
    gg <- gg + guides(fill = guide_legend(title = ..(eval(parse(text = (ifelse(test = is.null(db.fill_title()) || db.fill_title() == '', yes = "input[['dragvars']]$target$labels", no = "db.fill_title()"))))))) +
      scale_fill_manual(values = my.palette) +
      coord_polar(theta = "y") +
      theme_void()
    
    gg <- gg + theme(
      legend.title = element_text(family = ..(c.family(input[['legend_title_family']])), face = ..(input[['legend_title_face']]), size = ..(input[['legend_title_size']]), colour = ..(input[['legend_title_colour']]), hjust = ..(input[['legend_title_hjust']]), vjust = ..(input[['legend_title_vjust']]), angle = ..(input[['legend_title_angle']]), lineheight = ..(input[['legend_title_lineheight']]), margin = margin(t = ..(input[['legend_title_margin_top']]), r = ..(input[['legend_title_margin_right']]), b = ..(input[['legend_title_margin_bottom']]), l = ..(input[['legend_title_margin_left']]), unit = "pt")),
      legend.text = element_text(family = ..(c.family(input[['legend_text_family']])), face = ..(input[['legend_text_face']]), size = ..(input[['legend_text_size']]), colour = ..(input[['legend_text_colour']]), hjust = ..(input[['legend_text_hjust']]), vjust = ..(input[['legend_text_vjust']]), angle = ..(input[['legend_text_angle']]), lineheight = ..(input[['legend_text_lineheight']]), margin = margin(t = ..(input[['legend_text_margin_top']]), r = ..(input[['legend_text_margin_right']]), b = ..(input[['legend_text_margin_bottom']]), l = ..(input[['legend_text_margin_left']]), unit = "pt")),
      legend.direction = ..(input[['legend_direction']]),
      legend.position = ..(text.x.y(input[['legend_position']], input[['legend_position_x']], input[['legend_position_y']], 'position')),
      legend.justification = ..(text.x.y(input[['legend_justification']], input[['legend_justification_x']], input[['legend_justification_y']], 'justification')),
      legend.key.height = grid::unit(..(input[['legend_key_height']]), "points"),
      legend.key.width = grid::unit(..(input[['legend_key_width']]), "points"),
    )
    
    gg
  })
  
  # Show Plot Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(data.table)
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
    tags$h3('What is a Pie Chart?'),
    tags$p(style = 'text-align:left', "A pie chart is a type of graphical representation used to illustrate the proportions of different categories in a dataset as slices of a circle. Each slice's size (angle and area) represents the proportion or percentage that the category contributes to the whole."),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$h3('When you should use a pie chart?'),
    tags$p(style = 'text-align:left', "Pie charts have a fairly narrow use-case that is encapsulated particularly well by its definition. In order to use a pie chart, you must have some kind of whole amount that is divided into a number of distinct parts. Your primary objective in a pie chart should be to compare each group’s contribution to the whole, as opposed to comparing groups to each other. If the above points are not satisfied, the pie chart is not appropriate, and a different plot type should be used instead."),
    tags$p(style = 'text-align:left', tags$strong("Showing Composition"), ": Pie charts are best used when you need to show how individual parts contribute to an overall total. They are commonly used to display the breakdown of populations, budget allocations, business market shares, and survey results."),
    tags$p(style = 'text-align:left', tags$strong("Limited Categories"), ": They are most effective when the number of categories is small (usually less than seven). Too many slices can make the chart hard to read and interpret."),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)












