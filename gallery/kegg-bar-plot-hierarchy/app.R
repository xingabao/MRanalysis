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
  kegg_demo <<- openxlsx::read.xlsx(glue('{www}/KEGG.xlsx'))
  
  fluidPage(
    useShinyjs(),
    web.statistic.baidu,
    web.statistic.google,
    tags$head(
      includeCSS("www/css/shiny-style.css"),
      HTML("<html lang='en'>"),
      tags$title('Bar Plot'),
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
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>ID</span>, <span class='var-num'>one</span>. The ID column in the KEGG enrichment results refers to the unique identifier of the KEGG pathway. These IDs are standardized KEGG pathway identifiers, usually in the form of \"hsaXXXXX\" for human pathways, where XXXXX is a numeric code representing a specific pathway.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Description</span>, <span class='var-num'>one</span>. The Description column provides the name of the pathway associated with the ID.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>pval</span>, <span class='var-num'>one</span>. The adjusted p-value, which accounts for multiple testing (e.g., using the Benjamini-Hochberg method).</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Count</span>, <span class='var-num'>one</span>. The number of genes associated with the function in the input set.</span>")
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
  hierarchy <- openxlsx::read.xlsx(glue('{www}/kegg_hierarchy.xlsx'))
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste('demo.zip') },
    content <- function(file){
      file.copy(glue("{www}/demo.zip"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(id = "show_data", choices = c('kegg_demo'), return_class = "tbl_df", read_fns = list(tsv = function(file) { data.table::fread(fread) }))
  
  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    
    aesthetics <- c('ID', 'Description', 'pval', 'Count')
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
        #   pval = find.first(var_choices, c('p', 'adj')), 
        #   Description = find.first(var_choices, c('Description')),
        #   Count = find.first(var_choices, c('Count')),
        #   ID = find.first(var_choices, c('ID'))
        # ),
        badge = TRUE,
        copySource = FALSE,
        width = "100%",
        height = "70px",
        replace = TRUE,
        ncolGrid = 4
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
        ncolGrid = 4
      )
    }
  })
  
  # 组件显示状态
  display.status <- reactive({
    !is.null(input[["dragvars"]]$target$ID) &&
      !is.null(input[["dragvars"]]$target$Description) &&
      !is.null(input[["dragvars"]]$target$pval) &&
      !is.null(input[["dragvars"]]$target$Count)
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
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 1080,  min = 100, max = 3000, step = 10)),
                 column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 720, min = 100, max = 3000, step = 10))
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
  
  # 获取各个值
  i.data <- reactive({
    try.c <- try({
      tmp <- imported$data()
      tmp %>% dplyr::select(
        input[['dragvars']]$target$ID,
        input[['dragvars']]$target$Description,
        input[['dragvars']]$target$pval,
        input[['dragvars']]$target$Count
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  # DEBOUNCE
  db.strip_text <- debounce(reactive({ input[['strip_text']] * 32 }), DEBOUNCE.0)
  db.axis_title <- debounce(reactive({ input[['axis_title']] * 32 }), DEBOUNCE.0)
  db.axis_size <- debounce(reactive({ input[['axis_size']] * 32 }), DEBOUNCE.0)
  db.count <- debounce(reactive({ input[['count']] * 9 }), DEBOUNCE.0)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.A)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.A)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    # KEGG <- openxlsx::read.xlsx(xlsxFile = 'E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/kegg-bar-plot-hierarchy/www/data/KEGG.xlsx')
    # hierarchy <- openxlsx::read.xlsx(xlsxFile = 'E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/kegg-bar-plot-hierarchy/www/data/kegg_hierarchy.xlsx')
    KEGG <- i.data()
    colnames(KEGG) <- c('ID', 'Description', 'p.adjust', 'Count')

    # 接收参数
    # ------------------------------------------------------------------------------------------------------------
    pvalueFilter <- input[['pval']]
    shownum <- input[['shownum']]

    KEGG <- left_join(x = KEGG, y = hierarchy, by = 'ID')

    KEGG <- KEGG[order(KEGG$p.adjust), ]
    KEGG <- KEGG[KEGG$p.adjust <= pvalueFilter,, drop = FALSE]
    KEGG <- KEGG %>%
      arrange(p.adjust) %>%
      slice_head(n = shownum) %>%
      group_by(level2) %>%
      arrange(level2, desc(Count)) %>%
      mutate(`-log10(P)` = -log10(p.adjust)) %>%
      mutate(Description = factor(Description, levels = Description))

    palette.aaas <- c.palette(input[['palette']])
    palette.aaas <- rep(palette.aaas, 100)
    palette.map <- setNames(palette.aaas[1:length(unique(KEGG$level2))], unique(KEGG$level2))
    
    KEGG <- KEGG %>%
      mutate(Color = palette.map[level2])  %>%
      mutate(Description = glue("<i style='color:{Color}'>{Description}</i>")) %>%
      mutate(Description = factor(Description, levels = Description))

    if (input[['order']] == 'Unsorted') {
      KEGG <- KEGG
    }  else {
      KEGG <- KEGG %>%
        mutate(Description = factor(Description, levels = Description[order(`-log10(P)`, decreasing = ifelse(input[['order']] == 'Descending', TRUE, FALSE))]))
    }
    
    max_count = max(KEGG$`-log10(P)`)
    max_axis = floor(max_count * 1.25)
    max_break = floor(max_axis / 5)
    if (max_break == 0) { max_break = 1 }
    
    gg <- ggplot(KEGG, aes(x = `-log10(P)`, y = Description)) +
      geom_bar(aes(fill = level2), stat = "identity") +
      scale_fill_manual(values = palette.map) +
      geom_text(aes(label = Count), size = db.count(), hjust = -0.25, vjust = 0.15, colour = input[['count_col']]) +
      facet_grid(
        level2 ~ .,
        scales = "free_y",
        space = "free_y"
      ) +
      labs(
        x = expression(-log10(P)),
        y = ""
      ) +
      scale_x_continuous(
        breaks = seq(0, max_axis, max_break),
        limits = c(0, max_axis),
        expand = c(0,0)
      ) +
      theme(
        legend.position = "none",
        strip.text.y = element_markdown(angle = 0, size = db.strip_text(), face = "bold"),
        strip.background = element_rect(fill = "lightgrey", color = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_markdown(size = db.axis_size(), face = "bold"),
        axis.text.x = element_text(size = db.axis_size(), face = "bold", angle = 0, hjust = 1, vjust = 1),
        axis.title.x = element_text(face = "bold", size = db.axis_title()),
        axis.line.x.bottom = element_line(color = "black"),
        panel.spacing = unit(1, 'lines'),
        panel.background = element_rect(fill = "white", color = "transparent"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = if (input[['panel.grid.major.y']]) { element_line(linewidth = .5, color = "lightgrey", linetype = "dotted") } else { element_blank() }
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
      
      req(db.axis_title(), cancelOutput = TRUE)
      req(db.axis_size (), cancelOutput = TRUE)
      req(db.count(), cancelOutput = TRUE)
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
  thisp <- 'bar_plot'
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
    '# Load your own data. Here use "openxlsx::read.xlsx" and "KEGG.xlsx" for example.'
    # data <- openxlsx::read.xlsx(xlsxFile = 'E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/kegg-bar-plot-hierarchy/www/data/KEGG.xlsx')
    data <- openxlsx::read.xlsx(xlsxFile = 'path/to/KEGG.xlsx')
    KEGG <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% 
      dplyr::select(..(input[['dragvars']]$target$ID), ..(input[['dragvars']]$target$Description), ..(input[['dragvars']]$target$pval), ..(input[['dragvars']]$target$Count))
    # hierarchy <- openxlsx::read.xlsx(xlsxFile = 'E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/kegg-bar-plot-hierarchy/www/data/kegg_hierarchy.xlsx')
    hierarchy <- openxlsx::read.xlsx(xlsxFile = 'path/to/kegg_hierarchy.xlsx')
   
    '# Merge KEGG data with hierarchy data'
    colnames(KEGG) <- c('ID', 'Description', 'p.adjust', 'Count')
    pvalueFilter <- ..(input[['pval']])
    shownum <- ..(input[['shownum']])
    KEGG <- left_join(x = KEGG, y = hierarchy, by = 'ID')
    KEGG <- KEGG[order(KEGG$p.adjust), ]
    KEGG <- KEGG[KEGG$p.adjust <= pvalueFilter,, drop = FALSE]
    KEGG <- KEGG %>%
      arrange(p.adjust) %>%
      slice_head(n = shownum) %>%
      group_by(level2) %>%
      arrange(level2, desc(Count)) %>%
      mutate(`-log10(P)` = -log10(p.adjust)) %>%
      mutate(Description = factor(Description, levels = Description))
    
    '# Set palette'
    palette.aaas <- ..(c.palette(input[['palette']]))
    palette.aaas <- rep(palette.aaas, 100)
    palette.map <- setNames(palette.aaas[1:length(unique(KEGG$level2))], unique(KEGG$level2))
    
    '# Data preparation'
    KEGG <- KEGG %>%
      mutate(Color = palette.map[level2])  %>%
      mutate(Description = glue("<i style='color:{Color}'>{Description}</i>")) %>%
      mutate(Description = factor(Description, levels = Description))
    
    if (..(input[['order']]) == 'Unsorted') {
      KEGG <- KEGG
    }  else {
      KEGG <- KEGG %>%
        mutate(Description = factor(Description, levels = Description[order(`-log10(P)`, decreasing = ..(ifelse(input[['order']] == 'Descending', TRUE, FALSE)))]))
    }
    
    max_count = max(KEGG$`-log10(P)`)
    max_axis = floor(max_count * 1.2)
    max_break = floor(max_axis / 5)
    if (max_break == 0) { max_break = 1 }
    
    '# Draw Plot'
    ggplot(KEGG, aes(x = `-log10(P)`, y = Description)) +
      geom_bar(aes(fill = level2), stat = "identity") +
      scale_fill_manual(values = palette.map) +
      geom_text(aes(label = Count), size = ..(db.count()), hjust = -0.25, vjust = 0.15, colour = ..(input[['count_col']])) +
      facet_grid(
        level2 ~ .,
        scales = "free_y",
        space = "free_y"
      ) +
      labs(
        x = expression(-log10(P)),
        y = ""
      ) +
      scale_x_continuous(
        breaks = seq(0, max_axis, max_break),
        limits = c(0, max_axis),
        expand = c(0,0)
      ) +
      theme(
        legend.position = "none",
        strip.text.y = element_markdown(angle = 0, size = ..(db.strip_text()), face = "bold"),
        strip.background = element_rect(fill = "lightgrey", color = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_markdown(size = ..(db.axis_size()), face = "bold"),
        axis.text.x = element_text(size = ..(db.axis_size()), face = "bold", angle = 0, hjust = 1, vjust = 1),
        axis.title.x = element_text(face = "bold", size = ..(db.axis_title())),
        axis.line.x.bottom = element_line(color = "black"),
        panel.spacing = unit(1, 'lines'),
        panel.background = element_rect(fill = "white", color = "transparent"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = ..(if (input[['panel.grid.major.y']]) { quote(element_line(linewidth = .5, color = "lightgrey", linetype = "dotted")) } else { quote(element_blank()) })
      )
  })
  
  # Show Plot Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(glue)
        library(stringr)
        library(ggplot2)
        library(ggtext)
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
    tags$h3('KEGG enrichment'),
    tags$p(style = 'text-align:left', "KEGG enrichment analysis identifies which KEGG pathways are significantly enriched with the genes from your input list. This helps in understanding the biological processes, molecular functions, or pathways that are most relevant to your experiment or data. It is commonly used in functional genomics, transcriptomics, proteomics, and other areas of molecular biology."),
    tags$hr(),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$hr(),
    tags$h4('Key Features of a KEGG Enrichment Bar Plot'),
    tags$p(style = 'text-align:left', tags$strong("Pathway Names (Y-axis)"), ': The Y-axis lists the names of the enriched KEGG pathways. Each bar corresponds to a specific pathway, such as "AGE-RAGE signaling pathway in diabetic complications" or "Inflammatory bowel disease."'),
    tags$p(style = 'text-align:left', tags$strong("Number of Genes (X-axis)"), ': The X-axis represents the number of genes from the input list that map to each KEGG pathway. For example, the "Lipid and atherosclerosis" pathway in the image has 7 genes, while "TNF signaling pathway" has 6 genes.'),
    tags$p(style = 'text-align:left', tags$strong("Bars"), ": Each bar corresponds to a specific KEGG pathway, and its length reflects the number of genes that are involved in that pathway. Longer bars indicate that more genes from the input list were associated with that pathway."),
    tags$p(style = 'text-align:left', tags$strong("Categories (Right Side)"), ': The pathways are grouped into higher-level biological categories, which are shown on the right side of the plot. For example, pathways related to "Immune disease" or "Infectious disease: bacterial" are grouped together. These categories help in interpreting the broader biological context of the enriched pathways.'),
    tags$p(style = 'text-align:left', tags$strong("Color Coding"), ": The bars are often color-coded based on different biological categories or significance levels. In the image, for example, pathways related to immune diseases are shown in green, while infectious diseases are shown in teal."),
    tags$p(style = 'text-align:left', tags$strong("Annotations"), ": Some bar plots may include annotations such as p-values, gene counts, or adjusted p-values to indicate the statistical significance of the enrichment for each pathway."),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)