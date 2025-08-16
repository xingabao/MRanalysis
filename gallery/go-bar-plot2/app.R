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
  go_demo <<- openxlsx::read.xlsx(glue('{www}/GO.xlsx'))
  
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
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Description</span>, <span class='var-num'>one</span>. A brief description of the GO term.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>ONTOLOGY</span>, <span class='var-num'>one</span>. The category of the GO term (e.g., Biological Process [BP], Molecular Function [MF], Cellular Component [CC]).</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>p.adjust</span>, <span class='var-num'>one</span>. The adjusted p-value, which accounts for multiple testing (e.g., using the Benjamini-Hochberg method).</span>"),
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
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste('GO.xlsx') },
    content <- function(file){
      file.copy(glue("{www}/GO.xlsx"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(id = "show_data", choices = c('go_demo'), return_class = "tbl_df", read_fns = list(tsv = function(file) { data.table::fread(fread) }))
  
  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    
    aesthetics <- c('Description', 'ONTOLOGY', 'pval', 'Count')
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
        # selected = list(pval = find.first(
        #   var_choices, c('p', 'adj')), 
        #   Description = find.first(var_choices, c('Description')),
        #   Count = find.first(var_choices, c('Count')),
        #   ONTOLOGY = find.first(var_choices, c('ONTOLOGY')),
        #   geneID = find.first(var_choices, c('geneID'))
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
    !is.null(input[["dragvars"]]$target$Description) &&
      !is.null(input[["dragvars"]]$target$ONTOLOGY) &&
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
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 600,  min = 100, max = 3000, step = 10)),
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
        input[['dragvars']]$target$Description,
        input[['dragvars']]$target$ONTOLOGY,
        input[['dragvars']]$target$pval,
        input[['dragvars']]$target$Count
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  # DEBOUNCE
  db.axis_title <- debounce(reactive({ input[['axis_title']] * 32 }), DEBOUNCE.0)
  db.axis_size <- debounce(reactive({ input[['axis_size']] * 32 }), DEBOUNCE.0)
  db.count <- debounce(reactive({ input[['count']] * 9 }), DEBOUNCE.0)
  db.legend_text <- debounce(reactive({ input[['legend_text']] * 32 }), DEBOUNCE.0)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.A)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.A)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    # GO <- openxlsx::read.xlsx(xlsxFile = 'E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/go-bar-plot/www/data/GO.xlsx')
    # GO <- GO[, c('Description', 'ONTOLOGY', 'p.adjust', 'Count')]
    GO <- i.data()
    colnames(GO) <- c('Description', 'ONTOLOGY', 'p.adjust', 'Count')

    # 接收参数
    # ------------------------------------------------------------------------------------------------------------
    pvalueFilter <- input[['pval']]
    shownum <- input[['shownum']]
    if (input[['self_palette']]) {
      ontology.col <- rep(c.palette(input[['palette']]), 3)[1:3]
    } else {
      ontology.col <- c(input[['bp_col']], input[['cc_col']], input[['mf_col']])
    }
    # 数据整理
    # ------------------------------------------------------------------------------------------------------------
    GO$Description <- str_wrap(GO$Description, 40)
    data <- GO[order(GO$p.adjust), ]
    datasig <- data[data$p.adjust < pvalueFilter,, drop = FALSE]
    BP <- datasig[datasig$ONTOLOGY == "BP",, drop = FALSE]
    CC <- datasig[datasig$ONTOLOGY == "CC",, drop = FALSE]
    MF <- datasig[datasig$ONTOLOGY == "MF",, drop = FALSE]
    BP <- na.omit(head(BP, shownum))
    CC <- na.omit(head(CC, shownum))
    MF <- na.omit(head(MF, shownum))
    datasig <- rbind.data.frame(BP, CC, MF)
    datasig <- datasig %>% mutate(`-log10(P)` = -log10(p.adjust))
    
    palette.map <- setNames(ontology.col[1:length(unique(GO$ONTOLOGY))], unique(GO$ONTOLOGY))
    
    GO. <- datasig %>%
      group_by(ONTOLOGY) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(Color = palette.map[ONTOLOGY]) %>%
      arrange(ONTOLOGY, desc(Count)) %>%
      mutate(`-log10(P)` = -log10(p.adjust)) %>%
      mutate(Description = glue("<i style='color:{Color}'>{Description}</i>"))
      
    if (input[['order']] == 'Unsorted') {
      GO. <- GO.
    }  else {
      GO. <- GO. %>%
        mutate(Description = factor(Description, levels = Description[order(`-log10(P)`, decreasing = ifelse(input[['order']] == 'Descending', TRUE, FALSE))]))
    }

    # Draw Plot
    max_count = max(GO.$`-log10(P)`, na.rm = TRUE)
    max_axis = floor(max_count * 1.25)
    max_break = floor(max_axis / 5)
    if (max_break == 0) { max_break = 1 }
    
    gg <- ggplot(GO., aes(x = Description, y = `-log10(P)`)) +
      geom_bar(aes(fill = ONTOLOGY), stat = "identity", width = 0.6) +
      geom_text(aes(label = Count), size = db.count(), hjust = 0.5, vjust = -0.5, colour = input[['count_col']]) +
      scale_fill_manual(values = palette.map) +
      facet_grid(~ ONTOLOGY, scales = "free", space = "free") +
      labs(
        x = "",
        y = expression(-log10(P))
      ) +
      scale_y_continuous(breaks = seq(0, max_axis, max_break), limits = c(0, max_axis), expand = c(0, 0)) +
      theme(
        panel.background = element_rect(fill = "white", color = "transparent"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = if (input[['panel.grid.major.y']]) { element_line(linewidth = .5, color = "lightgrey", linetype = "dotted") } else { element_blank() },
        panel.spacing.x = unit(0, "lines"),
        axis.text.x = element_markdown(color = "black", angle = 60, hjust = 1, vjust = 1, size = db.axis_size()),
        axis.text.y = element_text(color = "black", size = db.axis_size()),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 60),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = db.axis_title()),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = db.legend_text()),
        legend.position = 'inside',
        legend.position.inside = c(0.5, 0.95),
        legend.direction =  'horizontal',
        legend.key.width = unit(1, units = 'cm'),
        legend.key.height = unit(0.35, units = 'cm'),
        legend.key.spacing.y = unit(0.15, units = 'cm')
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
      req(db.legend_text(), cancelOutput = TRUE)
      
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
    '# Load your own data. Here use "openxlsx::read.xlsx" and "GO.xlsx" for example.'
    # data <- openxlsx::read.xlsx('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/go-bar-plot/www/data/GO.xlsx')
    data <- openxlsx::read.xlsx('path/to/GO.xlsx')
    GO <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% 
      dplyr::select(..(input[['dragvars']]$target$Description), ..(input[['dragvars']]$target$ONTOLOGY), ..(input[['dragvars']]$target$pval), ..(input[['dragvars']]$target$geneID), ..(input[['dragvars']]$target$Count))
    
    '# Set palette'
    ontology.col <- ..(if (input[['self_palette']]) {
      rep(c.palette(input[['palette']]), 3)[1:3]
    } else {
      c(input[['bp_col']], input[['cc_col']], input[['mf_col']])
    })
    
    '# Data preparation'
    colnames(GO) <- c('Description', 'ONTOLOGY', 'p.adjust', 'Count')
    pvalueFilter <- ..(input[['pval']])
    shownum <- ..(input[['shownum']])
    GO$Description <- str_wrap(GO$Description, 40)
    data <- GO[order(GO$p.adjust), ]
    datasig <- data[data$p.adjust < pvalueFilter,, drop = FALSE]
    BP <- datasig[datasig$ONTOLOGY == "BP",, drop = FALSE]
    CC <- datasig[datasig$ONTOLOGY == "CC",, drop = FALSE]
    MF <- datasig[datasig$ONTOLOGY == "MF",, drop = FALSE]
    BP <- na.omit(head(BP, shownum))
    CC <- na.omit(head(CC, shownum))
    MF <- na.omit(head(MF, shownum))
    datasig <- rbind.data.frame(BP, CC, MF)
    datasig <- datasig %>% mutate(`-log10(P)` = -log10(p.adjust))

    palette.map <- setNames(ontology.col[1:length(unique(GO$ONTOLOGY))], unique(GO$ONTOLOGY))
    
    GO. <- datasig %>%
      group_by(ONTOLOGY) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(Color = palette.map[ONTOLOGY]) %>%
      arrange(ONTOLOGY, desc(Count)) %>%
      mutate(`-log10(P)` = -log10(p.adjust)) %>%
      mutate(Description = glue("<i style='color:{Color}'>{Description}</i>"))
    
    if (..(input[['order']]) == 'Unsorted') {
      GO. <- GO.
    }  else {
      GO. <- GO. %>%
        mutate(Description = factor(Description, levels = Description[order(`-log10(P)`, decreasing = ..(ifelse(input[['order']] == 'Descending', TRUE, FALSE)))]))
    }
    
    '# Draw Plot'
    max_count = max(GO.$`-log10(P)`, na.rm = TRUE)
    max_axis = floor(max_count * 1.25)
    max_break = floor(max_axis / 5)
    if (max_break == 0) { max_break = 1 }
    
    ggplot(GO., aes(x = Description, y = `-log10(P)`)) +
      geom_bar(aes(fill = ONTOLOGY), stat = "identity", width = 0.6) +
      geom_text(aes(label = Count), size = ..(db.count()), hjust = 0.5, vjust = -0.5, colour = ..(input[['count_col']])) +
      scale_fill_manual(values = palette.map) +
      facet_grid(~ ONTOLOGY, scales = "free", space = "free") +
      labs(
        x = "",
        y = expression(-log10(P))
      ) +
      scale_y_continuous(breaks = seq(0, max_axis, max_break), limits = c(0, max_axis), expand = c(0, 0)) +
      theme(
        panel.background = element_rect(fill = "white", color = "transparent"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = ..(if (input[['panel.grid.major.y']]) { quote(element_line(linewidth = .5, color = "lightgrey", linetype = "dotted")) } else { quote(element_blank()) }),
        panel.spacing.x = unit(0, "lines"),
        axis.text.x = element_markdown(color = "black", angle = 60, hjust = 1, vjust = 1, size = ..(db.axis_size())),
        axis.text.y = element_text(color = "black", size = ..(db.axis_size())),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 60),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = ..(db.axis_title())),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = ..(db.legend_text())),
        legend.position = 'inside',
        legend.position.inside = c(0.5, 0.95),
        legend.direction =  'horizontal',
        legend.key.width = unit(1, units = 'cm'),
        legend.key.height = unit(0.35, units = 'cm'),
        legend.key.spacing.y = unit(0.15, units = 'cm')
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
    tags$h3('Gene Ontology (GO) enrichment'),
    tags$p(style = 'text-align:left', "GO enrichment analysis is a method used to identify overrepresented biological terms or pathways in a set of genes."),
    tags$hr(),
    tags$h3('What is a Bar plot?'),
    tags$p(style = 'text-align:left', "A bar plot in GO (Gene Ontology) enrichment analysis is a graphical representation used to visualize the results of the enrichment analysis."),
    tags$p(style = 'text-align:left', "It shows the most significantly enriched GO terms (Biological Processes, Cellular Components, and Molecular Functions) in a gene set, along with their significance and the number of genes involved. The bar plot is a convenient way to summarize and interpret the biological relevance of a set of genes."),
    tags$hr(),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$br(),
    tags$h3("Here’s what it typically involves:"),
    tags$p(style = 'text-align:left', tags$strong("BP (Biological Process)"), ': Shown in ', tags$strong(style = 'color:#3B4992', 'blue'), ' on the left side, these represent biological processes or pathways that the genes are involved in. Examples include "neuroinflammatory response" and "neutrophil migration.'),
    tags$p(style = 'text-align:left', tags$strong("CC (Cellular Component)"), ': Shown in ', tags$strong(style = 'color:#FF0000', 'red'), ', these refer to the locations within the cell where the gene products are localized or active, such as "nuclear envelope lumen" and "endoplasmic reticulum lumen.'),
    tags$p(style = 'text-align:left', tags$strong("MF (Molecular Function)"), ': Shown in ', tags$strong(style = 'color:#008B45', 'green'), ', these represent the molecular activities of gene products, such as "cytokine activity" and "receptor ligand activity.'),
    tags$p(style = 'text-align:left', tags$strong("Bar Length (Enrichment Significance)"), ': The length of each bar corresponds to the -log10(p-value), which indicates the statistical significance of the enrichment. A longer bar represents a more significant result.'),
    tags$p(style = 'text-align:left', tags$strong("Gene Counts (Numbers on Bars)"), ': The small numbers at the end of each bar represent the number of genes associated with the respective GO term.'),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)