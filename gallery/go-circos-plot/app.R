#' @title
#'
#' @description
#' 
# Load Global
source('global.R')
source('up.R')
source('src/i18n.R')

# load R packages
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(circlize)))
suppressMessages(suppressWarnings(library(ComplexHeatmap)))
suppressMessages(suppressWarnings(library(RColorBrewer)))

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
      tags$title('Circos Plot'),
      tags$link(rel = "shortcut icon", href = "img/favicon.ico"),
    ),
    tags$style(
      HTML(".col-sm-1 { width:3.5%; !important }")
    ),
    wellPanel(
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
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>ID</span>, <span class='var-num'>one</span>. The unique identifier for the GO term.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Description</span>, <span class='var-num'>one</span>. A brief description of the GO term.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>ONTOLOGY</span>, <span class='var-num'>one</span>. The category of the GO term (e.g., Biological Process [BP], Molecular Function [MF], Cellular Component [CC]). <strong style='color:#0000FF'>Note that only BP, MF and CC are recognized here.</strong></span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>BgRatio</span>, <span class='var-num'>one</span>. The ratio of genes associated with the GO term in the background set (e.g., all genes).</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>GeneRatio</span>, <span class='var-num'>one</span>. The ratio of genes associated with the GO term in your input list.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>p.adjust</span>, <span class='var-num'>one</span>. The adjusted p-value, which accounts for multiple testing (e.g., using the Benjamini-Hochberg method).</span>"),
             ),
             uiOutput(outputId = 'ui_aesthetics'),
             
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.3}</strong></div>")),
             uiOutput(outputId = 'is_add_plot_buttom'),
             
      ),
      column(width = 8, 
             column(width = 12, uiOutput(outputId = 'modify_plot_params')),
             tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
             column(width = 12,
                    mainPanel(
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:960px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
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
    
    aesthetics <- c('ID', 'Description', 'ONTOLOGY', 'BgRatio', 'GeneRatio', 'pval')
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
        #   ID = find.first(var_choices, c('ID')),
        #   ONTOLOGY = find.first(var_choices, c('ONTOLOGY')),
        #   BgRatio = find.first(var_choices, c('BgRatio')),
        #   GeneRatio = find.first(var_choices, c('GeneRatio'))
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
    !is.null(input[["dragvars"]]$target$pval) &&
      !is.null(input[["dragvars"]]$target$ID) &&
      !is.null(input[["dragvars"]]$target$Description) &&
      !is.null(input[["dragvars"]]$target$ONTOLOGY) &&
      !is.null(input[["dragvars"]]$target$BgRatio) &&
      !is.null(input[["dragvars"]]$target$GeneRatio)
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
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 1400,  min = 100, max = 3000, step = 10)),
                 column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 1000, min = 100, max = 3000, step = 10))
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
  i.data <- reactive({
    try.c <- try({
      tmp <- imported$data()
      tmp %>% dplyr::select(
        input[['dragvars']]$target$ID,
        input[['dragvars']]$target$Description,
        input[['dragvars']]$target$ONTOLOGY,
        input[['dragvars']]$target$BgRatio,
        input[['dragvars']]$target$GeneRatio,
        input[['dragvars']]$target$pval
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  # DEBOUNCE
  db.axis_size <- debounce(reactive({ input[['axis_size']] * 2 }), DEBOUNCE.L)
  db.term_size <- debounce(reactive({ input[['term_size']] * 2 }), DEBOUNCE.L)
  db.bg_gene_size <- debounce(reactive({ input[['bg_gene_size']] * 2 }), DEBOUNCE.L)
  db.enriched_gene_size <- debounce(reactive({ input[['enriched_gene_size']] * 2 }), DEBOUNCE.L)
  db.legend_size <- debounce(reactive({ input[['legend_size']] * 20 }), DEBOUNCE.L)
  db.margin_right <- debounce(reactive({ input[['margin_right']] }), DEBOUNCE.L)
  db.left_right <- debounce(reactive({ input[['left_right']] * 2 }), DEBOUNCE.L)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.P)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.P)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # 加载数据集
    # GO <- openxlsx::read.xlsx(xlsxFile = 'E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/go-circos-plot/www/data/GO.xlsx')
    # ------------------------------------------------------------------------------------------------------------
    GO <- i.data()
    colnames(GO) <- c('ID', 'Description', 'ONTOLOGY', 'BgRatio', 'GeneRatio', 'p.adjust')
    
    # 接收参数
    # ------------------------------------------------------------------------------------------------------------
    pvalueFilter <- input[['pval']]
    shownum <- input[['shownum']]
    if (input[['self_palette']]) {
      ontology.col <- c.palette(input[['palette_outer']])[1:3]
      logpvalue.col <- c.palette(input[['palette_inner']])
    } else {
      ontology.col <- c(input[['bp_col']], input[['cc_col']], input[['mf_col']])
      logpvalue.col <- c(input[['low_col']], input[['high_col']])
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
    data <- rbind.data.frame(BP, CC, MF)
    main.col <- ontology.col[as.numeric(as.factor(data$ONTOLOGY))]
    
    BgGene <- as.numeric(sapply(strsplit(data$BgRatio, "/"), '[', 1))
    Gene <- as.numeric(sapply(strsplit(data$GeneRatio, '/'), '[', 1))
    ratio <- Gene/BgGene
    
    logpvalue <- -log(data$p.adjust, 10)
    logpvalue.col <- colorRampPalette(logpvalue.col)(8)
    f <- colorRamp2(breaks = c(0, 2, 4, 6, 8, 10, 15, 20), colors = logpvalue.col)
    BgGene.col <- f(logpvalue)
    df <- data.frame(GO = data$ID, start = 1, end = max(BgGene))
    rownames(df) <- df$GO
    bed2 <- data.frame(GO = data$ID, start = 1, end = BgGene, BgGene = BgGene, BgGene.col = BgGene.col)
    bed3 <- data.frame(GO = data$ID, start = 1, end = Gene, BgGene = Gene)
    bed4 <- data.frame(GO = data$ID, start = 1, end = max(BgGene), ratio = ratio, col = main.col)
    bed4$ratio = bed4$ratio/max(bed4$ratio)*9.5
    
    # Draw plot
    # ------------------------------------------------------------------------------------------------------------
    par(omi = c(0.1, 0.1, 0.1, db.margin_right()))
    circos.par(track.margin = c(0.01, 0.01))
    circos.genomicInitialize(df, plotType = "none")
    circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
      sector.index = get.cell.meta.data("sector.index")
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      circos.text(mean(xlim), mean(ylim), sector.index, cex = db.term_size(), facing = "bending.inside", niceFacing = TRUE)
    }, track.height = 0.08, bg.border = NA, bg.col = main.col)
    
    for(si in get.all.sector.index()) {
      circos.axis(h = "top", labels.cex = db.axis_size(), sector.index = si,track.index = 1, major.at = seq(0, max(BgGene), by = 100), labels.facing = "clockwise")
    }
    circos.genomicTrack(
      bed2, ylim = c(0, 1), track.height = 0.1, bg.border = "white",
      panel.fun = function(region, value, ...) {
        i = getI(...)
        circos.genomicRect(region, value, ytop = 0, ybottom = 1, col = value[, 2], border = NA, ...)
        circos.genomicText(region, value, y = 0.4, labels = value[, 1], adj = 0, cex = db.bg_gene_size(), ...)
      }
    )
    circos.genomicTrack(
      bed3, ylim = c(0, 1), track.height = 0.1, bg.border = "white",
      panel.fun = function(region, value, ...) {
        i = getI(...)
        circos.genomicRect(region, value, ytop = 0, ybottom = 1, col = '#000000', border = NA, ...)
        circos.genomicText(region, value, y = 0.4, labels = value[, 1], cex = db.enriched_gene_size(), adj = 0, ...)
      }
    )
    circos.genomicTrack(
      bed4, ylim = c(0, 10), track.height = 0.35, bg.border = "white", bg.col = "grey90",
      panel.fun = function(region, value, ...) {
        cell.xlim = get.cell.meta.data("cell.xlim")
        cell.ylim = get.cell.meta.data("cell.ylim")
        for(j in 1:9) {
          y = cell.ylim[1] + (cell.ylim[2] - cell.ylim[1])/10*j
          circos.lines(cell.xlim, c(y, y), col = "#FFFFFF", lwd = 0.3)
        }
        circos.genomicRect(region, value, ytop = 0, ybottom = value[, 1], col = value[, 2], border = NA, ...)
      }
    )
    circos.clear()
    
    middle.legend <- Legend(
      labels = c('Number of Background Genes', 'Number of Enriched Genes', 'Rich Factor(0-1)'),
      type = "points", pch = c(15, 15, 17),
      labels_gp = gpar(fontsize = db.legend_size()),
      legend_gp = gpar(col = c(logpvalue.col[1], '#000000', '#FFFFFF')),
      title = "", nrow = 3, size = unit(3, "mm")
    )
    circle_size = unit(1, "snpc")
    BP.legend <- Legend(
      labels = paste0(BP$ID, ':', BP$Description),
      type = "points", pch = 15,
      title_gp = gpar(fontsize = db.legend_size()),
      labels_gp = gpar(fontsize = db.legend_size()),
      legend_gp = gpar(col = ontology.col[1]), title_position = "topleft",
      title = "Biological Process", nrow = nrow(BP), size = unit(3, "mm"), grid_height = unit(5, "mm"),
      grid_width = unit(5, "mm")
    )
    CC.legend <- Legend(
      labels = paste0(CC$ID, ':', CC$Description),
      type = "points", pch = 15,
      title_gp = gpar(fontsize = db.legend_size()),
      labels_gp = gpar(fontsize = db.legend_size()),
      legend_gp = gpar(col = ontology.col[2]), title_position = "topleft",
      title = "Cellular Component", nrow = nrow(CC), size = unit(3, "mm"), grid_height = unit(5, "mm"),
      grid_width = unit(5, "mm")
    )
    MF.legend <- Legend(
      labels = paste0(MF$ID, ':', MF$Description),
      type = "points", pch = 15,
      title_gp = gpar(fontsize = db.legend_size()),
      labels_gp = gpar(fontsize = db.legend_size()),
      legend_gp = gpar(col = ontology.col[3]), title_position = "topleft",
      title = "Molecular Function", nrow = nrow(MF), size = unit(3, "mm"), grid_height = unit(5, "mm"),
      grid_width = unit(5, "mm")
    )
    logp.legend <- Legend(
      labels = c('(0,2]', '(2,4]', '(4,6]', '(6,8]', '(8,10]', '(10,15]', '(15,20]', '>=20'),
      type = "points", pch = 16, 
      title_gp = gpar(fontsize = db.legend_size()),
      labels_gp = gpar(fontsize = db.legend_size()),
      legend_gp = gpar(col = logpvalue.col), title = "-log10(Pvalue)",
      title_position = "topleft", grid_height = unit(5, "mm"), grid_width = unit(5, "mm"),
      size = unit(3, "mm")
    )
    lgd = packLegend(BP.legend, CC.legend, MF.legend, middle.legend, logp.legend)
    circle_size = unit(1, "snpc")
    ComplexHeatmap::draw(lgd, x = circle_size*db.left_right(), y = circle_size*0.5, just = "left")
    
    message(glue('{Sys.time()} Plot Success'))
  }
  
  # 同时支持自定义图片大小
  # ------------------------------------------------------------------------------------------------------------
  observeEvent(input[['show_data-confirm']], {
    
    shinyjs::hide('io-albert-gg')
    shinyjs::hide('introbox-download')
    shinyjs::hide('variable-des')
    shinyWidgets::updateSwitchInput(session = session, inputId = 'plot_interactively', value = FALSE)
    
    observeEvent(input[['plot_button']], {
      
      req(db.axis_size(), cancelOutput = TRUE)
      req(db.term_size (), cancelOutput = TRUE)
      req(db.bg_gene_size(), cancelOutput = TRUE)
      req(db.enriched_gene_size(), cancelOutput = TRUE)
      req(db.legend_size(), cancelOutput = TRUE)
      req(db.margin_right(), cancelOutput = TRUE)
      req(db.left_right(), cancelOutput = TRUE)
      
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
  thisp <- 'go_circos_plot'
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
      dplyr::select(..(input[['dragvars']]$target$ID), ..(input[['dragvars']]$target$Description), ..(input[['dragvars']]$target$ONTOLOGY), ..(input[['dragvars']]$target$BgRatio), ..(input[['dragvars']]$target$GeneRatio), ..(input[['dragvars']]$target$pval))
    
    '# Set palette'
    if (..(input[['self_palette']])) {
      ontology.col <- ..(c.palette(input[['palette_outer']])[1:3])
      logpvalue.col <- ..(c.palette(input[['palette_inner']]))
    } else {
      ontology.col <- ..(c(input[['bp_col']], input[['cc_col']], input[['mf_col']]))
      logpvalue.col <- ..(c(input[['low_col']], input[['high_col']]))
    }
    '# Data preparation'
    colnames(GO) <- c('ID', 'Description', 'ONTOLOGY', 'BgRatio', 'GeneRatio', 'p.adjust')
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
    data <- rbind.data.frame(BP, CC, MF)
    main.col <- ontology.col[as.numeric(as.factor(data$ONTOLOGY))]
    
    BgGene <- as.numeric(sapply(strsplit(data$BgRatio, "/"), '[', 1))
    Gene <- as.numeric(sapply(strsplit(data$GeneRatio, '/'), '[', 1))
    ratio <- Gene/BgGene
    
    logpvalue <- -log(data$p.adjust, 10)
    logpvalue.col <- colorRampPalette(logpvalue.col)(8)
    f <- colorRamp2(breaks = c(0, 2, 4, 6, 8, 10, 15, 20), colors = logpvalue.col)
    BgGene.col <- f(logpvalue)
    df <- data.frame(GO = data$ID, start = 1, end = max(BgGene))
    rownames(df) <- df$GO
    bed2 <- data.frame(GO = data$ID, start = 1, end = BgGene, BgGene = BgGene, BgGene.col = BgGene.col)
    bed3 <- data.frame(GO = data$ID, start = 1, end = Gene, BgGene = Gene)
    bed4 <- data.frame(GO = data$ID, start = 1, end = max(BgGene), ratio = ratio, col = main.col)
    bed4$ratio = bed4$ratio/max(bed4$ratio)*9.5
    '# Draw plot'
    pdf(file = 'GO-circis.pdf', height = ..(db.height()/100), width = ..(db.width()/100))
    par(omi = c(0.1, 0.1, 0.1, ..(db.margin_right())))
    circos.par(track.margin = c(0.01, 0.01))
    circos.genomicInitialize(df, plotType = "none")
    circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
      sector.index = get.cell.meta.data("sector.index")
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      circos.text(mean(xlim), mean(ylim), sector.index, cex = ..(db.term_size()), facing = "bending.inside", niceFacing = TRUE)
    }, track.height = 0.08, bg.border = NA, bg.col = main.col)
    
    for(si in get.all.sector.index()) {
      circos.axis(h = "top", labels.cex = ..(db.axis_size()), sector.index = si,track.index = 1, major.at = seq(0, max(BgGene), by = 100), labels.facing = "clockwise")
    }
    circos.genomicTrack(
      bed2, ylim = c(0, 1), track.height = 0.1, bg.border = "white",
      panel.fun = function(region, value, ...) {
        i = getI(...)
        circos.genomicRect(region, value, ytop = 0, ybottom = 1, col = value[, 2], border = NA, ...)
        circos.genomicText(region, value, y = 0.4, labels = value[, 1], adj = 0, cex = ..(db.bg_gene_size()), ...)
      }
    )
    circos.genomicTrack(
      bed3, ylim = c(0, 1), track.height = 0.1, bg.border = "white",
      panel.fun = function(region, value, ...) {
        i = getI(...)
        circos.genomicRect(region, value, ytop = 0, ybottom = 1, col = '#000000', border = NA, ...)
        circos.genomicText(region, value, y = 0.4, labels = value[, 1], cex = ..(db.enriched_gene_size()), adj = 0, ...)
      }
    )
    circos.genomicTrack(
      bed4, ylim = c(0, 10), track.height = 0.35, bg.border = "white", bg.col = "grey90",
      panel.fun = function(region, value, ...) {
        cell.xlim = get.cell.meta.data("cell.xlim")
        cell.ylim = get.cell.meta.data("cell.ylim")
        for(j in 1:9) {
          y = cell.ylim[1] + (cell.ylim[2] - cell.ylim[1])/10*j
          circos.lines(cell.xlim, c(y, y), col = "#FFFFFF", lwd = 0.3)
        }
        circos.genomicRect(region, value, ytop = 0, ybottom = value[, 1], col = value[, 2], border = NA, ...)
      }
    )
    circos.clear()
    
    middle.legend <- Legend(
      labels = c('Number of Background Genes', 'Number of Enriched Genes', 'Rich Factor(0-1)'),
      type = "points", pch = c(15, 15, 17), legend_gp = gpar(col = c(logpvalue.col[1], '#000000', '#FFFFFF')),
      title = "", nrow = 3, size = unit(..(db.legend_size()), "mm")
    )
    circle_size = unit(1, "snpc")
    BP.legend <- Legend(
      labels = paste0(BP$ID, ':', BP$Description),
      type = "points", pch = 15,
      legend_gp = gpar(col = ontology.col[1]), title_position = "topleft",
      title = "Biological Process", nrow = nrow(BP),size = unit(..(db.legend_size()), "mm"), grid_height = unit(5, "mm"),
      grid_width = unit(5, "mm")
    )
    CC.legend <- Legend(
      labels = paste0(CC$ID, ':', CC$Description),
      type = "points", pch = 15,
      legend_gp = gpar(col = ontology.col[2]), title_position = "topleft",
      title = "Cellular Component", nrow = nrow(CC), size = unit(..(db.legend_size()), "mm"), grid_height = unit(5, "mm"),
      grid_width = unit(5, "mm")
    )
    MF.legend <- Legend(
      labels = paste0(MF$ID, ':', MF$Description),
      type = "points", pch = 15,
      legend_gp = gpar(col = ontology.col[3]), title_position = "topleft",
      title = "Molecular Function", nrow = nrow(MF), size = unit(..(db.legend_size()), "mm"), grid_height = unit(5, "mm"),
      grid_width = unit(5, "mm")
    )
    logp.legend <- Legend(
      labels = c('(0,2]', '(2,4]', '(4,6]', '(6,8]', '(8,10]', '(10,15]', '(15,20]', '>=20'),
      type = "points", pch = 16, legend_gp = gpar(col = logpvalue.col), title = "-log10(Pvalue)",
      title_position = "topleft", grid_height = unit(5, "mm"), grid_width = unit(5, "mm"),
      size = unit(..(db.legend_size()), "mm")
    )
    lgd = packLegend(BP.legend, CC.legend, MF.legend, middle.legend, logp.legend)
    circle_size = unit(1, "snpc")
    ComplexHeatmap::draw(lgd, x = circle_size*..(db.left_right()), y = circle_size*0.5, just = "left")
    '# End device'
    dev.off()
  })
  
  # Show Plot Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(circlize)
        library(stringr)
        library(ComplexHeatmap)
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
      code = as.character(xfun::session_info(c('circlize', 'dplyr', 'stringr', 'ComplexHeatmap'))),
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
    tags$h3('What is a Circos Plot?'),
    tags$p(style = 'text-align:left', "A Circos plot is a circular visualization tool often used in genomics to display relationships and comparisons between different data sets. "),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "800px", height = "auto", alt = "ZOOM IN")),
    tags$h3('Gene Ontology (GO) enrichment'),
    tags$p(style = 'text-align:left', "GO enrichment analysis is a method used to identify overrepresented biological terms or pathways in a set of genes. "),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)