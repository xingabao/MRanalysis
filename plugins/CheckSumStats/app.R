#' @title
#'
#' @description
#'

# Load R packages

# devtools::install_github("MRCIEU/CheckSumStats")
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(cowplot)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(CheckSumStats)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinyjs)))
suppressMessages(suppressWarnings(library(shinyWidgets)))
suppressMessages(suppressWarnings(library(shinymeta)))
suppressMessages(suppressWarnings(library(shinycssloaders)))
suppressMessages(suppressWarnings(library(esquisse)))
suppressMessages(suppressWarnings(library(formattable)))

source('src/up.R')

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

options(shiny.maxRequestSize = tryCatch(if (Sys.getenv("MRANALYSIS_MAX_SIZE") == '') 100000 * 1024^2 else eval(parse(text = Sys.getenv("MRANALYSIS_MAX_SIZE"))), error = function(e) 100000 * 1024^2))

utils::data("refdat_1000G_superpops")
snplist <- unique(refdat_1000G_superpops$SNP)

dragulaInput. <- MRanalysisBase::dragulaInput.
environment(dragulaInput.) <- getNamespace("esquisse")
if (bindingIsLocked("dragulaInput", getNamespace("esquisse"))) {
  unlockBinding("dragulaInput", getNamespace("esquisse"))
}
assignInNamespace(x = "dragulaInput", value = dragulaInput., ns = "esquisse")

js <- HTML("
$(function() {
  $('.shiny-input-container .input-group-btn .btn').on('click', function() {
     const id = $(this).find('input[type=\"file\"]').attr('id');
     Shiny.setInputValue(id + '_click', Math.random());
  })    
})")

#
ui <- fluidPage(
  titlePanel("Check SumStats Quality"),
  useShinyjs(),
  singleton(tags$head(tags$script(js))),
  tags$head(
    includeCSS(system.file('extdata', 'CSS/shiny-style.css', package = 'MRanalysisBase')),
    HTML("<html lang='en'>"),
    tags$link(rel = 'shortcut icon', href = '/XINGABAO/img/favicon.ico'),
    MRanalysisBase::web.statistic.baidu,
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
      .sidebar {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 10px;
      }
      .loader {
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
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      class = "sidebar",
      
      div(
        id = 'fileUploadButton',
        fileInput(inputId = "fileUpload", label = "Upload GWAS Summary Data", accept = c(".csv", ".txt", ".tsv", ".csv.gz", ".txt.gz", ".tsv.gz"))
      ),
      
      uiOutput(outputId = 'is_add_import'),
      uiOutput(outputId = 'ui_aesthetics'),
      uiOutput(outputId = 'is_add_button'),
      
      tags$hr(),
      tags$h4("Introduction"),
      tags$p("This application was designed to check the allele frequency in the summary data file 
           by comparing the allele frequencies in the uploaded dataset to those in the 
           1000 Genomes superpopulation.
           The presence of SNPs with allele frequency > 0.5 in the uploaded dataset implies an allele frequency conflict. If the majority of SNPs in the GWAS summary stats have allele frequency conflict, it is recommended not using for the downstream Mendelian randomization analysis."),
      tags$hr(),
      tags$h4("How to use this application"),
      HTML("<span>When using this app, you should make sure that your data columns must be in the correct format and have <strong>SNP</strong>, <strong>effect_allele</strong>, <strong>other_allele</strong> and <strong>eaf</strong> columns, below is our demo data for reference.</span>"),
      div(
        class = "button-container",
        tags$br(),
        tags$a(href = '/XINGABAO/intro/CheckSumStats.gif', target = "_blank", tags$img(src = '/XINGABAO/intro/CheckSumStats.gif', width = "720px", height = "auto", alt = "ZOOM IN")),
      ),
      tags$hr(),
      div(
        tags$h4("References and Acknowledgments"),
        tags$p("This application uses packages mainly from:"),
        tags$p("Design and quality control of large-scale two-sample Mendelian randomization studies"),
        tags$p("doi: https://doi.org/10.1101/2021.07.30.21260578"),
        
        tags$p("For more information, please visit the following links:"),
        tags$ul(
          tags$li(tags$a(href = "https://github.com/MRCIEU/CheckSumStats?tab=readme-ov-file", "CheckSumStats")),
          
        ),
        tags$p("Special thanks to the developers of the R packages used in this application.")
      )
    ),
    
    column(width = 7, 
           column(width = 12, uiOutput(outputId = 'modify_plot_params')),
           tags$br(), tags$br(), tags$br(), tags$br(), 
           column(width = 12,
                  mainPanel(
                    fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:540px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
                  )),
           column(width = 12, id = 'introbox-download',  uiOutput(outputId = 'intro_box')),
           column(width = 12, id = 'tabledatahere')
    )
  )
)

server <- function(input, output) {
  
  www = 'demo'
  demo_data <<- data.table::fread(glue('{www}/sumstats.csv'))
  
  gwas.file <- reactiveVal(NULL)
  gwas.filepath <- reactiveVal(NULL)
  plot.status <- reactiveVal(FALSE)
  i.data <- reactiveVal(NULL)
  s.data <- reactiveVal(NULL)
  a.data <- reactiveVal(NULL)
  
  # 组件显示状态
  display.status <- reactive({
    !is.null(input[["dragvars"]]$target$SNP) &&
      !is.null(input[["dragvars"]]$target$effect_allele) &&
      !is.null(input[["dragvars"]]$target$other_allele) &&
      !is.null(input[["dragvars"]]$target$eaf)
  })
  
  # 导入数据
  output[['is_add_import']] <- renderUI({
    req(input$fileUpload)
    fluidRow(
      div(class = "button-container", actionButton("btnImport", label = "Import", class = "btn-custom")),
    )
  })
  
  # 点击绘图按钮是否展示
  output[['is_add_button']] <- renderUI({
    if (display.status()) {
      shinyjs::show('up_ui')
      fluidRow(
        div(class = "button-container", actionButton(inputId = "btnPlot", label = "Check & Plot", class = "btn-custom")),
        column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 1000,  min = 100, max = 3000, step = 10)),
        column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 720, min = 100, max = 3000, step = 10))
      )
    } else {
      shinyjs::hide('io-albert-gg')
      shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
      column(12,
             div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                 shiny::downloadButton(outputId = 'downloadDemo', label = 'Download demo data', class = 'btn-success')
             ),
             tags$br(), tags$br()
      )
    }
  })
  
  # 对应图的一些个性化设置
  output[['modify_plot_params']] <- renderUI({
    tags$div(
      id = 'up_ui',
      column(width = 12, up.ui(width = 2)), tags$br(), tags$br(),
      style = 'display:none'
    )
  })
  
  # introBox
  output[['intro_box']] <- renderUI({
    if (display.status() && plot.status()) {
      tags$div(
        tags$style(
          HTML('.panel-default { border-color:#FFFFFF }'),
          HTML('div.panel-heading.card-header { color:#FFFFFF; background-color:#FFFFFF; border-color:#FFFFFF }'),
          HTML('div.panel-body.card-body { display:none }'),
          HTML('div.modal-content { min-width:960px }')
        ),
        column(width = 12,
               class = 'up-col-sm-12-20',
               tags$div(
                 tags$style(
                   HTML('.down { min-width:12vh; }'),
                 )
               ),
               shiny::downloadButton(outputId = 'download_gg_tiff', label = 'TIFF Format', class = "down", width = '750px'),
               shiny::downloadButton(outputId = 'download_gg_pdf', label = 'PDF Format', class = "down", width = '750px'),
               shiny::downloadButton(outputId = 'download_gg_svg', label = 'SVG Format', class = "down", width = '750px'),
               shiny::actionButton(inputId = 'show_codes', label = 'Show Codes', icon = icon('code'), class = "down"),
               shiny::actionButton(inputId = 'show_sessioninfo', label = 'Show R Session Info', icon  = icon('info'), class = "down"),
        ),
        tags$br(), tags$br(), 
        column(width = 12, verbatimTextOutput('show_sessioninfo_output'))
      )
    }
  })
  
  # DEBOUNCE
  db.width <- debounce(reactive({ input[['WIDTH']] }), MRanalysisBase::DEBOUNCE.P)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), MRanalysisBase::DEBOUNCE.P)
  db.ncol <- debounce(reactive({ input[['ncol']] }), MRanalysisBase::DEBOUNCE.A)
  db.title_size <- debounce(reactive({ input[['title_size']] }), MRanalysisBase::DEBOUNCE.A)
  db.title_y_size <- debounce(reactive({ input[['title_y_size']] }), MRanalysisBase::DEBOUNCE.A)
  db.axis_title_size <- debounce(reactive({ input[['axis_title_size']] }), MRanalysisBase::DEBOUNCE.A)
  db.axis_text_size <- debounce(reactive({ input[['axis_text_size']] }), MRanalysisBase::DEBOUNCE.A)
  
  # 绘图部分
  plot.reactive <- function() {
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    i.data <- na.omit(s.data())
    i.data$p_value <- i.data$eaf
    i.data <- CheckSumStats::format_data(dat = i.data, rsid = "SNP", effect_allele = "effect_allele", other_allele = "other_allele", eaf = "eaf", p = "p_value")
    i.data$p <- NULL
    
    # 设置绘图参数
    # ------------------------------------------------------------------------------------------------------------
    Title = "Allele frequency in test dataset vs 1000 genomes super populations"
    Ylab = 'Allele frequency in test dataset'
    common <- theme_classic() + theme(
      axis.text = element_text(size = db.axis_text_size()),
      axis.title.x = element_text(size = db.axis_title_size(), vjust = -1),
      plot.margin = margin(t = 25, l = 20, r = 20, b = 25, unit = 'pt')
    )
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    ncol <- db.ncol()
    
    txt <- glue('plot_grid(AFR, AMR, EAS, EUR, SAS, ALL, ncol = {ncol})')
    pops <- input[['population']]
    if ('AFR' %in% pops) {
      AFR <- make_plot_maf(ref_1000G = c("AFR"), target_dat = i.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      AFR <- NULL
      txt <- gsub('AFR, ', '', txt)
    }
    if ('AMR' %in% pops) {
      AMR <- make_plot_maf(ref_1000G = c("AMR"), target_dat = i.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      AMR <- NULL
      txt <- gsub('AMR, ', '', txt)
    }
    if ('EAS' %in% pops) {
      EAS <- make_plot_maf(ref_1000G = c("EAS"), target_dat = i.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      EAS <- NULL
      txt <- gsub('EAS, ', '', txt)
    }
    if ('EUR' %in% pops) {
      EUR <- make_plot_maf(ref_1000G = c("EUR"), target_dat = i.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      EUR <- NULL
      txt <- gsub('EUR, ', '', txt)
    }
    if ('SAS' %in% pops) {
      SAS <- make_plot_maf(ref_1000G = c("SAS"), target_dat = i.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      SAS <- NULL
      txt <- gsub('SAS, ', '', txt)
    }
    if ('ALL' %in% pops) {
      ALL <- make_plot_maf(ref_1000G = c("ALL"), target_dat = i.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      ALL <- NULL
      txt <- gsub('ALL, ', '', txt)
    }
    
    gg <- eval(parse(text = txt))
    title <- cowplot::ggdraw() + cowplot::draw_label(Title,  fontface = "plain", x = 0, hjust = 0, size = db.title_size()) + ggplot2::theme(plot.margin = ggplot2::margin(0, 0,  0, 7))
    gg <- cowplot::plot_grid(title, gg, ncol = 1, rel_heights = c(0.05, 1))
    y.grob <- grid::textGrob(Ylab, gp = grid::gpar(col = '#000000', fontsize = db.title_y_size()), rot = 90)
    x.grob <- grid::textGrob('', gp = grid::gpar(col = '#000000', fontsize = db.title_y_size()))
    gg <- gridExtra::arrangeGrob(gg, left = y.grob, bottom = x.grob)
    gg <- ggpubr::as_ggplot(gg)
    
    message(glue('{Sys.time()} Plot Success'))
    gg
  }
  
  # Upload Status
  IMDF <- reactive({
    req(input$fileUpload)
  })
  
  # 点击 Import 按钮
  observeEvent(input$btnImport, {
    
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
        style = "text-align: center;",
        tags$p("Please wait while data is being processed."),
        tags$div(class = "loader")
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
      aesthetics <- c('SNP', 'effect_allele', 'other_allele', 'eaf')
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
          selected = list(
            SNP = MRanalysisBase::multi_pattern_match(names(data), list("SNP", "rsid", "rs", "id")),
            effect_allele = MRanalysisBase::multi_pattern_match(names(data), list("effect_allele", c("effect", "allele"), c("alternative", "allele"), "ALT")),
            other_allele = MRanalysisBase::multi_pattern_match(names(data), list("other_allele", c("other", "allele"), c("non-effect", "allele"), c("reference", "allele"), "REF")),
            eaf = MRanalysisBase::multi_pattern_match(names(data), list("eaf", "AF", 'freq'))
          ),
          badge = TRUE,
          copySource = FALSE,
          width = "100%",
          height = "70px",
          replace = FALSE
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
          replace = FALSE
        )
      }
    })
    
    shinyjs::show('is_add_button')
    removeModal()
  })
  
  
  # Draw plot
  shinyjs::show('io-albert-gg')
  output[['io.albert.gg']] <- renderUI({
    plotOutput(outputId = 'io.albert.gg.o', height = db.height(), width = db.width()) %>% withSpinner(type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35')
  })
  
  # 点击 Check 按钮
  observeEvent(input$btnPlot, {
    
    shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
    
    if (is.null(input$fileUpload)) {
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
        style = "text-align: center;",
        tags$p("Please wait while data is being processed."),
        tags$div(class = "loader")
      ),
      easyClose = FALSE,
      footer = NULL
    ))
    
    e.data <- i.data() %>% dplyr::select(
      input[['dragvars']]$target$SNP,
      input[['dragvars']]$target$effect_allele,
      input[['dragvars']]$target$other_allele,
      input[['dragvars']]$target$eaf
    )
    
    colnames(e.data) <- c('SNP', 'effect_allele', 'other_allele', 'eaf')
    e.data <- e.data[e.data$SNP %in% snplist, ]
    s.data(e.data)
    
    tmp <- i.data() %>% as.data.frame()
    tmp <- eval(parse(text = glue('tmp[tmp${input[["dragvars"]]$target$SNP} %in% snplist, ]')))
    a.data(tmp)
    
    shinyjs::show('modify_plot_params')
    shinyjs::show('intro_box')
    shinyjs::show('tabledatahere')
    
    if (display.status()) {
      shiny::insertUI(
        selector = '#tabledatahere',
        where = 'beforeBegin',
        immediate = TRUE,
        ui = column(
          width = 12, 
          id = 'datatable_tbl_box', 
          tags$hr(),
          withSpinner({ 
            DT::dataTableOutput(outputId = 'datatable') 
          }, type = 8, 
          color = '#EE3F4D', 
          size = 1, 
          color.background = '#2F2F35',
          caption = 'Loading data, please be patient...')
        )
      )
    }
    
    shinyjs::show('io-albert-gg')
    output[['io.albert.gg.o']] <- renderPlot({
      spsComps::shinyCatch({ 
        plot.reactive()
      }, trace_back = FALSE)
    })
    
    # datatable
    output[['datatable']]  <- DT::renderDT({
      a.data() %>% formattable::formattable(x = .) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single',
        extensions = 'Buttons', 
        options = list(
          dom = 'Bfrtlip', scrollX = TRUE, paging = TRUE, 
          buttons = list(
            list(extend = 'copy', filename =  'checked_data', title = 'checked_data', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'checked_data', title = 'checked_data', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'collection',
                 buttons = list(
                   list(extend = 'csv', filename = 'checked_data', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                   list(extend = 'excel', filename = 'checked_data', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))), text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    plot.status(TRUE)
    removeModal()
  })
  
  # 下载图片
  # 下载图片 TIFF
  # ------------------------------------------------------------------------------------------------------------
  thisp <- 'check'
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
    '# Load your own data. Here use "data.table::fread" and "sumstats.csv" for example.'
    # i.data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/plugins/CheckSumStats/www/data/sumstats.csv')
    i.data <- data.table::fread('path/to/your/data')
    '# Filter data'
    i.data <- i.data %>% 
      as.data.frame()%>% 
      dplyr::select(
      ..(input[['dragvars']]$target$SNP),
      ..(input[['dragvars']]$target$effect_allele),
      ..(input[['dragvars']]$target$other_allele),
      ..(input[['dragvars']]$target$eaf)
    )
    a.data <- eval(parse(text = paste0('tmp[tmp$', ..(input[["dragvars"]]$target$SNP), ' %in% snplist, ]')))
    '# Format data'
    colnames(i.data) <- c('SNP', 'effect_allele', 'other_allele', 'eaf')
    utils::data("refdat_1000G_superpops")
    snplist <- unique(refdat_1000G_superpops$SNP)
    s.data <- i.data[i.data$SNP %in% snplist, ]
    s.data <- na.omit(s.data)
    s.data$p_value <- s.data$eaf
    '# Get the trait summary data ready for the QC checks.'
    c.data <- CheckSumStats::format_data(dat = s.data, rsid = "SNP", effect_allele = "effect_allele", other_allele = "other_allele", eaf = "eaf", p = "p_value")
    '# Set plotting parameters.'
    Title = "Allele frequency in test dataset vs 1000 genomes super populations"
    Ylab = 'Allele frequency in test dataset'
    common <- theme_classic() + theme(
      axis.text = element_text(size = ..(db.axis_text_size())),
      axis.title.x = element_text(size = ..(db.axis_title_size()), vjust = -1),
      plot.margin = margin(t = 25, l = 20, r = 20, b = 25, unit = 'pt')
    )
    '# Draw Plot'
    ncol <- ..(db.ncol())
    txt <- glue('plot_grid(AFR, AMR, EAS, EUR, SAS, ALL, ncol = {ncol})')
    pops <- ..(input[['population']])
    if ('AFR' %in% pops) {
      AFR <- make_plot_maf(ref_1000G = c("AFR"), target_dat = c.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      AFR <- NULL
      txt <- gsub('AFR, ', '', txt)
    }
    if ('AMR' %in% pops) {
      AMR <- make_plot_maf(ref_1000G = c("AMR"), target_dat = c.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      AMR <- NULL
      txt <- gsub('AMR, ', '', txt)
    }
    if ('EAS' %in% pops) {
      EAS <- make_plot_maf(ref_1000G = c("EAS"), target_dat = c.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      EAS <- NULL
      txt <- gsub('EAS, ', '', txt)
    }
    if ('EUR' %in% pops) {
      EUR <- make_plot_maf(ref_1000G = c("EUR"), target_dat = c.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      EUR <- NULL
      txt <- gsub('EUR, ', '', txt)
    }
    if ('SAS' %in% pops) {
      SAS <- make_plot_maf(ref_1000G = c("SAS"), target_dat = c.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      SAS <- NULL
      txt <- gsub('SAS, ', '', txt)
    }
    if ('ALL' %in% pops) {
      ALL <- make_plot_maf(ref_1000G = c("ALL"), target_dat = c.data) + labs(y = NULL, title = NULL, subtitle = NULL) + guides(color = 'none') + common
    } else {
      ALL <- NULL
      txt <- gsub('ALL, ', '', txt)
    }
    
    gg <- eval(parse(text = txt))
    title <- cowplot::ggdraw() + 
      cowplot::draw_label(Title,  fontface = "plain", x = 0, hjust = 0, size = ..(db.title_size())) + 
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0,  0, 7))
    gg <- cowplot::plot_grid(title, gg, ncol = 1, rel_heights = c(0.05, 1))
    y.grob <- grid::textGrob(Ylab, gp = grid::gpar(col = '#000000', fontsize = ..(db.title_y_size())), rot = 90)
    x.grob <- grid::textGrob('', gp = grid::gpar(col = '#000000', fontsize = ..(db.title_y_size())))
    gg <- gridExtra::arrangeGrob(gg, left = y.grob, bottom = x.grob)
    ggpubr::as_ggplot(gg)
  })
  
  # Show Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(glue)
        library(dplyr)
        library(ggplot2)
        library(cowplot)
        library(CheckSumStats)
      }),
      spsComps::shinyCatch({ output[['workflow_code']]() }, trace_back = TRUE)
    )
    
    displayCodeModal(
      code = formatR::tidy_source(text = as.character(code), args.newline = TRUE, output = FALSE)$text.tidy,
      title = "Code to reproduce data and plot",
      clip = NULL,
      size = 's'
    )
  })
  #+++++++++++++++++++++++++++++++  Show Code End ++++++++++++++++++++++++++++++
  
  # Show sessionInfo()
  observeEvent(input[['show_sessioninfo']], {
    displayCodeModal(
      code = as.character(xfun::session_info(c('glue', 'dplyr', 'ggplot2', 'CheckSumStats', 'cowplot'))),
      title = "Collect Information About the Current R Session",
      clip = NULL,
      size = 'l'
    )
  })
  
  # Download demo data
  output$downloadDemo <- downloadHandler(
    filename = function() {
      "sumstats.csv"
    },
    content = function(file) {
      write.csv(demo_data, file, row.names = FALSE)
    }
  )
  
  observeEvent({
    input[['fileUpload']]
  }, {
    req(input$fileUpload)
    shinyjs::show('is_add_import')
  })
  
  observeEvent({
    input[['fileUpload_click']]
  }, {
    shinyjs::hide('ui_aesthetics')
    shinyjs::hide('is_add_import')
    shinyjs::hide('is_add_button')
    shinyjs::hide('modify_plot_params')
    shinyjs::hide('intro_box')
    shinyjs::hide('tabledatahere')
    shinyjs::hide('io-albert-gg')
    shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
  })
  
}

# Run the application 
# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui, server = server)