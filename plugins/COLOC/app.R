#' @title Colocalization Analysis Tool
#'
#' @description A Shiny Application for Colocalization Analysis

# Load R packages
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(locuscomparer)))
suppressMessages(suppressWarnings(library(shinyWidgets)))

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

options(shiny.maxRequestSize = tryCatch(if (Sys.getenv("MRANALYSIS_MAX_SIZE") == '') 100000 * 1024^2 else eval(parse(text = Sys.getenv("MRANALYSIS_MAX_SIZE"))), error = function(e) 100000 * 1024^2))

dragulaInput. <- MRanalysisBase::dragulaInput.
environment(dragulaInput.) <- getNamespace("esquisse")
if (bindingIsLocked("dragulaInput", getNamespace("esquisse"))) {
  unlockBinding("dragulaInput", getNamespace("esquisse"))
}
assignInNamespace(x = "dragulaInput", value = dragulaInput., ns = "esquisse")

# COLOC.HyPrColoc - TODO
# Pairwise colocalization method, evaluates shared signals between trait pairs. - TODO
coloc.methods <- shinyWidgets::prepare_choices(
  data.frame(
    name = c('COLOC.ABF', 'COLOC.SUSIE', 'COLOC.PWCoCo', 'COLOC.eCAVIAR'),
    abb = c('COLOC.ABF', 'COLOC.SUSIE', 'COLOC.PWCoCo', 'COLOC.eCAVIAR'),
    description = c(
      'Bayesian framework estimating probability two traits share causal variant.',
      'Uses Sum of Single Effects model for complex multi-SNP colocalization.',
      'Hierarchical clustering approach identifies shared causal variants across traits.',
      'CAusal Variants Identication in Associated Regions.'
    )
  ),
  label = name, 
  value = abb,
  description = description
)

# 
out.setting <- shinyWidgets::prepare_choices(
  data.frame(
    name = c('csv', 'txt', 'tsv', 'csv.gz', 'txt.gz', 'tsv.gz'),
    abb = c('csv', 'txt', 'tsv', 'csv.gz', 'txt.gz', 'tsv.gz'), 
    description = c('Comma-separated values text file', 'Tab-separated values text file', 'Tab-separated values text file', 'Gzipped comma-separated values text file', 'Gzipped tab-separated values text file', 'Gzipped tab-separated values text file')
  ),
  label = name, 
  value = abb,
  description = description
)

# 加载数据组装代码
get_load_code <- function(fileext, fname, varname) {
  if (is.null(fileext)) {
    code <- substitute(var <- data.table::fread(file, verbose = FALSE), list(var = as.name(varname), file = fname))
  } else if (endsWith(fileext, ".vcf.gz") || endsWith(fileext, ".vcf")) {
    code <- substitute(var <- GWASkitR::read.vcf(file, verbose = FALSE), list(var = as.name(varname), file = fileext))
  } else {
    code <- substitute(var <- data.table::fread(file, verbose = FALSE), list(var = as.name(varname), file = fileext))
  }
  code
}

# 路径拼接函数
file.path2 <- function(...) {
  gsub("\\\\", "/", file.path(...))
}

# UI ----
# UI function for the Shiny application
ui.COLOC <- shiny::fluidPage(
  title = 'COLOC',
  shiny::tags$div(
    style = "display: flex; align-items: center;",
    shiny::titlePanel('COLOCALIZATION ANALYSIS'),
    shiny::actionButton(
      inputId = 'help',
      label = 'Help',
      icon = shiny::icon('question-circle'),
      class = "down",
      style = "margin-left: 20px; width: 80px !important; min-width: 80px !important; display: inline-block;"
    ),
    shiny::actionButton(
      inputId = 'show_codes',
      label = 'Show codes',
      class = "down",
      style = "margin-left: 20px; width: 100px !important; min-width: 80px !important; display: inline-block;"
    )
  ),
  shinyjs::useShinyjs(),
  tags$head(
    includeCSS(system.file('extdata', 'CSS/shiny-style.css', package = 'MRanalysisBase')),
    HTML("<html lang='en'>"),
    tags$link(rel = 'shortcut icon', href = '/XINGABAO/img/favicon.ico'),
    MRanalysisBase::web.statistic.baidu,
    tags$script(HTML("
      Shiny.addCustomMessageHandler('disableFile', function(message) {
        $('#' + message.id).prop('disabled', true);
        $('#' + message.id).parent().addClass('disabled');
      });
    ")),
    MRanalysisBase::disable_elements_js,
    MRanalysisBase::disable_elements_css,
    MRanalysisBase::sw_dropdown_js,
    MRanalysisBase::sw_dropdown_css,
    tags$style(HTML("
      h2 {
        margin-left: 20px !important;
      }
      .sidebar {
        background-color: #FFFFFF !important;
        margin-bottom: -10px !important;
        padding: 20px;
        border-radius: 10px;
      }
      .datamods-imports {
        margin-left: 23px !important;
        margin-right: 20px !important;
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
      #submitBtnUI1, #submitBtnUI2, #convertBtnUI, #downloadBtnUI, #demoBtnUI {
        text-align: center;
        margin-top: 15px;
      }
      #demoBtnUI {
        margin-bottom: 50px;
      }
      .btn-convert, .btn-download {
        background-color: #E50914 !important;
      }
      .btn-custom {
        background-color: #286090;
        border-color: #204d74;
        color: #fff;
        line-height: 1.42857143;
        border: none;
        padding: 6px 12px;
        border-radius: 4px;
        font-size: 14px;
        cursor: pointer;
        width: 150px !important;
        transition: background 0.2s;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
    .btn-custom:hover {
      background-color: #286090;
      color: #fff;
    }
    .btn-custom:focus {
      background-color: #D32F2F;
      color: #fff;
    }
    #datatable_tbl_box {
      margin-top: -20px !important;
    }
    .btn-file.disabled {
      pointer-events: none;
      background-color: #eee;
      color: #aaa;
      border-color: #ccc;
    }
    .sw-dropdown-in > div {
      margin-bottom: 7px;
    }
    .sw-dropdown-in > div:last-child {
      margin-bottom: 7px;
    }
    .vscomp-dropbox-container {
      z-index: 101 !important;
    }
    .shiny-input-container {
      margin-bottom: 10px;
    }
    #tbl_summary_box, #tbl_coloc_box td {
	    white-space: nowrap !important;
    }
    "))
  ),
  shiny::uiOutput(outputId = "output_setting_ui"),
  shiny::sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 4,
      class = 'sidebar',
      shiny::uiOutput("demoBtnUI"),
      shiny::uiOutput("validation_message"),
      h4("Step 1: Selecting a colocalization method."),
      shinyWidgets::virtualSelectInput(
        inputId = "coloc_method",
        label = NULL, 
        choices = coloc.methods,
        hasOptionDescription = TRUE,
        multiple = FALSE,
        dropboxWrapper = "body",
        width = "100%"
      ),
      shiny::tagList(
        tags$br(),
        shiny::tags$div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          h4("Step 2: Upload the GWAS sumstats."),
          div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
              shiny::actionButton(inputId = 'use_demo_data', label = 'Use demo data', class = 'btn-warning')
          ),
          div(
            id = 'runBtnUI',
            shiny::actionButton(
              inputId = 'run_analysis',
              label = 'Run Analysis',
              icon = shiny::icon('circle-play'),
              class = "btn-custom btn-convert",
              style = "margin-left: 40px; width: 130px !important; min-width: 80px !important; display: inline-block;"
            )
          ),
          div(
            id = 'downloadBtnUI',
            shiny::downloadButton(
              outputId = 'download_results',
              label = 'Download',
              icon = shiny::icon('circle-play'),
              class = "btn-custom btn-convert",
              style = "background-color: #00A087 !important; margin-left: 40px; width: 130px !important; min-width: 80px !important; display: inline-block;"
            )
          )
        ),
        shiny::fileInput(
          inputId = 'upload_data1', 
          label = 'First dataset (e.g., eQTL data).',
          multiple = FALSE,
          width = '100%', 
          placeholder = 'Please upload your GWAS summary data.', 
          accept = c('.vcf', '.gz', '.csv', '.tsv', '.txt'))
      ),
      shinycssloaders::withSpinner(ui_element = shiny::uiOutput("submitBtnUI1"), proxy.height = '25px', type = 7, size = 0.5, caption = 'Reading file ...'),
      shiny::uiOutput("showexplations"),
      shiny::uiOutput(outputId = 'ui_aesthetics1'),
      shiny::tags$br(),
      shiny::fileInput(
        inputId = 'upload_data2', 
        label = 'Second dataset (e.g., GWAS data).',
        multiple = FALSE,
        width = '100%', 
        placeholder = 'Please upload your GWAS summary data.', 
        accept = c('.vcf', '.gz', '.csv', '.tsv', '.txt')
      ),
      shinycssloaders::withSpinner(ui_element = shiny::uiOutput("submitBtnUI2"), proxy.height = '25px', type = 7, size = 0.5, caption = 'Reading file ...'),
      shiny::uiOutput(outputId = 'ui_aesthetics2')
    ),
    mainPanel = column(
      width = 8,
      id = 'right_results',
      shiny::column(width = 12, id = 'plotdatahere'),
      tags$br(),
      shiny::column(width = 12, id = 'tabledatahere')
    )
  )
)

# Server function for handling logic and reactivity
# Service ----
service.COLOC <- function(input, output, session) {
  
  disable_elements <- function(ids = NULL, classes = NULL, session) {
    session$sendCustomMessage("disable_elements", list(
      ids = unname(as.list(ids)),
      classes = unname(as.list(classes))
    ))
  }
  enable_elements <- function(ids = NULL, classes = NULL, session) {
    session$sendCustomMessage("enable_elements", list(
      ids = unname(as.list(ids)),
      classes = unname(as.list(classes))
    ))
  }
  
  shinyjs::hide('runBtnUI')
  shinyjs::hide('demoBtnUI')
  shinyjs::hide('downloadBtnUI')
  shinyjs::hide('ui_aesthetics1')
  shinyjs::hide('ui_aesthetics2')
  shinyjs::hide('show_codes')
  
  # Reactive value to store values ----
  validation.status <- shiny::reactiveVal(list(is_valid = TRUE, message = "First, please '<b><u><span style='color:red;'>select</span></u></b>' the colocalization method below."))
  download.status <- reactiveVal(value = FALSE)
  dat.load.name1 <- shiny::reactiveVal(value = NULL)
  dat.load.part1 <- shiny::reactiveVal(value = NULL)
  dat.load.part2 <- shiny::reactiveVal(value = NULL)
  dat.load.full1 <- shiny::reactiveVal(value = NULL)
  file.ready1 <- reactiveVal(value = FALSE)
  imported.data1 <- reactiveVal(value = NULL)
  converted.data1 <- reactiveVal(value = NULL)
  randomid <- reactiveVal(as.numeric(Sys.time()))
  COLOC.ana <- reactiveVal(value = FALSE)
  COLOC.plot <- reactiveVal(value = FALSE)
  COLOC.locuscompare <- reactiveVal(value = FALSE)
  COLOC.geni <- reactiveVal(value = FALSE)
  exposure.filepath <- reactiveVal(NULL)
  outcome.filepath <- reactiveVal(NULL)
  db.sample.size.e.c.r <- reactiveVal(NULL)
  db.sample.size.o.c.r <- reactiveVal(NULL)
  db.case.size.e.c.r <- reactiveVal(NULL)
  db.case.size.o.c.r <- reactiveVal(NULL)
  run.status <- reactiveVal(value = TRUE)
  sensitive.methods <- reactiveVal(value = c('COLOC.ABF', 'COLOC.SUSIE'))
  geni.plots.methods <- reactiveVal(value = c('COLOC.ABF-', 'COLOC.SUSIE-', 'COLOC.eCAVIAR-'))
  SNP.rel <- reactiveVal(NULL)
  
  # renderUI ----
  output$submitBtnUI1 <- renderUI({
    if (isTruthy(input$upload_data1) || isTruthy(exposure.filepath())) {
      shiny::actionButton("submit_data1", "Submit", class = "btn-custom")
    }
  })
  
  # Submit button
  output$submitBtnUI2 <- renderUI({
    if (isTruthy(input$upload_data2) || isTruthy(outcome.filepath())) {
      shiny::actionButton("submit_data2", "Submit", class = "btn-custom")
    }
  })
  
  # Display validation message ----
  output$validation_message <- shiny::renderUI({
    msg <- validation.status()$message
    is_valid <- validation.status()$is_valid
    if (is_valid) {
      shiny::tags$span(
        HTML(msg),
        style = "color: #00A087; font-weight: bold; font-size: 14px; font-family: 'Microsoft YaHei', Arial, sans-serif;"
      )
    } else {
      shiny::tags$span(
        HTML(msg),
        style = "color: #E50914; font-weight: bold; font-size: 18px; font-family: 'Microsoft YaHei', Arial, sans-serif;"
      )
    }
  })
  
  # Use demo data ----
  observeEvent(input[['use_demo_data']], {
    
    shinyjs::hide('submitBtnUI1')
    shinyjs::hide('submitBtnUI2')
    
    exposure.filepath('demo/pQTLdat.csv.gz')
    shiny::updateTextInput(session = session, inputId = 'sample_size_exposure', label = 'Sample Size (Exposure)', value = 35373)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure', label = 'Trait name (Exposure)', value = 'pQTL')
    outcome.filepath('demo/sumstats.csv.gz')
    shiny::updateTextInput(session = session, inputId = 'sample_size_outcome', label = 'Sample Size (Outcome)', value = 318014)
    shiny::updateTextInput(session = session, inputId = 'case_size_outcome', label = 'Case Size (Outcome)', value = 33043)
    shiny::updateTextInput(session = session, inputId = 'trait_name_outcome', label = 'Trait name (Outcome)', value = 'GWAS')
    shinyjs::hide('upload_data1')
    shinyjs::hide('upload_data2')
    shinyWidgets::updateRadioGroupButtons(session = session, inputId = 'metrics_type', selected = 'beta')
    shinyWidgets::updateRadioGroupButtons(session = session, inputId = 'data_type_exposure', selected = 'quant')
    shinyWidgets::updateRadioGroupButtons(session = session, inputId = 'data_type_outcome', selected = 'cc')
    
    shinyjs::show('submitBtnUI1')
  })
  
  
  # Observe file upload and validate inputs ----
  observeEvent(input$upload_data1, {
    
    shinyjs::hide('demoBtnUI')
    shinyjs::hide('use_demo_data')
    
    session$sendCustomMessage('disableFile', list(id = "upload_data1"))
    
    if (!is.null(input$upload_data1)) {
      
      file_ext <- tools::file_ext(input$upload_data1$name)
      allowed_exts <- c("vcf", "gz", "csv", "tsv", "txt")
      
      if (tolower(file_ext) %in% allowed_exts) {
        validation.status(list(is_valid = TRUE, message = "File uploaded successfully and format is valid. Then, click the '<b><u><span style='color:red;'>Submit</span></u></b>' button to proceed to the next step."))
      } else {
        err_msg <- "Invalid file format. Please upload a file with extension .vcf, .gz, .csv, .tsv, or .txt."
        validation.status(list(is_valid = FALSE, message = err_msg))
        stop(err_msg)
      }
      
      if (file_ext == 'vcf' || endsWith(input$upload_data1$name, ".vcf.gz")) {
        sumstats <- GWASkitR::read.vcf(input$upload_data1$datapath, nrows = 1000, verbose = FALSE)
      } else {
        sumstats <- data.table::fread(input$upload_data1$datapath, nrows = 1000)
      }
      dat.load.part1(sumstats)
    } 
    else {
      validation.status(list(is_valid = FALSE, message = "No file uploaded."))
    }
  })
  
  # Observe file upload and validate inputs
  observeEvent(input$upload_data2, {
    
    session$sendCustomMessage('disableFile', list(id = "upload_data2"))
    # shinyjs::hide('convertBtnUI')
    # shinyjs::hide('downloadBtnUI')
    # shinyjs::hide('right_results')
    # shinyjs::hide('output_setting_ui')
    # session$sendCustomMessage('disableFile', list(id = "upload_data1"))
    
    if (!is.null(input$upload_data2)) {
      
      file_ext <- tools::file_ext(input$upload_data2$name)
      allowed_exts <- c("vcf", "gz", "csv", "tsv", "txt")
      
      if (tolower(file_ext) %in% allowed_exts) {
        validation.status(list(is_valid = TRUE, message = "File uploaded successfully and format is valid. Then, click the '<b><u><span style='color:red;'>Submit</span></u></b>' button to proceed to the next step."))
      } else {
        err_msg <- "Invalid file format. Please upload a file with extension .vcf, .gz, .csv, .tsv, or .txt."
        validation.status(list(is_valid = FALSE, message = err_msg))
        stop(err_msg)
      }
      
      if (file_ext == 'vcf' || endsWith(input$upload_data2$name, ".vcf.gz")) {
        sumstats <- GWASkitR::read.vcf(input$upload_data2$datapath, nrows = 1000, verbose = FALSE)
      } else {
        sumstats <- data.table::fread(input$upload_data2$datapath, nrows = 1000)
      }
      # dat.load.name1('sumstats')
      dat.load.part2(sumstats)
    } else {
      validation.status(list(is_valid = FALSE, message = "No file uploaded."))
    }
  })
  
  # Handle submit button action ----
  observeEvent(input$submit_data1, {
    
    shinyjs::runjs('$("#coloc_method").css({"pointer-events":"none", "user-select":"none"});')
    
    output[['showexplations']] <- renderUI({
      tags$p(
        glue::glue('For quick extraction and manipulation, only a subset (about 1,000 rows) is displayed here.'),
        style = "color: #E50914; font-weight: bold; font-size: 14px; font-family: 'Microsoft YaHei', Arial, sans-serif;"
      )
    })
    
    if (!is.null(exposure.filepath())) {
      dat.load.part1(data.table::fread(exposure.filepath(), nrows = 1000))
      validation.status(list(is_valid = TRUE, message = "Use demo data. Then, click the '<b><u><span style='color:red;'>Submit</span></u></b>' button to proceed to the next step."))
      shinyjs::hide('use_demo_data')
      shinyjs::show('submitBtnUI2')
    }
    
    # validation.status(list(is_valid = TRUE, message = "Next, click the '<b><u><span style='color:red;'>Import data</span></u></b>' button to load the data."))
    
    shinyjs::hide('submitBtnUI1')
    shinyjs::hide('upload_gwas_ui1')
    # shinyjs::hide('convertBtnUI')
    # shinyjs::hide('downloadBtnUI')
    # shinyjs::hide('output_setting_ui')
    shinyjs::show('ui_aesthetics1')
    shiny::removeUI(selector = '#coloc_tbl_box', immediate = TRUE)
    shiny::removeUI(selector = '#coloc_plot_box', immediate = TRUE)
    
    # Choose Variable
    output[['ui_aesthetics1']] <- renderUI({
      
      aesthetics <- c('SNP', 'CHR', 'POS', 'effect_allele', 'other_allele', 'OR/beta/Z', 'eaf', 'se', 'pval')
      data = dat.load.part1() %>% as.data.frame()
      
      if (!is.null(data)) {
        var_choices <- setdiff(names(data), attr(data, "sf_column"))
        esquisse::dragulaInput(
          inputId = "dragvars1",
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
            CHR = MRanalysisBase::multi_pattern_match(names(data), list("CHR")),
            POS = MRanalysisBase::multi_pattern_match(names(data), list("POS", 'BP', 'start')),
            effect_allele = MRanalysisBase::multi_pattern_match(names(data), list("effect_allele", c("effect", "allele"), c("alternative", "allele"), "ALT")),
            other_allele = MRanalysisBase::multi_pattern_match(names(data), list("other_allele", c("other", "allele"), c("non-effect", "allele"), c("reference", "allele"), "REF")),
            eaf = MRanalysisBase::multi_pattern_match(names(data), list("eaf", "AF", 'freq')),
            `OR/beta/Z` = MRanalysisBase::multi_pattern_match(names(data), list("OR", "b", "beta", 'es', c('effect', 'size'), 'Z')),
            se = MRanalysisBase::multi_pattern_match(names(data), list('se', c('standard', 'error'))),
            pval = MRanalysisBase::multi_pattern_match(names(data), list("pval", "p", 'LP', 'log10P')),
            nsample = MRanalysisBase::multi_pattern_match(names(data), list("SS", 'sample', 'N')),
            ncontrol = MRanalysisBase::multi_pattern_match(names(data), list("control")),
            ncase = MRanalysisBase::multi_pattern_match(names(data), list("case"))
          ),
          badge = TRUE,
          copySource = FALSE,
          ncolGrid = 5,
          width = "100%",
          height = "50px",
          replace = FALSE
        )
      }
    })
  })
  
  # Handle submit button action
  observeEvent(input$submit_data2, {
    
    if (!is.null(outcome.filepath())) {
      dat.load.part2(data.table::fread(outcome.filepath(), nrows = 1000))
      validation.status(list(is_valid = TRUE, message = "Use demo data. Then, click the '<b><u><span style='color:red;'>Submit</span></u></b>' button to proceed to the next step."))
    }
    
    shinyjs::hide('submitBtnUI2')
    # shinyjs::hide('downloadBtnUI')
    # shinyjs::hide('output_setting_ui')
    shinyjs::show('ui_aesthetics2')
    shiny::removeUI(selector = '#coloc_tbl_box', immediate = TRUE)
    shiny::removeUI(selector = '#coloc_plot_box', immediate = TRUE)
    
    # Choose Variable
    output[['ui_aesthetics2']] <- renderUI({
      
      aesthetics <- c('SNP', 'CHR', 'POS', 'effect_allele', 'other_allele', 'OR/beta/Z', 'eaf', 'se', 'pval')
      data = dat.load.part2() %>% as.data.frame()
      
      if (!is.null(data)) {
        var_choices <- setdiff(names(data), attr(data, "sf_column"))
        esquisse::dragulaInput(
          inputId = "dragvars2",
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
            CHR = MRanalysisBase::multi_pattern_match(names(data), list("CHR")),
            POS = MRanalysisBase::multi_pattern_match(names(data), list("POS", 'BP', 'start')),
            effect_allele = MRanalysisBase::multi_pattern_match(names(data), list("effect_allele", c("effect", "allele"), c("alternative", "allele"), "ALT")),
            other_allele = MRanalysisBase::multi_pattern_match(names(data), list("other_allele", c("other", "allele"), c("non-effect", "allele"), c("reference", "allele"), "REF")),
            eaf = MRanalysisBase::multi_pattern_match(names(data), list("eaf", "AF", 'freq')),
            `OR/beta/Z` = MRanalysisBase::multi_pattern_match(names(data), list("OR", "b", "beta", 'es', c('effect', 'size'), 'Z')),
            se = MRanalysisBase::multi_pattern_match(names(data), list('se', c('standard', 'error'))),
            pval = MRanalysisBase::multi_pattern_match(names(data), list("pval", "p", 'LP', 'log10P')),
            nsample = MRanalysisBase::multi_pattern_match(names(data), list("SS", 'sample', 'N')),
            ncontrol = MRanalysisBase::multi_pattern_match(names(data), list("control")),
            ncase = MRanalysisBase::multi_pattern_match(names(data), list("case"))
          ),
          badge = TRUE,
          copySource = FALSE,
          ncolGrid = 5,
          width = "100%",
          height = "50px",
          replace = FALSE
        )
      }
    })
  })
  
  # Perform Coloclocation Analysis ----
  observeEvent(input$run_analysis, {
    
    shinyjs::hide('validation_message')
    
    disable_elements(
      ids = c('show_data-tabs-mode', 'dragvars-source-container', 'output_setting_ui', 'downloadBtnUI'), 
      classes = c("dragula-target", 'vscomp-value'),
      session
    )
    
    db.sample.size.e <- reactive({ isolate(input[['sample_size_exposure']]) })
    db.sample.size.o <- reactive({ isolate(input[['sample_size_outcome']]) })
    db.case.size.e <- reactive({ isolate(input[['case_size_exposure']]) })
    db.case.size.o <- reactive({ isolate(input[['case_size_outcome']]) })
    db.sample.size.e.c <- tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    db.sample.size.o.c <- tryCatch(as.numeric(db.sample.size.o()), error = function(e) NA)
    db.case.size.e.c <- tryCatch(as.numeric(db.case.size.e()), error = function(e) NA)
    db.case.size.o.c <- tryCatch(as.numeric(db.case.size.o()), error = function(e) NA)
    db.sample.size.e.c.r(db.sample.size.e.c)
    db.sample.size.o.c.r(db.sample.size.o.c)
    db.case.size.e.c.r(db.case.size.e.c)
    db.case.size.o.c.r(db.case.size.e.c)
    db.metrics.type <- reactive({ isolate(input[['metrics_type']]) })
    db.data.type.e <- reactive({ isolate(input[['data_type_exposure']]) })
    db.data.type.o <- reactive({ isolate(input[['data_type_outcome']]) })
    
    if (is.na(db.sample.size.e.c) | is.na(db.sample.size.o.c)) {
      spsComps::shinyCatch({ base::stop('The sample size must be provided.') }, trace_back = FALSE)
      run.status(FALSE)
    } else if (length(db.metrics.type()) == 0) {
      spsComps::shinyCatch({ base::stop('The metrics type (one of beta, OR or Z) must be provided.') }, trace_back = FALSE)
      run.status(FALSE)
    } else if (length(db.data.type.e()) == 0) {
      spsComps::shinyCatch({ base::stop('The data type of exposure (one of quant or cc) must be provided.') }, trace_back = FALSE)
      run.status(FALSE)
    } else if (length(db.data.type.o()) == 0) {
      spsComps::shinyCatch({ base::stop('The data type of outcome (one of quant or cc) must be provided.') }, trace_back = FALSE)
      run.status(FALSE)
    } else if ((input[['data_type_exposure']] == 'cc' & is.na(db.case.size.e.c)) | (input[['data_type_outcome']] == 'cc' & is.na(db.case.size.o.c))) {
      spsComps::shinyCatch({ base::stop('The Case size must be provided if data type is cc.') }, trace_back = FALSE)
      run.status(FALSE)
    } else {
      run.status(TRUE)
    }
    
    if (run.status()) {
      
      shinyjs::hide('runBtnUI')
      
      # Collect the required columns
      .cols.1 <- c(
        input[["dragvars1"]]$target[['SNP']], input[["dragvars1"]]$target[['CHR']], input[["dragvars1"]]$target[['POS']], 
        input[["dragvars1"]]$target[['effect_allele']], input[["dragvars1"]]$target[['other_allele']], input[["dragvars1"]]$target[['eaf']], 
        input[["dragvars1"]]$target[['OR/beta/Z']], input[["dragvars1"]]$target[['se']], input[["dragvars1"]]$target[['pval']], 
        input[["dragvars1"]]$target[['MAF']]
      )
      
      .cols.2 <- c(
        input[["dragvars2"]]$target[['SNP']], input[["dragvars2"]]$target[['CHR']], input[["dragvars2"]]$target[['POS']], 
        input[["dragvars2"]]$target[['effect_allele']], input[["dragvars2"]]$target[['other_allele']], input[["dragvars2"]]$target[['eaf']], 
        input[["dragvars2"]]$target[['OR/beta/Z']], input[["dragvars2"]]$target[['se']], input[["dragvars2"]]$target[['pval']], 
        input[["dragvars2"]]$target[['MAF']]
      )
      
      # Remove NULLs from .cols.1 to avoid checking for missing NULL columns
      .cols.1 <- .cols.1[!is.null(.cols.1)]
      .cols.2 <- .cols.2[!is.null(.cols.2)]
      
      # Load all data
      if (!is.null(exposure.filepath()) && !is.null(outcome.filepath())) {
        dat.full.1 <- data.table::fread(exposure.filepath())
        dat.full.2 <- data.table::fread(outcome.filepath())
      } else {
        file_ext <- tools::file_ext(input$upload_data1$name)
        if (file_ext == 'vcf' || endsWith(input$upload_data1$name, ".vcf.gz")) {
          dat.full.1 <- GWASkitR::read.vcf(input$upload_data1$datapath, verbose = FALSE)
        } else {
          dat.full.1 <- data.table::fread(input$upload_data1$datapath)
        }
        file_ext <- tools::file_ext(input$upload_data2$name)
        if (file_ext == 'vcf' || endsWith(input$upload_data2$name, ".vcf.gz")) {
          dat.full.2 <- GWASkitR::read.vcf(input$upload_data2$datapath, verbose = FALSE)
        } else {
          dat.full.2 <- data.table::fread(input$upload_data2$datapath)
        }
      }
      
      tab.panels <- list(
        tabPanel(
          title = 'Locuscompare Plot {locuscomparer}',
          shinycssloaders::withSpinner({
            uiOutput(outputId = paste0('locuscompare_plot_locuscomparer', randomid()))
          }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35',
          caption = 'Performing Colocalization Analysis, please wait. This may take a few minutes...')
        )
      )
      
      # 有条件添加
      if (input[['coloc_method']] %in% geni.plots.methods()) {
        tab.panels <- append(tab.panels, list(
          tabPanel(
            title = 'Locuscompare Plot {geni.plots}',
            shinycssloaders::withSpinner({
              uiOutput(outputId = paste0('locuscompare_plot_geni.plots', randomid()))
            }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35',
            caption = 'Performing Colocalization Analysis, please wait. This may take a few minutes...')
          )
        ))
      }
      
      # 有条件添加
      if (input[['coloc_method']] %in% sensitive.methods()) {
        tab.panels <- append(tab.panels, list(
          tabPanel(
            title = 'Sensitivity Plot',
            shinycssloaders::withSpinner({
              uiOutput(outputId = paste0('sensitivity_plot', randomid()))
            }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35',
            caption = 'Performing Colocalization Analysis, please wait. This may take a few minutes...')
          )
        ))
      }
      
      shiny::insertUI(
        selector = '#plotdatahere',
        where = 'beforeBegin',
        immediate = TRUE,
        ui = do.call(
          bs4Dash::tabBox,
          c(
            list(
              id = 'coloc_plot',
              title = NULL,
              width = 12,
              selected = 'Locuscompare Plot {locuscomparer}',
              status = 'success',
              solidHeader = FALSE,
              type = 'tabs'
            ),
            tab.panels
          )
        )
      )
      
      shiny::insertUI(
        selector = '#tabledatahere',
        where = 'beforeBegin',
        immediate = TRUE,
        ui =  bs4Dash::tabBox(
          id = 'coloc_tbl',
          title = NULL,
          width = 12,
          selected = 'Summary',
          status = 'success',
          solidHeader = FALSE,
          type = 'tabs',
          tabPanel(
            title = 'Summary',
            column(
              width = 12,
              id = 'tbl_summary_box',
              tags$hr(),
              shinycssloaders::withSpinner({ DT::dataTableOutput(outputId = paste0('tbl_summary_box', randomid())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = NULL))
          ),
          tabPanel(
            title = 'Colocalization (coloc) results table',
            column(
              width = 12,
              id = 'tbl_coloc_box',
              tags$hr(),
              shinycssloaders::withSpinner({ DT::dataTableOutput(outputId = paste0('tbl_coloc_box', randomid())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = NULL))
          ),
          tabPanel(
            title = 'Genomic region association',
            column(
              width = 12,
              id = 'tbl_association_box',
              tags$hr(),
              shinycssloaders::withSpinner({ DT::dataTableOutput(outputId = paste0('tbl_association_box', randomid())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = NULL))
          )
        )
      )
      
      ## Step 1: Perform analysis ----
      if (input[['coloc_method']] == 'COLOC.ABF') {
        COLOC.res <- spsComps::shinyCatch({ GWASkitR::COLOC.ABF(
          e.dat = dat.full.1,
          o.dat = dat.full.2,
          e.col.chr = input[["dragvars1"]]$target[['CHR']],
          e.col.pos = input[["dragvars1"]]$target[['POS']],
          e.col.snp = input[["dragvars1"]]$target[['SNP']],
          e.col.beta = input[["dragvars1"]]$target[['OR/beta/Z']],
          e.col.se = input[["dragvars1"]]$target[['se']],
          e.col.eaf = input[["dragvars1"]]$target[['eaf']],
          e.col.pval = input[["dragvars1"]]$target[['pval']],
          e.col.nsample = db.sample.size.e.c,
          e.type = input[["data_type_exposure"]], 
          e.prevalence = NA,
          o.col.snp = input[["dragvars2"]]$target[['SNP']],
          o.col.beta = input[["dragvars2"]]$target[['OR/beta/Z']],
          o.col.se = input[["dragvars2"]]$target[['se']],
          o.col.eaf = input[["dragvars2"]]$target[['eaf']],
          o.col.pval = input[["dragvars2"]]$target[['pval']],
          o.col.nsample = db.sample.size.o.c,
          o.type = input[["data_type_outcome"]],
          o.prevalence = NA
        ) })
      } else if (input[['coloc_method']] == 'COLOC.SUSIE') {
        COLOC.res <- spsComps::shinyCatch({ GWASkitR::COLOC.SUSIE(
          e.dat = dat.full.1,
          o.dat = dat.full.2,
          e.col.chr = input[["dragvars1"]]$target[['CHR']],
          e.col.pos = input[["dragvars1"]]$target[['POS']],
          e.col.snp = input[["dragvars1"]]$target[['SNP']],
          e.col.beta = input[["dragvars1"]]$target[['OR/beta/Z']],
          e.col.se = input[["dragvars1"]]$target[['se']],
          e.col.eaf = input[["dragvars1"]]$target[['eaf']],
          e.col.pval = input[["dragvars1"]]$target[['pval']],
          e.col.ncase = ifelse(test = input[["data_type_exposure"]] == 'cc', yes = db.case.size.e.c, no = NA),
          e.col.nsample = db.sample.size.e.c,
          e.type = input[["data_type_exposure"]], 
          o.col.snp = input[["dragvars2"]]$target[['SNP']],
          o.col.beta = input[["dragvars2"]]$target[['OR/beta/Z']],
          o.col.se = input[["dragvars2"]]$target[['se']],
          o.col.eaf = input[["dragvars2"]]$target[['eaf']],
          o.col.pval = input[["dragvars2"]]$target[['pval']],
          o.col.ncase = ifelse(test = input[["data_type_outcome"]] == 'cc', yes = db.case.size.o.c, no = NA),
          o.col.nsample = db.sample.size.o.c,
          o.type = input[["data_type_outcome"]],
          plink = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['population_type']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['population_type']] })))
        ) })
      } else if (input[['coloc_method']] == 'COLOC.PWCoCo') {
        temp.ddr <- ifelse(test = Sys.info()['sysname'] == 'Linux', yes = file.path(tempdir(), 'PWCoCo.data'), no = file.path(Sys.getenv('HOME'), 'PWCoCo.data'))
        temp.dir <- ifelse(test = Sys.info()['sysname'] == 'Linux', yes = file.path(tempdir(), 'PWCoCo'), no = glue("{Sys.getenv('HOME')}/PWCoCo"))
        if (!dir.exists(temp.ddr)) { dir.create(temp.ddr, recursive = TRUE) } else { unlink(temp.ddr, recursive = TRUE); dir.create(temp.ddr, recursive = TRUE) }
        if (!dir.exists(temp.dir)) { dir.create(temp.dir, recursive = TRUE) } else { unlink(temp.dir, recursive = TRUE); dir.create(temp.dir, recursive = TRUE) }
        cols1 = c(input[["dragvars1"]]$target[['SNP']], input[["dragvars1"]]$target[['effect_allele']], input[["dragvars1"]]$target[['other_allele']], 
                  input[["dragvars1"]]$target[['eaf']], input[["dragvars1"]]$target[['OR/beta/Z']], input[["dragvars1"]]$target[['se']], input[["dragvars1"]]$target[['pval']])
        cols2 = c(input[["dragvars2"]]$target[['SNP']], input[["dragvars2"]]$target[['effect_allele']], input[["dragvars2"]]$target[['other_allele']], 
                  input[["dragvars2"]]$target[['eaf']], input[["dragvars2"]]$target[['OR/beta/Z']], input[["dragvars2"]]$target[['se']], input[["dragvars2"]]$target[['pval']])
        data.table::fwrite(x = dat.full.1 %>% dplyr::select(all_of(cols1)), file = file.path(temp.ddr, 'sumstat1.txt'), sep = '\t')
        data.table::fwrite(x = dat.full.2 %>% dplyr::select(all_of(cols2)), file = file.path(temp.ddr, 'sumstat2.txt'), sep = '\t')
        COLOC.res <- spsComps::shinyCatch({ GWASkitR::COLOC.PWCoCo(
          sum_stats1 = file.path2(temp.ddr, 'sumstat1.txt'),
          sum_stats2 = file.path2(temp.ddr, 'sumstat2.txt'),
          p_cutoff1 = as.numeric(input[["p_cutoff1"]]),
          p_cutoff2 = as.numeric(input[["p_cutoff2"]]),
          top_snp = as.numeric(input[["top_snp"]]),
          ld_window = as.numeric(input[["ld_window"]]),
          collinear = as.numeric(input[["collinear"]]),
          maf = as.numeric(input[["maf"]]),
          freq_threshold = as.numeric(input[["freq_threshold"]]),
          init_h4 = as.numeric(input[["init_h4"]]),
          coloc_pp = c(1e-04, 1e-04, 1e-05),
          n1 = db.sample.size.e.c,
          n2 = db.sample.size.o.c,
          n1_case = if (is.na(db.case.size.e.c)) NULL else db.case.size.e.c,
          n2_case = if (is.na(db.case.size.o.c)) NULL else db.case.size.o.c,
          chr = if (input[["chr"]] == '') NULL else input[["chr"]],
          out_cond = TRUE,
          pve1 = NULL,
          pve2 = NULL,
          pve_file1 = NULL,
          pve_file2 = NULL,
          threads = 4,
          snp.index = as.integer(input[["snp.index"]]),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['population_type']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['population_type']] }))),
          pwcoco = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/pwcoco/build/pwcoco', no = 'E:/tools/PWCoCo/PWCOCO.exe'),
          save_path = temp.dir,
          prefix = "pwcoco_result",
          verbose = TRUE
        ) })
        
        if (!dir.exists(temp.ddr)) { dir.create(temp.ddr, recursive = TRUE) } else { unlink(temp.ddr, recursive = TRUE); dir.create(temp.ddr, recursive = TRUE) }
      } else if (input[['coloc_method']] == 'COLOC.eCAVIAR') {
        # https://github.com/boxiangliu/hcasmc_eqtl/blob/master/eCAVIAR/ecaviar.sh
        e.dat. <- dat.full.1
        o.dat. <- dat.full.2
        # Error: The LD files for GWAS and eQTL do not have the same number of SNPs
        SNPs <- intersect(e.dat.$SNP, o.dat.$SNP)
        e.dat.o <- GWASkitR::impute_beta_or_z(
          sumstats = e.dat. %>% filter(SNP %in% SNPs),
          col.se = input[["dragvars1"]]$target[['se']],
          col.beta = input[["dragvars1"]]$target[['OR/beta/Z']]
        ) %>% dplyr::select(input[["dragvars1"]]$target[['SNP']], 
                            input[["dragvars1"]]$target[['CHR']],
                            input[["dragvars1"]]$target[['POS']], 
                            input[["dragvars1"]]$target[['pval']], 
                            Z)
        
        o.dat.o <- GWASkitR::impute_beta_or_z(
          sumstats = o.dat. %>% filter(SNP %in% SNPs),
          col.se = input[["dragvars2"]]$target[['se']],
          col.beta = input[["dragvars2"]]$target[['OR/beta/Z']]
        ) %>% dplyr::select(input[["dragvars2"]]$target[['SNP']], 
                            input[["dragvars2"]]$target[['CHR']],
                            input[["dragvars2"]]$target[['POS']], 
                            input[["dragvars2"]]$target[['pval']], 
                            Z)
        
        colnames(e.dat.o) <- c('SNP', 'CHR', 'POS', 'pval', 'Z')
        colnames(o.dat.o) <- c('SNP', 'CHR', 'POS', 'pval', 'Z')
        
        e.dat <- e.dat.o %>% dplyr::select(SNP, pval, Z)
        o.dat <- o.dat.o %>% dplyr::select(SNP, pval, Z)
        
        
        plink = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe')
        bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['population_type']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['population_type']] })))
        
        e.dat.cp <- ieugwasr::ld_clump(
          clump_kb = input[['clump_kb']],
          clump_r2 = input[['clump_r2']],
          clump_p = 0.99,
          pop = input[['population_type']],
          dplyr::tibble(rsid = e.dat$SNP, pval = o.dat$pval),
          plink_bin = plink,
          bfile = bfile
        )
        
        o.dat.cp <- ieugwasr::ld_clump(
          clump_kb = input[['clump_kb']],
          clump_r2 = input[['clump_r2']],
          clump_p = 0.99,
          pop = input[['population_type']],
          dplyr::tibble(rsid = o.dat$SNP, pval = o.dat$pval),
          plink_bin = plink,
          bfile = bfile
        )
        
        SNPs <- intersect(e.dat.cp$rsid, o.dat.cp$rsid)
        
        e.dat <- e.dat %>% filter(SNP %in% SNPs)
        o.dat <- o.dat %>% filter(SNP %in% SNPs)
        
        e.dat.ld <- tryCatch({
          ieugwasr::ld_matrix_local(
            variants = e.dat$SNP,
            bfile = bfile,
            plink_bin = plink,
            with_alleles = FALSE
          )
        })
        
        o.dat.ld <- tryCatch({
          ieugwasr::ld_matrix_local(
            variants = o.dat$SNP,
            bfile = bfile,
            plink_bin = plink,
            with_alleles = FALSE
          )
        })
        
        SNPs <- rownames(e.dat.ld)
        e.dat <- e.dat %>% filter(SNP %in% SNPs)
        o.dat <- o.dat %>% filter(SNP %in% SNPs)
        
        otdir <- file.path(MRanalysisBase::MRtempdir(), 'CAVIAR')
        if (dir.exists(otdir)) { unlink(otdir, recursive = TRUE) }
        if (!dir.exists(otdir)) { dir.create(otdir, recursive = TRUE) }
        write.table(e.dat, glue('{otdir}/e.dat.txt'), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table(o.dat, glue('{otdir}/o.dat.txt'), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table(e.dat.ld, glue('{otdir}/e.dat.ld'), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table(o.dat.ld, glue('{otdir}/o.dat.ld'), sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
        
        shell = glue::glue('/tools/caviar/CAVIAR-C++/eCAVIAR -o {otdir}/CAVIAR -l {otdir}/e.dat.ld -l {otdir}/o.dat.ld -z {otdir}/e.dat.txt -z {otdir}/o.dat.txt -r 0.95')
        system(shell, intern = FALSE)
        
        CAVIAR_col <- read.table(glue::glue('{otdir}/CAVIAR_col'), sep = '\t', header = TRUE)
        in_fn1 <- e.dat.[, c('SNP', 'pval')]
        in_fn2 <- o.dat.[, c('SNP', 'pval')]
        colnames(in_fn1) <- c('rsid', 'pval')
        colnames(in_fn2) <- c('rsid', 'pval')
        assoc <- left_join(x = e.dat.o, y = o.dat.o, by = 'SNP') %>% dplyr::select(SNP, CHR.x, POS.x, Z.x, Z.y)
        colnames(assoc) <- c('marker', 'chr', 'pos', 'z_1', 'z_2')
        
        COLOC.res <- list()
        COLOC.res[['summary']] <- CAVIAR_col[1, ]
        COLOC.res[['results']] <- CAVIAR_col
        COLOC.res[['snp.sig']] <- CAVIAR_col$SNP_ID[1]
        COLOC.res[['in_fn1']] <- in_fn1
        COLOC.res[['in_fn2']] <- in_fn2
        COLOC.res[['assoc']] <- assoc
        COLOC.res[['SNP']] <- e.dat.o$SNP
      }
      COLOC.ana(COLOC.res)
      
      ## Step 2. Organize the results for visualization ----
      if (input[['coloc_method']] == 'COLOC.ABF') {
        coloc.dat <- spsComps::shinyCatch({ GWASkitR::coloc_result_to_plot(
          x = COLOC.ana(),
          plink = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['population_type']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['population_type']] })))
        ) })
      } else if (input[['coloc_method']] == 'COLOC.SUSIE') {
        coloc.dat <- spsComps::shinyCatch({ GWASkitR::coloc_result_to_plot(
          x = COLOC.ana(),
          rs = COLOC.ana()$snp.sig[1]
        ) })
      } else if (input[['coloc_method']] == 'COLOC.PWCoCo') {
        coloc.dat <- spsComps::shinyCatch({ GWASkitR::coloc_result_to_plot(
          x = COLOC.ana(),
          rs = COLOC.ana()$snp.sig[1],
          plink = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['population_type']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['population_type']] })))
        ) })
      } else if (input[['coloc_method']] == 'COLOC.eCAVIAR') {
        ld_matrix <- tryCatch({
          ieugwasr::ld_matrix_local(
            variants = COLOC.ana()$SNP,
            bfile = bfile,
            plink_bin = plink,
            with_alleles = FALSE
          )
        })
        geni.dat <- left_join(x = e.dat.o, y = o.dat.o, by = 'SNP') %>% dplyr::select(SNP, CHR.x, POS.x, Z.x, Z.y)
        colnames(geni.dat) <- c('marker', 'chr', 'pos', 'z_1', 'z_2')
        geni.dat <- geni.dat %>% dplyr::filter(marker %in% colnames(ld_matrix))
        geni.dat <- geni.dat[match(colnames(ld_matrix), geni.dat$marker), ]
        coloc.dat <- list()
        coloc.dat[['pop']] <- input[['population_type']]
        coloc.dat[['ld_matrix']] <- ld_matrix
        coloc.dat[['geni.dat']] <- geni.dat
        coloc.dat[['in_fn1']] <- COLOC.ana()$in_fn1
        coloc.dat[['in_fn2']] <- COLOC.ana()$in_fn2
        coloc.dat[['SNP']] <- COLOC.ana()$SNP
      }
      
      COLOC.plot(coloc.dat)
      SNP.rel(COLOC.ana()$snp.sig[1])
      shinyWidgets::updateVirtualSelect(inputId = 'coloc_snp', label = 'Select SNP', choices = COLOC.plot()$geni.dat$marker, selected = SNP.rel(), session = session)
      
      ## Step 3. ----
      l.pot <- function() {
        res <- try({
          locuscomparer::locuscompare(
            in_fn1 = coloc.dat[["in_fn1"]],
            in_fn2 = coloc.dat[["in_fn2"]],
            title1 = input[['trait_name_exposure']],
            title2 = input[['trait_name_outcome']],
            genome = ifelse(input[['reference_version']] == 'hg19/GRCh37', yes = 'hg19', no = 'hg38'),
            snp = ifelse(input[['coloc_snp']] == '', yes = SNP.rel(), no = input[['coloc_snp']][1]),
            population = coloc.dat[["pop"]],
            combine = TRUE
          )
        }, silent = TRUE)
        if (inherits(res, "try-error")) {
          message("locuscompare error: ", res)
          download.status(FALSE)
          shinyjs::show('runBtnUI')
          return(NULL)
        }
        download.status(TRUE)
        res
      }
      spsComps::shinyCatch({ COLOC.locuscompare(l.pot()) })
      
      if (input[['coloc_method']] %in% geni.plots.methods()) {
        spsComps::shinyCatch({ COLOC.geni(geni.plots::fig_region_stack(
          data = coloc.dat[['geni.dat']],
          traits = c(input[['trait_name_exposure']], input[['trait_name_outcome']]),
          corr = coloc.dat[['ld_matrix']],
          build = ifelse(input[['reference_version']] == 'hg19/GRCh37', yes = 37, no = 38),
          title_center = TRUE
        )) })
      }
      
      output[[paste0('tbl_summary_box', randomid())]] <- DT::renderDT({
        (if (input[['coloc_method']] == 'COLOC.PWCoCo') COLOC.ana()$coloc.dat[as.integer(input[["snp.index"]]), ] else COLOC.ana()$summary) %>%
          formattable::formattable(x = .) -> dt
        formattable::as.datatable(
          dt,
          rownames = FALSE,
          selection = 'single',
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrti', scrollX = TRUE, paging = TRUE,
            buttons = list(
              list(
                extend = 'copy', 
                filename =  'checked_data',
                title = 'checked_data',
                exportOptions = list(modifier = list(page = 'current'))
              ),
              list(
                extend = 'print',
                filename =  'checked_data',
                title = 'checked_data', 
                exportOptions = list(modifier = list(page = 'current'))
              ))))
      })
      
      output[[paste0('tbl_coloc_box', randomid())]] <- DT::renderDT({
        (if (input[['coloc_method']] == 'COLOC.PWCoCo') COLOC.ana()$coloc.dat else COLOC.ana()$results) %>%
          formattable::formattable(x = .) -> dt
        formattable::as.datatable(
          dt,
          rownames = FALSE,
          selection = 'single',
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtlip', scrollX = TRUE, paging = TRUE,
            buttons = list(
              list(
                extend = 'copy', 
                filename =  'checked_data',
                title = 'checked_data',
                exportOptions = list(modifier = list(page = 'current'))
              ),
              list(
                extend = 'print',
                filename =  'checked_data',
                title = 'checked_data', 
                exportOptions = list(modifier = list(page = 'current'))
              )),
            lengthMenu = list(c(5, 10, 50, 100, -1), c('5', '10', '50', '100', 'All'))))
      })
      
      output[[paste0('tbl_association_box', randomid())]] <- DT::renderDT({
        COLOC.plot()$geni.dat %>%
          formattable::formattable(x = .) -> dt
        formattable::as.datatable(
          dt,
          rownames = FALSE,
          selection = 'single',
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtlip', scrollX = TRUE, paging = TRUE,
            buttons = list(
              list(
                extend = 'copy', 
                filename =  'checked_data',
                title = 'checked_data',
                exportOptions = list(modifier = list(page = 'current'))
              ),
              list(
                extend = 'print',
                filename =  'checked_data',
                title = 'checked_data', 
                exportOptions = list(modifier = list(page = 'current'))
              )),
            lengthMenu = list(c(5, 10, 50, 100, -1), c('5', '10', '50', '100', 'All'))))
      })
      
      output[[paste0('locuscompare_plot_locuscomparer', randomid())]] <- renderUI({
        fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'locuscompare_plot_locuscomparer_draw', width = '600px', height = '480px')))
      })
      
      output[[paste0('locuscompare_plot_geni.plots', randomid())]] <- renderUI({
        fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'locuscompare_plot_geni.plots_draw', width = '800px', height = '700px')))
      })
      
      if (input[['coloc_method']] %in% sensitive.methods()) {
        output[[paste0('sensitivity_plot', randomid())]] <- renderUI({
          fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'sensitivity_plot_draw', width = '800px', height = '400px')))
        })
        
        output[['sensitivity_plot_draw']] <- renderPlot({
          spsComps::shinyCatch({
            if (input[['coloc_method']] == 'COLOC.ABF') {
              coloc::sensitivity(
                obj = COLOC.res[["coloc.dat"]],
                rule = "H4 > 0.8",
                plot.manhattans = TRUE
              )
            } else if (input[['coloc_method']] == 'COLOC.SUSIE') {
              coloc::sensitivity(
                obj = COLOC.res[["coloc.dat"]],
                rule = "H4 > 0.8",
                row = 1,
                dataset1 = COLOC.res[["dataset1"]],
                dataset2 = COLOC.res[["dataset2"]],
                plot.manhattans = TRUE
              )
            }
          }, trace_back = FALSE)
        })
      }
      
      output[['locuscompare_plot_locuscomparer_draw']] <- renderPlot({
        spsComps::shinyCatch({
          COLOC.locuscompare()
        }, trace_back = FALSE)
      })
      
      output[['locuscompare_plot_geni.plots_draw']] <- renderPlot({
        spsComps::shinyCatch({
          COLOC.geni()
        }, trace_back = FALSE)
      })
      
      if (download.status()) shinyjs::show('downloadBtnUI')
    }
    
    enable_elements(
      ids = c('show_data-tabs-mode', 'dragvars-source-container', 'output_setting_ui', 'downloadBtnUI'), 
      classes = c("dragula-target", 'vscomp-value'),
      session
    )
    
    # TODO
    if (FALSE) shinyjs::show('show_codes')
  })
  
  # Righttop setting ----
  output[['output_setting_ui']] <- renderUI({
    shinyWidgets::dropdown(
      tags$h4("Setting for colocalization analysis"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = 'sample_size_exposure',
            label = shiny::HTML('Sample Size (Exposure) <span style="color: red;">*</span>'),
            placeholder = 'Sample size.',
            value = ''
          ),
          shiny::uiOutput(outputId = "sample_size_exposure_warning")
        ),
        shiny::conditionalPanel(
          condition = "input.data_type_exposure == 'cc'",
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = 'case_size_exposure',
              label = shiny::HTML('Case Size (Exposure) <span style="color: red;">*</span>'),
              placeholder = 'Case size.',
              value = ''
            ),
            shiny::uiOutput(outputId = "case_size_exposure_warning")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = 'sample_size_outcome',
            label = shiny::HTML('Sample Size (Outcome) <span style="color: red;">*</span>'),
            placeholder = 'Sample size.',
            value = ''
          ),
          shiny::uiOutput(outputId = "sample_size_outcome_warning")
        ),
        shiny::conditionalPanel(
          condition = "input.data_type_outcome == 'cc'",
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = 'case_size_outcome',
              label = shiny::HTML('Case Size (Outcome) <span style="color: red;">*</span>'),
              placeholder = 'Case size.',
              value = ''
            ),
            shiny::uiOutput(outputId = "case_size_exposure_outcome")
          )
        )
      ),
      shiny::textInput(inputId = 'trait_name_exposure', label = 'Trait Name (Exposure)', placeholder = 'Please input the trait name.', value = 'trait1'),
      shiny::textInput(inputId = 'trait_name_outcome', label = 'Trait Name (Outcome)', placeholder = 'Please input the trait name.', value = 'trait2'),
      shinyWidgets::radioGroupButtons(
        inputId = "metrics_type",
        label = shiny::HTML('Metrics (Type) <span style="color: red;">*</span>'),
        choices = c("Beta Coefficient" = 'beta', "Odds Ratio" = 'OR', 'Z-score' = 'Z'),
        selected = character(0),
        individual = TRUE,
        width = "100%",
        checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
      shiny::conditionalPanel(
        condition = "input.coloc_method == 'COLOC.ABF' || input.coloc_method == 'COLOC.SUSIE' || input.coloc_method == 'COLOC.HyPrColoc' || input.coloc_method == 'COLOC.PWCoCo'",
        shinyWidgets::radioGroupButtons(
          inputId = "data_type_exposure",
          label = shiny::HTML('Data Type (Exposure) <span style="color: red;">*</span>'),
          choices = c("quant (quantitative)" = 'quant', "cc (case-control)" = 'cc'),
          selected = character(0),
          individual = TRUE,
          width = "100%",
          checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
        shinyWidgets::radioGroupButtons(
          inputId = "data_type_outcome",
          label = shiny::HTML('Data Type (Outcome) <span style="color: red;">*</span>'),
          choices = c("quant (quantitative)" = 'quant', "cc (case-control)" = 'cc'),
          selected = character(0),
          individual = TRUE,
          width = "100%",
          checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
      ),
      shiny::conditionalPanel(
        condition = "input.coloc_method == 'COLOC.PWCoCo'",
        tags$br(),
        tags$h4("Setting for PWCoCo"),
        shinyWidgets::dropdownButton(
          inputId = "PWCoCo_setting",
          label = "Setting for PWCoCo",
          icon = icon("add"),
          status = "primary",
          circle = FALSE,
          tooltip = TRUE,
          up = TRUE,
          size = 'sm',
          tags$div(
            style = "display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'p_cutoff1. The default is 5e-08',
              shiny::textInput(
                inputId = 'p_cutoff1',
                label = 'p_cutoff1', value = 5e-08
              )
            ),
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'p_cutoff2. The default is 5e-08',
              shiny::textInput(
                inputId = 'p_cutoff2',
                label = 'p_cutoff2', value = 5e-08
              )
            )
          ),
          tags$div(
            style = "display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'top_snp. The default is 1e+10',
              shiny::textInput(
                inputId = 'top_snp',
                label = 'top_snp', value = 1e+10
              )
            ),
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'ld_window. The default is 1e+07',
              shiny::textInput(
                inputId = 'ld_window',
                label = 'ld_window', value = 1e+07
              )
            )
          ),
          tags$div(
            style = "display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'collinear. The default is 0.9',
              shiny::textInput(
                inputId = 'collinear',
                label = 'collinear', value = 0.9
              )
            ),
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'maf. The default is 0.1',
              shiny::textInput(
                inputId = 'maf',
                label = 'maf', value = 0.1
              )
            )
          ),
          tags$div(
            style = "display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'freq_threshold. The default is 0.2',
              shiny::textInput(
                inputId = 'freq_threshold',
                label = 'freq_threshold', value = 0.2
              )
            ),
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'init_h4. The default is 100',
              shiny::textInput(
                inputId = 'init_h4',
                label = 'init_h4', value = 100
              )
            )
          ),
          tags$div(
            style = "display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap;",
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'chr. The default is NULL',
              shiny::textInput(
                inputId = 'chr',
                label = 'chr', value = ''
              )
            ),
            tags$div(
              style = "flex: 1 1 150px; min-width: 120px;",
              title = 'snp.index. The default is 1',
              shiny::textInput(
                inputId = 'snp.index',
                label = 'snp.index', value = 1
              )
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.coloc_method == 'COLOC.eCAVIAR'",
        tags$br(),
        tags$h4("Setting for linkage disequilibrium"),
        tags$div(
          style = "display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap;",
          tags$div(
            style = "flex: 1 1 150px; min-width: 120px;",
            title = 'Clumping r2 cut off. The default is 0.1.',
            shiny::numericInput(
              inputId = 'clump_r2',
              label = 'Clumping r2 cut off.',
              value = 0.1, min = 0, max = 1, step = 0.1
            )
          ),
          tags$div(
            style = "flex: 1 1 150px; min-width: 120px;",
            title = 'Clumping distance cutoff. The default is 250.',
            shiny::numericInput(
              inputId = 'clump_kb',
              label = 'Clumping distance cutoff.',
              value = 250, min = 0, step = 1
            )
          )
        )
      ),
      tags$br(),
      shinyWidgets::radioGroupButtons(inputId = "reference_version", label = "Reference Version", choices = c("hg19/GRCh37", "hg38/GRCh38"), justified = TRUE),
      shinyWidgets::radioGroupButtons(
        inputId = "population_type",
        label = "Population",
        choices = c("EUR", "AMR", "EAS", "AFR", "SAS"),
        individual = TRUE,
        width = "100%",
        checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))),
      shinyWidgets::virtualSelectInput(
        inputId = 'coloc_snp',
        label = 'Select SNP',
        choices = '',
        selected = '',
        optionsCount = 7,
        noOfDisplayValues = 16,
        multiple = FALSE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      ),
      style = "material-circle", icon = icon("gear"), right = TRUE,
      status = "danger", width = "450px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  })
  
  nwarning <- tags$span(
    "⚠️ Please enter a valid positive integer!",
    style = "color: red; font-size: 10px;"
  )
  
  output$sample_size_exposure_warning <- renderUI({
    req(input$sample_size_exposure)
    if (!grepl("^\\d+$", input$sample_size_exposure)) {
      nwarning
    } else {
      ""
    }
  })
  
  output$sample_size_outcome_warning <- renderUI({
    req(input$sample_size_outcome)
    if (!grepl("^\\d+$", input$sample_size_outcome)) {
      nwarning
    } else {
      ""
    }
  })
  
  output$case_size_exposure_warning <- renderUI({
    req(input$case_size_exposure)
    if (!grepl("^\\d+$", input$case_size_exposure)) {
      nwarning
    } else {
      ""
    }
  })
  
  output$case_size_exposure_outcome <- renderUI({
    req(input$case_size_outcome)
    if (!grepl("^\\d+$", input$case_size_outcome)) {
      nwarning
    } else {
      ""
    }
  })
  
  # Download ----
  output[['download_results']] <- downloadHandler(
    filename = function() { glue::glue('io.coloc.{format(Sys.time(), "%y%m%d%H%M%S")}.zip') },
    content = function(zipfilename) {
      spsComps::shinyCatch({
        temp_dir <- tempdir()
        
        # Locuscompare Plot {locuscomparer}
        message(glue('{Sys.time()} add Locuscompare Plot {{locuscomparer}}'))
        width = ifelse(test = is.null(input[['locuscompare_plot_locuscomparer_draw_size']]$width), yes = 680 / 100, no = input[['locuscompare_plot_locuscomparer_draw_size']]$width / 100)
        height = ifelse(test = is.null(input[['locuscompare_plot_locuscomparer_draw_size']]$height), yes = 480 / 100, no = input[['locuscompare_plot_locuscomparer_draw_size']]$height / 100)
        locuscompare_plota_locuscomparer.pdf.file = file.path(temp_dir, 'locuscompare_plota_locuscomparer.pdf')
        plot <- COLOC.locuscompare()
        ggplot2::ggsave(plot, filename = locuscompare_plota_locuscomparer.pdf.file, width = width, height = height, device = 'pdf', units = 'in')
        
        # Locuscompare Plot {geni.plots}
        if (input[['coloc_method']] %in% geni.plots.methods()) {
          message(glue('{Sys.time()} add Locuscompare Plot {{geni.plots}}'))
          width = ifelse(test = is.null(input[['locuscompare_plot_geni.plots_draw_size']]$width), yes = 800 / 100, no = input[['locuscompare_plot_geni.plots_draw_size']]$width / 100)
          height = ifelse(test = is.null(input[['locuscompare_plot_geni.plots_draw_size']]$height), yes = 700 / 100, no = input[['locuscompare_plot_geni.plots_draw_size']]$height / 100)
          locuscompare_plota_geni.plots.pdf.file = file.path(temp_dir, 'locuscompare_plota_geni.plots.pdf')
          plot <- COLOC.geni()
          ggplot2::ggsave(plot, filename = locuscompare_plota_geni.plots.pdf.file, width = width, height = height, device = 'pdf', units = 'in')
        }
        
        # Sensitivity Plot
        if (input[['coloc_method']] %in% sensitive.methods()) {
          message(glue('{Sys.time()} add Sensitivity Plot.'))
          width = ifelse(test = is.null(input[['sensitivity_plot_draw_size']]$width), yes = 800 / 100, no = input[['sensitivity_plot_draw_size']]$width / 100)
          height = ifelse(test = is.null(input[['sensitivity_plot_draw_size']]$height), yes = 400 / 100, no = input[['sensitivity_plot_draw_size']]$height / 100)
          sensitivity_plot.pdf.file = file.path(temp_dir, 'sensitivity_plot.pdf')
          pdf(file = sensitivity_plot.pdf.file, width = width, height = height)
          if (input[['coloc_method']] == 'COLOC.ABF') {
            coloc::sensitivity(
              obj = COLOC.ana()[["coloc.dat"]],
              rule = "H4 > 0.8",
              plot.manhattans = TRUE
            )
          } else if (input[['coloc_method']] == 'COLOC.SUSIE') {
            coloc::sensitivity(
              obj = COLOC.ana()[["coloc.dat"]],
              rule = "H4 > 0.8",
              row = 1,
              dataset1 = COLOC.ana()[["dataset1"]],
              dataset2 = COLOC.ana()[["dataset2"]],
              plot.manhattans = TRUE
            )
          }
          dev.off()
        }
        
        # Summary
        summary.csv.file = file.path(temp_dir, 'summary.csv')
        write.csv(x = (if (input[['coloc_method']] == 'COLOC.PWCoCo') COLOC.ana()$coloc.dat[as.integer(input[["snp.index"]]), ] else COLOC.ana()$summary), file = summary.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Colocalization (coloc) results table
        coloc.csv.file = file.path(temp_dir, 'coloc_results.csv')
        write.csv(x = (if (input[['coloc_method']] == 'COLOC.PWCoCo') COLOC.ana()$coloc.dat else COLOC.ana()$results), file = coloc.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Genomic region association
        association.csv.file = file.path(temp_dir, 'association_results.csv')
        write.csv(x = COLOC.plot()$geni.dat, file = association.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Zip files
        if (input[['coloc_method']] %in% sensitive.methods() & input[['coloc_method']] %in% geni.plots.methods()) {
          zip::zip(zipfile = zipfilename, files = c('locuscompare_plota_locuscomparer.pdf', 'locuscompare_plota_geni.plots.pdf', 'sensitivity_plot.pdf', 'summary.csv', 'coloc_results.csv', 'association_results.csv'), root = temp_dir)
        } else if (!input[['coloc_method']] %in% sensitive.methods() & input[['coloc_method']] %in% geni.plots.methods()) {
          zip::zip(zipfile = zipfilename, files = c('locuscompare_plota_locuscomparer.pdf', 'locuscompare_plota_geni.plots.pdf', 'summary.csv', 'coloc_results.csv', 'association_results.csv'), root = temp_dir)
        } else if (input[['coloc_method']] %in% sensitive.methods() & !input[['coloc_method']] %in% geni.plots.methods()) {
          zip::zip(zipfile = zipfilename, files = c('locuscompare_plota_locuscomparer.pdf', 'sensitivity_plot.pdf', 'summary.csv', 'coloc_results.csv', 'association_results.csv'), root = temp_dir)
        } else {
          zip::zip(zipfile = zipfilename, files = c('locuscompare_plota_locuscomparer.pdf', 'summary.csv', 'coloc_results.csv', 'association_results.csv'), root = temp_dir)
        }
        base::message(glue('{Sys.time()} Download all results.'))
      }, trace_back = FALSE)
    }, contentType = 'application/zip')
  
  # Display.status ----
  display.status1 <- reactive({
    !is.null(input[["dragvars1"]]$target[['SNP']]) &&
      !is.null(input[["dragvars1"]]$target[['CHR']]) &&
      !is.null(input[["dragvars1"]]$target[['POS']]) &&
      !is.null(input[["dragvars1"]]$target[['OR/beta/Z']]) &&
      !is.null(input[["dragvars1"]]$target[['eaf']]) &&
      !is.null(input[["dragvars1"]]$target[['se']]) &&
      !is.null(input[["dragvars1"]]$target[['pval']])
  })
  
  display.status2 <- reactive({
    !is.null(input[["dragvars2"]]$target[['SNP']]) &&
      !is.null(input[["dragvars2"]]$target[['CHR']]) &&
      !is.null(input[["dragvars2"]]$target[['POS']]) &&
      !is.null(input[["dragvars2"]]$target[['OR/beta/Z']]) &&
      !is.null(input[["dragvars2"]]$target[['eaf']]) &&
      !is.null(input[["dragvars2"]]$target[['se']]) &&
      !is.null(input[["dragvars2"]]$target[['pval']])
  })
  
  # Observe ----
  shiny::observe({
    shinyjs::hide('runBtnUI')
    if (display.status1() & display.status2()) {
      shinyjs::show('runBtnUI')
    }
  })
  
  # Show codes ----
  output[['workflow_code_abf']] <- shinymeta::metaRender(renderText, {
    
    '# Load exposure data'
    ..(get_load_code(input$upload_data1$name, exposure.filepath(), "dat.full.1"))
    ..(get_load_code(input$upload_data2$name, outcome.filepath(), "dat.full.2"))
    
    '#Perform colocalization analysis'
    COLOC.res <- GWASkitR::COLOC.ABF(
      e.dat = dat.full.1,
      o.dat = dat.full.2,
      e.col.chr = ..(input[["dragvars1"]]$target[['CHR']]),
      e.col.pos = ..(input[["dragvars1"]]$target[['POS']]),
      e.col.snp = ..(input[["dragvars1"]]$target[['SNP']]),
      e.col.beta = ..(input[["dragvars1"]]$target[['OR/beta/Z']]),
      e.col.se = ..(input[["dragvars1"]]$target[['se']]),
      e.col.eaf = ..(input[["dragvars1"]]$target[['eaf']]),
      e.col.pval = ..(input[["dragvars1"]]$target[['pval']]),
      e.col.nsample = ..(db.sample.size.e.c.r()),
      e.type = ..(input[["data_type_exposure"]]), 
      e.prevalence = NA,
      o.col.snp = ..(input[["dragvars2"]]$target[['SNP']]),
      o.col.beta = ..(input[["dragvars2"]]$target[['OR/beta/Z']]),
      o.col.se = ..(input[["dragvars2"]]$target[['se']]),
      o.col.eaf = ..(input[["dragvars2"]]$target[['eaf']]),
      o.col.pval = ..(input[["dragvars2"]]$target[['pval']]),
      o.col.nsample = ..(db.sample.size.o.c.r()),
      o.type = ..(input[["data_type_outcome"]]),
      o.prevalence = NA
    )
    
    '# Organize the results for visualization'
    coloc.dat <- GWASkitR::coloc_result_to_plot(
      x = COLOC.res,
      plink = ..(ifelse(test = Sys.info()['sysname'] == 'Linux', yes = 'your/path/to/plink', no = 'your/path/to/plink.exe')),
      bfile = ..(ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('your/path/to/plink/1kg.v3/', isolate({ input[['population_type']] })), no = paste0('your/path/to/plink/1kg.v3/', isolate({ input[['population_type']] }))))
    )
    
    '# Visualize the colocalization analysis results'
    locuscomparer::locuscompare(
      in_fn1 = coloc.dat[["in_fn1"]],
      in_fn2 = coloc.dat[["in_fn2"]],
      title1 = ..(input[['trait_name_exposure']]),
      title2 = ..(input[['trait_name_outcome']]),
      genome = ..(ifelse(input[['reference_version']] == 'hg19/GRCh37', yes = 'hg19', no = 'hg38')),
      snp = ..(ifelse(is.null(input[['coloc_snp']]), yes = coloc.dat[["SNP"]], no = input[['coloc_snp']][1])),
      population = coloc.dat[["pop"]],
      combine = TRUE
    )
    
    geni.plots::fig_region_stack(
      data = coloc.dat[['geni.dat']],
      traits = ..(c(input[['trait_name_exposure']], input[['trait_name_outcome']])),
      corr = coloc.dat[['ld_matrix']],
      build = ..(ifelse(input[['reference_version']] == 'hg19/GRCh37', yes = 37, no = 38)),
      title_center = TRUE
    )
    
    '# Sensitivity plot'
    coloc::sensitivity(
      obj = COLOC.res[["coloc.dat"]],
      rule = "H4 > 0.8",
      plot.manhattans = TRUE
    )
  })
  
  observeEvent(input[['show_codes']], {
    if (input[['coloc_method']] == 'COLOC.ABF') {
      code <- shinymeta::expandChain(
        quote({
          library(GWASkitR)
          library(locuscomparer)
          library(geni.plots)
        }),
        spsComps::shinyCatch({ output[['workflow_code_abf']]() }, trace_back = TRUE)
      )
    } else if (input[['coloc_method']] == 'COLOC.ABF') {
      code <- shinymeta::expandChain(
        quote({
          library(GWASkitR)
          library(locuscomparer)
          library(geni.plots)
        }),
        spsComps::shinyCatch({ output[['workflow_code']]() }, trace_back = TRUE)
      )
    }
    
    shinymeta::displayCodeModal(
      code = formatR::tidy_source(text = as.character(code), args.newline = TRUE, output = FALSE)$text.tidy,
      title = "Code to reproduce this workflow.",
      clip = NULL,
      size = 'l'
    )
  })
  
  observeEvent({
    input[['coloc_snp']]
  }, {
    try({
      if (SNP.rel() != input[['coloc_snp']]) {
        shinyjs::show('runBtnUI')
        shinyjs::hide('downloadBtnUI')
        shinyjs::hide('coloc_tbl_box')
        shinyjs::hide('coloc_plot_box')
      }
    }, silent = TRUE)
  })
  
  # HELP
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help - GWAS Summary Statistics Format Tool",
      size = "l",
      easyClose = TRUE,
      fade = TRUE,
      footer = modalButton("Close (Esc)"),
      tags$iframe(
        src = '/XINGABAO/HTML/help.COLOC.html',
        width = "100%",
        height = "600px",
        style = "border: none;"
      )
    ))
  })
}

# Launch the Shiny application
shinyApp(ui = ui.COLOC, server = service.COLOC, options = list(port = 80))