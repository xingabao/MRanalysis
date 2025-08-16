#' @title GWAS Summary Statistics Format Tool
#'
#' @description A Shiny application for formatting GWAS summary statistics based on selected categories and subcategories.

# Load R packages
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(data.table)))
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

# Define categories and subcategories for dropdown menus
categories <- shinyWidgets::prepare_choices(
  data.frame(
    name = c('GWAS'), # , 'eQTL', 'pQTL'
    abb = c('GWAS'),  # , 'eQTL', 'pQTL'
    description = c('GWAS sumstats data') # , 'Expression quantitative trait Loci', 'Protein quantitative trait loci'
  ),
  label = name, 
  value = abb,
  description = description
)

categories.GWAS <- shinyWidgets::prepare_choices(
  data.frame(
    name = c('Common'),
    abb = c('Common'), 
    description = c('Upload local data in .vcf/.vcf.gz, .txt/.txt.gz, .csv/.csv.gz, etc. formats.')
  ),
  label = name, 
  value = abb,
  description = description
)

categories.eQTL <- shinyWidgets::prepare_choices(
  data.frame(
    name = c('Common', 'eQTL catalog'),
    abb = c('Common', 'eQTL catalog'), 
    description = c('本地上传数据，格式为 .vcf/.vcf.gz, .txt/.txt.gz, .csv/.csv.gz, etc.', 'eQTL catalog')
  ),
  label = name, 
  value = abb,
  description = description
)

categories.pQTL <- shinyWidgets::prepare_choices(
  data.frame(
    name = c('Common', 'pQTL catalog'),
    abb = c('Common', 'pQTL catalog'), 
    description = c('本地上传数据，格式为 .vcf/.vcf.gz, .txt/.txt.gz, .csv/.csv.gz, etc.', 'pQTL catalog')
  ),
  label = name, 
  value = abb,
  description = description
)

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

categories. <- list(
  'GWAS' = categories.GWAS,
  'eQTL' = categories.eQTL,
  'pQTL' = categories.pQTL
)

# UI function for the Shiny application
ui.format <- shiny::fluidPage(
  title = 'FORMAT',
  shiny::tags$div(
    style = "display: flex; align-items: center;",
    shiny::titlePanel('GWAS SUMSTATS FORMAT'),
    shiny::actionButton(
      inputId = 'help',
      label = 'Help',
      icon = shiny::icon('question-circle'),
      class = "down",
      style = "margin-left: 20px; width: 80px !important; min-width: 80px !important; display: inline-block;"
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
      #submitBtnUI, #confirmBtnUI, #convertBtnUI, #downloadBtnUI, #demoBtnUI {
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
    #showexplations {
      margin-left: 25px;
    }
    "))
  ),
  shiny::uiOutput(outputId = "output_setting_ui"),
  shiny::sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 5,
      class = 'sidebar',
      shiny::uiOutput("demoBtnUI"),
      shiny::uiOutput("validation_message"),
      h4("Step 1: select the format conversion mode."),
      shiny::sidebarPanel(
        width = "100%",
        fluidRow(
          # First-level dropdown for selecting category
          column(
            width = 4,  
            shinyWidgets::virtualSelectInput(
              inputId = "first_category",
              label = "Select Category", 
              choices = categories,
              hasOptionDescription = TRUE,
              multiple = FALSE,
              dropboxWrapper = "body",
              width = "100%"
            )
          ),
          # Second-level dropdown (updated dynamically)
          column(
            width = 8,
            shinyWidgets::virtualSelectInput(
              inputId = "second_category",
              label = "Select Subcategory", 
              choices = NULL,
              hasOptionDescription = TRUE,
              multiple = FALSE,
              dropboxWrapper = "body",
              width = "100%"
            )
          )
        ),
        shiny::uiOutput("upload_gwas_ui"),
        shinycssloaders::withSpinner(ui_element = shiny::uiOutput("submitBtnUI"), proxy.height = '25px', type = 7, size = 0.5, caption = 'Reading file ...')
      ),
      shiny::uiOutput("showexplations"),
      tags$div(
        id = 'show_data_datamods',
        MRanalysisBase::import_ui_demo(
          id = 'show_data',
          from = c('env')
        )
      )
      
    ),
    mainPanel = column(
      id = 'right_results',
      width = 7,
      shiny::tags$span("Some regular expression matching is performed here to enable rapid identification, but there may be occasional mismatches."),
      shiny::tags$br(),
      shiny::tags$span(
        HTML("Please adjust according to the actual data as needed."),
        style = "color: #00A087; font-weight: bold; font-size: 14px; font-family: 'Microsoft YaHei', Arial, sans-serif;"
      ),
      shiny::uiOutput(outputId = 'ui_aesthetics'),
      shiny::tableOutput("contents"),
      shiny::uiOutput("confirmBtnUI"),
      shiny::uiOutput("convertBtnUI"),
      shiny::uiOutput("downloadBtnUI"),
      shiny::tags$br(),
      shiny::uiOutput("showcomments"),
      shiny::column(width = 12, id = 'tabledatahere')
    )
  )
)

# Server function for handling logic and reactivity
service.format <- function(input, output, session) {
  
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
  
  # Reactive value to store values
  validation.status <- shiny::reactiveVal(list(is_valid = TRUE, message = "First, please '<b><u><span style='color:red;'>select</span></u></b>' the format conversion mode below."))
  dat.load.name <- shiny::reactiveVal(value = NULL)
  dat.load.part <- shiny::reactiveVal(value = NULL)
  dat.load.full <- shiny::reactiveVal(value = NULL)
  file.ready <- reactiveVal(value = FALSE)
  imported.data <- reactiveVal(value = NULL)
  converted.data <- reactiveVal(value = NULL)
  randomid <- reactiveVal(as.numeric(Sys.time()))
  
  # Download Demo data
  output[['demo_data']] <- shiny::downloadHandler(
    filename <- function(){ paste('GWAS_FORMAT_DEMO.zip') },
    content <- function(file){
      file.copy(glue("demo/GWAS_FORMAT_DEMO.zip"), file)
    }, contentType = NULL)
  
  # Update subcategory dropdown based on selected category
  shiny::observe({
    selected.category <- input$first_category
    subcategories.choices <- categories.[[selected.category]]
    shinyWidgets::updateVirtualSelect(session = session, inputId = 'second_category', choices = subcategories.choices)
  })
  
  # 监控第二栏的选择内容, 显示指定组件
  observeEvent(input$second_category, {
    
    shinyjs::hide('upload_gwas_ui')
    shinyjs::hide('convertBtnUI')
    shinyjs::hide('downloadBtnUI')
    shinyjs::hide('confirmBtnUI')
    shinyjs::hide('right_results')
    shinyjs::hide('show_data_datamods')
    shinyjs::hide('output_setting_ui')
    
    # Ensure both inputs are selected
    req(input$first_category, input$second_category)
    
    shinyjs::hide('demoBtnUI')
    
    category <- input$first_category
    subcategory <- input$second_category
    
    # Define different parameter inputs based on category and subcategory
    if (category == "GWAS" && subcategory %in% c("Common")) {
      shinyjs::show('upload_gwas_ui')
      validation.status(list(is_valid = TRUE, message = "Next, click the '<b><u><span style='color:red;'>Browse</span></u></b>' button and follow the instructions to upload the GWAS dataset that needs format conversion."))
    }
    
  })
  
  # Dynamically render UI content based on selected Category and Subcategory
  output$upload_gwas_ui <- renderUI({
    
    # Ensure both inputs are selected
    req(input$first_category, input$second_category)
    
    category <- input$first_category
    subcategory <- input$second_category
    
    # 定义一个通用的上传文件组件
    upload.tag <- function(accept) {
      shiny::tagList(
        h4("Step 2: Upload the GWAS summary data."),
        shiny::fileInput(
          inputId = 'upload_data', 
          label = 'Upload your GWAS summary data.',
          multiple = FALSE,
          width = '100%', 
          placeholder = 'Please upload your GWAS summary data.', 
          accept = accept)
      )
    }
    
    # Define different parameter inputs based on category and subcategory
    if (category == "GWAS" && subcategory %in% c("Common")) {
      shiny::tagList(upload.tag(c('.vcf', '.gz', '.csv', '.tsv', '.txt')))
      # spsComps::shinyCatch(message("some message"))
      # if (category == "GWAS" && subcategory == "Common") {
      #   # Only show additional inputs and submit button if validation passes
      #   if (validation.status()$is_valid) {
      #     shiny::tagList(
      #       upload.tag(c('.vcf', '.gz', '.csv', '.tsv', '.txt')),
      #       shiny::textInput("gwas_id2", "GWAS ID", placeholder = "Enter GWAS ID"),
      #       shiny::numericInput("pvalue_threshold2", "P-value Threshold", value = 5e-8, min = 0, max = 1, step = 1e-9),
      #       shiny::actionButton("submit_data", "Submit", class = "btn-custom")
      #     )
      #   } else {
      #     shiny::tagList(upload.tag(c('.vcf', '.gz', '.csv', '.tsv', '.txt')))
      #   }
      # }
    }
  })
  
  output$demoBtnUI <- renderUI({
    shiny::downloadButton("demo_data", "demo data", class = "btn-custom btn-download")
  })
  
  output$submitBtnUI <- renderUI({
    req(input$upload_data)
    shiny::actionButton("submit_data", "Submit", class = "btn-custom")
  })
  
  output$confirmBtnUI <- renderUI({
    shiny::actionButton("confirm_data", "Confirm", class = "btn-custom")
  })
  
  output$convertBtnUI <- renderUI({
    shiny::actionButton("convert_data", "Convert", class = "btn-custom btn-convert")
  })
  
  output$downloadBtnUI <- renderUI({
    shiny::downloadButton("download_data", "Download", class = "btn-custom btn-download")
  })
  
  # Observe file upload and validate inputs
  observeEvent(input$upload_data, {
    
    shinyjs::hide('convertBtnUI')
    shinyjs::hide('downloadBtnUI')
    shinyjs::hide('confirmBtnUI')
    shinyjs::hide('right_results')
    shinyjs::hide('output_setting_ui')
    session$sendCustomMessage('disableFile', list(id = "upload_data"))
    
    if (!is.null(input$upload_data)) {
      
      file_ext <- tools::file_ext(input$upload_data$name)
      allowed_exts <- c("vcf", "gz", "csv", "tsv", "txt")
      
      if (tolower(file_ext) %in% allowed_exts) {
        validation.status(list(is_valid = TRUE, message = "File uploaded successfully and format is valid. Then, click the '<b><u><span style='color:red;'>Submit</span></u></b>' button to proceed to the next step."))
      } else {
        err_msg <- "Invalid file format. Please upload a file with extension .vcf, .gz, .csv, .tsv, or .txt."
        validation.status(list(is_valid = FALSE, message = err_msg))
        stop(err_msg)
      }
      
      if (file_ext == 'vcf' || endsWith(input$upload_data$name, ".vcf.gz")) {
        sumstats <<- GWASkitR::read.vcf(input$upload_data$datapath, nrows = 1000, verbose = FALSE)
      } else {
        sumstats <<- data.table::fread(input$upload_data$datapath, nrows = 1000)
      }
      dat.load.name('sumstats')
      dat.load.part(sumstats)
    } else {
      validation.status(list(is_valid = FALSE, message = "No file uploaded."))
    }
  })
  
  # Display validation message
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
  
  # Handle submit button action
  observeEvent(input$submit_data, {
    
    output[['showexplations']] <- renderUI({
      tags$p(
        glue::glue('For quick extraction and manipulation, only a subset (about 1,000 rows) is displayed here.'),
        style = "color: #E50914; font-weight: bold; font-size: 14px; font-family: 'Microsoft YaHei', Arial, sans-serif;"
      )
    })
    
    shinyjs::runjs('$("#first_category").css({"pointer-events":"none", "user-select":"none"});')
    shinyjs::runjs('$("#second_category").css({"pointer-events":"none", "user-select":"none"});')
    validation.status(list(is_valid = TRUE, message = "Next, click the '<b><u><span style='color:red;'>Import data</span></u></b>' button to load the data."))
    
    shinyjs::hide('submitBtnUI')
    shinyjs::hide('convertBtnUI')
    shinyjs::hide('downloadBtnUI')
    shinyjs::hide('output_setting_ui')
    shinyjs::show('confirmBtnUI')
    shinyjs::show('show_data_datamods')
    shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
    
    # Import Data
    imported <- MRanalysisBase::import_server_demo(
      demo_data = dat.load.name(), 
      id = "show_data", 
      choices = dat.load.name(), 
      return_class = "tbl_df", 
      read_fns = list(gz = function(file) { data.table::fread(file) }),
      filter.type = c('virtualSelect', 'range', 'range'),
      tab.view = TRUE,
      tab.update = TRUE,
      tab.validate = FALSE,
      tab.filter = TRUE
    )
    
    imported.data(imported)
    
    # Choose Variable
    output[['ui_aesthetics']] <- renderUI({
      
      aesthetics <- c('SNP', 'CHR', 'POS', 'effect_allele', 'other_allele', 'eaf', 'OR/beta/Z', 'se', 'pval', 'nsample', 'ncase', 'ncontrol', 'MAF')
      data = imported$data()
      
      if (!is.null(data)) {
        var_choices <- setdiff(names(data), attr(data, "sf_column"))
        esquisse::dragulaInput(
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
            CHR = MRanalysisBase::multi_pattern_match(names(data), list("CHR")),
            POS = MRanalysisBase::multi_pattern_match(names(data), list("POS", 'BP', 'start')),
            effect_allele = MRanalysisBase::multi_pattern_match(names(data), list("effect_allele", c("effect", "allele"), c("alternative", "allele"), "ALT")),
            other_allele = MRanalysisBase::multi_pattern_match(names(data), list("other_allele", c("other", "allele"), c("non-effect", "allele"), c("reference", "allele"), "REF")),
            eaf = MRanalysisBase::multi_pattern_match(names(data), list("eaf", "AF", 'freq')),
            `OR/beta/Z` = MRanalysisBase::multi_pattern_match(names(data), list("OR", "b", 'es', c('effect', 'size'), 'Z')),
            se = MRanalysisBase::multi_pattern_match(names(data), list('se', c('standard', 'error'))),
            pval = MRanalysisBase::multi_pattern_match(names(data), list("pval", "p", 'LP', 'log10P')),
            nsample = MRanalysisBase::multi_pattern_match(names(data), list("SS", 'sample', 'N')),
            ncontrol = MRanalysisBase::multi_pattern_match(names(data), list("control")),
            ncase = MRanalysisBase::multi_pattern_match(names(data), list("case")),
            MAF = MRanalysisBase::multi_pattern_match(names(data), list("MAF"))
          ),
          badge = TRUE,
          copySource = FALSE,
          ncolGrid = 7,
          width = "100%",
          height = "50px",
          replace = FALSE
        )
      }
    })
    
    observeEvent(input[['show_data-confirm']], {
      
      shinyjs::hide('validation_message')
      shinyjs::hide('convertBtnUI')
      shinyjs::hide('downloadBtnUI')
      shinyjs::hide('output_setting_ui')
      shinyjs::show('right_results')
      shinyjs::show('confirmBtnUI')
      shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
    })
    # Validate additional inputs before proceeding
    # if (nchar(stringr::str_trim(input$gwas_id2)) == 0) {
    #   validation.status(list(is_valid = FALSE, message = "Please enter a valid GWAS ID."))
    # } else if (is.na(input$pvalue_threshold2) || input$pvalue_threshold2 <= 0 || input$pvalue_threshold2 > 1) {
    #   validation.status(list(is_valid = FALSE, message = "Please enter a valid P-value threshold between 0 and 1."))
    # } else {
    #   validation.status(list(is_valid = TRUE, message = "All parameters are valid. Processing data..."))
    #   # Add your data processing logic here after successful validation
    #   output$contents <- shiny::renderText({
    #     paste("Processing file:", input$upload_data$name, "\nGWAS ID:", input$gwas_id2, "\nP-value Threshold:", input$pvalue_threshold2)
    #   })
    # }
  })
  
  observeEvent(input$convert_data, {
    
    disable_elements(
      ids = c('show_data_datamods', 'show_data-tabs-mode', 'dragvars-source-container', 'output_setting_ui', 'downloadBtnUI'), 
      classes = c("dragula-target", 'vscomp-value'),
      session
    )
    
    randomid(as.numeric(Sys.time()))
    
    shinyjs::show('downloadBtnUI')
    shinyjs::hide('convertBtnUI')
    shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
    
    shiny::insertUI(
      selector = '#tabledatahere',
      where = 'beforeBegin',
      immediate = TRUE,
      ui = column(
        width = 12,
        id = 'datatable_tbl_box',
        tags$hr(), 
        shinycssloaders::withSpinner({ DT::dataTableOutput(outputId = paste0('datatable', randomid())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Transforming data format, please wait. This may take a few minutes...'))
    )
    
    # Collect the required columns
    .cols <- c(
      input[["dragvars"]]$target[['SNP']], input[["dragvars"]]$target[['CHR']], input[["dragvars"]]$target[['POS']], 
      input[["dragvars"]]$target[['effect_allele']], input[["dragvars"]]$target[['other_allele']], input[["dragvars"]]$target[['eaf']], 
      input[["dragvars"]]$target[['OR/beta/Z']], input[["dragvars"]]$target[['se']], input[["dragvars"]]$target[['pval']], 
      input[["dragvars"]]$target[['nsample']], input[["dragvars"]]$target[['ncontrol']], input[["dragvars"]]$target[['ncase']], 
      input[["dragvars"]]$target[['MAF']]
    )
    
    # Remove NULLs from .cols to avoid checking for missing NULL columns
    .cols <- .cols[!is.null(.cols)]
    
    # Load all data
    file_ext <- tools::file_ext(input$upload_data$name)
    if (file_ext == 'vcf' || endsWith(input$upload_data$name, ".vcf.gz")) {
      dat.full <- GWASkitR::read.vcf(input$upload_data$datapath, verbose = FALSE)
    } else {
      dat.full <- data.table::fread(input$upload_data$datapath)
    }
    
    # Compare the columns
    coldf <- MRanalysisBase::compare_column_changes(dat.load.part(), imported.data()$data())
    
    # Generate the code
    cmd <- as.character(imported.data()$code())
    cmd <- paste0(cmd, collapse = ' %>% ')
    cmd <- gsub('^((\\s*%>%)+\\s*)|(\\s*(%>%\\s*)+)$', '', cmd)
    
    if (cmd != 'data') {
      cmd <- sub("^\\s*data\\s*%>%\\s*", "", cmd)
      cmd <- paste0("dat.full %>% dplyr::select(all_of(.cols)) %>% ", cmd)
    } else {
      cmd <- paste0("dat.full %>% dplyr::select(all_of(.cols))")
    }
    
    # Select needed columns.
    names(dat.full) <- coldf$Modified
    converted.data(eval(parse(text = cmd)))
    
    output[['showcomments']] <- renderUI({
      tags$p(glue::glue('The dataset contains {nrow(converted.data())} rows in total. For viewing purposes, only a subset (about 1,000 rows) is displayed here.'))
    })
    
    enable_elements(
      ids = c('show_data_datamods', 'show_data-tabs-mode', 'dragvars-source-container', 'output_setting_ui', 'downloadBtnUI'), 
      classes = c("dragula-target", 'vscomp-value'),
      session
    )
    
    # `shiny::renderDataTable()` is deprecated as of shiny 1.8.1.
    # Please use `DT::renderDT()` instead.
    output[[paste0('datatable', randomid())]] <- DT::renderDT({
      converted.data() %>% slice_head(n = 1000) %>%
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
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
  })
  
  # ALBERT
  output$download_data <- downloadHandler(
    filename = function() {
      glue::glue("{input$outfile_name}.{input$outfile_format}")
    },
    content = function(file) {
      data.table::fwrite(x = converted.data(), file = file, row.names = FALSE, sep = ifelse(test = input$outfile_format %in% c('csv', 'csv.gz'), yes = ',', no = '\t'))
    }
  )
  
  observeEvent(input$confirm_data, {
    shinyjs::show('convertBtnUI')
    shinyjs::show('output_setting_ui')
    shinyjs::hide('confirmBtnUI')
  })
  
  output[['output_setting_ui']] <- renderUI({
    shinyWidgets::dropdown(
      tags$h4("Setting for output"),
      shinyWidgets::prettyToggle(
        inputId = "pval_log10p_status",
        icon_on = icon("check"),
        label_on = "Normal p-value for effect estimate.", 
        icon_off = icon("check"),
        label_off = "-log10 p-value for effect estimate.",
        shape = 'round',
        width = 360
      ),
      shiny::conditionalPanel(
        condition = "input.pval_log10p_status === true",
        shinyWidgets::awesomeCheckbox(
          inputId = "pval_convert_status",
          label = HTML('<span style="color:#1B9E77;">Convert a regular p-value to a -log10 p-value.</span>'), 
          value = FALSE,
          status = "info",
          width = 360
        )
      ),
      shiny::conditionalPanel(
        condition = "input.pval_log10p_status === false",
        shinyWidgets::awesomeCheckbox(
          inputId = "log10p_convert_status",
          label = HTML('<span style="color:#C01A34;">Convert -log10 p-value to a regular p-value.</span>'), 
          value = FALSE,
          status = "info",
          width = 360
        )
      ),
      shiny::textInput(inputId = 'outfile_name', label = 'Output file name', value = 'sumstats', placeholder = 'Please enter the file name.'),
      shinyWidgets::virtualSelectInput(
        inputId = "outfile_format",
        label = "Select output file format",
        choices = out.setting,
        search = TRUE,
        hasOptionDescription = TRUE,
        multiple = FALSE,
        dropboxWrapper = "body",
        selected = 'txt.gz',
        width = "100%"
      ),
      # pickerInput(inputId = 'xcol2', label = 'X Variable', choices = names(iris), options = list(`style` = "btn-info"), width = 360),
      # pickerInput(inputId = 'ycol2', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]], options = list(`style` = "btn-warning")),
      # sliderInput(inputId = 'clusters2', label = 'Cluster count', value = 3, min = 1, max = 9),
      style = "material-circle", icon = icon("gear"), right = TRUE,
      status = "danger", width = "400px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
  })
  
  shinyjs::hide('convertBtnUI')
  shinyjs::hide('downloadBtnUI')
  shinyjs::hide('confirmBtnUI')
  shinyjs::hide('upload_gwas_ui')
  shinyjs::hide('show_data_datamods')
  shinyjs::hide('right_results')
  shinyjs::hide('output_setting_ui')
  
  
  observeEvent( {
    input[["dragvars"]]
  }, {
    shinyjs::hide('convertBtnUI')
    shinyjs::hide('downloadBtnUI')
    shinyjs::hide('output_setting_ui')
    shinyjs::show('confirmBtnUI')
    shiny::removeUI(selector = '#datatable_tbl_box', immediate = TRUE)
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
        src = '/XINGABAO/HTML/help.FORMAT.html',
        width = "100%",
        height = "600px",
        style = "border: none;"
      )
    ))
  })
  
}

# Launch the Shiny application
shinyApp(ui = ui.format, server = service.format, options = list(port = 80))