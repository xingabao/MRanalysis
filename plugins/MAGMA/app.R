#' @title
#'
#' @description
# Load Global
# https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004219
# file:///E:/tools/magma_v1.10/manual_v1.10.pdf

# Load R packages
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(MRanalysisBase)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(bs4Dash)))
suppressMessages(suppressWarnings(library(shinyjs)))
suppressMessages(suppressWarnings(library(shinyWidgets)))
suppressMessages(suppressWarnings(library(fresh)))
suppressMessages(suppressWarnings(library(shinyvalidate)))
suppressMessages(suppressWarnings(library(shinymeta)))
suppressMessages(suppressWarnings(library(shinycssloaders)))
suppressMessages(suppressWarnings(library(formattable)))
suppressMessages(suppressWarnings(library(org.Hs.eg.db)))
suppressMessages(suppressWarnings(library(GO.db)))

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

# ui
ui.alb <- function() {
  
  snp_data <<- read.csv('demo/snp_data.csv')
  
  dark.c = '#36292F'
  light.c = '#FFFFFF'
  
  bs4Dash::dashboardPage(
    dark = NULL,
    help = NULL,
    freshTheme = fresh::create_theme(
      fresh::bs4dash_vars(
        navbar_light_color = dark.c,
        navbar_light_active_color = dark.c,
        navbar_light_hover_color = dark.c,
        navbar_dark_color = light.c,
        navbar_dark_active_color = light.c,
        navbar_dark_hover_color = light.c
      ),
      bs4dash_yiq(
        contrasted_threshold = 10,
        text_dark = light.c,
        text_light = dark.c
      ),
      bs4dash_layout(
        sidebar_width = '400px',
        main_bg = NULL
      )
    ),
    options = NULL,
    header = bs4Dash::dashboardHeader(
      skin = 'light',
      status = dark.c,
      title = dashboardBrand(
        title = tags$img(src = '/XINGABAO/img/mr_logo.png', title = "MRanalysis", width = "300px"),
        color = NULL,
        href = MR.HOME
      )
    ),
    sidebar = bs4DashSidebar(
      skin = 'light',
      bs4SidebarMenu(
        id = 'sidebarmenu',
        br(),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Introduction</p>'), tabName = 'tab-intro', icon = icon('house', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Load SNP Data</p>'), tabName = 'tab-upl-exp', icon = icon('upload', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Gene Annotation</p>'), tabName = 'tab-one', icon = icon('gears',lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Gene Analysis</p>'), tabName = 'tab-two', icon = icon('paypal', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Gene-set Analysis</p>'), tabName = 'tab-three', icon = icon('calculator', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Current R Session</p>'), tabName = 'tab-r-session', icon = icon('info',lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Contact us</p>'), tabName = 'tab-contact-us', icon = icon('compass', class = 'nav-icon'))
      )
    ),
    body = bs4Dash::dashboardBody(
      tags$head(
        useShinyjs(),
        includeCSS(system.file('extdata', 'CSS/shiny-style.css', package = 'MRanalysisBase')),
        tags$link(rel = 'shortcut icon', href = '/XINGABAO/img/favicon.ico'),
        MRanalysisBase::web.statistic.baidu,
        tags$title('MAGMA'),
        tags$style(HTML("
        #step_one_gene_annotation_tbl td {
          white-space: nowrap !important;
        }
      "))
      ),
      bs4TabItems(
        
        # ++++++++++++++++++++++++ tabName = 'tab-r-session' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-r-session',
          fluidRow(
            id = 'tab-r-session-all',
            column(12, h5("Step 6. Collect Information About the Current R Session."))
          ),
          tags$div(
            class = 'btn-albert',
            shiny::actionButton(inputId = 'show_codes', label = 'Show code', class = 'btn-danger btn-floatsession')
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-contact-us' ++++++++++++++++++++++++ 
        bs4Dash::bs4TabItem(
          tabName = 'tab-contact-us',
          br(),
          {
            html. <- system.file('extdata', 'HTML/Contact.in.html', package = 'MRanalysisBase')
            tryCatch(
              expr = {
                if (nzchar(html.) && file.exists(html.)) {
                  suppressWarnings(shiny::includeHTML(path = html.))
                } else {
                  stop("file not found")
                }
              },
              error = function(e) {
                tags$div(style = "color: grey; font-style: italic;", "404 - Page not found.")
              }
            )
          }
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-intro' ++++++++++++++++++++++++ 
        bs4Dash::bs4TabItem(
          tabName = 'tab-intro',
          br(),
          {
            html. <- system.file('extdata', 'HTML/intro.plugins.MAGMA.html', package = 'MRanalysisBase')
            tryCatch(
              expr = {
                if (nzchar(html.) && file.exists(html.)) {
                  suppressWarnings(shiny::includeHTML(path = html.))
                } else {
                  stop("file not found")
                }
              },
              error = function(e) {
                tags$div(style = "color: grey; font-style: italic;", "404 - Page not found.")
              }
            )
          }
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-upl-exp' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-upl-exp',
          fluidRow(
            
            bs4Dash::box(
              id = 'step_uploaded_data',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 1. Choose SNP data.</strong>"),
              status = 'warning',
              width = 8,
              tags$div(
                id = 'uploaded_data_env_div',
                datamods::import_globalenv_ui(
                  id = 'uploaded_data_env',
                  globalenv = TRUE,
                  packages = c(),
                  title = NULL
                ),
                uiOutput(outputId = 'ui_aesthetics_env'),
              ),
              tags$div(
                id = 'uploaded_data_file_div',
                datamods::import_file_ui(
                  id = 'uploaded_data_file',
                  file_extensions = c('.csv', '.tsv', '.txt', '.bed', '.xls', '.xlsx', '.gz'),
                  title = NULL
                ),
                uiOutput(outputId = 'ui_aesthetics_file')
              ),
              column(12,
                     div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                         shiny::actionButton(inputId = 'use_demo_data', label = 'Use demo data', class = 'btn-warning')
                     )
              ),
              tags$hr(),
              HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>SNP</span>, namely SNP ID in dbSNP databases, an it is an identifier for a SNP, which represents a specific variation in the genome.</span><br>"),
              HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>CHR</span> indicates the chromosome on which the SNP is located in the genome (1, 2, 3, .., X, Y).</span><br>"),
              HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>POS</span> denotes the position of the SNP on the chromosome.</span><br>"),
              HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [optional] <span class='var-var'>P.VALUE</span> is a statistical measure used to assess the assocaition between a SNP and a studied trait, such as a disease. <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;If this value is NULL, perform gene analysis on raw GWAS data. Otherwise, perform gene analysis on SNP P-values.</span>")
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'next_upload_data', label = 'Next', class = 'btn-danger btn-float')
            )
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-one' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-one',
          fluidRow(
            id = 'tab-one-all',
            bs4Dash::box(
              id = 'step_gene_annotation',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 2. Gene Annotation.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              tags$div(
                title = 'For the gene location files, gene locations for protein-coding genes were obtained from the NCBI site. Locations are defined as the region from transcription start site to transcription stop site. These location files contain Entrez gene IDs.',
                shinyWidgets::virtualSelectInput(
                  inputId = 'gene_location',
                  label = h6('Gene location files'),
                  choices = c('Gene locations, build 38' = 'NCBI38', 'Gene locations, build 37' = 'NCBI37.3', 'Gene locations, build 36' = 'NCBI36.3', 'SNP synonyms, dbSNP 151' = 'dbsnp151.synonyms'),
                  search = TRUE,
                  multiple = FALSE,
                  showValueAsTags = TRUE,
                  selected = 'NCBI37.3'
                )
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_gene_annotation', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_gene_annotation', label = 'Next', class = 'btn-danger btn-float')
            )
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-two' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-two',
          fluidRow(
            id = 'tab-two-all',
            bs4Dash::box(
              id = 'step_gene_analysis',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 3. Gene Analysis</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              column(
                width = 8, 
                shinyWidgets::pickerInput(
                  inputId = 'pval_file_status',
                  label = h6('Gene analysis mode'), 
                  choices = c('To perform gene analysis on SNP p-values.' = TRUE, 'To perform gene analysis on raw GWAS data.' = FALSE),
                  multiple = FALSE,
                  selected = TRUE,
                  choicesOpt = list(
                    content = sprintf("<span class='label label-%s'>%s</span>", 
                                      c('info', 'danger'),
                                      c('To perform gene analysis on SNP p-values.', 'To perform gene analysis on raw GWAS data.')))
                )
              ),
              column(
                width = 6, 
                conditionalPanel(
                  condition = paste0('input.pval_file_status === "TRUE"'),
                  tags$div(
                    title = NULL,
                    column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = 'sample_size', label = h6('Sample size'), value = NULL))),
                  )
                )
              ),
              column(
                width = 6, 
                tags$div(
                  title = 'The gene analysis in MAGMA is based on a multiple linear principal components regression model, using an F-test to compute the gene p-value.',
                  shinyWidgets::virtualSelectInput(
                    inputId = 'bfile_type',
                    label = h6('Population'),
                    choices = c('African (AFR)' = 'g1000_afr', 'Admixed American (AMR)' = 'g1000_amr', 'East Asian (EAS)' = 'g1000_eas', 'European (EUR)' = 'g1000_eur', 'South Asian (SAS)' = 'g1000_sas'),
                    search = TRUE,
                    multiple = FALSE,
                    showValueAsTags = TRUE,
                    selected = 'g1000_eur'
                  )
                )
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_gene_analysis', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_gene_analysis', label = 'Next', class = 'btn-danger btn-float')
            )
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-three' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-three',
          fluidRow(
            id = 'tab-three-all',
            bs4Dash::box(
              id = 'step_gene_set_analysis',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 4. Gene-set Analysis</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              shinyWidgets::prettyToggle(inputId = 'which_gene_set', label_on = 'Upload a gene-set file.', label_off = 'Use load demo gene-set file.', icon_on = icon("check"), icon_off = icon("check"), value = FALSE),
              conditionalPanel(
                condition = paste0('input.which_gene_set === true'),
                tags$div(
                  title = NULL,
                  shiny::fileInput(inputId = 'set_annotation_file_upload', label = h6('Gene-set file'), multiple = FALSE, placeholder = 'Upload a gene-set file.')
                )
              ),
              conditionalPanel(
                condition = paste0('input.which_gene_set === false'),
                tags$div(
                  title = NULL,
                  shinyWidgets::pickerInput(
                    inputId = 'set_annotation_file_local',
                    label = h6('Gene-set file'), 
                    choices = list.files(ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/references/MAGMA/msigdb', no = 'E:/references/MAGMA/msigdb')),
                    multiple = FALSE,
                    selected = 'msigdb.v2024.1.Hs.entrez.gmt'
                  )
                )
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_gene_set_analysis', label = 'Run', class = 'btn-danger btn-floatrun'),
              tags$div(id = 'download_gene_set_analysis', shiny::downloadButton(outputId = 'download_gene_set_analysis_button', label = 'Download', class = 'btn-danger btn-floatdl')),
              shiny::actionButton(inputId = 'sessionInfo', label = 'Session Info', class = 'btn-danger btn-floatsession')
            )
          )
        )  # end bs4TabItem
      )
    ),
    controlbar = NULL,
    scrollToTop = TRUE,
    title = 'MAGMA'
  )
}


server.ert <- function(input, output, session) {
  
  shinyjs::hide('tab-one-all')
  shinyjs::hide('tab-two-all')
  shinyjs::hide('tab-three-all')
  shinyjs::hide('tab-r-session-all')
  shinyjs::hide('uploaded_data_env_div')
  shinyjs::hide('confirm_gene_annotation')
  shinyjs::hide('next_gene_annotation')
  shinyjs::hide('confirm_gene_analysis')
  shinyjs::hide('next_gene_analysis')
  shinyjs::hide('confirm_gene_set_analysis')
  shinyjs::hide('download_gene_set_analysis')
  shinyjs::hide('sessionInfo')
  shinyjs::hide('show_codes')
  
  tempdir. <- reactiveVal(tempdir())
  demo.status <- reactiveVal(FALSE)
  pval.file.status <- reactiveVal(TRUE)
  step.one.ifile <- reactiveVal(NULL)
  step.two.ifile <- reactiveVal(NULL)
  dat.gene.annotation <- reactiveVal(NULL)
  dat.gene.analysis <- reactiveVal(NULL)
  dat.gene.set.analysis.gsa.out <- reactiveVal(NULL)
  dat.gene.set.analysis.gsa.genes <- reactiveVal(NULL)
  dat.gene.set.analysis.gsa.sets <- reactiveVal(NULL)
  run.step.two <- reactiveVal(FALSE)
  gene_annotation_random <- reactiveVal(as.numeric(Sys.time()))
  gene_analysis_random <- reactiveVal(as.numeric(Sys.time()))
  gene_analysis_set_random <- reactiveVal(as.numeric(Sys.time()))
  session_random <- reactiveVal(as.numeric(Sys.time()))
  
  imported_env <- datamods::import_globalenv_server(
    id = 'uploaded_data_env',
    return_class = 'tbl_df'
  )
  
  imported_file <- datamods::import_file_server(
    id = 'uploaded_data_file', 
    return_class = 'tbl_df',
    read_fns = list(
      xls = function(file, sheet, skip, encoding) {
        readxl::read_xls(path = file, sheet = sheet, skip = skip)
      },
      xlsx = function(file) { openxlsx::read.xlsx(file) },
      gz = function(file) { data.table::fread(file) })
  )
  
  # Choose Variable
  output[['ui_aesthetics_env']] <- renderUI({
    aesthetics <- c('RSID', 'CHR', 'POS', 'P.VALUE')
    data = imported_env$data()
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, 'sf_column'))
      esquisse::dragulaInput(
        inputId = 'dragvars_env',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        selected = list(
          RSID = MRanalysisBase::multi_pattern_match(var_choices, list("SNP", "rsid", "rs", "id")),
          CHR = MRanalysisBase::multi_pattern_match(var_choices, list("CHR")),
          P.VALUE = MRanalysisBase::multi_pattern_match(var_choices, list("pval", "p", 'LP', 'log10P'))
        ),
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = FALSE
      )
    } else {
      esquisse::dragulaInput(
        inputId = 'dragvars_env',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = '',
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = FALSE
      )
    }
  })
  
  output[['ui_aesthetics_file']] <- renderUI({
    aesthetics <- c('RSID', 'CHR', 'POS', 'P.VALUE')
    spsComps::shinyCatch({
      data = imported_file$data()
    }, trace_back = FALSE)
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, 'sf_column'))
      esquisse::dragulaInput(
        inputId = 'dragvars_file',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        selected = list(
          RSID = MRanalysisBase::multi_pattern_match(var_choices, list("SNP", "rsid", "rs", "id")),
          CHR = MRanalysisBase::multi_pattern_match(var_choices, list("CHR")),
          # POS = MRanalysisBase::multi_pattern_match(var_choices, list("POS", 'BP', 'start')),
          P.VALUE = MRanalysisBase::multi_pattern_match(var_choices, list("pval", "p", 'LP', 'log10P'))
        ),
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = FALSE
      )
    } else {
      esquisse::dragulaInput(
        inputId = 'dragvars_file',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = '',
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = FALSE
      )
    }
  })
  
  observeEvent(input[['use_demo_data']], {
    shinyjs::hide('uploaded_data_file_div')
    shinyjs::hide('use_demo_data')
    shinyjs::show('uploaded_data_env_div')
    
    demo.status(TRUE)
    
    shiny::updateTextInput(session = session, inputId = 'sample_size', label = 'Sample size', value = 339224)
  })
  
  observeEvent(input[['uploaded_data_file-confirm']], {
    shinyjs::hide('use_demo_data')
  })
  
  d.env.status <- reactive({ 
    !is.null(input[['dragvars_env']]$target$RSID) &&
      !is.null(input[['dragvars_env']]$target$CHR) && 
      !is.null(input[['dragvars_env']]$target$POS)
  })
  
  d.file.status <- reactive({ 
    !is.null(input[['dragvars_file']]$target$RSID) &&
      !is.null(input[['dragvars_file']]$target$CHR) && 
      !is.null(input[['dragvars_file']]$target$POS)
  })
  
  observeEvent(input[['next_upload_data']], {
    if (demo.status()) {
      if (d.env.status()) {
        bs4Dash::updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-one'
        )
        shinyjs::show('tab-one-all')
        shinyjs::show('confirm_gene_annotation')
        
        dat = imported_env$data()
        
        dat.one = data.frame(
          RSID = dat[, input[['dragvars_env']]$target$RSID],
          CHR = dat[, input[['dragvars_env']]$target$CHR],
          POS = dat[, input[['dragvars_env']]$target$POS]
        )
        
        if (is.null(input[['dragvars_env']]$target$P.VALUE)) {  shinyWidgets::updatePrettyToggle(session = session, inputId = 'pval_file_status', value = FALSE); pval.file.status(FALSE) }
        dat.two = data.frame(
          RSID = dat[, input[['dragvars_env']]$target$RSID],
          P.VALUE = dat[, input[['dragvars_env']]$target$P.VALUE]
        )
        
        step.one.ifile(glue('{tempdir.()}/IVs.txt'))
        step.two.ifile(glue('{tempdir.()}/SNPP.txt'))
        
        write.table(x = dat.one, file = step.one.ifile(), quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
        write.table(x = dat.two, file = step.two.ifile(), quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
        
      } else {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
    } else {
      if (d.file.status()) {
        bs4Dash::updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-one'
        )
        shinyjs::show('tab-one-all')
        shinyjs::show('confirm_gene_annotation')
        
        dat = imported_file$data()
        
        dat.one = data.frame(
          RSID = dat[, input[['dragvars_file']]$target$RSID],
          CHR = dat[, input[['dragvars_file']]$target$CHR],
          POS = dat[, input[['dragvars_file']]$target$POS]
        )
        
        if (is.null(input[['dragvars_file']]$target$P.VALUE)) {  shinyWidgets::updatePrettyToggle(session = session, inputId = 'pval_file_status', value = FALSE); pval.file.status(FALSE) }
        dat.two = data.frame(
          RSID = dat[, input[['dragvars_file']]$target$RSID],
          P.VALUE = dat[, input[['dragvars_file']]$target$P.VALUE]
        )
        
        step.one.ifile(glue('{tempdir.()}/IVs.txt'))
        step.two.ifile(glue('{tempdir.()}/SNPP.txt'))
        
        write.table(x = dat.one, file = step.one.ifile(), quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
        write.table(x = dat.two, file = step.two.ifile(), quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
        
      } else {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
    }
  })
  
  observeEvent(input[['next_gene_annotation']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-two'
    )
    shinyjs::show('tab-two-all')
    shinyjs::show('confirm_gene_analysis')
  })
  
  observeEvent(input[['next_gene_analysis']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-three'
    )
    shinyjs::show('tab-three-all')
    shinyjs::show('confirm_gene_set_analysis')
  })
  
  observeEvent(input[['sessionInfo']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-r-session'
    )
    shinyjs::show('tab-r-session-all')
  })
  
  observeEvent(input[['sessionInfo']], {
    
    session_random(as.numeric(Sys.time()))
    shiny::removeUI(selector = '#show_sessioninfo', multiple = TRUE, immediate = TRUE)
    shiny::insertUI(
      selector = '#tab-r-session-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'show_sessioninfo',
        title = HTML('Collect Information About the Current R Session'),
        status = 'success',
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ verbatimTextOutput(outputId = paste0('show_sessioninfo_output', session_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Collecting Information About the Current R Session, please wait...'))
    )
    
    output[[paste0('show_sessioninfo_output', session_random())]] <- renderPrint({
      xfun::session_info(c('dplyr', 'DT'))
    })
    
    shinyjs::show('show_codes')
  })
  
  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {
    '# Load your own data. Here use "data.table::fread" and "snp_data.csv" for example.'
    # i.data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/plugins/MAGMA/wdata/snp_data.csv')
    i.data <- data.table::fread('path/to/your/data')
    '# Format data'
    dat.one <- data.frame(
      RSID = i.data[, ..(input[['dragvars_env']]$target$RSID)],
      CHR = i.data[, ..(input[['dragvars_env']]$target$CHR)],
      POS = i.data[, ..(input[['dragvars_env']]$target$POS)]
    )
    if (is.null(..(input[['dragvars_env']]$target$P.VALUE))) {
      dat.two <- data.frame(
        RSID = i.data[, ..(input[['dragvars_env']]$target$RSID)]
      )
    } else {
      dat.two <- data.frame(
        RSID = i.data[, ..(input[['dragvars_env']]$target$RSID)],
        P.VALUE = i.data[, ..(input[['dragvars_env']]$target$P.VALUE)]
      )
    }
    wkdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    step.one.ifile <- glue('{wkdir}/IVs.txt')
    step.two.ifile <- glue('{wkdir}/SNPP.txt')
    write.table(x = dat.one, file = step.one.ifile, quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
    write.table(x = dat.two, file = step.two.ifile, quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
    '# MAGMA'
    magma.path = 'path/to/your/magma_v1.10'
    references.path = 'path/to/your/references'
    # magma.path = 'E:/tools/magma_v1.10'
    magma.bin = glue('{magma.path}/magma.exe')
    reference.path = paste0(references.path, '/reference/', ..(input[["gene_location"]]), '/', ..(input[["gene_location"]]), '.gene.loc')
    bfile.path = paste0(references.path, '/bfile/', ..(input[["bfile_type"]]), '/', ..(input[["bfile_type"]]))
    '# Gene Annotation'
    system(glue('{magma.bin} --annotate --snp-loc {step.one.ifile} --gene-loc {reference.path} --out {wkdir}/Step1'), intern = FALSE)
    '# Gene Analysis'
    if (..(pval.file.status())) {
      system(paste0(glue('{magma.bin} --bfile {bfile.path} --pval {step.two.ifile} N='), ..(input[["sample_size"]]), glue(' --gene-annot {wkdir}/Step1.genes.annot --out {wkdir}/Step2')), intern = FALSE)
    } else {
      system(glue('{magma.bin} --bfile {bfile.path} --gene-annot {wkdir}/Step1.genes.annot --out {wkdir}/Step2'), intern = FALSE)
    }
    '# Gene-set Analysis'
    if (..(input[['which_gene_set']])) {
      gmt.path = ..(input[['set_annotation_file_upload']] )
    } else {
      gmt.path = paste0(references.path, '/msigdb/', ..(input[["set_annotation_file_local"]])) 
    }
    system(glue('{magma.bin} --gene-results {wkdir}/Step2.genes.raw --set-annot {gmt.path} --out {wkdir}/Step3'), intern = FALSE)
  })
  
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(glue)
      }),
      spsComps::shinyCatch({ output[['workflow_code']]() }, trace_back = TRUE)
    )
    
    displayCodeModal(
      code = formatR::tidy_source(text = as.character(code), args.newline = TRUE, output = FALSE)$text.tidy,
      title = "Code to reproduce this workflow.",
      clip = NULL,
      size = 'l'
    )
  })
  #+++++++++++++++++++++++++++++++  Show Code End ++++++++++++++++++++++++++++++
  
  observeEvent(input[['confirm_gene_annotation']], {
    
    gene_annotation_random(as.numeric(Sys.time()))
    dat.gene.annotation(NULL)
    shinyjs::hide('confirm_gene_annotation')
    
    shiny::removeUI(selector = '#step_one_gene_annotation_tbl', multiple = TRUE, immediate = TRUE)
    
    shiny::insertUI(
      selector = '#tab-one-all',
      where = 'beforeBegin',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_one_gene_annotation_tbl',
        title = HTML('Gene Annotation'),
        status = 'success',
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('gene_annotation', gene_annotation_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform gene annotation, please wait...'))
    )
    
    output[[paste0('gene_annotation', gene_annotation_random())]]  <- DT::renderDT({
      
      spsComps::shinyCatch({
        magma.path = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/magma_v1.10', no = 'E:/tools/magma_v1.10')
        references.path = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/references/MAGMA', no = 'E:/references/MAGMA')
        magma.bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = glue('{magma.path}/magma'), no = glue('{magma.path}/magma.exe'))
        reference.path = glue('{references.path}/reference/{isolate(input[["gene_location"]])}/{isolate(input[["gene_location"]])}.gene.loc')
        system(glue('{magma.bin} --annotate --snp-loc {step.one.ifile()} --gene-loc {reference.path} --out {tempdir.()}/Step1'), intern = FALSE)
        A = system(glue('{magma.bin} --annotate --snp-loc {magma.path}/.test/IVs.txt --gene-loc {references.path}/reference/NCBI37.3/NCBI37.3.gene.loc --out {magma.path}/.test/Step1'), intern = TRUE, ignore.stderr = TRUE)
      })
      
      read.albert <- function(file) {
        Lines = readLines(file)
        DF = data.frame()
        for (line in Lines) {
          if (substr(line, start = 1, stop = 1) != '#') {
            linein = stringr::str_split(line, pattern = '\t')[[1]]
            rss = linein[3:length(linein)]
            for (rs in rss) {
              DF. = data.frame(entrez = linein[1], pos = linein[2], SNP = rs)
              DF = rbind(DF, DF.)
            }
          }
        }
        return(DF)
      }
      
      spsComps::shinyCatch({
        if (Sys.info()['sysname'] == 'Linux') {
          dat.one = read.albert(file = glue('{tempdir.()}/Step1.genes.annot'))
        } else {
          dat.one = read.albert(file = glue('{tempdir.()}/Step1.genes.annot.txt'))
        }
        colnames(dat.one) = c('Entrez', 'CHR:START:END', 'rs')
        dat.gene.annotation(dat.one)
      }, trace_back = FALSE)
      
      collapse_br_html <- function(go_vec, n = 3) {
        go_vec <- unique(unlist(strsplit(go_vec, "<br>")))
        go_vec <- go_vec[!grepl("NA", go_vec)]
        len <- length(go_vec)
        if (len == 0) {
          return('-')
        } else if (len <= n) { 
          return(paste(go_vec, collapse = "<br>"))
        }
        uniq_id <- paste0("goid_", paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
        shown <- paste(go_vec[1:n], collapse = "<br>")
        hidden <- paste(go_vec, collapse = "<br>")
        
        html <- sprintf(
          '<span id="%s_shown">%s<br>
      <span style="cursor:pointer;color:#1a73e8;" onclick="document.getElementById(\'%s_shown\').style.display=\'none\';document.getElementById(\'%s_hidden\').style.display=\'inline\';">…</span>
    </span>
    <span id="%s_hidden" style="display:none;">
      %s<br>
      <span style="cursor:pointer;color:#1a73e8;" onclick="document.getElementById(\'%s_hidden\').style.display=\'none\';document.getElementById(\'%s_shown\').style.display=\'inline\';">…</span>
    </span>',
          uniq_id, shown, uniq_id, uniq_id, uniq_id, hidden, uniq_id, uniq_id
        )
        gsub("\n\\s+", "", html)
      }
      
      if (TRUE) {
        KEGG.N <- MRanalysisBase::KEGG.N
      }
      
      if (TRUE) {
        DrugBank <- MRanalysisBase::DrugBank
        DrugBank.tmp <- DrugBank %>% dplyr::select(DID, UNIPORT) %>% distinct()
        DGP.N <- DrugBank.tmp$DID
        names(DGP.N) <-  DrugBank.tmp$UNIPORT
        DrugBank.tmp <- DrugBank %>% dplyr::select(DID, DRUG.NAME) %>% distinct()
        DGN.N <- DrugBank.tmp$DRUG.NAME
        names(DGN.N) <-  DrugBank.tmp$DID
        rm(DrugBank.tmp)
      }
      
      if (TRUE) {
        OpenTargetS <- MRanalysisBase::OpenTargetS
        OpenTargetS.tmp <- OpenTargetS %>% dplyr::select(DID, ENSEMBL) %>% distinct()
        OTG.N <- OpenTargetS.tmp$DID
        names(OTG.N) <-  OpenTargetS.tmp$ENSEMBL
        OpenTargetS.tmp <- OpenTargetS %>% dplyr::select(DID, DRUG.NAME) %>% distinct()
        OTN.N <- OpenTargetS.tmp$DRUG.NAME
        names(OTN.N) <-  OpenTargetS.tmp$DID
        OpenTargetS.tmp <- OpenTargetS %>% dplyr::select(DID, NCTID) %>% distinct() %>% na.omit() %>% tidyr::separate_rows(NCTID, sep = ",") %>% distinct()
        DCN.N <- OpenTargetS.tmp$NCTID
        names(DCN.N) <-  OpenTargetS.tmp$DID
        rm(OpenTargetS.tmp)
      }
      
      cols <- c('SYMBOL', 'GENENAME', 'ENSEMBL', 'MAP', 'UNIPROT', 'GO', 'PATH')
      anndf <- suppressWarnings({ AnnotationDbi::select(org.Hs.eg.db, keys = dat.gene.annotation()$Entrez, columns = cols, keytype = "ENTREZID") })
      GO.N <- AnnotationDbi::Term(unique(na.omit(anndf$GO)))
      annDF <- anndf %>%
        dplyr::group_by(ENTREZID) %>%
        dplyr::mutate(
          GO. = case_when(
            is.na(GO) ~ '',
            TRUE ~ glue::glue('<a href="https://www.ebi.ac.uk/QuickGO/term/{GO}" target="_blank" title="Evidence Level: {EVIDENCE}">{GO} [{ONTOLOGY}] {GO.N[GO]}</a>')
          ),
          UNIPROT. = case_when(
            is.na(UNIPROT) ~ '',
            TRUE ~ glue::glue('<a href="https://www.uniprot.org/uniprotkb/{UNIPROT}/entry" target="_blank">{UNIPROT}</a>')
          ),
          PATH = case_when(
            is.na(PATH) ~ '',
            TRUE ~ glue::glue('hsa{PATH}')
          ),
          PATH. = case_when(
            PATH == '' ~ '',
            TRUE ~ glue::glue('<a href="https://www.kegg.jp/entry/{PATH}" target="_blank">{PATH} {KEGG.N[PATH]}</a>')
          ),
          DRUGBANK = case_when(
            is.na(UNIPROT) ~ '',
            TRUE ~ DGP.N[UNIPROT]
          ),
          DRUGBANK. = case_when(
            DRUGBANK == '' ~ '',
            TRUE ~ glue::glue('<a href="https://go.drugbank.com/drugs/{DRUGBANK}" target="_blank" title="{DRUGBANK}">{DGN.N[DRUGBANK]}</a>')
          ),
          CHEMBL = case_when(
            is.na(ENSEMBL) ~ '',
            TRUE ~ OTG.N[ENSEMBL]
          ),
          CHEMBL. = case_when(
            CHEMBL == '' ~ '',
            TRUE ~ glue::glue('<a href="https://www.ebi.ac.uk/chembl/explore/compound/{CHEMBL}" target="_blank" title="{CHEMBL}">{OTN.N[CHEMBL]}</a>')
          ),
          NCTID = case_when(
            is.na(DCN.N[CHEMBL]) ~ '',
            TRUE ~ DCN.N[CHEMBL]
          ),
          NCTID. = case_when(
            NCTID == '' ~ '',
            TRUE ~ glue::glue('<a href="https://clinicaltrials.gov/study/{NCTID}" target="_blank" title="{NCTID}, {CHEMBL}">{NCTID}</a>')
          ),
        ) %>%
        dplyr::reframe(
          GO. = collapse_br_html(unique(GO.), n = 3),
          UNIPROT. = collapse_br_html(unique(UNIPROT.), n = 3),
          PATH. = collapse_br_html(unique(PATH.), n = 3),
          DRUGBANK. = collapse_br_html(unique(DRUGBANK.), n = 3),
          CHEMBL. = collapse_br_html(unique(CHEMBL.), n = 3),
          NCTID. = collapse_br_html(unique(NCTID.), n = 3),
          across(-matches("\\.$"), ~paste(unique(na.omit(.)), collapse = ","))
        ) %>%
        dplyr::select(-EVIDENCE, -ONTOLOGY) %>%
        dplyr::rename(Entrez = ENTREZID) %>%
        dplyr::right_join(dat.gene.annotation(), by = 'Entrez') %>%
        dplyr::mutate(
          ENTREZID. = case_when(
            is.na(Entrez) ~ '',
            TRUE ~ glue::glue('<a href="https://www.ncbi.nlm.nih.gov/gene/?term={Entrez}" target="_blank">{Entrez}</a>')
          ),
          rs. = case_when(
            is.na(rs) ~ '',
            TRUE ~ glue::glue('<a href="https://www.ncbi.nlm.nih.gov/snp/?term={rs}" target="_blank">{rs}</a>')
          )
        ) %>%
        dplyr::select(Entrez, ENTREZID., `CHR:START:END`, rs, rs., SYMBOL, GENENAME, dplyr::everything())
      
      scols <- c('ENTREZID.', 'CHR:START:END', 'rs.', 'SYMBOL', 'GENENAME', 'GO.', 'PATH.', 'UNIPROT.', 'DRUGBANK.', 'CHEMBL.', 'NCTID.')
      dcols <- c('Entrez', 'CHR:START:END', 'rs', 'SYMBOL', 'GO', 'PATH', 'UNIPROT', 'DRUGBANK', 'CHEMBL', 'NCTID')
      inds <- MRanalysisBase::get_col_index0(annDF, dcols)
      DT::datatable(
        annDF, 
        rownames = FALSE, 
        escape = FALSE,
        selection = 'single', 
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtlip',  
          scrollX = TRUE,
          paging = TRUE, 
          columnDefs = list(
            list(visible = FALSE, targets = setdiff(seq(ncol(annDF)) - 1, MRanalysisBase::get_col_index0(annDF, scols)))
          ),
          buttons = list(
            list(extend = 'copy', filename =  'gene_annotation', title = 'gene_annotation', exportOptions = list(columns = inds, modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'gene_annotation', title = 'gene_annotation', exportOptions = list(columns = inds, modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'gene_annotation', title = NULL, exportOptions = list(columns = inds, modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'gene_annotation', title = NULL, exportOptions = list(columns = inds, modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(5, 10, 20, 50, 100, -1), c('5', '10', '30', '50', '100', 'All')))
      )
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_one_gene_annotation_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_gene_annotation_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    observeEvent(input$table_gene_annotation_loaded, {
      shinyjs::show('next_gene_annotation')
    })
  })
  
  observeEvent(input[['confirm_gene_analysis']], {
    
    gene_analysis_random(as.numeric(Sys.time()))
    run.step.two(TRUE)
    dat.gene.analysis(NULL)
    shinyjs::hide('confirm_gene_analysis')
    shinyjs::hide('step_gene_analysis')
    
    shiny::removeUI(selector = '#step_two_gene_analysis_tbl', multiple = TRUE, immediate = TRUE)
    
    shiny::insertUI(
      selector = '#tab-two-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_two_gene_analysis_tbl',
        title = HTML('Gene Analysis'),
        status = 'success',
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('gene_analysis', gene_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform gene analysis, please wait...'))
    )
    
    output[[paste0('gene_analysis', gene_analysis_random())]]  <- DT::renderDT({
      
      if (run.step.two()) {
        spsComps::shinyCatch({
          magma.path = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/magma_v1.10', no = 'E:/tools/magma_v1.10')
          references.path = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/references/MAGMA', no = 'E:/references/MAGMA')
          magma.bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = glue('{magma.path}/magma'), no = glue('{magma.path}/magma.exe'))
          bfile.path = glue('{references.path}/bfile/{isolate(input[["bfile_type"]])}/{isolate(input[["bfile_type"]])}')
          if (isolate(pval.file.status())) {
            system(glue('{magma.bin} --bfile {bfile.path} --pval {step.two.ifile()} N={isolate(input[["sample_size"]])} --gene-annot {tempdir.()}/Step1.genes.annot --out {tempdir.()}/Step2'), intern = FALSE)
          } else {
            system(glue('{magma.bin} --bfile {bfile.path} --gene-annot {tempdir.()}/Step1.genes.annot --out {tempdir.()}/Step2'), intern = FALSE)
          }
        })
        
        spsComps::shinyCatch({
          if (Sys.info()['sysname'] == 'Linux') {
            dat.two = data.table::fread(file = glue('{tempdir.()}/Step2.genes.out'), header = TRUE)
          } else {
            dat.two = data.table::fread(file = glue('{tempdir.()}/Step2.genes.out.txt'), header = TRUE)
          }
          dat.gene.analysis(dat.two)
        }, trace_back = FALSE)
      }
      
      if (!is.null(dat.gene.analysis())) {
        shinyjs::show('next_gene_analysis')
        shinyjs::show('step_gene_analysis')
      }
      
      if (run.step.two()) {
        dat.gene.analysis() %>%
          formattable::formattable(
            x = .,
            list(
              P = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', 'red')))
            )) -> dt
        as.datatable(
          dt, 
          rownames = FALSE, 
          selection = 'single', 
          extensions = 'Buttons', 
          options = list(
            dom = 'Bfrtlip',  
            scrollX = TRUE,
            paging = TRUE, 
            buttons = list(
              list(extend = 'copy', filename =  'gene_analysis', title = 'gene_analysis', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'gene_analysis', title = 'gene_analysis', exportOptions = list(modifier = list(page = 'current'))),
              list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = 'gene_analysis', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                  list(extend = 'excel', filename = 'gene_analysis', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                ),
                text = 'Download data')),
            lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All')))
        )
      }
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_two_gene_analysis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_gene_analysis_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    observeEvent(input$table_gene_analysis_loaded, {
      if (!is.null(dat.gene.analysis())) {
        shinyjs::show('next_gene_analysis')
        shinyjs::show('step_gene_analysis')
      }
    })
  })
  
  observeEvent(input[['confirm_gene_set_analysis']], {
    
    gene_analysis_set_random(as.numeric(Sys.time()))
    shinyjs::hide('confirm_gene_set_analysis')
    
    shiny::removeUI(selector = '#step_two_gene_set_analysis_gsa_genes_tbl', multiple = TRUE, immediate = TRUE)
    shiny::removeUI(selector = '#step_two_gene_set_analysis_gsa_sets', multiple = TRUE, immediate = TRUE)
    shiny::removeUI(selector = '#step_two_gene_set_analysis_gsa_out_tbl', multiple = TRUE, immediate = TRUE)
    
    shiny::insertUI(
      selector = '#tab-three-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_two_gene_set_analysis_gsa_genes_tbl',
        title = HTML('Summary of Gene Association Results'),
        status = 'success',
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('gene_set_analysis_gsa_genes', gene_analysis_set_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform Gene-set Analysis, please wait...'))
    )
    
    shiny::insertUI(
      selector = '#tab-three-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_two_gene_set_analysis_gsa_sets',
        title = HTML('Significant Pathways after Multiple Testing Adjustment'),
        status = 'success',
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ verbatimTextOutput(outputId = paste0('gene_set_analysis_gsa_sets', gene_analysis_set_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform Gene-set Analysis, please wait...'))
    )
    
    shiny::insertUI(
      selector = '#tab-three-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_two_gene_set_analysis_gsa_out_tbl',
        title = HTML('Summary of Pathway Enrichment Analysis'),
        status = 'success',
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('gene_set_analysis_gsa_out', gene_analysis_set_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform Gene-set Analysis, please wait...'))
    )
    
    output[[paste0('gene_set_analysis_gsa_out', gene_analysis_set_random())]]  <- DT::renderDT({
      
      spsComps::shinyCatch({
        magma.path = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/magma_v1.10', no = 'E:/tools/magma_v1.10')
        references.path = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/references/MAGMA', no = 'E:/references/MAGMA')
        magma.bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = glue('{magma.path}/magma'), no = glue('{magma.path}/magma.exe'))
        if (input[['which_gene_set']]) {
          gmt.path = input[['set_annotation_file_upload']] 
        } else {
          gmt.path = glue('{references.path}/msigdb/{input[["set_annotation_file_local"]]}') 
        }
        system(glue('{magma.bin} --gene-results {tempdir.()}/Step2.genes.raw --set-annot {gmt.path} --out {tempdir.()}/Step3'), intern = FALSE)
      })
      
      spsComps::shinyCatch({
        if (Sys.info()['sysname'] == 'Linux') {
          dat.three.gsa.out = data.table::fread(file = glue('{tempdir.()}/Step3.gsa.out'), header = TRUE, skip = 2)
          dat.three.gsa.genes = data.table::fread(file = glue('{tempdir.()}/Step3.gsa.genes.out'), header = TRUE, skip = 1)
          dat.three.gsa.sets = readLines(glue('{tempdir.()}/Step3.gsa.sets.genes.out'))
        } else {
          dat.three.gsa.out = data.table::fread(file = glue('{tempdir.()}/Step3.gsa.out.txt'), header = TRUE, skip = 2)
          dat.three.gsa.genes = data.table::fread(file = glue('{tempdir.()}/Step3.gsa.genes.out.txt'), header = TRUE, skip = 1)
          dat.three.gsa.sets = readLines(glue('{tempdir.()}/Step3.gsa.sets.genes.out.txt'))
        }
        dat.gene.set.analysis.gsa.out(dat.three.gsa.out)
        dat.gene.set.analysis.gsa.genes(dat.three.gsa.genes)
        dat.gene.set.analysis.gsa.sets(dat.three.gsa.sets)
      }, trace_back = FALSE)
      
      dat.gene.set.analysis.gsa.out() %>% arrange(P) %>%
        formattable::formattable(
          x = .,
          list(
            P = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', 'red')))
          )) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single', 
        extensions = 'Buttons', 
        options = list(
          dom = 'Bfrtlip',  
          scrollX = TRUE,
          paging = TRUE, 
          buttons = list(
            list(extend = 'copy', filename =  'gsa.out', title = 'gsa.out', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'gsa.out', title = 'gsa.out', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'gsa.out', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'gsa.out', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All')))
      )
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_two_gene_set_analysis_gsa_out_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_gene_set_analysis_gas_out_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    observeEvent(input$table_gene_set_analysis_gas_out_loaded, {
      shinyjs::show('sessionInfo')
      shinyjs::hide('download_gene_set_analysis')
    })
    
    output[[paste0('gene_set_analysis_gsa_genes', gene_analysis_set_random())]]  <- DT::renderDT({
      dat.gene.set.analysis.gsa.genes() %>%
        formattable::formattable(x = .) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single',
        extensions = 'Buttons', 
        options = list(
          dom = 'Bfrtlip',  
          scrollX = TRUE,
          paging = TRUE, 
          buttons = list(
            list(extend = 'copy', filename =  'gsa.out', title = 'gsa.out', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'gsa.out', title = 'gsa.out', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'gsa.out', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'gsa.out', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All')))
      )
    })
    
    output[[paste0('gene_set_analysis_gsa_sets', gene_analysis_set_random())]] <- renderPrint({
      spsComps::shinyCatch({
        cat(dat.gene.set.analysis.gsa.sets(), sep = '\n')
      }, trace_back = FALSE)
    })
    
  })
  
  observeEvent({
    input[['dragvars_env']]
    input[['dragvars_file']]
    input[['uploaded_data_env-confirm']]
    input[['uploaded_data_file-confirm']]
  }, {
    shinyjs::hide('tab-one-all')
    shinyjs::hide('tab-two-all')
    shinyjs::hide('tab-three-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('confirm_gene_annotation')
    shinyjs::hide('next_gene_annotation')
    shinyjs::hide('confirm_gene_analysis')
    shinyjs::hide('next_gene_analysis')
    shinyjs::hide('confirm_gene_set_analysis')
    shinyjs::hide('download_gene_set_analysis')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['gene_location']]
  }, {
    shiny::removeUI(selector = '#step_one_gene_annotation_tbl', multiple = TRUE, immediate = TRUE)
    shinyjs::show('confirm_gene_annotation')
    shinyjs::hide('tab-two-all')
    shinyjs::hide('tab-three-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('next_gene_annotation')
    shinyjs::hide('confirm_gene_analysis')
    shinyjs::hide('next_gene_analysis')
    shinyjs::hide('confirm_gene_set_analysis')
    shinyjs::hide('download_gene_set_analysis')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['pval_file_status']]
    input[['sample_size']]
    input[['bfile_type']]
  }, {
    run.step.two(FALSE)
    dat.gene.analysis(NULL)
    shiny::removeUI(selector = '#step_two_gene_analysis_tbl', multiple = TRUE, immediate = TRUE)
    shinyjs::show('confirm_gene_analysis')
    shinyjs::hide('tab-three-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('next_gene_analysis')
    shinyjs::hide('confirm_gene_set_analysis')
    shinyjs::hide('download_gene_set_analysis')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
  })
}

# Use for standalone testing, 
# comment out the following line when used as a component.
# shinyApp(ui = ui.alb, server = server.ert, options = list(host = '10.0.12.2', port = 3390))
shinyApp(ui = ui.alb, server = server.ert, options = list(port = 80))