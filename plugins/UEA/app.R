#' @title
#'
#' @description
#' 

# Load R packages
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(fresh)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinyjs)))
suppressMessages(suppressWarnings(library(bs4Dash)))
suppressMessages(suppressWarnings(library(tibble)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(shinymeta)))
suppressMessages(suppressWarnings(library(shinycssloaders)))
suppressMessages(suppressWarnings(library(formattable)))
suppressMessages(suppressWarnings(library(clusterProfiler)))
suppressMessages(suppressWarnings(library(org.Hs.eg.db)))
suppressMessages(suppressWarnings(library(ggplot2)))

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

#
ui.alb <- function() {
  
  gene_data <<- readxl::read_xlsx('demo/genes.xlsx')
  pathway_data <<- readxl::read_xlsx('demo/pathways.xlsx')
  
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
        href = MRanalysisBase::MR.HOME
      )
    ),
    sidebar = bs4DashSidebar(
      skin = 'light',
      bs4SidebarMenu(
        id = 'sidebarmenu',
        br(),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Introduction</p>'), tabName = 'tab-intro', icon = icon('house', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Load Data</p>'), tabName = 'tab-upl-exp', icon = icon('upload', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Enrichment Analysis</p>'), tabName = 'tab-ana', icon = icon('calculator',lib = 'font-awesome', class = 'nav-icon')),
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
        tags$title('UEA')
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
            html. <- system.file('extdata', 'HTML/intro.plugins.UEA.html', package = 'MRanalysisBase')
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
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 1. Choose Gene/Protein/Metabolite/... data.</strong>"),
              status = 'warning',
              width = 8,
              tags$div(
                id = 'uploaded_data_env_div',
                mainPanel(
                  class = 'show-data-col-sm-12',
                  width = 12,
                  shiny::tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "1. Choose Gene/Protein/Metabolite/...",
                      datamods::import_globalenv_ui(
                        id = 'uploaded_data_env_gene',
                        globalenv = TRUE,
                        packages = c(),
                        title = NULL
                      ),
                      HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> Step 2. Choose variables</strong></div>")),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>item</span>, select <span class='var-num'>one</span> column as input to the Universal Pathway Enrichment Analysis.</span>"),
                      uiOutput(outputId = 'ui_aesthetics_env_gene'),
                    ),
                    tabPanel(
                      "2. Import the pathway database",
                      datamods::import_globalenv_ui(
                        id = 'uploaded_data_env_pathway',
                        globalenv = TRUE,
                        packages = c(),
                        title = NULL
                      ),
                      HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> Step 2. Choose variables</strong></div>")),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>item</span>, select <span class='var-num'>one</span> column, the id or name of the Gene/Protein/Metabolite/... in the pathway database.</span>"),
                      tags$br(),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>pathway</span>, select <span class='var-num'>one</span> column, the id or name of the pathway in the pathway database.</span>"),
                      uiOutput(outputId = 'ui_aesthetics_env_pathway'),
                    ),
                  )
                )
                
                
              ),
              tags$div(
                id = 'uploaded_data_file_div',
                mainPanel(
                  class = 'show-data-col-sm-12',
                  width = 12,
                  shiny::tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "1. Choose Gene/Protein/Metabolite/...",
                      datamods::import_file_ui(
                        id = 'uploaded_data_file_gene',
                        file_extensions = c('.csv', '.tsv', '.txt', '.xls', '.xlsx'),
                        title = NULL
                      ),
                      HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> Step 2. Choose variables</strong></div>")),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>item</span>, select <span class='var-num'>one</span> column as input to the Universal Pathway Enrichment Analysis.</span>"),
                      uiOutput(outputId = "ui_aesthetics_file_gene"),
                    ),
                    tabPanel(
                      "2. Import the pathway database",
                      datamods::import_file_ui(
                        id = 'uploaded_data_file_pathway',
                        file_extensions = c('.csv', '.tsv', '.txt', '.xls', '.xlsx'),
                        title = NULL
                      ),
                      HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> Step 2. Choose variables</strong></div>")),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>item</span>, select <span class='var-num'>one</span> column, the id or name of the Gene/Protein/Metabolite/... in the pathway database.</span>"),
                      tags$br(),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>pathway</span>, select <span class='var-num'>one</span> column, the id or name of the pathway in the pathway database.</span>"),
                      uiOutput(outputId = "ui_aesthetics_file_pathway"),
                    ),
                  )
                )
              ),
              column(12,
                     div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                         shiny::actionButton(inputId = 'use_demo_data', label = 'Use demo data', class = 'btn-warning')
                     ),
                     div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                         shiny::downloadButton(outputId = 'download_demo_data', label = 'Download demo data', class = 'btn-success')
                     )
              ),
              tags$hr()
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'next_upload_data', label = 'Next', class = 'btn-danger btn-float')
            )
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-one' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-ana',
          fluidRow(
            id = 'tab-ana-all',
            bs4Dash::box(
              id = 'step_para_one',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 2. Parameters for GO Enrichment Analysis.</strong>"),
              status = 'warning',
              width = 4,
              collapsed = FALSE,
              collapsible = TRUE,
              column(width = 12,
                     tags$div(
                       title = 'one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"',
                       shinyWidgets::virtualSelectInput(inputId = 'pAdjustMethod', label = h5("pAdjustMethod"), choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), search = TRUE, multiple = FALSE, selected = 'BH')
                     ))
            ),
            bs4Dash::box(
              id = 'step_para_twoa',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;</strong>"),
              status = 'warning',
              width = 4,
              collapsed = FALSE,
              collapsible = TRUE,
              column(width = 12,
                     tags$div(
                       title = 'adjusted pvalue cutoff on enrichment tests to report',
                       shiny::sliderInput(inputId = 'pvalueCutoff', label = h5("p.value.Cutoff"), min = 0, max = 1, value = 0.05, step = 0.01)
                     )),
              column(width = 12,
                     tags$div(
                       title = 'qvalue cutoff on enrichment tests to report as significant. Tests must pass i) pvalueCutoff on unadjusted pvalues, ii) pvalueCutoff on adjusted pvalues and iii) qvalueCutoff on qvalues to be reported.',
                       shiny::sliderInput(inputId = 'qvalueCutoff', label = h5("q.value.Cutoff"), min = 0, max = 1, value = 0.2, step = 0.01)
                     ))
            ),
            bs4Dash::box(
              id = 'step_para_twob',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;</strong>"),
              status = 'warning',
              width = 4,
              collapsed = FALSE,
              collapsible = TRUE,
              column(width = 12,
                     tags$div(
                       title = 'minimal size of genes annotated by Ontology term for testing.',
                       numericInput(inputId = 'minGSSize', label = h5("Minimal Size of Genes"), min = 1, value = 10, step = 10)
                     )),
              column(width = 12,
                     tags$div(
                       title = 'maximal size of genes annotated for testing.',
                       numericInput(inputId = 'maxGSSize', label = h5("Maximal  Size of Genes"), min = 1, value = 500, step = 100)
                     ))
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_analysis', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'sessionInfo', label = 'Session Info', class = 'btn-danger btn-floatsession')
            )
          )
        )  # end bs4TabItem
      )
    ),
    controlbar = NULL,
    scrollToTop = TRUE,
    title = 'UEA'
  )
}


server.ert <- function(input, output, session) {
  
  shinyjs::hide('tab-ana')
  shinyjs::hide('tab-r-session-all')
  shinyjs::hide('uploaded_data_env_div')
  shinyjs::hide('sessionInfo')
  shinyjs::hide('show_codes')
  
  demo.status <- reactiveVal(FALSE)
  uea.table <- reactiveVal(NULL)
  enrichment_data_random <- reactiveVal(as.numeric(Sys.time()))
  session_random <- reactiveVal(as.numeric(Sys.time()))
 
  # Import Data
  imported_env_gene <- datamods::import_globalenv_server(
    id = 'uploaded_data_env_gene',
    return_class = 'tbl_df'
  )
  
  imported_file_gene <- datamods::import_file_server(
    id = 'uploaded_data_file_gene', 
    return_class = 'tbl_df',
    read_fns = list(
      xls = function(file, sheet, skip, encoding) {
        readxl::read_xls(path = file, sheet = sheet, skip = skip)
      },
      xlsx = function(file) { openxlsx::read.xlsx(file) },
      gz = function(file) { data.table::fread(file) })
  )
  
  imported_env_pathway <- datamods::import_globalenv_server(
    id = 'uploaded_data_env_pathway',
    return_class = 'tbl_df'
  )
  
  imported_file_pathway <- datamods::import_file_server(
    id = 'uploaded_data_file_pathway', 
    return_class = 'tbl_df',
    read_fns = list(
      xls = function(file, sheet, skip, encoding) {
        readxl::read_xls(path = file, sheet = sheet, skip = skip)
      },
      xlsx = function(file) { openxlsx::read.xlsx(file) },
      gz = function(file) { data.table::fread(file) })
  )
  
  # Choose Variable
  output[['ui_aesthetics_env_gene']] <- renderUI({
    aesthetics <- c('item')
    data = imported_env_gene$data()
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, 'sf_column'))
      esquisse::dragulaInput(
        inputId = 'dragvars_env_gene',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        # selected = list(item = find.first(var_choices, 'item')),
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    } else {
      esquisse::dragulaInput(
        inputId = 'dragvars_env_gene',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = '',
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    }
  })
  
  output[['ui_aesthetics_file_gene']] <- renderUI({
    aesthetics <- c('item')
    spsComps::shinyCatch({
      data = imported_file_gene$data()
    }, trace_back = FALSE)
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, 'sf_column'))
      esquisse::dragulaInput(
        inputId = 'dragvars_file_gene',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        # selected = list(item = find.first(var_choices, 'item')),
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    } else {
      esquisse::dragulaInput(
        inputId = 'dragvars_file_gene',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = '',
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    }
  })
  
  output[['ui_aesthetics_env_pathway']] <- renderUI({
    aesthetics <- c('item', 'pathway')
    data = imported_env_pathway$data()
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, 'sf_column'))
      esquisse::dragulaInput(
        inputId = 'dragvars_env_pathway',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        # selected = list(item = find.first(var_choices, 'item'), pathway = find.first(var_choices, 'pathway')),
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    } else {
      esquisse::dragulaInput(
        inputId = 'dragvars_env_pathway',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = '',
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    }
  })
  
  output[['ui_aesthetics_file_pathway']] <- renderUI({
    aesthetics <- c('item', 'pathway')
    spsComps::shinyCatch({
      data = imported_file_pathway$data()
    }, trace_back = FALSE)
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, 'sf_column'))
      esquisse::dragulaInput(
        inputId = 'dragvars_file_pathway',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choiceValues = var_choices,
        choiceNames = esquisse:::badgeType(
          col_name = var_choices,
          col_type = esquisse:::col_type(data[, var_choices])
        ),
        # selected = list(item = find.first(var_choices, 'item'), pathway = find.first(var_choices, 'pathway')),
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    } else {
      esquisse::dragulaInput(
        inputId = 'dragvars_file_pathway',
        sourceLabel = 'Variables',
        targetsLabels = c(aesthetics),
        targetsIds = c(aesthetics),
        choices = '',
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
      )
    }
  })
  
  
  observeEvent(input[['use_demo_data']], {
    shinyjs::hide('uploaded_data_file_div')
    shinyjs::hide('use_demo_data')
    shinyjs::hide('download_demo_data')
    shinyjs::show('uploaded_data_env_div')
    
    demo.status(TRUE)
    
    shiny::updateTextInput(session = session, inputId = 'sample_size', label = 'Sample size', value = 339224)
  })
  
  observeEvent(input[['uploaded_data_file-confirm']], {
    shinyjs::hide('use_demo_data')
  })
  
  d.env.status.gene <- reactive({ 
    !is.null(input[['dragvars_env_gene']]$target$item)
  })
  
  d.file.status.gene <- reactive({ 
    !is.null(input[['dragvars_file_gene']]$target$item)
  })
  
  d.env.status.pathway <- reactive({ 
    !is.null(input[['dragvars_env_pathway']]$target$item) &&
      !is.null(input[['dragvars_env_pathway']]$target$pathway)
  })
  
  d.file.status.pathway <- reactive({ 
    !is.null(input[['dragvars_file_pathway']]$target$item) &&
      !is.null(input[['dragvars_env_pathway']]$target$pathway)
  })
  
  observeEvent(input[['next_upload_data']], {
    if (demo.status()) {
      if (!d.env.status.gene()) {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
      if (!d.env.status.pathway()) {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
      if (d.env.status.gene() && d.env.status.pathway()) {
        updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-ana'
        )
      }
    } else {
      if (!d.file.status.gene()) {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
      if (!d.file.status.pathway()) {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
      if (d.file.status.gene() && d.file.status.pathway()) {
        updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-ana'
        )
      }
    }
  })
  
  
  # 获取各个值
  i.data.gene <- reactive({
    if (demo.status()) {
      try.c <- try({
        tmp <- imported_env_gene$data()
        tmp <- tmp %>% dplyr::select(
          input[['dragvars_env_gene']]$target$item) %>% data.frame()
        tmp[, 1]
      }, silent = QUITE)
    } else {
      try.c <- try({
        tmp <- imported_file_gene$data()
        tmp <- tmp %>% dplyr::select(
          input[['dragvars_file_gene']]$target$item) %>% data.frame()
        tmp[, 1]
      }, silent = QUITE)
    }
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  i.data.pathway <- reactive({
    if (demo.status()) {
      try.c <- try({
        tmp <- imported_env_pathway$data()
        tmp <- tmp %>% dplyr::select(
          input[['dragvars_env_pathway']]$target$pathway, input[['dragvars_env_pathway']]$target$item) %>% data.frame()
      }, silent = QUITE)
    } else {
      try.c <- try({
        tmp <- imported_file_pathway$data()
        tmp <- tmp %>% dplyr::select(
          input[['dragvars_file_pathway']]$target$pathway, input[['dragvars_file_pathway']]$target$item) %>% data.frame()
      }, silent = QUITE)
    }
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
    tmp
  })
  
  
  observeEvent(input[['confirm_analysis']], {
    
    shinyjs::hide('confirm_analysis')
    shinyjs::hide('tab-ana-all')
    
    shiny::removeUI(selector = '#step_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_analysis_plot_box', immediate = TRUE)
    shiny::insertUI(
      selector = '#tab-ana-all',
      where = 'beforeBegin',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_analysis_tbl',
        title = HTML('GO Enrichment Result'),
        status = 'success',
        width = 12,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('enrichment_data', enrichment_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Performing GO Enrichment Analysis, please be patient...'))
    )
    
    shiny::insertUI(
      selector = '#tab-ana-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::tabBox(
        id = 'step_analysis_plot',
        title = NULL,
        width = 12,
        selected = 'Dot Plot',
        status = 'success',
        solidHeader = FALSE,
        type = 'tabs',
        tabPanel(
          title = 'Dot Plot',
          fluidRow(
            column(width = 3, shiny::sliderInput(inputId = 'dot_plot_top', label = h6("Show Top Items"), min = 1, max = 20, value = 5, step = 1)),
            column(width = 3, shiny::sliderInput(inputId = 'dot_plot_width', label = h6("WIDTH"), min = 100, max = 1280, value = 1080, step = 20)),
            column(width = 3, shiny::sliderInput(inputId = 'dot_plot_height', label = h6("HEIGHT"), min = 100, max = 1280, value = 600, step = 20))
          ),
          withSpinner({ uiOutput(outputId = paste0('dot_plot', enrichment_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Draw dot plot, please wait...')
        )
      )
    )
    
    
    output[[paste0('enrichment_data', enrichment_data_random())]] <- DT::renderDT({
      
      message(glue('{Sys.time()} Pathway Enrichment Analysis Starts Running'))
      
      # 读入基因/代谢物等信息
      # ------------------------------------------------------------------------------------------------------------
      items <- i.data.gene()
      pathways <- i.data.pathway()
      
      # 开始富集分析
      # ------------------------------------------------------------------------------------------------------------
      kk = clusterProfiler::enricher(
        gene = items,
        pvalueCutoff = isolate(input[['pvalueCutoff']]),
        pAdjustMethod = isolate(input[['pAdjustMethod']]),
        universe = NULL,
        minGSSize = isolate(input[['minGSSize']]),
        maxGSSize = isolate(input[['maxGSSize']]),
        qvalueCutoff = isolate(input[['qvalueCutoff']]),
        TERM2GENE = pathways
      )
      
      kk@result <- kk@result %>%
        dplyr::rename(
          itemRatio = GeneRatio,
          itemID = geneID
        )
      
      uea.table(kk)
      
      if (!is.null(uea.table())) {
        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        f.data = uea.table()@result
        f.data %>% dplyr::select('Description', 'Count', 'itemRatio', 'BgRatio', 'pvalue', 'p.adjust', everything()) %>%
          formattable::formattable(
            x = .,
            list(
              Count = color_bar('#14FFB1FF', fun = unit.scale),
              pvalue = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', ifelse(x < 0.01, '#EB4D4B', '#FF0000')))),
              p.adjust = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', ifelse(x < 0.01, '#EB4D4B', '#FF0000')))),
              qvalue = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', ifelse(x < 0.01, '#EB4D4B', '#FF0000'))))
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
              list(extend = 'copy', filename =  'GO_enrichment', title = 'GO_enrichment', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'GO_enrichment', title = 'GO_enrichment', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'collection',
                   buttons = list(
                     list(
                       extend = 'csv', 
                       filename = 'GO_enrichment',
                       title = NULL, exportOptions = list(
                         columns = ':visible',
                         modifier = list(page = 'current'))),
                     list(
                       extend = 'excel', 
                       filename = 'GO_enrichment', 
                       title = NULL, exportOptions = list(
                         columns = ':visible', 
                         modifier = list(page = 'current')))
                   ),
                   text = 'Download data')),
            lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
      }
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_analysis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_ans_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    output[[paste0('dot_plot', enrichment_data_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'dot_plot_draw', height = input[['dot_plot_height']], width = input[['dot_plot_width']])))
    })
    
    output[['dot_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({ 
        DF <- uea.table()@result %>%
          filter(pvalue < isolate(input[['pvalueCutoff']]), qvalue < isolate(input[['qvalueCutoff']])) %>%
          top_n(input[['dot_plot_top']]) %>% 
          arrange(desc(Count)) %>% 
          mutate(Pathway = factor(Description, levels = Description)) %>% 
          mutate(Pathway = str_wrap(Pathway, width = 60))
        
        DF$BgRatio <- sapply(DF$BgRatio, function(x) {
          nums <- unlist(strsplit(x, "/"))
          as.numeric(nums[1]) / as.numeric(nums[2])
        })
        
        ggplot(DF, aes(BgRatio, Pathway)) +
          geom_point(aes(size = Count, color = p.adjust), shape = 16) +
          scale_colour_gradient(low = "#3B4992FF", high = "#EE0000FF") +
          theme_bw() +
          theme(
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16),
            plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
          )
        
      }, trace_back = FALSE)
    })
    
    observeEvent(input$table_ans_loaded, {
      shinyjs::show('sessionInfo')
      shinyjs::show('tab-ana-all')
    })
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
      xfun::session_info(c('dplyr', 'stringr', 'ggplot2', 'clusterProfiler'))
    })
    
    shinyjs::show('show_codes')
  })
  
  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {
    '# Load your own data. Here use "openxlsx::read.xlsx" and "genes.xlsx" and "pathways.xlsx" for example.'
    # i.data <- openxlsx::read.xlsx('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/plugins/UEA/www/data/genes.xlsx')
    # p.data <- openxlsx::read.xlsx('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/plugins/UEA/www/data/pathways.xlsx')
    i.data <- openxlsx::read.xlsx('path/to/genes.xlsx')
    p.data <- openxlsx::read.xlsx('path/to/pathways.xlsx')
    '# Arrange data'
    i.data.genes <- i.data %>% 
      dplyr::select(..(if (demo.status()) { input[['dragvars_env_gene']]$target$item } else { input[['dragvars_file_gene']]$target$item })) %>% 
      data.frame()
    i.data.pathways <- p.data %>% 
      dplyr::select(..(if (demo.status()) { c(input[['dragvars_env_pathway']]$target$pathway, input[['dragvars_env_pathway']]$target$item) } else { c(input[['dragvars_file_pathway']]$target$pathway, input[['dragvars_file_pathway']]$target$item) })) %>% 
      data.frame()
    '# Enrichment analysis'
    kk <- clusterProfiler::enricher(
      gene = i.data.genes[, ..(if (demo.status()) { input[['dragvars_env_gene']]$target$item } else { input[['dragvars_file_gene']]$target$item })],
      pvalueCutoff = ..(input[['pvalueCutoff']]),
      pAdjustMethod = ..(input[['pAdjustMethod']]),
      universe = NULL,
      minGSSize = ..(input[['minGSSize']]),
      maxGSSize = ..(input[['maxGSSize']]),
      qvalueCutoff = ..(input[['qvalueCutoff']]),
      TERM2GENE = i.data.pathways
    )
    '# rename'
    kk@result %>%
      dplyr::rename(
        itemRatio = GeneRatio,
        itemID = geneID
      )
    '# Prepare Plotting Data'
    DF <- kk@result %>%
      filter(pvalue < ..(input[['pvalueCutoff']]), qvalue < ..(input[['qvalueCutoff']])) %>%
      top_n(..(input[['dot_plot_top']])) %>% 
      arrange(desc(Count)) %>% 
      mutate(Pathway = factor(Description, levels = Description)) %>% 
      mutate(Pathway = str_wrap(Pathway, width = 60))
    
    DF$BgRatio <- sapply(DF$BgRatio, function(x) {
      nums <- unlist(strsplit(x, "/"))
      as.numeric(nums[1]) / as.numeric(nums[2])
    })
    '# Draw Plot'
    ggplot(DF, aes(BgRatio, Pathway)) +
      geom_point(aes(size = Count, color = p.adjust), shape = 16) +
      scale_colour_gradient(low = "#3B4992FF", high = "#EE0000FF") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  })
  
  
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(stringr)
        library(ggplot2)
        library(clusterProfiler)
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
  
  observeEvent({
    input[['dragvars_env']]
    input[['dragvars_file']]
    input[['uploaded_data_env-confirm']]
    input[['uploaded_data_file-confirm']]
  }, {
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['pvalueCutoff']]
    input[['pAdjustMethod']]
    input[['qvalueCutoff']]
    input[['minGSSize']]
    input[['maxGSSize']]
  }, {
    shinyjs::show('confirm_analysis')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste('UEA.zip') },
    content <- function(file){
      file.copy("demo/demo.zip", file)
    }, contentType = NULL)
}

# Use for standalone testing, 
# comment out the following line when used as a component.
# shinyApp(ui = ui.alb, server = server.ert, options = list(host = '10.0.12.2', port = 3390))
shinyApp(ui = ui.alb, server = server.ert, options = list(port = 80))