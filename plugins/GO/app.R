#' @title
#'
#' @description
#' 
# Load Global

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
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(clusterProfiler)))
suppressMessages(suppressWarnings(library(org.Hs.eg.db)))

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
  
  gene_data <<- MRanalysisBase::Genes
  
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
        bs4SidebarMenuItem(HTML('<p class="nav-text">Load Gene Data</p>'), tabName = 'tab-upl-exp', icon = icon('upload', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">GO enrichment Analysis</p>'), tabName = 'tab-ana', icon = icon('calculator',lib = 'font-awesome', class = 'nav-icon')),
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
        tags$title('GO')
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
            html. <- system.file('extdata', 'HTML/intro.plugins.GO.html', package = 'MRanalysisBase')
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
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 1. Choose Gene data.</strong>"),
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
                column(
                  width = 4, 
                  shinyWidgets::virtualSelectInput(
                    inputId = 'input_type_env',
                    label = h5("Confirm input type"),
                    choices = c('Please select one' = 'ENTREZID', 'SYMBOL'),
                    search = TRUE,
                    multiple = FALSE,
                    selected = 'ALBERT'
                  )
                )
              ),
              tags$div(
                id = 'uploaded_data_file_div',
                datamods::import_file_ui(
                  id = 'uploaded_data_file',
                  file_extensions = c('.csv', '.tsv', '.txt', '.xls', '.xlsx'),
                  title = NULL
                ),
                uiOutput(outputId = 'ui_aesthetics_file'),
                column(
                  width = 4, 
                  shinyWidgets::virtualSelectInput(
                    inputId = 'input_type_file',
                    label = h5("Confirm input type"),
                    choices = c('ENTREZID', 'SYMBOL'),
                    search = TRUE,
                    multiple = FALSE,
                    selected = 'ALBERT'
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
              tags$hr(),
              HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Gene</span>. Select the column containing genes, which can be either gene symbol or Entrez ID. Additionally, you will need to manually confirm the input type below.</span><br>")
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'next_upload_data', label = 'Next', class = 'btn-danger btn-float')
            ),
            tags$div(
              tags$br(), tags$br(), tags$br(), tags$br(),
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
                       title = 'One of "BP", "MF", and "CC" subontologies, or "ALL" for all three.',
                       shinyWidgets::virtualSelectInput(inputId = 'ont', label = h5("Ontology"), choices = c('ALL', 'BP', 'MF', 'CC'), search = TRUE, multiple = FALSE, selected = 'ALL')
                     )),
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
    title = 'GO'
  )
}


server.ert <- function(input, output, session) {
  
  shinyjs::hide('tab-ana')
  shinyjs::hide('tab-ana-all')
  shinyjs::hide('tab-r-session-all')
  shinyjs::hide('uploaded_data_env_div')
  shinyjs::hide('sessionInfo')
  shinyjs::hide('show_codes')
  
  demo.status <- reactiveVal(FALSE)
  go.table <- reactiveVal(NULL)
  enrichment_data_random <- reactiveVal(as.numeric(Sys.time()))
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
    aesthetics <- c('Gene')
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
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
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
        replace = TRUE
      )
    }
  })
  
  output[['ui_aesthetics_file']] <- renderUI({
    aesthetics <- c('Gene')
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
        badge = TRUE,
        width = '100%',
        height = '80px',
        copySource = FALSE,
        replace = TRUE
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
  
  d.env.status <- reactive({ 
    !is.null(input[['dragvars_env']]$target$Gene)
  })
  
  d.file.status <- reactive({ 
    !is.null(input[['dragvars_file']]$target$Gene)
  })
  
  d.env.type.status <- reactive({ 
    input[['input_type_env']] != ''
  })
  
  d.file.type.status <- reactive({ 
    input[['input_type_file']] != ''
  })
  
  observeEvent(input[['next_upload_data']], {
    if (demo.status()) {
      if (!d.env.status()) {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
      if (!d.env.type.status()) {
        spsComps::shinyCatch({ base::stop('Input type must be confirmed!') }, trace_back = FALSE)
      }
      if (d.env.status() && d.env.type.status()) {
        updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-ana'
        )
        shinyjs::show('tab-ana-all')
      }
    } else {
      if (!d.file.status()) {
        spsComps::shinyCatch({ base::stop('Missing necessary variables!') }, trace_back = FALSE)
      }
      if (!d.file.type.status()) {
        spsComps::shinyCatch({ base::stop('Input type must be confirmed!') }, trace_back = FALSE)
      }
      if (d.file.status() && d.file.type.status()) {
        updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-ana'
        )
        shinyjs::show('tab-ana-all')
      }
    }
  })
  
  
  # 获取各个值
  i.data <- reactive({
    if (demo.status()) {
      try.c <- try({
        tmp <- imported_env$data()
        tmp <- tmp %>% dplyr::select(
          input[['dragvars_env']]$target$Gene) %>% data.frame()
        tmp[, 1]
      }, silent = QUITE)
    } else {
      try.c <- try({
        tmp <- imported_file$data()
        tmp <- tmp %>% dplyr::select(
          input[['dragvars_file']]$target$Gene) %>% data.frame()
        tmp[, 1]
      }, silent = QUITE)
    }
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  observeEvent(input[['confirm_analysis']], {
    
    shinyjs::hide('confirm_analysis')
    shinyjs::hide('tab-ana-all')
    
    shiny::removeUI(selector = '#step_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_analysis_plot_box', immediate = TRUE)
    shiny::removeUI(selector = '#table_ans_loaded', immediate = TRUE)
    
    enrichment_data_random(as.numeric(Sys.time()))
    
    shiny::insertUI(
      selector = '#tab-ana-all',
      where = 'afterEnd',
      immediate = TRUE,
      ui =  bs4Dash::tabBox(
        id = 'step_analysis_plot',
        title = NULL,
        width = 12,
        selected = 'Bar Plot',
        status = 'success',
        solidHeader = FALSE,
        type = 'tabs',
        tabPanel(
          title = 'Bar Plot',
          fluidRow(
            column(width = 3, shiny::sliderInput(inputId = 'bar_plot_top', label = h6("Show Top Items"), min = 1, max = 10, value = 5, step = 1)),
            column(width = 3, shiny::sliderInput(inputId = 'bar_plot_width', label = h6("WIDTH"), min = 100, max = 1280, value = 1080, step = 20)),
            column(width = 3, shiny::sliderInput(inputId = 'bar_plot_height', label = h6("HEIGHT"), min = 100, max = 1280, value = 600, step = 20))
          ),
          withSpinner({ uiOutput(outputId = paste0('bar_plot', enrichment_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Draw bar plot, please wait...')
        ),
        tabPanel(
          title = 'Dot Plot',
          fluidRow(
            column(width = 3, shiny::sliderInput(inputId = 'dot_plot_top', label = h6("Show Top Items"), min = 1, max = 10, value = 5, step = 1)),
            column(width = 3, shiny::sliderInput(inputId = 'dot_plot_width', label = h6("WIDTH"), min = 100, max = 1280, value = 1080, step = 20)),
            column(width = 3, shiny::sliderInput(inputId = 'dot_plot_height', label = h6("HEIGHT"), min = 100, max = 1280, value = 600, step = 20))
          ),
          withSpinner({ uiOutput(outputId = paste0('dot_plot', enrichment_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Draw dot plot, please wait...')
        )
      )
    )
    
    shiny::insertUI(
      selector = '#tab-ana-all',
      where = 'afterEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_analysis_tbl',
        title = HTML('GO Enrichment Result'),
        status = 'success',
        width = 12,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('enrichment_data', enrichment_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Performing GO Enrichment Analysis, please be patient...'))
    )
    
    output[[paste0('enrichment_data', enrichment_data_random())]]  <- DT::renderDT({
      spsComps::shinyCatch({
        if (isolate(input[['input_type_env']]) == 'SYMBOL' || isolate(input[['input_type_file']]) == 'SYMBOL') {
          hg <- bitr(i.data(), fromType = "SYMBOL", toType = c("ENTREZID", "SYMBOL"), OrgDb = "org.Hs.eg.db")
        } else {
          hg <- bitr(i.data(), fromType = "ENTREZID", toType = c("ENTREZID", "SYMBOL"), OrgDb = "org.Hs.eg.db")
        }
        go <- enrichGO(
          hg$ENTREZID, OrgDb = org.Hs.eg.db, 
          ont = isolate(input[['ont']]),
          pAdjustMethod = isolate(input[['pAdjustMethod']]),
          pvalueCutoff = isolate(input[['pvalueCutoff']]),
          qvalueCutoff = isolate(input[['qvalueCutoff']]),
          minGSSize = isolate(input[['minGSSize']]),
          maxGSSize  = isolate(input[['maxGSSize']]),
          keyType = 'ENTREZID'
        )
        for (index in 1:nrow(go@result)) {
          SS = c()
          gid = go@result[index, 'geneID']
          gids = str_split(gid, '/')[[1]]
          for (g in gids) {
            S = hg[hg$ENTREZID == g, ]$SYMBOL
            SS = c(SS, S)
          }
          go@result[index, 'geneSymbol'] = paste0(SS, collapse = '/')
        }
        if (isolate(input[['ont']]) != 'ALL') {
          go@result['ONTOLOGY'] = isolate(input[['ont']])
        }
        go.table(go)
      })
      
      if (!is.null(go.table())) {
        
        shinyjs::show('sessionInfo')
        shinyjs::show('tab-ana-all')
        
        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        f.data = go.table()
        f.data@result %>% dplyr::select(everything(), 'Count', 'ID', 'Description', 'GeneRatio', 'BgRatio', 'pvalue', 'p.adjust', 'qvalue', 'geneID', 'geneSymbol') %>%
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
    
    output[[paste0('bar_plot', enrichment_data_random())]] <- renderUI({
      req(go.table(), cancelOutput = TRUE)
      fluidRow(
        shinyjqui::jqui_resizable(plotOutput(outputId = 'bar_plot_draw', height = input[['bar_plot_height']], width = input[['bar_plot_width']]))
      )
    })
    
    output[[paste0('dot_plot', enrichment_data_random())]] <- renderUI({
      req(go.table(), cancelOutput = TRUE)
      fluidRow(
        shinyjqui::jqui_resizable(plotOutput(outputId = 'dot_plot_draw', height = input[['dot_plot_height']], width = input[['dot_plot_width']]))
      )
    })
  })
  
  observe({
    jsCode <- "setInterval(function() { if ($('#step_analysis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_ans_loaded', true); clearInterval(this); }}, 100);"
    runjs(jsCode)
  })
  
  observeEvent(input$table_ans_loaded, {
    
    output[['bar_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({
        graphics::barplot(
          go.table(),
          x = "GeneRatio",
          color = "p.adjust",
          showCategory = input[['bar_plot_top']],
          split="ONTOLOGY") +
          facet_grid(ONTOLOGY ~ ., scale = 'free') +
          theme(
            text = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            strip.text = element_text(size = 16),
            legend.text = element_text(size = 16),
            axis.title = element_text(size = 16)
          )
      }, trace_back = FALSE)
    })
    
    output[['dot_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({ 
        enrichplot::dotplot(
          go.table(), 
          x = "GeneRatio", 
          color = "p.adjust", 
          size = "Count",
          showCategory = input[['dot_plot_top']],
          split = "ONTOLOGY") +
          facet_grid(ONTOLOGY ~ ., scale = 'free') +
          theme(
            text = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            strip.text = element_text(size = 16),
            legend.text = element_text(size = 16),
            axis.title = element_text(size = 16)
          )
      }, trace_back = FALSE)
    })
    
    shinyjs::show('sessionInfo')
    shinyjs::show('tab-ana-all')
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
      xfun::session_info(c('dplyr', 'stringr', 'DT', 'clusterProfiler', 'org.Hs.eg.db'))
    })
    
    shinyjs::show('show_codes')
  })
  
  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {
    
    '# input genes'
    i.data <- ..(i.data())
    '# Biological Id TRanslator'
    if (..(isolate(input[['input_type_env']])) == 'SYMBOL') {
      hg <- clusterProfiler::bitr(i.data, fromType = "SYMBOL", toType = c("ENTREZID", "SYMBOL"), OrgDb = "org.Hs.eg.db")
    } else {
      hg <- clusterProfiler::bitr(i.data, fromType = "ENTREZID", toType = c("ENTREZID", "SYMBOL"), OrgDb = "org.Hs.eg.db")
    }
    '# GO Enrichment Analysis of a gene set'
    go <- clusterProfiler::enrichGO(
      hg$ENTREZID, OrgDb = org.Hs.eg.db, 
      ont = ..(isolate(input[['ont']])),
      pAdjustMethod = ..(isolate(input[['pAdjustMethod']])),
      pvalueCutoff = ..(isolate(input[['pvalueCutoff']])),
      qvalueCutoff = ..(isolate(input[['qvalueCutoff']])),
      minGSSize = ..(isolate(input[['minGSSize']])),
      maxGSSize  = ..(isolate(input[['maxGSSize']])),
      keyType = 'ENTREZID'
    )
    '# Entrez IDs mapping Gene Symbols'
    for (index in 1:nrow(go@result)) {
      SS = c()
      gid = go@result[index, 'geneID']
      gids = str_split(gid, '/')[[1]]
      for (g in gids) {
        S = hg[hg$ENTREZID == g, ]$SYMBOL
        SS = c(SS, S)
      }
      go@result[index, 'geneSymbol'] = paste0(SS, collapse = '/')
    }
    '# Add Ontology'
    if (..(isolate(input[['ont']])) != 'ALL') {
      go@result['ONTOLOGY'] = ..(isolate(input[['ont']]))
    }
    '# Draw bar plot'
    graphics::barplot(
      go, 
      x = "GeneRatio", 
      color = "p.adjust",
      showCategory = ..(input[['bar_plot_top']]),
      split="ONTOLOGY") +
      facet_grid(ONTOLOGY ~ ., scale = 'free') +
      theme(
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16)
      )
    '# Draw dot plot'
    enrichplot::dotplot(
      go, 
      x = "GeneRatio", 
      color = "p.adjust", 
      size = "Count",
      showCategory = ..(input[['dot_plot_top']]),
      split = "ONTOLOGY") +
      facet_grid(ONTOLOGY ~ ., scale = 'free') +
      theme(
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16)
      )
  })
  
  
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(clusterProfiler)
        library(org.Hs.eg.db)
        library(stringr)
        library(ggplot2)
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
    input[['uploaded_data_file_div']]
    input[['uploaded_data_file']]
    input[['uploaded_data_env-confirm']]
    input[['uploaded_data_file-confirm']]
  }, {
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('tab-ana-all')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
    shinyjs::show('confirm_analysis')
    shiny::removeUI(selector = '#step_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_analysis_plot_box', immediate = TRUE)
  })
  
  observeEvent({
    input[['ont']]
    input[['pvalueCutoff']]
    input[['pAdjustMethod']]
    input[['qvalueCutoff']]
    input[['minGSSize']]
    input[['maxGSSize']]
  }, {
    shinyjs::show('confirm_analysis')
    shinyjs::hide('sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_analysis_plot_box', immediate = TRUE)
  })
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste('Genes.xlsx') },
    content <- function(file){
      file.copy(glue("demo/genes.xlsx"), file)
    }, contentType = NULL)
}
# 
# Use for standalone testing, 
# comment out the following line when used as a component.
# shinyApp(ui = ui.alb, server = server.ert, options = list(host = '10.0.12.2', port = 3390))
shinyApp(ui = ui.alb, server = server.ert, options = list(port = 80))