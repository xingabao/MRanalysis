#' @title
#'
#' @description
#' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8159796/

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
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ggdag)))

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

options(shiny.maxRequestSize = tryCatch(if (Sys.getenv("MRANALYSIS_MAX_SIZE") == '') 100000 * 1024^2 else eval(parse(text = Sys.getenv("MRANALYSIS_MAX_SIZE"))), error = function(e) 100000 * 1024^2))

# ui
ui.analysis.mr_two_step_mr_local <- function() {
  
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
        bs4SidebarMenuItem(HTML('<p class="nav-text">Load GWAS Summary Data</p>'), tabName = 'tab-upl-exp', icon = icon('upload', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Select Instrumental Variables</p>'), tabName = 'tab-siv', icon = icon('gears',lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Remove confounding factors </p>'), tabName = 'tab-con', icon = icon('paypal', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Mendelian Randomization</p>'), tabName = 'tab-2mr', icon = icon('calculator', lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Current R Session</p>'), tabName = 'tab-r-session', icon = icon('info',lib = 'font-awesome', class = 'nav-icon')),
        bs4SidebarMenuItem(HTML('<p class="nav-text">Contact us</p>'), tabName = 'tab-contact-us', icon = icon('compass', class = 'nav-icon'))
      )
    ),
    body = bs4Dash::dashboardBody(
      tags$head(
        useShinyjs(),
        includeCSS(system.file('extdata', 'CSS/shiny-style.css', package = 'MRanalysisBase')),
        tags$link(rel = 'shortcut icon', href = '/XINGABAO/img/favicon.ico'),
        tags$title('MR analysis'),
        MRanalysisBase::web.statistic.baidu
      ),
      bs4TabItems(

        # ++++++++++++++++++++++++ tabName = 'tab-r-session' ++++++++++++++++++++++++
        bs4TabItem(
          tabName = 'tab-r-session',
          fluidRow(
            id = 'tab-r-session-all',
            column(12, h5("Step 7. Collect Information About the Current R Session."))
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
            html. <- system.file('extdata', 'HTML/intro.analysis.MMR-local.html', package = 'MRanalysisBase')
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
          bs4Dash::tabBox(
            id = "step_upload_gwas_data",
            title = NULL,
            width = 12,
            selected = 'Step 1. Choose exposure data.',
            status = 'success',
            solidHeader = FALSE,
            type = 'tabs',
            tabPanel(
              title = 'Step 1. Choose exposure data.',
              fluidRow(
                bs4Dash::box(
                  id = 'step_upload_exposure_data',
                  title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose exposure data.</strong>"),
                  status = 'warning',
                  width = 6,
                  shiny::fileInput(
                    inputId = 'exposure',
                    accept = c('.vcf.gz'),
                    label = 'Upload a file for exposure.',
                    multiple = FALSE
                  ),
                  verbatimTextOutput('exposure_file'),
                  column(12,
                         div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                             shiny::actionButton(inputId = 'use_demo_data', label = 'Use demo data', class = 'btn-warning'),
                             shiny::actionButton(inputId = 'reset', label = 'Reset', class = 'btn-info btn-stepone')
                         )
                  ),
                  column(12,
                         div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                             shiny::downloadButton(outputId = 'download_demo_data', label = 'Download demo data', class = 'btn-success')
                         )
                  )
                ),
                bs4Dash::box(
                  id = 'step_upload_exposure_data_information',
                  title = HTML("<strong>&nbsp;Information for exposure</strong>"),
                  status = 'warning',
                  width = 6,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  fluidRow(
                    column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = 'sample_size_exposure', label = 'Sample size', value = NULL))),
                    column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = 'case_size_exposure', label = 'Case size', value = NULL))),
                    column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = 'trait_name_exposure', label = 'Trait Name', value = NULL))),
                    column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = 'trait_id_exposure', label = 'Trait ID', value = NULL)))
                  )
                )
              )
            ),
            tabPanel(
              title = 'Step 2. Choose mediation data.',
              fluidRow(
                bs4Dash::box(
                  id = 'step_upload_mediation_data',
                  title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose mediation data.</strong>"),
                  status = 'warning',
                  width = 6,
                  shiny::fileInput(
                    inputId = 'mediation',
                    accept = c('.vcf.gz'),
                    label = 'Upload a file for mediation.',
                    multiple = FALSE
                  ),
                  verbatimTextOutput('mediation_file')
                ),
                bs4Dash::box(
                  id = 'step_upload_mediation_data_information',
                  title = HTML("<strong>&nbsp;Information for mediation</strong>"),
                  status = 'warning',
                  width = 6,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  fluidRow(
                    column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = 'sample_size_mediation', label = 'Sample size', value = NULL))),
                    column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = 'case_size_mediation', label = 'Case size', value = NULL))),
                    column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = 'trait_name_mediation', label = 'Trait Name', value = NULL))),
                    column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = 'trait_id_mediation', label = 'Trait ID', value = NULL)))
                  )
                )
              )
            ),
            tabPanel(
              title = 'Step 3. Choose outcome data.',
              fluidRow(
                bs4Dash::box(
                  id = 'step_upload_outcome_data',
                  title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose outcome data.</strong>"),
                  status = 'warning',
                  width = 6,
                  shiny::fileInput(
                    inputId = 'outcome',
                    accept = c('.vcf.gz'),
                    label = 'Upload a file for outcome.',
                    multiple = FALSE
                  ),
                  verbatimTextOutput('outcome_file')
                ),
                bs4Dash::box(
                  id = 'step_upload_outcome_data_information',
                  title = HTML("<strong>&nbsp;Information for outcome</strong>"),
                  status = 'warning',
                  width = 6,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  fluidRow(
                    column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = 'sample_size_outcome', label = 'Sample size', value = NULL))),
                    column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = 'case_size_outcome', label = 'Case size', value = NULL))),
                    column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = 'trait_name_outcome', label = 'Trait Name', value = NULL))),
                    column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = 'trait_id_outcome', label = 'Trait ID', value = NULL)))
                  )
                )
              )
            )
          ),
          tags$div(
            class = 'btn-albert',
            shiny::actionButton(inputId = 'next_upload_data', label = 'Next', class = 'btn-danger btn-float')
          )
        ),  # end bs4TabItem

        # ++++++++++++++++++++++++ tabName = 'tab-siv' ++++++++++++++++++++++++
        bs4TabItem(
          tabName = 'tab-siv',
          fluidRow(
            id = 'tab-siv-all',
            column(12, h5("Step 4. Filter instruments.")),
            bs4Dash::box(
              id = 'step_select_ivs',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 4.1. Genetic variants significantly linked to the exposure/mediation/outcome factor.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              tags$div(
                title = 'Significance threshold. The default is 5e-6.',
                shiny::numericInput(inputId = 'p1', label = "The significance threshold for selecting instrumental variables.", value = 5E-6, step = 1E-8)
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_select_ivs', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_select_ivs', label = 'Next', class = 'btn-danger btn-float')
            ),
            bs4Dash::box(
              id = 'step_ld_clump',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 4.2. Perform LD clumping.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = TRUE,
              collapsible = TRUE,
              tags$div(
                title = 'Clumping r2 cut off. The default is 0.001.',
                shiny::numericInput(inputId = 'clump_r2', label = 'Clumping r2 cut off.', value = 0.001, min = 0, step = 0.001)),
              tags$div(
                title = 'Clumping distance cutoff. The default is 10000.',
                shiny::numericInput(inputId = 'clump_kb', label = 'Clumping distance cutoff.', value = 10000, min = 0, step = 10000)),
              tags$div(
                title = 'Clumping sig level for index variants. Default = 1 (i.e. no threshold).',
                shiny::numericInput(inputId = 'clump_p', label = 'Clumping sig level for index variants.', value = 0.99, min = 0, max = 1, step = 0.01)),
              tags$div(
                title = 'Super-population to use as reference panel. Default = "EUR". Options are "EUR", "SAS", "EAS", "AFR", "AMR".',
                shinyWidgets::virtualSelectInput(
                  inputId = 'pop',
                  label = h6('Super-population to use as reference panel.'),
                  choices = c('African (AFR)' = 'AFR', 'Admixed American (AMR)' = 'AMR', 'East Asian (EAS)' = 'EAS', 'European (EUR)' = 'EUR', 'South Asian (SAS)' = 'SAS'),
                  search = TRUE,
                  multiple = FALSE,
                  showValueAsTags = TRUE,
                  selected = 'EUR'
                )
              )
            ),
            bs4Dash::box(
              id = 'step_r2_f',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 4.3. Calculate R<sup>2</sup> and F value.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              prettyToggle(
                value = TRUE,
                inputId = "caiculate_f_statitics_mode",
                label_on = HTML("Calculate the R<sup>2</sup> and F value of Instruments."),
                icon_on = icon("check"),
                status_on = "info",
                status_off = "warning",
                label_off = HTML("Don't calculate the R<sup>2</sup> and F value of Instruments."),
                icon_off = icon("check")
              ),
              conditionalPanel(
                condition = paste0('input.caiculate_f_statitics_mode === true'),
                tags$div(
                  title = 'Threhold of F value. The default is 10.',
                  shiny::numericInput(inputId = 'f', label = 'Threhold of F value', value = 10, min = 0))
              )
            )
          )
        ),  # end bs4TabItem

        # ++++++++++++++++++++++++ tabName = 'tab-con' ++++++++++++++++++++++++
        bs4TabItem(
          tabName = 'tab-con',
          fluidRow(
            id = 'tab-con-all',
            bs4Dash::tabBox(
              id = 'step_select_ivs',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 5. Remove confounding factors.</strong>"),
              width = 12,
              selected = 'Exposure',
              status = 'success',
              solidHeader = FALSE,
              type = 'tabs',
              tabPanel(
                title = 'Exposure',
                bs4Dash::box(
                  id = 'step_select_ivs_exposure',
                  title = 'Exposure',
                  status = 'warning',
                  width = 12,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  uiOutput(outputId = 'confounding-ui-exposure')
                  # HTML('<h5>To further refine the selection of IVs, you can consider identifying confounding factors through the use of <a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank">PhenoScanner</a>.</h5>'),
                  # HTML('<a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank"><img src="img/phenoscanner.png"  title = "PhenoScanner"></a>')
                )
              ),
              tabPanel(
                title = 'Mediation',
                bs4Dash::box(
                  id = 'step_select_ivs_mediation',
                  title = 'Mediation',
                  status = 'warning',
                  width = 12,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  uiOutput(outputId = 'confounding-ui-mediation')
                  # HTML('<h5>To further refine the selection of IVs, you can consider identifying confounding factors through the use of <a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank">PhenoScanner</a>.</h5>'),
                  # HTML('<a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank"><img src="img/phenoscanner.png"  title = "PhenoScanner"></a>')
                )
              ),
              tabPanel(
                title = 'Outcome',
                bs4Dash::box(
                  id = 'step_select_ivs_outcome',
                  title = 'Outcome',
                  status = 'warning',
                  width = 12,
                  collapsed = FALSE,
                  collapsible = TRUE,
                  uiOutput(outputId = 'confounding-ui-outcome')
                  # HTML('<h5>To further refine the selection of IVs, you can consider identifying confounding factors through the use of <a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank">PhenoScanner</a>.</h5>'),
                  # HTML('<a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank"><img src="img/phenoscanner.png"  title = "PhenoScanner"></a>')
                )
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_select_ivs_con', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_select_ivs_con', label = 'Next', class = 'btn-danger btn-float'),
              tags$div(id = 'download_select_ivs_con', shiny::downloadButton(outputId = 'download_select_ivs_con', label = 'Download', class = 'btn-danger btn-floatdl'))
            )
          )
        ), # end bs4TabItem

        # ++++++++++++++++++++++++ tabName = 'tab-2mr' ++++++++++++++++++++++++
        bs4TabItem(
          tabName = 'tab-2mr',
          fluidRow(
            id = 'tab-2mr-all',
            column(12, h5("Step 6. Perform two-sample Mendelian randomization.")),
            bs4Dash::box(
              id = 'step_two_step_mr_analysis',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Options for MR analysis.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              uiOutput(outputId = '2mr-ui')
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_2mr_analysis', label = 'Run', class = 'btn-danger btn-floatrun'),
              tags$div(id = 'download_2mr_analysis', shiny::downloadButton(outputId = 'download_2mr_analysis', label = 'Download', class = 'btn-danger btn-floatdl')),
              shiny::actionButton(inputId = 'mmr_sessionInfo', label = 'Session Info', class = 'btn-danger btn-floatsession')
            )
          )
        ) # end bs4TabItem
      )
    ),
    controlbar = NULL,
    scrollToTop = TRUE,
    title = 'MMR'
  )
}


server.analysis.mr_two_step_mr_local <- function(input, output, session) {
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste0('demo-', as.numeric(Sys.time()), '.zip') },
    content <- function(file){
      file.copy(glue("demo/demo.zip"), file)
    }, contentType = NULL)
  
  shinyjs::hide('tab-siv-all')
  shinyjs::hide('tab-con-all')
  shinyjs::hide('tab-2mr-all')
  shinyjs::hide('tab-r-session-all')
  shinyjs::hide('next_upload_data')
  shinyjs::hide('next_select_ivs')
  shinyjs::hide('next_select_ivs_con')
  shinyjs::hide('download_select_ivs_con')
  shinyjs::hide('download_2mr_analysis')
  shinyjs::hide('show_codes')
  
  exposure.file <- reactiveVal(NULL)
  exposure.filepath <- reactiveVal(NULL)
  exposure.vi.filepath <- reactiveVal(NULL)
  mediation.file <- reactiveVal(NULL)
  mediation.filepath <- reactiveVal(NULL)
  mediation.vi.filepath <- reactiveVal(NULL)
  outcome.file <- reactiveVal(NULL)
  outcome.filepath <- reactiveVal(NULL)
  outcome.vi.filepath <- reactiveVal(NULL)
  exposure_data <- reactiveVal(TRUE)
  exposure_data_clump <- reactiveVal(TRUE)
  exposure_data_r2f <- reactiveVal(TRUE)
  mediation_data <- reactiveVal(TRUE)
  mediation_data_clump <- reactiveVal(TRUE)
  mediation_data_r2f <- reactiveVal(TRUE)
  outcome_data <- reactiveVal(TRUE)
  outcome_data_clump <- reactiveVal(TRUE)
  outcome_data_r2f <- reactiveVal(TRUE)
  exposure_all_ivs <- reactiveVal(TRUE)
  mediation_all_ivs <- reactiveVal(TRUE)
  outcome_all_ivs <- reactiveVal(TRUE)
  hat_data.e.o <- reactiveVal(TRUE)
  mr_odds_res.e.o <- reactiveVal(TRUE)
  mr_odds_res_het.e.o <- reactiveVal(TRUE)
  mr_odds_res_pleio.e.o <- reactiveVal(TRUE)
  hat_data.e.m <- reactiveVal(TRUE)
  mr_odds_res.e.m <- reactiveVal(TRUE)
  mr_odds_res_het.e.m <- reactiveVal(TRUE)
  mr_odds_res_pleio.e.m <- reactiveVal(TRUE)
  hat_data.m.o <- reactiveVal(TRUE)
  mr_odds_res.m.o <- reactiveVal(TRUE)
  mr_odds_res_het.m.o <- reactiveVal(TRUE)
  mr_odds_res_pleio.m.o <- reactiveVal(TRUE)
  hat_data.o.e <- reactiveVal(TRUE)
  mr_odds_res.o.e <- reactiveVal(TRUE)
  mr_odds_res_het.o.e <- reactiveVal(TRUE)
  mr_odds_res_pleio.o.e <- reactiveVal(TRUE)
  mr_mmr_result <- reactiveVal(TRUE)
  mr.result <- reactiveVal(NULL)
  exposure_data_random <- reactiveVal(as.numeric(Sys.time()))
  mediation_data_random <- reactiveVal(as.numeric(Sys.time()))
  outcome_data_random <- reactiveVal(as.numeric(Sys.time()))
  mr_analysis_random <- reactiveVal(as.numeric(Sys.time()))
  
  db.sample.size.e <- debounce(reactive({ input[['sample_size_exposure']] }), DEBOUNCE.A)
  db.case.size.e <- debounce(reactive({ input[['case_size_exposure']] }), DEBOUNCE.A)
  db.sample.size.m <- debounce(reactive({ input[['sample_size_mediation']] }), DEBOUNCE.A)
  db.case.size.m <- debounce(reactive({ input[['case_size_mediation']] }), DEBOUNCE.A)
  db.sample.size.o <- debounce(reactive({ input[['sample_size_outcome']] }), DEBOUNCE.A)
  db.case.size.o <- debounce(reactive({ input[['case_size_outcome']] }), DEBOUNCE.A)
  
  observeEvent(input[['reset']], {
    shinyjs::refresh()
  })
  
  iv <- InputValidator$new()
  iv$add_rule('clump_r2', sv_gte(0))
  iv$add_rule('clump_kb', sv_gte(0))
  iv$add_rule('clump_p', sv_between(0, 1))
  iv$add_rule('sample_size_exposure', sv_gt(0))
  iv$add_rule('sample_size_outcome', sv_gt(0))
  iv$add_rule('f', sv_gte(0))
  iv$enable()
  
  output[['exposure_file']] = renderPrint({ exposure.file() })
  output[['mediation_file']] = renderPrint({ mediation.file() })
  output[['outcome_file']] = renderPrint({ outcome.file() })
  
  observeEvent(input[['use_demo_data']], {
    exposure.file('ebi-a-GCST006942.vcf.gz')
    exposure.filepath(glue('demo/ebi-a-GCST006942.vcf.gz'))
    shiny::updateTextInput(session = session, inputId = 'sample_size_exposure', label = 'Sample size', value = 376352)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure', label = 'Trait name', value = 'Feeling lonely')
    shiny::updateTextInput(session = session, inputId = 'trait_id_exposure', label = 'Trait ID', value = 'ebi-a-GCST006942')
    shinyjs::hide('exposure')
    
    mediation.file('ieu-b-142.vcf.gz')
    mediation.filepath(glue('demo/ieu-b-142.vcf.gz'))
    shiny::updateTextInput(session = session, inputId = 'sample_size_mediation', label = 'Sample size', value = 249752)
    shiny::updateTextInput(session = session, inputId = 'trait_name_mediation', label = 'Trait name', value = 'Cigarettes smoked per day')
    shiny::updateTextInput(session = session, inputId = 'trait_id_mediation', label = 'Trait ID', value = 'ieu-b-142')
    shinyjs::hide('mediation')
    
    outcome.file('ebi-a-GCST009981.vcf.gz')
    outcome.filepath(glue('demo/ebi-a-GCST009981.vcf.gz'))
    shiny::updateTextInput(session = session, inputId = 'sample_size_outcome', label = 'Sample size', value = 49164)
    shiny::updateTextInput(session = session, inputId = 'case_size_outcome', label = 'Case size', value = 9487)
    shiny::updateTextInput(session = session, inputId = 'trait_name_outcome', label = 'Trait name', value = 'Major depressive disorder in trauma-unexposed individuals')
    shiny::updateTextInput(session = session, inputId = 'trait_id_outcome', label = 'Trait ID', value = 'ebi-a-GCST009981')
    shinyjs::hide('outcome')
    
    shinyjs::show('next_upload_data')
  })
  
  observeEvent(input[['exposure']], {
    shinyjs::hide('next_upload_data')
    exposure.file(input[['exposure']]$name)
    exposure.filepath(input[['exposure']]$datapath)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure', label = 'Trait name', value = 'exposure')
    shiny::updateTextInput(session = session, inputId = 'trait_id_exposure', label = 'Trait ID', value = strsplit(input[['exposure']]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    db.sample.size.e.c = tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    db.sample.size.m.c = tryCatch(as.numeric(db.sample.size.m()), error = function(e) NA)
    db.sample.size.o.c = tryCatch(as.numeric(db.sample.size.o()), error = function(e) NA)
    db.sample.size.c = !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0 && !is.na(db.sample.size.m.c) && db.sample.size.m.c > 0 && !is.na(db.sample.size.o.c) && db.sample.size.o.c > 0
    if ((!is.null(input[['mediation_file']]) || !is.null(mediation.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && db.sample.size.c) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['mediation']], {
    shinyjs::hide('next_upload_data')
    mediation.file(input[['mediation']]$name)
    mediation.filepath(input[['mediation']]$datapath)
    shiny::updateTextInput(session = session, inputId = 'trait_name_mediation', label = 'Trait name', value = 'mediation')
    shiny::updateTextInput(session = session, inputId = 'trait_id_mediation', label = 'Trait ID', value = strsplit(input[['mediation']]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    db.sample.size.e.c = tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    db.sample.size.m.c = tryCatch(as.numeric(db.sample.size.m()), error = function(e) NA)
    db.sample.size.o.c = tryCatch(as.numeric(db.sample.size.o()), error = function(e) NA)
    db.sample.size.c = !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0 && !is.na(db.sample.size.m.c) && db.sample.size.m.c > 0 && !is.na(db.sample.size.o.c) && db.sample.size.o.c > 0
    if ((!is.null(input[['exposure_file']]) || !is.null(exposure.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && db.sample.size.c) {
      shinyjs::show('next_upload_data')
    }
  })

  observeEvent(input[['outcome']], {
    shinyjs::hide('next_upload_data')
    outcome.file(input[['outcome']]$name)
    outcome.filepath(input[['outcome']]$datapath)
    shiny::updateTextInput(session = session, inputId = 'trait_name_outcome', label = 'Trait name', value = 'outcome')
    shiny::updateTextInput(session = session, inputId = 'trait_id_outcome', label = 'Trait ID', value = strsplit(input[['outcome']]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    db.sample.size.e.c = tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    db.sample.size.m.c = tryCatch(as.numeric(db.sample.size.m()), error = function(e) NA)
    db.sample.size.o.c = tryCatch(as.numeric(db.sample.size.o()), error = function(e) NA)
    db.sample.size.c = !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0 && !is.na(db.sample.size.m.c) && db.sample.size.m.c > 0 && !is.na(db.sample.size.o.c) && db.sample.size.o.c > 0
    if ((!is.null(input[['exposure_file']]) || !is.null(exposure.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && db.sample.size.c) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent({
    input[['sample_size_exposure']]
    input[['sample_size_mediation']]
    input[['sample_size_outcome']]
  }, {
    shinyjs::hide('next_upload_data')
    db.sample.size.e.c = tryCatch(as.numeric(input[['sample_size_exposure']]), error = function(e) NA)
    db.sample.size.m.c = tryCatch(as.numeric(input[['sample_size_mediation']]), error = function(e) NA)
    db.sample.size.o.c = tryCatch(as.numeric(input[['sample_size_outcome']]), error = function(e) NA)
    db.sample.size.c = !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0 && !is.na(db.sample.size.m.c) && db.sample.size.m.c > 0 && !is.na(db.sample.size.o.c) && db.sample.size.o.c > 0
    if ((!is.null(input[['exposure_file']]) || !is.null(exposure.file())) && (!is.null(input[['mediation_file']]) || !is.null(mediation.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && db.sample.size.c) {
      shinyjs::show('next_upload_data')
    }
  })
  
  output[['download_2mr_analysis']] <- downloadHandler(
    filename = function() { glue::glue('io.MMR.{format(Sys.time(), "%y%m%d%H%M%S")}.zip') },
    content = function(zipfilename) {
      spsComps::shinyCatch({
        temp_dir <- tempdir()
        
        # Forest Plot
        message(glue('{Sys.time()} {id} add Forest Plot.'))
        width = ifelse(test = is.null(input[['forest_plot_draw_size']]$width), yes = 720 / 60, no = input[['forest_plot_draw_size']]$width / 60)
        height = ifelse(test = is.null(input[['forest_plot_draw_size']]$height), yes = 240 / 60, no = input[['forest_plot_draw_size']]$height / 60)
        forest_plot.pdf.file = file.path(temp_dir, 'forest_plot.pdf')
        pdf(file = forest_plot.pdf.file, width = width, height = height)
        EO.P = mr_odds_res.e.o(); EO.P$method = paste0('(E -> O) ', EO.P$method)
        EM.P = mr_odds_res.e.m(); EM.P$method = paste0('(E -> M) ', EM.P$method)
        MO.P = mr_odds_res.m.o(); MO.P$method = paste0('(M -> O) ', MO.P$method)
        if (is.null(mr_odds_res.o.e())) {
          OE.P = NULL
        } else {
          OE.P = mr_odds_res.o.e(); OE.P$method = paste0('(O -> E) ', OE.P$method)
        }
        forest.x(dat = rbind(EO.P, EM.P, MO.P, OE.P) %>% na.omit(), shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(4, 1), vline = 1, xrang = NULL, text.layout = c(0, 6.8, 7.0, 9.5, 10.5))
        dev.off()
        
        # DAG Plot
        message(glue('{Sys.time()} {id} add DAG Plot.'))
        width = ifelse(test = is.null(input[['dag_plot_draw_size']]$width), yes = 720 / 60, no = input[['dag_plot_draw_size']]$width / 60)
        height = ifelse(test = is.null(input[['dag_plot_draw_size']]$height), yes = 320 / 60, no = input[['dag_plot_draw_size']]$height / 60)
        dag_plot.pdf.file = file.path(temp_dir, 'dag_plot.pdf')
        EO.P = mr_odds_res.e.o()
        EM.P = mr_odds_res.e.m()
        MO.P = mr_odds_res.m.o()
        MM.P = mr_mmr_result()
        EM.PP = ifelse(test = EM.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(EM.P$pval, 4)}'))
        MO.PP = ifelse(test = MO.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(MO.P$pval, 4)}'))
        EO.PP = ifelse(test = EO.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(EO.P$pval, 4)}'))
        MM.PP = ifelse(test = MM.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(MM.P$pval, 4)}'))
        e.name = input[['trait_name_exposure']]
        m.name = input[['trait_name_mediation']]
        o.name = input[['trait_name_outcome']]
        prop = round((MM.P$b.mediation / (MM.P$b.mediation + MM.P$b.direct)) * 100, 3)
        gg <- MRanalysisBase::dag.m(
          m.t = m.name,
          e.t = e.name,
          o.t = o.name,
          e.m.t = glue('beta1: {round(EM.P$b, 4)} ({round(EM.P$lo_ci, 4)} to {round(EM.P$up_ci, 4)})\n{EM.PP}'),
          m.o.t = glue('beta2: {round(MO.P$b, 4)} ({round(MO.P$lo_ci, 4)} to {round(MO.P$up_ci, 4)})\n{MO.PP}'),
          e.o.d.t = glue('Total effect: {round(EO.P$b, 4)} ({round(EO.P$lo_ci, 4)} to {round(EO.P$up_ci, 4)}), {EO.PP}'),
          e.o.i.t = glue('Indirect effect: {round(MM.P$b.mediation, 4)} ({round(MM.P$lo_ci.mediation, 4)} to {round(MM.P$up_ci.mediation, 4)}), {MM.PP}\nProp: {prop}%')
        )
        ggsave(filename = dag_plot.pdf.file, plot = gg, device = 'pdf', width = width, height = height)
        
        # Instruments for use in MR from the exposure data
        instruments.csv.file = file.path(temp_dir, 'instruments.exposure.csv')
        instruments.csv.F.file = file.path(temp_dir, 'instruments.exposure.F.csv')
        f.data = exposure_data_r2f(); v.data = exposure_data_r2f()
        f.data = f.data[f.data$F > isolate(input[['f']]), ]
        f.data$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']]))
        f.data$exposure = ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']]))
        v.data$F = NULL
        v.data$R2 = NULL
        write.csv(x = v.data, file = instruments.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        write.csv(x = f.data, file = instruments.csv.F.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Instruments for use in MR from the mediation data
        instruments.csv.file = file.path(temp_dir, 'instruments.mediation.csv')
        instruments.csv.F.file = file.path(temp_dir, 'instruments.mediation.F.csv')
        f.data = mediation_data_r2f(); v.data = mediation_data_r2f()
        f.data = f.data[f.data$F > isolate(input[['f']]), ]
        f.data$id.mediation = ifelse(test = is.null(isolate(input[['trait_id_mediation']])) || isolate(input[['trait_id_mediation']]) == '', yes = 'id.mediation', no = isolate(input[['trait_id_mediation']]))
        f.data$mediation = ifelse(test = is.null(isolate(input[['trait_name_mediation']])) || isolate(input[['trait_name_mediation']]) == '', yes = 'name.mediation', no = isolate(input[['trait_name_mediation']]))
        v.data$F = NULL
        v.data$R2 = NULL
        write.csv(x = v.data, file = instruments.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        write.csv(x = f.data, file = instruments.csv.F.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Instruments for use in MR from the outcome data
        if (!is.null(outcome_data_r2f())) {
          instruments.csv.file = file.path(temp_dir, 'instruments.outcome.csv')
          instruments.csv.F.file = file.path(temp_dir, 'instruments.outcome.F.csv')
          f.data = outcome_data_r2f(); v.data = outcome_data_r2f()
          f.data = f.data[f.data$F > isolate(input[['f']]), ]
          f.data$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']]))
          f.data$outcome = ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']]))
          v.data$F = NULL
          v.data$R2 = NULL
          write.csv(x = v.data, file = instruments.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
          write.csv(x = f.data, file = instruments.csv.F.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        }
        
        # Causal Effect Estimates
        mr_results.csv.file = file.path(temp_dir, 'mr_results.csv')
        AA = if (is.null(mr_odds_res.o.e())) { 
          rbind(mr_odds_res.e.o(), mr_odds_res.e.m(), mr_odds_res.m.o()) 
        } else { 
          rbind(mr_odds_res.e.o(), mr_odds_res.e.m(), mr_odds_res.m.o(), mr_odds_res.o.e()) 
        }
        write.csv(x = AA, file = mr_results.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
       
        # Mendelian randomisation for mediation analysis
        mr_results.csv.file = file.path(temp_dir, 'mr_mmr_result.csv')
        write.csv(x = mr_mmr_result(), file = mr_results.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Heterogeneity Tests
        mr_heterogeneity.csv.file = file.path(temp_dir, 'mr_heterogeneity.csv')
        BB = if (is.null(mr_odds_res_het.o.e())) { 
          rbind(mr_odds_res_het.e.o(), mr_odds_res_het.e.m(), mr_odds_res_het.m.o()) 
        } else { 
          rbind(mr_odds_res_het.e.o(), mr_odds_res_het.e.m(), mr_odds_res_het.m.o(), mr_odds_res_het.o.e()) 
        }
        write.csv(x = BB, file = mr_heterogeneity.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Horizontal Pleiotropy
        mr_pleiotropy.csv.file = file.path(temp_dir, 'mr_pleiotropy.csv')
        CC  = if (is.null(mr_odds_res_pleio.o.e())) { 
          rbind(mr_odds_res_pleio.e.o(), mr_odds_res_pleio.e.m(), mr_odds_res_pleio.m.o())
        } else { 
          rbind(mr_odds_res_pleio.e.o(), mr_odds_res_pleio.e.m(), mr_odds_res_pleio.m.o(), mr_odds_res_pleio.o.e())
        }
        write.csv(x = rbind(mr_odds_res_pleio.e.o(), mr_odds_res_pleio.e.m(), mr_odds_res_pleio.m.o(), mr_odds_res_pleio.o.e()), file = mr_pleiotropy.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Zip files
        if (is.null(mr_odds_res.o.e())) {
          zip::zip(zipfile = zipfilename, files = c('dag_plot.pdf', 'forest_plot.pdf', 'instruments.exposure.csv', 'instruments.exposure.F.csv', 'instruments.mediation.csv', 'instruments.mediation.F.csv', 'mr_results.csv', 'mr_mmr_result.csv', 'mr_heterogeneity.csv', 'mr_pleiotropy.csv'), root = temp_dir)
        } else {
          zip::zip(zipfile = zipfilename, files = c('dag_plot.pdf', 'forest_plot.pdf', 'instruments.exposure.csv', 'instruments.exposure.F.csv', 'instruments.mediation.csv', 'instruments.mediation.F.csv', 'instruments.outcome.csv', 'instruments.outcome.F.csv', 'mr_results.csv', 'mr_mmr_result.csv', 'mr_heterogeneity.csv', 'mr_pleiotropy.csv'), root = temp_dir)
        }
        
        message(glue('{Sys.time()} {id} Download all results.'))
      }, trace_back = FALSE)
    }, contentType = 'application/zip')
  
  output[['download_select_ivs_con']] <- downloadHandler(
    filename = function() { glue::glue('io.MMR.IVs.{format(Sys.time(), "%y%m%d%H%M%S")}.zip') },
    content = function(zipfilename) {
      spsComps::shinyCatch({
        temp_dir <- tempdir()
        # Causal Effect Estimates
        e.vis.file = file.path(temp_dir, 'exposure.ivs.txt')
        e.vi = isolate(input[['confounding-exposure']])
        if (is.null(e.vi)) { e.vi = exposure_data_r2f()$SNP }
        write.table(x = e.vi, file = e.vis.file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        # Causal Effect Estimates
        m.vis.file = file.path(temp_dir, 'mediation.ivs.txt')
        m.vi = isolate(input[['confounding-mediation']])
        if (is.null(m.vi)) { m.vi = mediation_data_r2f()$SNP }
        write.table(x = m.vi, file = m.vis.file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        # Causal Effect Estimates
        o.vis.file = file.path(temp_dir, 'outcome.ivs.txt')
        o.vi = isolate(input[['confounding-outcome']])
        if (is.null(o.vi)) { o.vi = outcome_data_r2f()$SNP }
        write.table(x = o.vi, file = o.vis.file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        # Zip files
        zip::zip(zipfile = zipfilename, files = c('exposure.ivs.txt', 'mediation.ivs.txt', 'outcome.ivs.txt'), root = temp_dir)
        
        base::message(glue('{Sys.time()} {id} Download all Instruments.'))
      }, trace_back = FALSE)
    })
  
  vcf.data.e <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        e.file.tmp = exposure.filepath()
        e.file.filter = glue('{dirname(e.file.tmp)}/LP-{basename(e.file.tmp)}')
        system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
        system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
        e.file = e.file.filter
      } else {
        e.file = exposure.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(e.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
  })
  
  vcf.data.m <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        e.file.tmp = mediation.filepath()
        e.file.filter = glue('{dirname(e.file.tmp)}/LP-{basename(e.file.tmp)}')
        system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
        system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
        e.file = e.file.filter
      } else {
        e.file = mediation.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(e.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'mediation')
  })
  
  vcf.data.o <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        e.file.tmp = outcome.filepath()
        e.file.filter = glue('{dirname(e.file.tmp)}/LP-{basename(e.file.tmp)}')
        system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
        system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
        e.file = e.file.filter
      } else {
        e.file = outcome.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(e.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'outcome')
  })
  
  vcf.data.e.m <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        elp.vi = exposure.vi.filepath()
        o.file.tmp = mediation.filepath()
        o.file.filter = glue('{dirname(o.file.tmp)}/LP-{basename(o.file.tmp)}')
        system(glue('{BCFTOOLS} index -t {o.file.tmp}'), intern = FALSE)
        system(glue('{BCFTOOLS} view -i "ID=@{elp.vi}" {o.file.tmp} -Oz -o {o.file.filter}'), intern = FALSE)
        o.file = o.file.filter
      } else {
        o.file = mediation.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(o.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'mediation')
  })
  
  vcf.data.e.o <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        elp.vi = exposure.vi.filepath()
        o.file.tmp = outcome.filepath()
        o.file.filter = glue('{dirname(o.file.tmp)}/LP-{basename(o.file.tmp)}')
        system(glue('{BCFTOOLS} index -t {o.file.tmp}'), intern = FALSE)
        system(glue('{BCFTOOLS} view -i "ID=@{elp.vi}" {o.file.tmp} -Oz -o {o.file.filter}'), intern = FALSE)
        o.file = o.file.filter
      } else {
        o.file = outcome.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(o.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'outcome')
  })
  
  vcf.data.m.o <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        elp.vi = mediation.vi.filepath()
        o.file.tmp = outcome.filepath()
        o.file.filter = glue('{dirname(o.file.tmp)}/LP-{basename(o.file.tmp)}')
        system(glue('{BCFTOOLS} index -t {o.file.tmp}'), intern = FALSE)
        system(glue('{BCFTOOLS} view -i "ID=@{elp.vi}" {o.file.tmp} -Oz -o {o.file.filter}'), intern = FALSE)
        o.file = o.file.filter
      } else {
        o.file = outcome.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(o.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'outcome')
  })
  
  vcf.data.o.e <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        elp.vi = outcome.vi.filepath()
        o.file.tmp = exposure.filepath()
        o.file.filter = glue('{dirname(o.file.tmp)}/LP-{basename(o.file.tmp)}')
        system(glue('{BCFTOOLS} index -t {o.file.tmp}'), intern = FALSE)
        system(glue('{BCFTOOLS} view -i "ID=@{elp.vi}" {o.file.tmp} -Oz -o {o.file.filter}'), intern = FALSE)
        o.file = o.file.filter
      } else {
        o.file = exposure.filepath()
      }
      vcfRT = VariantAnnotation::readVcf(o.file)
    })
    gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
  })
  
  observeEvent(input[['next_upload_data']], {
    if (!is.null(exposure.filepath()) && !is.null(outcome.filepath())) {
      updatebs4TabItems(
        inputId = 'sidebarmenu',
        session = session,
        selected = 'tab-siv'
      )
      shinyjs::show('tab-siv-all')
    } else {
      if (is.null(exposure.filepath())) {
        spsComps::shinyCatch({ base::stop('Exposure Data Not Selected!') }, trace_back = FALSE)
      }
      if (is.null(mediation.filepath())) {
        spsComps::shinyCatch({ base::stop('Mediation Data Not Selected!') }, trace_back = FALSE)
      }
      if (is.null(outcome.filepath())) {
        spsComps::shinyCatch({ base::stop('Outcome Data Not Selected!') }, trace_back = FALSE)
      }
    }
  })
  
  observeEvent(input[['next_select_ivs']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-con'
    )
    shinyjs::show('tab-con-all')
    shinyjs::show('confirm_select_ivs_con')
  })
  
  observeEvent(input[['next_select_ivs_con']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-2mr'
    )
    shinyjs::show('tab-2mr-all')
    shinyjs::show('confirm_2mr_analysis')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_2mr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_mmr_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_het_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_pleio_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
  })
  
  observeEvent(input[['mmr_sessionInfo']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-r-session'
    )
    shinyjs::show('tab-r-session-all')
  })
  
  observeEvent(input[['confirm_select_ivs']], {
    
    exposure_data_random(as.numeric(Sys.time()))
    mediation_data_random(as.numeric(Sys.time()))
    outcome_data_random(as.numeric(Sys.time()))
    
    shinyjs::hide('confirm_select_ivs')
    shinyjs::hide('tab-siv-all')
    
    shiny::removeUI(selector = '#step_select_vis_tbl_box', immediate = TRUE)
    
    shiny::insertUI(
      selector = '#tab-siv-all',
      where = 'beforeBegin',
      immediate = TRUE,
      ui =  bs4Dash::tabBox(
        id = 'step_select_vis_tbl',
        title = NULL,
        width = 12,
        selected = 'Instruments of exposure data',
        status = 'success',
        solidHeader = FALSE,
        type = 'tabs',
        tabPanel(
          title = 'Instruments of exposure data',
          withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data', exposure_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Find instruments of exposure for use in MR from uploaded data, please be patient...')
        ),
        tabPanel(
          title = 'Instruments of mediation data',
          withSpinner({ DT::dataTableOutput(outputId = paste0('mediation_data', mediation_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Find instruments of mediation for use in MR from uploaded data, please be patient...')
        ),
        tabPanel(
          title = 'Instruments of outcome data',
          withSpinner({ DT::dataTableOutput(outputId = paste0('outcome_data', outcome_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Find instruments of outcome for use in MR from uploaded data, please be patient...')
        )
      )
    )
    
    # Exposure
    output[[paste0('exposure_data', exposure_data_random())]]  <- DT::renderDT({
      
      # 1
      spsComps::shinyCatch({
        e.data = subset(vcf.data.e(), pval.exposure < shiny::isolate(input[['p1']]))
        exposure_data(e.data)
      })
      
      spsComps::shinyCatch({
        e.data = exposure_data()
        c.data <- ieugwasr::ld_clump(
          clump_kb = isolate({ as.numeric(input[['clump_kb']]) }),
          clump_r2 = isolate({ as.numeric(input[['clump_r2']]) }),
          clump_p = isolate({ as.numeric(input[['clump_p']]) }),
          pop = isolate({ input[['pop']] }),
          dplyr::tibble(rsid = e.data$SNP, pval = e.data$pval.exposure, id = e.data$id.exposure),
          plink_bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['pop']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
        )
        exposure_data_clump(base::merge(e.data, c.data, by.x = 'SNP', by.y = 'rsid'))
      })
      
      spsComps::shinyCatch({
        m.data = exposure_data_clump()
        if (is.null(db.sample.size.e()) || db.sample.size.e() == '') {
          N = m.data[1, 'samplesize.exposure']
        } else {
          N = as.numeric(db.sample.size.e())
          m.data$samplesize.exposure = N
          if (!is.null(db.case.size.e()) && db.case.size.e() != '') {
            try({ m.data$ncase.exposure = as.numeric(db.case.size.e()) }, silent = TRUE)
            try({ m.data$ncontrol.exposure = as.numeric(db.sample.size.e()) - as.numeric(db.case.size.e()) }, silent = TRUE)
          }
        }
        f.data <- base::transform(m.data, R2 = (2 * (beta.exposure^2) * eaf.exposure * (1 - eaf.exposure)) / (2 * beta.exposure^2 * eaf.exposure * (1 - eaf.exposure) + 2 * se.exposure^2 * N * eaf.exposure * (1 - eaf.exposure)))
        f.data <- base::transform(f.data, F = R2 * (N - 2) / (1 - R2))
        if (isolate(input[['caiculate_f_statitics_mode']])) {
          f.data = f.data[f.data$F > isolate(input[['f']]), ]
        }
        exposure_data_r2f(f.data)
      })
      
      # 2
      spsComps::shinyCatch({
        e.data = subset(vcf.data.m(), pval.mediation < shiny::isolate(input[['p1']]))
        mediation_data(e.data)
      })
      
      spsComps::shinyCatch({
        e.data = mediation_data()
        c.data <- ieugwasr::ld_clump(
          clump_kb = isolate({ as.numeric(input[['clump_kb']]) }),
          clump_r2 = isolate({ as.numeric(input[['clump_r2']]) }),
          clump_p = isolate({ as.numeric(input[['clump_p']]) }),
          pop = isolate({ input[['pop']] }),
          dplyr::tibble(rsid = e.data$SNP, pval = e.data$pval.mediation, id = e.data$id.mediation),
          plink_bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['pop']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
        )
        mediation_data_clump(base::merge(e.data, c.data, by.x = 'SNP', by.y = 'rsid'))
      })

      spsComps::shinyCatch({
        m.data = mediation_data_clump()
        if (is.null(db.sample.size.m()) || db.sample.size.m() == '') {
          N = m.data[1, 'samplesize.mediation']
        } else {
          N = as.numeric(db.sample.size.m())
          m.data$samplesize.mediation = N
          if (!is.null(db.case.size.m()) && db.case.size.m() != '') {
            try({ m.data$ncase.mediation = as.numeric(db.case.size.m()) }, silent = TRUE)
            try({ m.data$ncontrol.mediation = as.numeric(db.sample.size.m()) - as.numeric(db.case.size.m()) }, silent = TRUE)
          }
        }
        f.data <- base::transform(m.data, R2 = (2 * (beta.mediation^2) * eaf.mediation * (1 - eaf.mediation)) / (2 * beta.mediation^2 * eaf.mediation * (1 - eaf.mediation) + 2 * se.mediation^2 * N * eaf.mediation * (1 - eaf.mediation)))
        f.data <- base::transform(f.data, F = R2 * (N - 2) / (1 - R2))
        if (isolate(input[['caiculate_f_statitics_mode']])) {
          f.data = f.data[f.data$F > isolate(input[['f']]), ]
        }
        mediation_data_r2f(f.data)
      })
      
      # 3
      spsComps::shinyCatch({
        e.data = subset(vcf.data.o(), pval.outcome < shiny::isolate(input[['p1']]))
        outcome_data(e.data)
      })
      if (nrow(outcome_data()) > 0) {
        spsComps::shinyCatch({
          e.data = outcome_data()
          c.data <- ieugwasr::ld_clump(
            clump_kb = isolate({ as.numeric(input[['clump_kb']]) }),
            clump_r2 = isolate({ as.numeric(input[['clump_r2']]) }),
            clump_p = isolate({ as.numeric(input[['clump_p']]) }),
            pop = isolate({ input[['pop']] }),
            dplyr::tibble(rsid = e.data$SNP, pval = e.data$pval.outcome, id = e.data$id.outcome),
            plink_bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
            bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['pop']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
          )
          outcome_data_clump(base::merge(e.data, c.data, by.x = 'SNP', by.y = 'rsid'))
        })
        
        spsComps::shinyCatch({
          m.data = outcome_data_clump()
          if (is.null(db.sample.size.o()) || db.sample.size.o() == '') {
            N = m.data[1, 'samplesize.outcome']
          } else {
            N = as.numeric(db.sample.size.o())
            m.data$samplesize.outcome = N
            if (!is.null(db.case.size.o()) && db.case.size.o() != '') {
              try({ m.data$ncase.outcome = as.numeric(db.case.size.o()) }, silent = TRUE)
              try({ m.data$ncontrol.outcome = as.numeric(db.sample.size.o()) - as.numeric(db.case.size.o()) }, silent = TRUE)
            }
          }
          f.data = base::transform(m.data, R2 = (2 * (beta.outcome^2) * eaf.outcome * (1 - eaf.outcome)) / (2 * beta.outcome^2 * eaf.outcome * (1 - eaf.outcome) + 2 * se.outcome^2 * N * eaf.outcome * (1 - eaf.outcome)))
          f.data = base::transform(f.data, F = R2 * (N - 2) / (1 - R2))
          if (isolate(input[['caiculate_f_statitics_mode']])) {
            f.data = f.data[f.data$F > isolate(input[['f']]), ]
          }
          outcome_data_r2f(f.data)
        })
      } else {
        outcome_data_r2f(NULL)
      }
      
      if (nrow(exposure_data_r2f()) > 0 && nrow(mediation_data_r2f()) > 0) {
        shinyjs::show('next_select_ivs')
        shinyjs::show('tab-siv-all')
      }
      
      # Table
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      f.data = exposure_data_r2f()
      if (isolate(input[['caiculate_f_statitics_mode']])) {
        f.data = f.data[f.data$F > isolate(input[['f']]), ]
      }
      f.data$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']]))
      f.data$exposure = ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']]))
      f.data %>% dplyr::select(c('exposure', 'id.exposure', 'SNP', 'chr.exposure', 'pos.exposure', 'effect_allele.exposure', 'other_allele.exposure', 'beta.exposure', 'se.exposure', 'pval.exposure', 'eaf.exposure', 'samplesize.exposure', 'mr_keep.exposure', 'pval_origin.exposure', 'R2', 'F')) %>%
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale),
            R2 = color_bar('#FA614B', fun = unit.scale),
            F = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  'instrumental_variables_exposure', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'instrumental_variables_exposure', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'collection',
                 buttons = list(
                   list(extend = 'csv', filename = 'instrumental_variables_exposure', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                   list(extend = 'excel', filename = 'instrumental_variables_exposure', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                 ),
                 text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    observe({
      jsCode <- paste0("setInterval(function() { if ($('#", "exposure_data').find('tbody tr').length > 0) { Shiny.onInputChange('table_ivs_loaded_exposure', true); clearInterval(this); }}, 100);")
      runjs(jsCode)
    })
    
    # Mediation
    output[[paste0('mediation_data', mediation_data_random())]]  <- DT::renderDT({
      
      if (nrow(exposure_data_r2f()) > 0 && nrow(mediation_data_r2f()) > 0) {
        shinyjs::show('next_select_ivs')
        shinyjs::show('tab-siv-all')
      }
      
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      f.data = mediation_data_r2f()
      if (isolate(input[['caiculate_f_statitics_mode']])) {
        f.data = f.data[f.data$F > isolate(input[['f']]), ]
      }
      f.data$id.mediation = ifelse(test = is.null(isolate(input[['trait_id_mediation']])) || isolate(input[['trait_id_mediation']]) == '', yes = 'id.mediation', no = isolate(input[['trait_id_mediation']]))
      f.data$mediation = ifelse(test = is.null(isolate(input[['trait_name_mediation']])) || isolate(input[['trait_name_mediation']]) == '', yes = 'name.mediation', no = isolate(input[['trait_name_mediation']]))
      f.data %>% dplyr::select(c('mediation', 'id.mediation', 'SNP', 'chr.mediation', 'pos.mediation', 'effect_allele.mediation', 'other_allele.mediation', 'beta.mediation', 'se.mediation', 'pval.mediation', 'eaf.mediation', 'samplesize.mediation', 'mr_keep.mediation', 'pval_origin.mediation', 'R2', 'F')) %>%
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.mediation')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.mediation')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.mediation = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.mediation = color_bar('#FA614B', fun = unit.scale),
            R2 = color_bar('#FA614B', fun = unit.scale),
            F = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  'instrumental_variables_mediation', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'instrumental_variables_mediation', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'collection',
                 buttons = list(
                   list(extend = 'csv', filename = 'instrumental_variables_mediation', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                   list(extend = 'excel', filename = 'instrumental_variables_mediation', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                 ),
                 text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    observe({
      jsCode <- paste0("setInterval(function() { if ($('#", "mediation_data').find('tbody tr').length > 0) { Shiny.onInputChange('table_ivs_loaded_mediation', true); clearInterval(this); }}, 100);")
      runjs(jsCode)
    })
    
    # Outcome
    output[[paste0('outcome_data', outcome_data_random())]]  <- DT::renderDT({
      
      if (nrow(exposure_data_r2f()) > 0 && nrow(mediation_data_r2f()) > 0 && length(nrow(outcome_data_r2f()) > 0) == 1) {
        shinyjs::show('next_select_ivs')
        shinyjs::show('tab-siv-all')

        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        f.data = outcome_data_r2f()
        if (isolate(input[['caiculate_f_statitics_mode']])) {
          f.data = f.data[f.data$F > isolate(input[['f']]), ]
        }
        f.data$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']]))
        f.data$outcome = ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']]))
        f.data %>% dplyr::select(c('outcome', 'id.outcome', 'SNP', 'chr.outcome', 'pos.outcome', 'effect_allele.outcome', 'other_allele.outcome', 'beta.outcome', 'se.outcome', 'pval.outcome', 'eaf.outcome', 'samplesize.outcome', 'mr_keep.outcome', 'pval_origin.outcome', 'R2', 'F')) %>%
          formattable::formattable(
            x = .,
            list(
              area(col = c('se.outcome')) ~ color_tile("#DeF7E9", "#71CA97"),
              area(col = c('eaf.outcome')) ~ color_tile("#8ABCD1", "#ED2F6A"),
              beta.outcome = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
              pval.outcome = color_bar('#FA614B', fun = unit.scale),
              R2 = color_bar('#FA614B', fun = unit.scale),
              F = color_bar('#FA614B', fun = unit.scale)
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
              list(extend = 'copy', filename =  'instrumental_variables_outcome', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'instrumental_variables_outcome', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
              list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = 'instrumental_variables_outcome', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                  list(extend = 'excel', filename = 'instrumental_variables_outcome', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                ),
                text = 'Download data')),
            lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
      }
    })
    
    observe({
      jsCode <- paste0("setInterval(function() { if ($('#", "outcome_data').find('tbody tr').length > 0) { Shiny.onInputChange('table_ivs_loaded_outcome', true); clearInterval(this); }}, 100);")
      runjs(jsCode)
    })
    
    observeEvent(list(input$table_ivs_loaded_exposure, input$table_ivs_loaded_mediation, input$table_ivs_loaded_outcome), {
      if ((!is.null(input$table_ivs_loaded_exposure) && !is.null(input$table_ivs_loaded_mediation) && !is.null(input$table_ivs_loaded_outcome))) {
        shinyjs::show('next_select_ivs')
        shinyjs::show('tab-siv-all')
      }
      if (class(exposure_data_r2f()) != 'logical' && class(mediation_data_r2f()) != 'logical' && class(outcome_data_r2f()) != 'logical') {
        if (nrow(exposure_data_r2f()) > 0 && nrow(mediation_data_r2f()) > 0) {
          shinyjs::show('next_select_ivs')
          shinyjs::show('tab-siv-all')
        }
      }
    })
  })
  
  output[['confounding-ui-exposure']] <- renderUI({
    f.data = exposure_data_r2f()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = 'confounding-exposure',
        label = 'Select SNPs (instrumental variables) for further analysis',
        choices = f.data$SNP,
        selected = f.data$SNP,
        optionsCount = 7,
        noOfDisplayValues = 256,
        multiple = TRUE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      )
    }
  })
  
  output[['confounding-ui-mediation']] <- renderUI({
    f.data = mediation_data_r2f()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = 'confounding-mediation',
        label = 'Select SNPs (instrumental variables) for further analysis',
        choices = f.data$SNP,
        selected = f.data$SNP,
        optionsCount = 7,
        noOfDisplayValues = 256,
        multiple = TRUE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      )
    }
  })
  
  output[['confounding-ui-outcome']] <- renderUI({
    f.data = outcome_data_r2f()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = 'confounding-outcome',
        label = 'Select SNPs (instrumental variables) for further analysis',
        choices = f.data$SNP,
        selected = f.data$SNP,
        optionsCount = 7,
        noOfDisplayValues = 256,
        multiple = TRUE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      )
    }
  })
  
  observeEvent(input[['confirm_select_ivs_con']], {
    e.file.tmp = exposure.filepath()
    e.file.vi = glue('{dirname(e.file.tmp)}/LP-{basename(e.file.tmp)}.vi')
    exposure.vi.filepath(e.file.vi)
    spsComps::shinyCatch({
      e.vi = isolate(input[['confounding-exposure']])
      if (is.null(e.vi)) { e.vi = exposure_data_r2f()$SNP }
      exposure_all_ivs(e.vi)
      write.table(x = e.vi, file = e.file.vi, quote = FALSE, sep = '\t', col.names = FALSE, row.names = FALSE)
    })
    
    m.file.tmp = mediation.filepath()
    m.file.vi = glue('{dirname(m.file.tmp)}/LP-{basename(m.file.tmp)}.vi')
    mediation.vi.filepath(m.file.vi)
    spsComps::shinyCatch({
      m.vi = isolate(input[['confounding-mediation']])
      if (is.null(m.vi)) { m.vi = mediation_data_r2f()$SNP }
      mediation_all_ivs(m.vi)
      write.table(x = m.vi, file = m.file.vi, quote = FALSE, sep = '\t', col.names = FALSE, row.names = FALSE)
    })
    
    o.file.tmp = outcome.filepath()
    o.file.vi = glue('{dirname(o.file.tmp)}/LP-{basename(o.file.tmp)}.vi')
    outcome.vi.filepath(o.file.vi)
    spsComps::shinyCatch({
      o.vi = isolate(input[['confounding-outcome']])
      if (is.null(o.vi)) { o.vi = outcome_data_r2f()$SNP }
      outcome_all_ivs(o.vi)
      write.table(x = o.vi, file = o.file.vi, quote = FALSE, sep = '\t', col.names = FALSE, row.names = FALSE)
    })
    shinyjs::hide('confirm_select_ivs_con')
    shinyjs::show('next_select_ivs_con')
    shinyjs::show('download_select_ivs_con')
  })
  
  output[['2mr-ui']] <- renderUI({
    f.data = exposure_data_r2f()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = '2mr-methods',
        label = 'Select MR method',
        choices = setNames(TwoSampleMR::mr_method_list()$obj, paste0(TwoSampleMR::mr_method_list()$name, ' (', TwoSampleMR::mr_method_list()$obj, ')')),
        selected = 'mr_ivw',
        optionsCount = 7,
        noOfDisplayValues = 16,
        multiple = FALSE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      )
    }
  })
  
  observeEvent(input[['confirm_2mr_analysis']], {
    
    mr.result(NULL)
    shinyjs::hide('confirm_2mr_analysis')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
    
    shiny::removeUI(selector = '#step_2mr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_mmr_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_het_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_pleio_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
    mr_analysis_random(as.numeric(Sys.time()))
    
    shiny::insertUI(
      selector = '#tab-2mr-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_2mr_analysis_tbl',
        title = HTML('Causal Effect Estimates'),
        status = 'success',
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('mr_data', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform two sample MR on each SNP individually, please wait...'))
    )
    
    shiny::insertUI(
      selector = '#tab-2mr-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_2mr_analysis_mmr_tbl',
        title = HTML('Mendelian randomisation for mediation analysis'),
        status = 'success',
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('mr_data_mmr', mr_analysis_random()))  }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform Mendelian randomization for mediation analysis, please wait...'))
    )
    
    shiny::insertUI(
      selector = '#tab-2mr-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_2mr_analysis_het_tbl',
        title = HTML('Heterogeneity Tests'),
        status = 'success',
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('mr_data_het', mr_analysis_random()))  }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Get heterogeneity statistics, please wait...'))
    )
    
    shiny::insertUI(
      selector = '#tab-2mr-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_2mr_analysis_pleio_tbl',
        title = HTML('Horizontal Pleiotropy'),
        status = 'success',
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('mr_data_pleio', mr_analysis_random()))  }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Test for horizontal pleiotropy in MR analysis, please wait...'))
    )
    
    shiny::insertUI(
      selector = '#tab-2mr-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::tabBox(
        id = 'step_2mr_analysis_plot',
        title = NULL,
        width = 12,
        selected = 'Forest Plot',
        status = 'success',
        solidHeader = FALSE,
        type = 'tabs',
        tabPanel(
          title = 'Forest Plot',
          withSpinner({ uiOutput(outputId = paste0('forest_plot', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Draw forest plot, please wait...')
        ),
        tabPanel(
          title = 'DAG Plot',
          withSpinner({ uiOutput(outputId = paste0('dag_plot', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Create scatter plot with lines showing the causal estimate for different MR tests, please wait...')
        )
      )
    )
    
    output[[paste0('mr_data', mr_analysis_random())]] <- DT::renderDT({
      
      spsComps::shinyCatch({
        # Exposure → Outcome
        f.data = exposure_data_r2f() %>% dplyr::filter(SNP %in% exposure_all_ivs())
        o.data = subset(vcf.data.e.o(), pval.outcome > shiny::isolate(input[['p1']]))
        o.data = merge(o.data, f.data, by.x = 'SNP', by.y = 'SNP')
        
        f.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']]))
        o.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']]))
        
        e.dat = TwoSampleMR::format_data(
          dat = f.data,
          type = "exposure",
          snps = f.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.exposure',
          se_col = 'se.exposure',
          effect_allele_col = 'effect_allele.exposure',
          other_allele_col = 'other_allele.exposure',
          pval_col = 'pval.exposure',
          eaf_col = 'eaf.exposure'
        )
        
        o.dat = TwoSampleMR::format_data(
          dat = o.data,
          type = "outcome",
          snps = o.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.outcome',
          se_col = 'se.outcome',
          effect_allele_col = 'effect_allele.outcome',
          other_allele_col = 'other_allele.outcome',
          pval_col = 'pval.outcome',
          eaf_col = 'eaf.outcome'
        )
        
        h.dat = TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
        h.dat$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']]))
        h.dat$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']]))
        hat_data.e.o(h.dat)
        
        # Exposure → Mediation
        f.data = exposure_data_r2f() %>% dplyr::filter(SNP %in% exposure_all_ivs())
        o.data = subset(vcf.data.e.m(), pval.mediation > shiny::isolate(input[['p1']]))
        o.data = merge(o.data, f.data, by.x = 'SNP', by.y = 'SNP')
        
        f.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']]))
        o.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_mediation']])) || isolate(input[['trait_name_mediation']]) == '', yes = 'name.mediation', no = isolate(input[['trait_name_mediation']]))
        
        e.dat = TwoSampleMR::format_data(
          dat = f.data,
          type = 'exposure',
          snps = f.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.exposure',
          se_col = 'se.exposure',
          effect_allele_col = 'effect_allele.exposure',
          other_allele_col = 'other_allele.exposure',
          pval_col = 'pval.exposure',
          eaf_col = 'eaf.exposure'
        )
        o.dat = TwoSampleMR::format_data(
          dat = o.data,
          type = 'outcome',
          snps = o.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.mediation',
          se_col = 'se.mediation',
          effect_allele_col = 'effect_allele.mediation',
          other_allele_col = 'other_allele.mediation',
          pval_col = 'pval.mediation',
          eaf_col = 'eaf.mediation'
        )
        h.dat = TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
        h.dat$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']]))
        h.dat$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_mediation']])) || isolate(input[['trait_id_mediation']]) == '', yes = 'id.mediation', no = isolate(input[['trait_id_mediation']]))
        hat_data.e.m(h.dat)
        
        # Mediation → Outcome
        f.data = mediation_data_r2f() %>% dplyr::filter(SNP %in% mediation_all_ivs())
        o.data = subset(vcf.data.m.o(), pval.outcome > shiny::isolate(input[['p1']]))
        o.data = merge(o.data, f.data, by.x = 'SNP', by.y = 'SNP')
        
        f.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_mediation']])) || isolate(input[['trait_name_mediation']]) == '', yes = 'name.mediation', no = isolate(input[['trait_name_mediation']]))
        o.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']]))
        
        e.dat = TwoSampleMR::format_data(
          dat = f.data,
          type = 'exposure',
          snps = f.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.mediation',
          se_col = 'se.mediation',
          effect_allele_col = 'effect_allele.mediation',
          other_allele_col = 'other_allele.mediation',
          pval_col = 'pval.mediation',
          eaf_col = 'eaf.mediation'
        )
        o.dat = TwoSampleMR::format_data(
          dat = o.data,
          type = 'outcome',
          snps = o.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.outcome',
          se_col = 'se.outcome',
          effect_allele_col = 'effect_allele.outcome',
          other_allele_col = 'other_allele.outcome',
          pval_col = 'pval.outcome',
          eaf_col = 'eaf.outcome'
        )
        h.dat = TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
        h.dat$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_mediation']])) || isolate(input[['trait_id_mediation']]) == '', yes = 'id.mediation', no = isolate(input[['trait_id_mediation']]))
        h.dat$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']]))
        hat_data.m.o(h.dat)
      })
      
      # Outcome → Exposure
      if (length(na.omit(isolate(outcome_all_ivs()))) == 0) {
        o.data <- NULL
        hat_data.o.e(NULL)
      } else {
        f.data = outcome_data_r2f() %>% dplyr::filter(SNP %in% outcome_all_ivs())
        o.data = subset(vcf.data.o.e(), pval.exposure > shiny::isolate(input[['p1']]))
        o.data = merge(o.data, f.data, by.x = 'SNP', by.y = 'SNP')
        
        f.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']]))
        o.data$Phenotype = ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']]))
        
        e.dat = TwoSampleMR::format_data(
          dat = f.data,
          type = "exposure",
          snps = f.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.outcome',
          se_col = 'se.outcome',
          effect_allele_col = 'effect_allele.outcome',
          other_allele_col = 'other_allele.outcome',
          pval_col = 'pval.outcome',
          eaf_col = 'eaf.outcome'
        )
        
        o.dat = TwoSampleMR::format_data(
          dat = o.data,
          type = "outcome",
          snps = o.data$SNP,
          snp_col = 'SNP',
          beta_col = 'beta.exposure',
          se_col = 'se.exposure',
          effect_allele_col = 'effect_allele.exposure',
          other_allele_col = 'other_allele.exposure',
          pval_col = 'pval.exposure',
          eaf_col = 'eaf.exposure'
        )
        
        h.dat = TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
        h.dat$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']]))
        h.dat$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']]))
        hat_data.o.e(h.dat)
      }
      
      spsComps::shinyCatch({
        # Exposure → Outcome
        res.mr = data.frame()
        for (method in isolate(input[['2mr-methods']])) {
          res.mr.tmp = try({TwoSampleMR::mr(
            hat_data.e.o(),
            method_list = method
          )}, silent = TRUE)
          if (!'try-error' %in% class(res.mr.tmp)) {
            res.mr = rbind(res.mr, res.mr.tmp)
          } else {
            base::warning(glue('The "{method}" method is not suitable; an error occurred, skipping this method.'))
          }
        }
        
        mr.odds = TwoSampleMR::generate_odds_ratios(res.mr)
        mr.odds = mr.odds %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
        mr.odds$group = 'Exposure to Outcome'
        mr_odds_res.e.o(mr.odds)
        
        heter.tab = TwoSampleMR::mr_heterogeneity(hat_data.e.o()); heter.tab$group = 'Exposure to Outcome'; mr_odds_res_het.e.o(heter.tab)
        pleio.tab = TwoSampleMR::mr_pleiotropy_test(hat_data.e.o()); pleio.tab$group = 'Exposure to Outcome'; mr_odds_res_pleio.e.o(pleio.tab)
        
        # Exposure → Mediation
        res.mr = data.frame()
        for (method in isolate(input[['2mr-methods']])) {
          res.mr.tmp = try({TwoSampleMR::mr(
            hat_data.e.m(),
            method_list = method
          )}, silent = TRUE)
          if (!'try-error' %in% class(res.mr.tmp)) {
            res.mr = rbind(res.mr, res.mr.tmp)
          } else {
            base::warning(glue('The "{method}" method is not suitable; an error occurred, skipping this method.'))
          }
        }
        mr.odds = TwoSampleMR::generate_odds_ratios(res.mr)
        mr.odds = mr.odds %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
        mr.odds$group = 'Exposure to Mediation'
        mr_odds_res.e.m(mr.odds)
        
        heter.tab = TwoSampleMR::mr_heterogeneity(hat_data.e.m()); heter.tab$group = 'Exposure to Mediation'; mr_odds_res_het.e.m(heter.tab)
        pleio.tab = TwoSampleMR::mr_pleiotropy_test(hat_data.e.m()); pleio.tab$group = 'Exposure to Mediation'; mr_odds_res_pleio.e.m(pleio.tab)
        
        # Mediation → Outcome
        res.mr = data.frame()
        for (method in isolate(input[['2mr-methods']])) {
          res.mr.tmp = try({TwoSampleMR::mr(
            hat_data.m.o(),
            method_list = method
          )}, silent = TRUE)
          if (!'try-error' %in% class(res.mr.tmp)) {
            res.mr = rbind(res.mr, res.mr.tmp)
          } else {
            base::warning(glue('The "{method}" method is not suitable; an error occurred, skipping this method.'))
          }
        }
        mr.odds = TwoSampleMR::generate_odds_ratios(res.mr)
        mr.odds = mr.odds %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
        mr.odds$group = 'Mediation to Outcome'
        mr_odds_res.m.o(mr.odds)
        
        heter.tab = TwoSampleMR::mr_heterogeneity(hat_data.m.o()); heter.tab$group = 'Mediation to Outcome'; mr_odds_res_het.m.o(heter.tab)
        pleio.tab = TwoSampleMR::mr_pleiotropy_test(hat_data.m.o()); pleio.tab$group = 'Mediation to Outcome'; mr_odds_res_pleio.m.o(pleio.tab)

        # Outcome → Exposure
        if (is.null(hat_data.o.e())) {
          mr_odds_res.o.e(NULL)
          mr_odds_res_het.o.e(NULL)
          mr_odds_res_pleio.o.e(NULL)
        } else {
          res.mr = data.frame()
          for (method in isolate(input[['2mr-methods']])) {
            res.mr.tmp = try({TwoSampleMR::mr(
              hat_data.o.e(),
              method_list = method
            )}, silent = TRUE)
            if (!'try-error' %in% class(res.mr.tmp)) {
              res.mr = rbind(res.mr, res.mr.tmp)
            } else {
              base::warning(glue('The "{method}" method is not suitable; an error occurred, skipping this method.'))
            }
          }
          
          if (nrow(res.mr) > 0) {
            mr.odds = TwoSampleMR::generate_odds_ratios(res.mr)
            mr.odds = mr.odds %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
            mr.odds$group = 'Outcome to Exposure'
            mr_odds_res.o.e(mr.odds)
            
            heter.tab = TwoSampleMR::mr_heterogeneity(hat_data.o.e()); heter.tab$group = 'Outcome to Exposure'; mr_odds_res_het.o.e(heter.tab)
            pleio.tab = TwoSampleMR::mr_pleiotropy_test(hat_data.o.e()); pleio.tab$group = 'Outcome to Exposure'; mr_odds_res_pleio.o.e(pleio.tab)
          } else {
            mr_odds_res.o.e(NULL)
            mr_odds_res_het.o.e(NULL)
            mr_odds_res_pleio.o.e(NULL)
          }
        }
      })

      mr.result(rbind(mr_odds_res.e.o(), mr_odds_res.e.m(), mr_odds_res.m.o(), mr_odds_res.o.e()))
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      rbind(mr_odds_res.e.o(), mr_odds_res.e.m(), mr_odds_res.m.o(), mr_odds_res.o.e()) %>%
        dplyr::select(c('group', everything())) %>%
        formattable::formattable(
          x = .,
          list(
            area(col = c('se')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            b = formatter('span', style = x ~ style(color = ifelse(x < 0, '#20894D', '#FC8C23'))),
            pval = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', 'red')))
          )) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single', 
        extensions = 'Buttons', 
        options = list(
          dom = 'Bfrt',  
          scrollX = TRUE,
          paging = FALSE, 
          buttons = list(
            list(extend = 'copy', filename =  'mr_result', title = 'mr_result', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'mr_result', title = 'mr_result', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'mr_result', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))
      
    })
    
    output[[paste0('mr_data_mmr', mr_analysis_random())]] <- DT::renderDT({
      
      if (!is.null(mr.result())) {
        shinyjs::hide('confirm_2mr_analysis')
        shinyjs::show('step_2mr_analysis')
        shinyjs::show('step_2mr_analysis_mmr_tbl')
        shinyjs::show('step_2mr_analysis_het_tbl')
        shinyjs::show('step_2mr_analysis_pleio_tbl')
        shinyjs::show('step_2mr_analysis_plot_box')
        shinyjs::show('download_2mr_analysis')
        shinyjs::show('mmr_sessionInfo')
        shinyjs::hide('show_codes')
      }
      
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      E2M3s. = mr_odds_res.e.m()
      M2O3s. = mr_odds_res.m.o()
      E2O3s. = mr_odds_res.e.o()
      b.mediation = E2M3s.$b * M2O3s.$b
      b.direct = E2O3s.$b - b.mediation
      se.mediation = sqrt(E2M3s.$b^2 * M2O3s.$se^2 + M2O3s.$b^2 * E2M3s.$se^2)
      Z = b.mediation/se.mediation
      m.q = 2*pnorm(q = abs(Z), lower.tail = FALSE)
      lo_ci.mediation = b.mediation - 1.96 * se.mediation
      up_ci.mediation = b.mediation + 1.96 * se.mediation
      mmr.dat = data.frame(
        id.exposure = E2M3s.$id.exposure,
        id.mediation = E2M3s.$id.outcome,
        id.outcome = M2O3s.$id.outcome,
        exposure = E2M3s.$exposure,
        mediation = E2M3s.$outcome,
        outcome = M2O3s.$outcome,
        b.mediation = b.mediation,
        b.direct = b.direct,
        se.mediation = se.mediation,
        pval.mediation = m.q,
        lo_ci.mediation = lo_ci.mediation,
        up_ci.mediation = up_ci.mediation,
        z = Z
      )
      mr_mmr_result(mmr.dat)
      mr_mmr_result() %>%
        formattable::formattable(
          x = .,
          list(
            pval.mediation = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', 'red')))
          )) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single', 
        extensions = 'Buttons',
        options = list(
          dom = 'Brt',
          scrollX = TRUE,
          paging = FALSE,
          buttons = list(
            list(extend = 'copy', filename =  'mr_result_mmr', title = 'mr_result_mmr', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'mr_result_mmr', title = 'mr_result_mmr', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'mr_result_mmr', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result_mmr', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))
    })
    
    output[[paste0('mr_data_het', mr_analysis_random())]] <- DT::renderDT({
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      rbind(mr_odds_res_het.e.o(), mr_odds_res_het.e.m(), mr_odds_res_het.m.o(), mr_odds_res_het.o.e()) %>%
        dplyr::select(c('group', everything())) %>%
        formattable::formattable(
          x = .,
          list(
            Q_pval = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', 'red')))
          )) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single', 
        extensions = 'Buttons',
        options = list(
          dom = 'Brt',
          scrollX = TRUE,
          paging = FALSE,
          buttons = list(
            list(extend = 'copy', filename =  'mr_result_het', title = 'mr_result_het', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'mr_result_het', title = 'mr_result_het', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'mr_result_het', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result_het', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))
    })
    
    output[[paste0('mr_data_pleio', mr_analysis_random())]] <- DT::renderDT({
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      rbind(mr_odds_res_pleio.e.o(), mr_odds_res_pleio.e.m(), mr_odds_res_pleio.m.o(), mr_odds_res_pleio.o.e()) %>%
        dplyr::select(c('group', everything())) %>%
        formattable::formattable(
          x = .,
          list(
            pval = formatter('span', style = x ~ style(color = ifelse(x > 0.05, '#000000', 'red')))
          )) -> dt
      as.datatable(
        dt, 
        rownames = FALSE, 
        selection = 'single', 
        extensions = 'Buttons', 
        options = list(
          dom = 'Brt',
          scrollX = TRUE,
          paging = FALSE,
          buttons = list(
            list(extend = 'copy', filename =  'mr_result_pleio', title = 'mr_result_pleio', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'mr_result_pleio', title = 'mr_result_pleio', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'mr_result_pleio', title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result_pleio', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_2mr_analysis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_2mr_analysis_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    observeEvent(input$table_2mr_analysis_loaded, {
      shinyjs::hide('confirm_2mr_analysis')
      if (!is.null(mr.result())) {
        shinyjs::show('step_2mr_analysis')
        shinyjs::show('step_2mr_analysis_mmr_tbl')
        shinyjs::show('step_2mr_analysis_het_tbl')
        shinyjs::show('step_2mr_analysis_pleio_tbl')
        shinyjs::show('step_2mr_analysis_plot_box')
        shinyjs::show('download_2mr_analysis')
        shinyjs::show('mmr_sessionInfo')
        shinyjs::hide('show_codes')
      }
    })
    
    output[[paste0('forest_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'forest_plot_draw', width = '840px', height = '300px')))
    })
    
    output[['forest_plot_draw']] <- renderPlot({
      EO.P = mr_odds_res.e.o(); EO.P$method = paste0('(E → O) ', EO.P$method)
      EM.P = mr_odds_res.e.m(); EM.P$method = paste0('(E → M) ', EM.P$method)
      MO.P = mr_odds_res.m.o(); MO.P$method = paste0('(M → O) ', MO.P$method)
      if (is.null(mr_odds_res.o.e())) {
        OE.P <- NULL
      } else {
        OE.P = mr_odds_res.o.e(); OE.P$method = paste0('(O → E) ', OE.P$method)
      }
      spsComps::shinyCatch({ 
        forest.x(dat = rbind(EO.P, EM.P, MO.P, OE.P) %>% na.omit(), shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(4, 1), vline = 1, xrang = NULL, text.layout = c(0, 6.8, 7.0, 9.5, 10.5))
      }, trace_back = FALSE)
    })
    
    output[[paste0('dag_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'dag_plot_draw', width = '840px', height = '320px')))
    })
    
    output[['dag_plot_draw']] <- renderPlot({

      EO.P = mr_odds_res.e.o()
      EM.P = mr_odds_res.e.m()
      MO.P = mr_odds_res.m.o()
      MM.P = mr_mmr_result()
      EM.PP = ifelse(test = EM.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(EM.P$pval, 4)}'))
      MO.PP = ifelse(test = MO.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(MO.P$pval, 4)}'))
      EO.PP = ifelse(test = EO.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(EO.P$pval, 4)}'))
      MM.PP = ifelse(test = MM.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(MM.P$pval, 4)}'))
      e.name = input[['trait_name_exposure']]
      m.name = input[['trait_name_mediation']]
      o.name = input[['trait_name_outcome']]
      prop = round((MM.P$b.mediation / (MM.P$b.mediation + MM.P$b.direct)) * 100, 3)
      MRanalysisBase::dag.m(
        m.t = m.name,
        e.t = e.name,
        o.t = o.name,
        e.m.t = glue('beta1: {round(EM.P$b, 4)} ({round(EM.P$lo_ci, 4)} to {round(EM.P$up_ci, 4)})\n{EM.PP}'),
        m.o.t = glue('beta2: {round(MO.P$b, 4)} ({round(MO.P$lo_ci, 4)} to {round(MO.P$up_ci, 4)})\n{MO.PP}'),
        e.o.d.t = glue('Total effect: {round(EO.P$b, 4)} ({round(EO.P$lo_ci, 4)} to {round(EO.P$up_ci, 4)}), {EO.PP}'),
        e.o.i.t = glue('Indirect effect: {round(MM.P$b.mediation, 4)} ({round(MM.P$lo_ci.mediation, 4)} to {round(MM.P$up_ci.mediation, 4)}), {MM.PP}\nProp: {prop}%')
      )
      
    })
  })
  
  observeEvent({
    input[['sample_size_exposure']]
    input[['case_size_exposure']]
    input[['trait_name_exposure']]
    input[['trait_id_exposure']]
    input[['sample_size_mediation']]
    input[['case_size_mediation']]
    input[['trait_name_mediation']]
    input[['trait_id_mediation']]
    input[['sample_size_outcome']]
    input[['case_size_outcome']]
    input[['trait_name_outcome']]
    input[['trait_id_outcome']]
  }, {
    shinyjs::hide('tab-siv-all')
    shinyjs::hide('tab-con-all')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('step_select_vis_tbl')
    shinyjs::hide('next_select_ivs')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_2mr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_select_vis_tbl_box', immediate = TRUE)
  })
  
  observeEvent({
    input[['sample_size_exposure']]
    input[['sample_size_mediation']]
    input[['sample_size_outcome']]
  }, {
    shinyjs::hide('next_upload_data')
    db.sample.size.e.c = tryCatch(as.numeric(input[['sample_size_exposure']]), error = function(e) NA)
    db.sample.size.m.c = tryCatch(as.numeric(input[['sample_size_mediation']]), error = function(e) NA)
    db.sample.size.o.c = tryCatch(as.numeric(input[['sample_size_outcome']]), error = function(e) NA)
    db.sample.size.c = !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0 && !is.na(db.sample.size.m.c) && db.sample.size.m.c > 0 && !is.na(db.sample.size.o.c) && db.sample.size.o.c > 0
    if (db.sample.size.c) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['mmr_sessionInfo']], {
    
    shiny::removeUI(selector = '#show_sessioninfo', immediate = TRUE)
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
        withSpinner({ verbatimTextOutput(outputId = 'show_sessioninfo_output') }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Collecting Information About the Current R Session, please wait...'))
    )
    
    output[['show_sessioninfo_output']] <- renderPrint({
      # xfun::session_info(c('dplyr', 'ggplot2', 'ggdag', 'DT', 'gwasglue', 'ieugwasr', 'TwoSampleMR', 'VariantAnnotation'))
      utils::sessionInfo()
    })
    
    shinyjs::show('show_codes')
  })
  
  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {
    
    '# define a function for forest plot'
    forest.x <- function(dat, shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(5, 2), vline = 1, xrang = NULL, text.layout = c(0, 6, 6.8, 8.5, 10)) {
      row.names(dat) <- dat$method
      method <- rownames(dat)
      or <- sprintf(paste0('%.', digit, 'f'), dat$'or')
      orLow  <- sprintf(paste0('%.', digit, 'f'), dat$'or_lci95')
      orHigh <- sprintf(paste0('%.', digit, 'f'), dat$'or_uci95')
      OR <- paste0(or, ' (', orLow, '-', orHigh, ')')
      pVal <- ifelse(dat$pval < 0.001, ' < 0.001', sprintf(paste0('%.', digit, 'f'), dat$pval))
      
      n <- nrow(dat)
      nRow <- n + 1
      ylim <- c(1, nRow)
      layout(matrix(c(1, 2), nc = 2), width = width.ratio)
      
      xlim <- c(0, 10)
      par(mar = c(4, 2.5, 2, 1))
      plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, xlab = '', ylab = '')
      text(text.layout[1], n:1, method, adj = 0, cex = text.cex)
      text(text.layout[2], n:1, pVal, adj = 1, cex = text.cex)
      text(text.layout[3], n + 1, 'P-value', cex = text.cex, font = 2, adj = 1)
      text(text.layout[4], n + 1, 'OR', cex = text.cex, font = 2, adj = 1)
      text(text.layout[5], n:1, OR, adj = 1, cex = text.cex)
      
      par(mar = c(4, 1, 2, 1), mgp = c(2, 0.5, 0), cex.lab = text.cex, cex.axis = text.cex)
      if (is.null(xrang)) {
        xlim = c(min(as.numeric(orLow)*0.75, as.numeric(orHigh)*0.75, 0.5), max(as.numeric(orLow) * 1.25, as.numeric(orHigh) * 1.25, 1))
      } else {
        xlim = xrang
      }
      if (xlim[1] > vline) { xlim[1] = vline }
      if (xlim[2] < vline) { xlim[2] = vline }
      plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, ylab = '', xaxs = 'i', xlab = 'OR')
      arrows(as.numeric(orLow), n:1, as.numeric(orHigh), n:1, angle = 90, code = 3, length = 0.05, col = line.col, lwd = 3)
      abline(v = vline, col = 'black', lty = 2, lwd = 2)
      boxcolor = ifelse(as.numeric(or) > 1, shape.col, shape.col)
      points(as.numeric(or), n:1, pch = 15, col = boxcolor, cex = 2)
      axis(1)
    }
    '# Load exposure data'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.filepath())), type = 'exposure')
    d.dat <- subset(vcf.dat, pval.exposure < ..(unname(shiny::isolate(input[['p1']]))))
    c.data <- ieugwasr::ld_clump(
      clump_kb = ..(isolate({ as.numeric(input[['clump_kb']]) })),
      clump_r2 = ..(isolate({ as.numeric(input[['clump_r2']]) })),
      clump_p = ..(isolate({ as.numeric(input[['clump_p']]) })),
      pop = ..(isolate({ input[['pop']] })),
      dplyr::tibble(rsid = d.dat$SNP, pval = d.dat$pval.exposure, id = d.dat$id.exposure),
      plink_bin = 'your/path/to/plink',
      bfile = 'your/path/to/1kg.v3/'
      # plink_bin = 'E:/tools/plink/plink.exe',
      # bfile = ..(paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
    )
    e.data <- base::merge(d.dat, c.data, by.x = 'SNP', by.y = 'rsid')
    N <- ..(ifelse(test = is.na(as.numeric(db.sample.size.e())), yes = 'please enter the sample size of exposure', no = as.numeric(db.sample.size.e())))
    ef.data <- base::transform(e.data, R2 = (2 * (beta.exposure^2) * eaf.exposure * (1 - eaf.exposure)) / (2 * beta.exposure^2 * eaf.exposure * (1 - eaf.exposure) + 2 * se.exposure^2 * N * eaf.exposure * (1 - eaf.exposure)))
    ef.data <- base::transform(ef.data, F = R2 * (N - 2) / (1 - R2))
    ef.data <- ef.data[ef.data$F > ..(isolate(input[['f']])), ]
    '# Load mediation data'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(mediation.filepath())), type = 'mediation')
    d.dat <- subset(vcf.dat, pval.mediation < ..(unname(shiny::isolate(input[['p1']]))))
    c.data <- ieugwasr::ld_clump(
      clump_kb = ..(isolate({ as.numeric(input[['clump_kb']]) })),
      clump_r2 = ..(isolate({ as.numeric(input[['clump_r2']]) })),
      clump_p = ..(isolate({ as.numeric(input[['clump_p']]) })),
      pop = ..(isolate({ input[['pop']] })),
      dplyr::tibble(rsid = d.dat$SNP, pval = d.dat$pval.mediation, id = d.dat$id.mediation),
      plink_bin = 'your/path/to/plink',
      bfile = 'your/path/to/1kg.v3/'
      # plink_bin = 'E:/tools/plink/plink.exe',
      # bfile = ..(paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
    )
    e.data <- base::merge(d.dat, c.data, by.x = 'SNP', by.y = 'rsid')
    N <- ..(ifelse(test = is.na(as.numeric(db.sample.size.m())), yes = 'please enter the sample size of mediation', no = as.numeric(db.sample.size.m())))
    mf.data <- base::transform(e.data, R2 = (2 * (beta.mediation^2) * eaf.mediation * (1 - eaf.mediation)) / (2 * beta.mediation^2 * eaf.mediation * (1 - eaf.mediation) + 2 * se.mediation^2 * N * eaf.mediation * (1 - eaf.mediation)))
    mf.data <- base::transform(mf.data, F = R2 * (N - 2) / (1 - R2))
    mf.data <- mf.data[mf.data$F > ..(isolate(input[['f']])), ]
    '# Load outcome data'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(outcome.filepath())), type = 'outcome')
    d.dat <- subset(vcf.dat, pval.outcome < ..(unname(shiny::isolate(input[['p1']]))))
    if (nrow(d.dat) > 0) {
      c.data <- ieugwasr::ld_clump(
        clump_kb = ..(isolate({ as.numeric(input[['clump_kb']]) })),
        clump_r2 = ..(isolate({ as.numeric(input[['clump_r2']]) })),
        clump_p = ..(isolate({ as.numeric(input[['clump_p']]) })),
        pop = ..(isolate({ input[['pop']] })),
        dplyr::tibble(rsid = d.dat$SNP, pval = d.dat$pval.outcome, id = d.dat$id.outcome),
        plink_bin = 'your/path/to/plink',
        bfile = 'your/path/to/1kg.v3/'
        # plink_bin = 'E:/tools/plink/plink.exe',
        # bfile = ..(paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
      )
      e.data <- base::merge(d.dat, c.data, by.x = 'SNP', by.y = 'rsid')
      N <- ..(ifelse(test = is.na(as.numeric(db.sample.size.o())), yes = 'please enter the sample size of outcome', no = as.numeric(db.sample.size.o())))
      of.data <- base::transform(e.data, R2 = (2 * (beta.outcome^2) * eaf.outcome * (1 - eaf.outcome)) / (2 * beta.outcome^2 * eaf.outcome * (1 - eaf.outcome) + 2 * se.outcome^2 * N * eaf.outcome * (1 - eaf.outcome)))
      of.data <- base::transform(of.data, F = R2 * (N - 2) / (1 - R2))
      of.data <- of.data[of.data$F > ..(isolate(input[['f']])), ]
    } else {
      of.data <- NULL
    }
    '# remove confounders'
    confounders.e <- ..(setdiff(exposure_data_r2f()$SNP, exposure_all_ivs()))
    confounders.m <- ..(setdiff(mediation_data_r2f()$SNP, mediation_all_ivs()))
    confounders.o <- ..(setdiff(outcome_data_r2f()$SNP, outcome_all_ivs()))
    all_ivs.e <- setdiff(ef.data$SNP, confounders.e)
    all_ivs.m <- setdiff(mf.data$SNP, confounders.m)
    all_ivs.o <- setdiff(of.data$SNP, confounders.o)
    ef.data <- ef.data %>% dplyr::filter(SNP %in% all_ivs.e)
    mf.data <- mf.data %>% dplyr::filter(SNP %in% all_ivs.m)
    if (is.null(all_ivs.o)) {
      of.data <- NULL
    } else {
      of.data <- of.data %>% dplyr::filter(SNP %in% all_ivs.o)
    }
    '# Exposure → Outcome'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(outcome.filepath())), type = 'outcome')
    o.data <- subset(vcf.dat, pval.outcome > ..(unname(shiny::isolate(input[['p1']]))))
    o.data <- merge(o.data, ef.data, by.x = 'SNP', by.y = 'SNP')
    ef.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']])))
    o.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])))
    e.dat <- TwoSampleMR::format_data(
      dat = ef.data,
      type = "exposure",
      snps = ef.data$SNP,
      snp_col = 'SNP',
      beta_col = 'beta.exposure',
      se_col = 'se.exposure',
      effect_allele_col = 'effect_allele.exposure',
      other_allele_col = 'other_allele.exposure',
      pval_col = 'pval.exposure',
      eaf_col = 'eaf.exposure'
    )
    o.dat <- TwoSampleMR::format_data(
      dat = o.data,
      type = "outcome",
      snps = o.data$SNP,
      snp_col = 'SNP',
      beta_col = 'beta.outcome',
      se_col = 'se.outcome',
      effect_allele_col = 'effect_allele.outcome',
      other_allele_col = 'other_allele.outcome',
      pval_col = 'pval.outcome',
      eaf_col = 'eaf.outcome'
    )
    h.dat.eo <- TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
    h.dat.eo$id.exposure <- ..(ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']])))
    h.dat.eo$id.outcome <- ..(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])))
    '# Exposure → Mediation'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(mediation.filepath())), type = 'mediation')
    o.data <- subset(vcf.dat, pval.mediation > ..(unname(shiny::isolate(input[['p1']]))))
    o.data <- merge(o.data, ef.data, by.x = 'SNP', by.y = 'SNP')
    ef.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']])))
    o.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_mediation']])) || isolate(input[['trait_name_mediation']]) == '', yes = 'name.mediation', no = isolate(input[['trait_name_mediation']])))
    e.dat <- TwoSampleMR::format_data(
      dat = ef.data,
      type = 'exposure',
      snps = ef.data$SNP,
      snp_col = 'SNP',
      beta_col = 'beta.exposure',
      se_col = 'se.exposure',
      effect_allele_col = 'effect_allele.exposure',
      other_allele_col = 'other_allele.exposure',
      pval_col = 'pval.exposure',
      eaf_col = 'eaf.exposure'
    )
    o.dat <- TwoSampleMR::format_data(
      dat = o.data,
      type = 'outcome',
      snps = o.data$SNP,
      snp_col = 'SNP',
      beta_col = 'beta.mediation',
      se_col = 'se.mediation',
      effect_allele_col = 'effect_allele.mediation',
      other_allele_col = 'other_allele.mediation',
      pval_col = 'pval.mediation',
      eaf_col = 'eaf.mediation'
    )
    h.dat.em <- TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
    h.dat.em$id.exposure <- ..(ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']])))
    h.dat.em$id.outcome <- ..(ifelse(test = is.null(isolate(input[['trait_id_mediation']])) || isolate(input[['trait_id_mediation']]) == '', yes = 'id.mediation', no = isolate(input[['trait_id_mediation']])))
    '# Mediation → Outcome'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(outcome.filepath())), type = 'outcome')
    o.data <- subset(vcf.dat, pval.outcome > ..(unname(shiny::isolate(input[['p1']]))))
    o.data <- merge(o.data, mf.data, by.x = 'SNP', by.y = 'SNP')
    mf.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_mediation']])) || isolate(input[['trait_name_mediation']]) == '', yes = 'name.mediation', no = isolate(input[['trait_name_mediation']])))
    o.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])))
    e.dat <- TwoSampleMR::format_data(
      dat = mf.data,
      type = 'exposure',
      snps = mf.data$SNP,
      snp_col = 'SNP',
      beta_col = 'beta.mediation',
      se_col = 'se.mediation',
      effect_allele_col = 'effect_allele.mediation',
      other_allele_col = 'other_allele.mediation',
      pval_col = 'pval.mediation',
      eaf_col = 'eaf.mediation'
    )
    o.dat <- TwoSampleMR::format_data(
      dat = o.data,
      type = 'outcome',
      snps = o.data$SNP,
      snp_col = 'SNP',
      beta_col = 'beta.outcome',
      se_col = 'se.outcome',
      effect_allele_col = 'effect_allele.outcome',
      other_allele_col = 'other_allele.outcome',
      pval_col = 'pval.outcome',
      eaf_col = 'eaf.outcome'
    )
    h.dat.mo <- TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
    h.dat.mo$id.exposure <- ..(ifelse(test = is.null(isolate(input[['trait_id_mediation']])) || isolate(input[['trait_id_mediation']]) == '', yes = 'id.mediation', no = isolate(input[['trait_id_mediation']])))
    h.dat.mo$id.outcome <- ..(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])))
    '# Outcome → Exposure'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.filepath())), type = 'exposure')
    o.data <- subset(vcf.dat, pval.exposure > ..(unname(shiny::isolate(input[['p1']]))))
    if (nrow(o.data) > 0) {
      o.data <- merge(o.data, of.data, by.x = 'SNP', by.y = 'SNP')
      of.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])))
      o.data$Phenotype <- ..(ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']])))
      e.dat <- TwoSampleMR::format_data(
        dat = of.data,
        type = "exposure",
        snps = of.data$SNP,
        snp_col = 'SNP',
        beta_col = 'beta.outcome',
        se_col = 'se.outcome',
        effect_allele_col = 'effect_allele.outcome',
        other_allele_col = 'other_allele.outcome',
        pval_col = 'pval.outcome',
        eaf_col = 'eaf.outcome'
      )
      o.dat <- TwoSampleMR::format_data(
        dat = o.data,
        type = "outcome",
        snps = o.data$SNP,
        snp_col = 'SNP',
        beta_col = 'beta.exposure',
        se_col = 'se.exposure',
        effect_allele_col = 'effect_allele.exposure',
        other_allele_col = 'other_allele.exposure',
        pval_col = 'pval.exposure',
        eaf_col = 'eaf.exposure'
      )
      h.dat.oe <- TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
      h.dat.oe$id.exposure <- ..(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])))
      h.dat.oe$id.outcome <- ..(ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']])))
    } else {
      h.dat.oe <- NULL
    }
    '# Perform MR analysis'
    res.mr.eo <- TwoSampleMR::mr(h.dat.eo, method_list = ..(isolate(input[['2mr-methods']])))
    mr.odds.eo <- TwoSampleMR::generate_odds_ratios(res.mr.eo)
    mr.odds.eo <- mr.odds.eo %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
    mr.odds.eo$group <- 'Exposure to Outcome'
    heter.tab.eo <- TwoSampleMR::mr_heterogeneity(h.dat.eo); heter.tab.eo$group = 'Exposure to Outcome'
    pleio.tab.eo <- TwoSampleMR::mr_pleiotropy_test(h.dat.eo); pleio.tab.eo$group = 'Exposure to Outcome'
    
    res.mr.em <- TwoSampleMR::mr(h.dat.em, method_list = ..(isolate(input[['2mr-methods']])))
    mr.odds.em <- TwoSampleMR::generate_odds_ratios(res.mr.em)
    mr.odds.em <- mr.odds.em %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
    mr.odds.em$group <- 'Exposure to Mediation'
    heter.tab.em <- TwoSampleMR::mr_heterogeneity(h.dat.em); heter.tab.em$group = 'Exposure to Mediation'
    pleio.tab.em <- TwoSampleMR::mr_pleiotropy_test(h.dat.em); pleio.tab.em$group = 'Exposure to Mediation'
    
    res.mr.mo <- TwoSampleMR::mr(h.dat.mo, method_list = ..(isolate(input[['2mr-methods']])))
    mr.odds.mo <- TwoSampleMR::generate_odds_ratios(res.mr.mo)
    mr.odds.mo <- mr.odds.mo %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
    mr.odds.mo$group <- 'Mediation to Outcome'
    heter.tab.mo <- TwoSampleMR::mr_heterogeneity(h.dat.mo); heter.tab.mo$group = 'Mediation to Outcome'
    pleio.tab.mo <- TwoSampleMR::mr_pleiotropy_test(h.dat.mo); pleio.tab.mo$group = 'Mediation to Outcome'
    
    if (is.null(h.dat.oe)) {
      mr.odds.oe <- NULL
      heter.tab.oe <- NULL
      pleio.tab.oe <- NULL
    } else {
      res.mr.oe <- TwoSampleMR::mr(h.dat.oe, method_list = ..(isolate(input[['2mr-methods']])))
      if (nrow(res.mr.oe) > 0) {
        mr.odds.oe <- TwoSampleMR::generate_odds_ratios(res.mr.oe)
        mr.odds.oe <- mr.odds.oe %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
        mr.odds.oe$group <- 'Outcome to Exposure'
        heter.tab.oe <- TwoSampleMR::mr_heterogeneity(h.dat.oe); heter.tab.oe$group = 'Outcome to Exposure'
        pleio.tab.oe <- TwoSampleMR::mr_pleiotropy_test(h.dat.oe); pleio.tab.oe$group = 'Outcome to Exposure'
      } else {
        heter.tab.oe <- NULL
        pleio.tab.oe <- NULL
      }
    }
    
    mr.odds <- rbind(mr.odds.eo, mr.odds.em, mr.odds.mo, mr.odds.oe)
    heter.tab <- rbind(heter.tab.eo, heter.tab.em, heter.tab.mo, heter.tab.oe)
    pleio.tab <- rbind(pleio.tab.eo, pleio.tab.em, pleio.tab.mo, pleio.tab.oe)
    
    '# Calculate Mediation Effect'
    b.mediation <- mr.odds.em$b * mr.odds.mo$b
    b.direct <- mr.odds.eo$b - b.mediation
    se.mediation <- sqrt(mr.odds.em$b^2 * mr.odds.mo$se^2 + mr.odds.mo$b^2 * mr.odds.em$se^2)
    Z <- b.mediation/se.mediation
    m.q <- 2*pnorm(q = abs(Z), lower.tail = FALSE)
    lo_ci.mediation <- b.mediation - 1.96 * se.mediation
    up_ci.mediation <- b.mediation + 1.96 * se.mediation
    mmr.dat <- data.frame(
      id.exposure = mr.odds.em$id.exposure,
      id.mediation = mr.odds.em$id.outcome,
      id.outcome = mr.odds.mo$id.outcome,
      exposure = mr.odds.em$exposure,
      mediation = mr.odds.em$outcome,
      outcome = mr.odds.mo$outcome,
      b.mediation = b.mediation,
      b.direct = b.direct,
      se.mediation = se.mediation,
      pval.mediation = m.q,
      lo_ci.mediation = lo_ci.mediation,
      up_ci.mediation = up_ci.mediation,
      z = Z
    )
    '# Forest Plot'
    EO.P <- mr.odds.eo; EO.P$method = paste0('(E → O) ', EO.P$method)
    EM.P <- mr.odds.em; EM.P$method = paste0('(E → M) ', EM.P$method)
    MO.P <- mr.odds.mo; MO.P$method = paste0('(M → O) ', MO.P$method)
    if (is.null(mr.odds.oe)) {
      OE.P <- NULL
    } else {
      OE.P <- mr.odds.oe; OE.P$method = paste0('(O → E) ', OE.P$method)
    }
    forest.x(dat = rbind(EO.P, EM.P, MO.P, OE.P) %>% na.omit(), shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(4, 1), vline = 1, xrang = NULL, text.layout = c(0, 6.8, 7.0, 9.5, 10.5))
    '# DAG Plot'
    EO.P <- mr.odds.eo
    EM.P <- mr.odds.em
    MO.P <- mr.odds.mo
    MM.P <- mmr.dat
    EM.PP <- ifelse(test = EM.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(EM.P$pval, 4)}'))
    MO.PP <- ifelse(test = MO.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(MO.P$pval, 4)}'))
    EO.PP <- ifelse(test = EO.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(EO.P$pval, 4)}'))
    MM.PP <- ifelse(test = MM.P$pval < 0.0001, yes = 'P < 0.0001', no = glue('p = {round(MM.P$pval, 4)}'))
    e.name <- ..(input[['trait_name_exposure']])
    m.name <- ..(input[['trait_name_mediation']])
    o.name <- ..(input[['trait_name_outcome']])
    prop <- round((MM.P$b.mediation / (MM.P$b.mediation + MM.P$b.direct)) * 100, 3)
    MRanalysisBase::dag.m(
      m.t = m.name,
      e.t = e.name,
      o.t = o.name,
      e.m.t = glue('beta1: {round(EM.P$b, 4)} ({round(EM.P$lo_ci, 4)} to {round(EM.P$up_ci, 4)})\n{EM.PP}'),
      m.o.t = glue('beta2: {round(MO.P$b, 4)} ({round(MO.P$lo_ci, 4)} to {round(MO.P$up_ci, 4)})\n{MO.PP}'),
      e.o.d.t = glue('Total effect: {round(EO.P$b, 4)} ({round(EO.P$lo_ci, 4)} to {round(EO.P$up_ci, 4)}), {EO.PP}'),
      e.o.i.t = glue('Indirect effect: {round(MM.P$b.mediation, 4)} ({round(MM.P$lo_ci.mediation, 4)} to {round(MM.P$up_ci.mediation, 4)}), {MM.PP}\nProp: {prop}%')
    )
  })
  
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(glue)
        library(dplyr)
        library(ggplot2)
        library(ieugwasr)
        library(gwasglue)
        library(TwoSampleMR)
        library(VariantAnnotation)
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
  
  observeEvent( {
    input[['p1']]
    input[['clump_r2']]
    input[['clump_kb']]
    input[['clump_p']]
    input[['pop']]
    input[['f']]
    input[['caiculate_f_statitics_mode']]
  }, {
    shinyjs::hide('tab-con-all')
    shinyjs::hide('tab-mrdat-all')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('step_select_vis_tbl_box')
    shinyjs::show('confirm_select_ivs')
    shinyjs::hide('confirm_select_ivs_con')
    shinyjs::hide('confirm_2mr_analysis')
    shinyjs::hide('next_select_ivs')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['confounding-exposure']]
  }, {
    shinyjs::show('confirm_select_ivs_con')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['confounding-mediation']]
  }, {
    shinyjs::show('confirm_select_ivs_con')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['confounding-outcome']]
  }, {
    shinyjs::show('confirm_select_ivs_con')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['2mr-methods']]
  }, {
    shinyjs::show('confirm_2mr_analysis')
    shinyjs::hide('step_2mr_analysis_tbl')
    shinyjs::hide('step_2mr_analysis_mmr_tbl')
    shinyjs::hide('step_2mr_analysis_het_tbl')
    shinyjs::hide('step_2mr_analysis_pleio_tbl')
    shinyjs::hide('step_2mr_analysis_plot_box')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
    shinyjs::hide('tab-r-session-all')
  })
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.analysis.mr_two_step_mr_local, server = server.analysis.mr_two_step_mr_local)