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
suppressMessages(suppressWarnings(library(MendelianRandomization)))

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

options(shiny.maxRequestSize = tryCatch(if (Sys.getenv("MRANALYSIS_MAX_SIZE") == '') 100000 * 1024^2 else eval(parse(text = Sys.getenv("MRANALYSIS_MAX_SIZE"))), error = function(e) 100000 * 1024^2))

# ui
ui.analysis.mr_mvmr_local <- function() {
  
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
        href = '/'
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
        bs4SidebarMenuItem(HTML('<p class="nav-text">Format data for MVMR </p>'), tabName = 'tab-mrdat', icon = icon('table-cells', lib = 'font-awesome', class = 'nav-icon')),
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
        MRanalysisBase::web.statistic.baidu,
        tags$script("function closeNotification() { document.getElementById('notification').style.display = 'none'; }")
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
            html. <- system.file('extdata', 'HTML/intro.analysis.MVMR-local.html', package = 'MRanalysisBase')
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
            id = 'step_upload_gwas_data',
            title = NULL,
            width = 12,
            selected = 'Step 1. Choose exposure datas.',
            status = 'success',
            solidHeader = FALSE,
            type = 'tabs',
            tabPanel(
              title = 'Step 1. Choose exposure datas.',
              fluidRow(
                id = 'upload_exposure_data_here',
                tags$div(
                  class = 'row',
                  bs4Dash::box(
                    id = 'step_upload_exposure_data_B',
                    title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose exposure data (B).</strong>"),
                    status = 'warning',
                    width = 6,
                    shiny::fileInput(
                      inputId = 'exposure_B',
                      accept = c('.vcf.gz'),
                      label = 'Upload a file for exposure.',
                      multiple = FALSE
                    ),
                    verbatimTextOutput('exposure_file_B'),
                    column(12,
                           div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                               shiny::actionButton(inputId = 'use_demo_data', label = 'Use demo data', class = 'btn-warning'),
                               shiny::actionButton(inputId = 'reset', label = 'Reset', class = 'btn-info btn-stepone')
                           )
                    )
                  ),
                  bs4Dash::box(
                    id = 'step_upload_exposure_data_information_B',
                    title = HTML("<strong>&nbsp;Information for exposure (B)</strong>"),
                    status = 'warning',
                    width = 6,
                    collapsed = FALSE,
                    collapsible = TRUE,
                    fluidRow(
                      column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = 'sample_size_exposure_B', label = 'Sample size', value = NULL))),
                      column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = 'case_size_exposure_B', label = 'Case size', value = NULL))),
                      column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = 'trait_name_exposure_B', label = 'Trait Name', value = NULL))),
                      column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = 'trait_id_exposure_B', label = 'Trait ID', value = NULL)))
                    )
                  )
                ),
                tags$div(
                  class = 'row',
                  bs4Dash::box(
                    id = 'step_upload_exposure_data_A',
                    title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose exposure data (A).</strong>"),
                    status = 'warning',
                    width = 6,
                    shiny::fileInput(
                      inputId = 'exposure_A',
                      accept = c('.vcf.gz'),
                      label = 'Upload a file for exposure.',
                      multiple = FALSE
                    ),
                    verbatimTextOutput('exposure_file_A'),
                    column(12,
                           div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                               shiny::downloadButton(outputId = 'download_demo_data', label = 'Download demo data', class = 'btn-success')
                           )
                    )
                  ),
                  bs4Dash::box(
                    id = 'step_upload_exposure_data_information_A',
                    title = HTML("<strong>&nbsp;Information for exposure (A)</strong>"),
                    status = 'warning',
                    width = 6,
                    collapsed = FALSE,
                    collapsible = TRUE,
                    fluidRow(
                      column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = 'sample_size_exposure_A', label = 'Sample size', value = NULL))),
                      column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = 'case_size_exposure_A', label = 'Case size', value = NULL))),
                      column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = 'trait_name_exposure_A', label = 'Trait Name', value = NULL))),
                      column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = 'trait_id_exposure_A', label = 'Trait ID', value = NULL)))
                    )
                  )
                ),
                HTML('
            <div class="notification" id="notification">
              <button class="close-btn" onclick="closeNotification()">&times;</button>
              <h4>Notice</h4>
              <p>This application supports 2 to 5 exposure variables. Please ensure that the number of exposures you input falls within this range to proceed with the analysis. If you have more than 5 exposure variables, consider selecting or grouping them before running the analysis.</p>
            </div>')
              )
            ),
            tabPanel(
              title = 'Step 2. Choose outcome data.',
              fluidRow(
                bs4Dash::box(
                  id = 'step_upload_mediation_data',
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
            shiny::actionButton(inputId = 'add_exposure', label = 'Add', class = 'btn-danger btn-floatadd'),
            shiny::actionButton(inputId = 'delete_exposure', label = 'Delete', class = 'btn-danger btn-floatrun'),
            shiny::actionButton(inputId = 'next_upload_data', label = 'Next', class = 'btn-danger btn-float')
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-siv' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-siv',
          fluidRow(
            id = 'tab-siv-all',
            column(12, h5("Step 3. Filter instruments for use in MR from exposure data.")),
            bs4Dash::box(
              id = 'step_select_ivs',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 3.1. Genetic variants significantly linked to the exposure factor.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              tags$div(
                title = 'Significance threshold. The default is 5e-8.',
                shiny::numericInput(inputId = 'p1', label = "The significance threshold for selecting instrumental variables.", value = 5E-8, step = 1E-8)
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_select_ivs', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_select_ivs', label = 'Next', class = 'btn-danger btn-float')
            ),
            bs4Dash::box(
              id = 'step_ld_clump',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 3.2. Perform LD clumping.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
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
            )
          )
        ),  # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-con' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-con',
          fluidRow(
            id = 'tab-con-all',
            bs4Dash::box(
              id = 'step_select_ivs_con',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 4. Remove confounding factors.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              uiOutput(outputId = 'confounding-ui')
              # HTML('<h5>To further refine the selection of IVs, you can consider identifying confounding factors through the use of <a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank">PhenoScanner</a>.</h5>'),
              # HTML('<a href="http://www.phenoscanner.medschl.cam.ac.uk/phenoscanner" target="_blank"><img src="img/phenoscanner.png"  title = "PhenoScanner"></a>')
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_select_ivs_con', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_select_ivs_con', label = 'Next', class = 'btn-danger btn-float'),
              tags$div(id = 'download_select_ivs_con', shiny::downloadButton(outputId = 'download_select_ivs_con', label = 'Download', class = 'btn-danger btn-floatdl'))
            )
          )
        ), # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-mrdat' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-mrdat',
          fluidRow(
            id = 'tab-mrdat-all',
            column(12, h5("Step 5. Format data for MVMR.")),
            fluidRow(id = 'step_format_data'),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_format_data', label = 'Run', class = 'btn-danger btn-floatrun'),
              shiny::actionButton(inputId = 'next_format_data', label = 'Next', class = 'btn-danger btn-float')
            )
          )
        ), # end bs4TabItem
        
        # ++++++++++++++++++++++++ tabName = 'tab-2mr' ++++++++++++++++++++++++ 
        bs4TabItem(
          tabName = 'tab-2mr',
          fluidRow(
            id = 'tab-2mr-all',
            column(12, h5("Step 6. Perform Multivariable Mendelian Randomization.")),
            bs4Dash::box(
              id = 'step_2mr_analysis',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Options for MVMR analysis.</strong>"),
              status = 'warning',
              width = 8,
              collapsed = FALSE,
              collapsible = TRUE,
              uiOutput(outputId = '2mr-ui')
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'confirm_mvmr_analysis', label = 'Run', class = 'btn-danger btn-floatrun'),
              tags$div(id = 'download_mvmr_analysis', shiny::downloadButton(outputId = 'download_mvmr_analysis', label = 'Download', class = 'btn-danger btn-floatdl')),
              shiny::actionButton(inputId = 'mvmr_sessionInfo', label = 'Session Info', class = 'btn-danger btn-floatsession')
            )
          )
        ) # end bs4TabItem
        
      )
    ),
    controlbar = NULL,
    scrollToTop = TRUE,
    title = 'MVMR'
  )
}


server.analysis.mr_mvmr_local <- function(input, output, session) {
  
  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste0('demo-', as.numeric(Sys.time()), '.zip') },
    content <- function(file){
      file.copy(glue("demo/demo.zip"), file)
    }, contentType = NULL)
  
  shinyjs::hide('tab-siv-all')
  shinyjs::hide('tab-con-all')
  shinyjs::hide('tab-mrdat-all')
  shinyjs::hide('tab-2mr-all')
  shinyjs::hide('tab-r-session-all')
  shinyjs::hide('next_upload_data')
  shinyjs::hide('next_select_ivs')
  shinyjs::hide('next_select_ivs_con')
  shinyjs::hide('next_format_data')
  shinyjs::hide('download_select_ivs_con')
  shinyjs::hide('download_mvmr_analysis')
  shinyjs::hide('show_codes')
  
  exposure.A.file <- reactiveVal(NULL)
  exposure.A.filepath <- reactiveVal(NULL)
  exposure.A.name <- reactiveVal('EA')
  exposure.B.file <- reactiveVal(NULL)
  exposure.B.filepath <- reactiveVal(NULL)
  exposure.B.name <- reactiveVal('EB')
  exposure.C.file <- reactiveVal(NULL)
  exposure.C.filepath <- reactiveVal(NULL)
  exposure.C.name <- reactiveVal('EC')
  exposure.D.file <- reactiveVal(NULL)
  exposure.D.filepath <- reactiveVal(NULL)
  exposure.D.name <- reactiveVal('ED')
  exposure.E.file <- reactiveVal(NULL)
  exposure.E.filepath <- reactiveVal(NULL)
  exposure.E.name <- reactiveVal('EE')
  exposure.name <- reactiveVal('AE')
  outcome.file <- reactiveVal(NULL)
  outcome.filepath <- reactiveVal(NULL)
  outcome.name <- reactiveVal('O')
  exposure_data_clump <- reactiveVal(NULL)
  exposure_data_clumo <- reactiveVal(NULL)
  exposure.vi.filepath <- reactiveVal(NULL)
  outcome.vi.filepath <- reactiveVal(NULL)
  outcome.mr.data <- reactiveVal(NULL)
  exposure.A.mr.data <- reactiveVal(NULL)
  exposure.B.mr.data <- reactiveVal(NULL)
  exposure.C.mr.data <- reactiveVal(NULL)
  exposure.D.mr.data <- reactiveVal(NULL)
  exposure.E.mr.data <- reactiveVal(NULL)
  mvmr.obj <- reactiveVal(NULL)
  mvmr.data <- reactiveVal(NULL)
  mvmr.hdat <- reactiveVal(NULL)
  mvmr.result <- reactiveVal(NULL)
  con.vis.status <- reactiveVal(NULL)
  all_ivs <- reactiveVal(NULL)
  exposure_data_random <- reactiveVal(as.numeric(Sys.time()))
  format_data_random <- reactiveVal(as.numeric(Sys.time()))
  mr_analysis_random <- reactiveVal(as.numeric(Sys.time()))
  
  # iv <- InputValidator$new()
  # iv$add_rule('mr_mvmedian_iterations', sv_gte(0))
  # iv$add_rule('mr_mvmedian_seed', sv_gte(0))
  # iv$enable()
  
  exposure.box.n <- reactiveVal(c('A', 'B'))
  exposure.box.o <- reactiveVal(c('C', 'D', 'E'))
  exposure.box.status <- reactiveVal(0)
  
  observeEvent(input[['reset']], {
    shinyjs::refresh()
  })
  
  observeEvent(input[['add_exposure']], {
    shinyjs::hide('next_upload_data')
    n.box = exposure.box.n()
    o.box = exposure.box.o()
    if (length(o.box) > 0) {
      
      shinyjs::hide('tab-siv-all')
      shinyjs::hide('tab-con-all')
      shinyjs::hide('tab-mrdat-all')
      shinyjs::hide('tab-2mr-all')
      shinyjs::hide('tab-r-session-all')
      shinyjs::hide('next_select_ivs')
      shinyjs::hide('next_select_ivs_con')
      shinyjs::hide('next_format_data')
      shinyjs::hide('download_select_ivs_con')
      shinyjs::hide('download_mvmr_analysis')
      
      idd = o.box[1]
      exposure.box.n(c(n.box, idd))
      exposure.box.o(o.box[-which(o.box == idd)])
      shiny::insertUI(
        selector = '#upload_exposure_data_here',
        where = 'afterBegin',
        immediate = TRUE,
        ui = fluidRow(
          id = glue::glue('upload_exposure_data_here_{idd}'),
          bs4Dash::box(
            id = glue::glue('step_upload_exposure_data_{idd}'),
            title = HTML(glue::glue("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose exposure data ({idd}).</strong>")),
            status = 'warning',
            width = 6,
            shiny::fileInput(
              inputId = glue::glue('exposure_{idd}'),
              accept = c('.vcf.gz'),
              label = 'Upload a file for exposure.',
              multiple = FALSE
            ),
            verbatimTextOutput(glue::glue('exposure_file_{idd}'))
          ),
          bs4Dash::box(
            id = glue::glue('step_upload_exposure_data_information_{idd}'),
            title = HTML(glue::glue("<strong>&nbsp;Information for exposure ({idd})</strong>")),
            status = 'warning',
            width = 6,
            collapsed = FALSE,
            collapsible = TRUE,
            fluidRow(
              column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = glue::glue('sample_size_exposure_{idd}'), label = 'Sample size', value = NULL))),
              column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = glue::glue('case_size_exposure_{idd}'), label = 'Case size', value = NULL))),
              column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = glue::glue('trait_name_exposure_{idd}'), label = 'Trait Name', value = NULL))),
              column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = glue::glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = NULL)))
            )
          )
        )
      )
    } else {
      spsComps::shinyCatch({
        base::warning(glue('MVMR can include up to {length(exposure.box.n())} exposures.'))
      }, position = 'top-full-width')
    }
  })

  observeEvent(input[['delete_exposure']], {
    n.box = rev(exposure.box.n())
    o.box = exposure.box.o()
    if (length(n.box) > 2) {
      idd = n.box[1]
      exposure.box.n(rev(n.box[-which(n.box == idd)]))
      exposure.box.o(sort(c(o.box, idd)))
      eval(parse(text = glue("exposure.{idd}.id(NULL)")))
      shiny::removeUI(selector = glue::glue('#upload_exposure_data_here_{idd}'), immediate = TRUE)
      shinyjs::hide('tab-siv-all')
      shinyjs::hide('tab-con-all')
      shinyjs::hide('tab-mrdat-all')
      shinyjs::hide('tab-2mr-all')
      shinyjs::hide('tab-r-session-all')
      shinyjs::hide('next_select_ivs')
      shinyjs::hide('next_select_ivs_con')
      shinyjs::hide('next_format_data')
      shinyjs::hide('download_select_ivs_con')
      shinyjs::hide('download_mvmr_analysis')
      shiny::removeUI(selector = '#step_select_vis_tbl', immediate = TRUE)
      shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
      shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
      shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
      shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
    } else {
      spsComps::shinyCatch({
        base::warning(glue('MVMR must contain at least 2 exposures.'))
      }, position = 'top-full-width')
    }
  })
 
  observeEvent(input$step_upload_gwas_data, {
    if (input$step_upload_gwas_data == 'Step 1. Choose exposure datas.') {
      shinyjs::show('add_exposure')
      shinyjs::show('delete_exposure')
    } else if (input$step_upload_gwas_data == 'Step 2. Choose outcome data.') {
      shinyjs::hide('add_exposure')
      shinyjs::hide('delete_exposure')
    }
  })

  vcf.data.e <- reactive({
    A = data.frame()
    B = data.frame()
    C = data.frame()
    D = data.frame()
    E = data.frame()
    spsComps::shinyCatch({
      if (!is.null(exposure.A.filepath())) {
        if (Sys.info()['sysname'] == 'Linux') {
          e.file.tmp = exposure.A.filepath()
          e.file.filter = glue('{dirname(e.file.tmp)}/LP-{exposure.A.name()}')
          system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
          system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
          e.file = e.file.filter
        } else {
          e.file = exposure.A.filepath()
        }
        vcfRT = VariantAnnotation::readVcf(e.file)
        A = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
        A$group = input[[glue('trait_name_exposure_A')]]
      }
      if (!is.null(exposure.B.filepath())) {
        if (Sys.info()['sysname'] == 'Linux') {
          e.file.tmp = exposure.B.filepath()
          e.file.filter = glue('{dirname(e.file.tmp)}/LP-{exposure.B.name()}')
          system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
          system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
          e.file = e.file.filter
        } else {
          e.file = exposure.B.filepath()
        }
        vcfRT = VariantAnnotation::readVcf(e.file)
        B = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
        B$group = input[[glue('trait_name_exposure_B')]]
      }
      if (!is.null(exposure.C.filepath())) {
        if (Sys.info()['sysname'] == 'Linux') {
          e.file.tmp = exposure.C.filepath()
          e.file.filter = glue('{dirname(e.file.tmp)}/LP-{exposure.C.name()}')
          system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
          system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
          e.file = e.file.filter
        } else {
          e.file = exposure.C.filepath()
        }
        vcfRT = VariantAnnotation::readVcf(e.file)
        C = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
        C$group = input[[glue('trait_name_exposure_C')]]
      }
      if (!is.null(exposure.D.filepath())) {
        if (Sys.info()['sysname'] == 'Linux') {
          e.file.tmp = exposure.D.filepath()
          e.file.filter = glue('{dirname(e.file.tmp)}/LP-{exposure.D.name()}')
          system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
          system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
          e.file = e.file.filter
        } else {
          e.file = exposure.D.filepath()
        }
        vcfRT = VariantAnnotation::readVcf(e.file)
        D = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
        D$group = input[[glue('trait_name_exposure_D')]]
      }
      if (!is.null(exposure.E.filepath())) {
        if (Sys.info()['sysname'] == 'Linux') {
          e.file.tmp = exposure.E.filepath()
          e.file.filter = glue('{dirname(e.file.tmp)}/LP-{exposure.E.name()}')
          system(glue("{BCFTOOLS} view -i 'LP>=4.30103' {e.file.tmp} -Oz -o {e.file.filter}"), intern = FALSE)
          system(glue("{BCFTOOLS} index -t {e.file.filter}"), intern = FALSE)
          e.file = e.file.filter
        } else {
          e.file = exposure.E.filepath()
        }
        vcfRT = VariantAnnotation::readVcf(e.file)
        E = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
        E$group = input[[glue('trait_name_exposure_E')]]
      }
    })
    rbind(A, B, C, D, E)
  })
  
  vcf.data.o <- reactive({
    spsComps::shinyCatch({
      if (Sys.info()['sysname'] == 'Linux') {
        elp.vi = exposure.vi.filepath()
        o.file.tmp = outcome.filepath()
        o.file.filter = glue('{dirname(o.file.tmp)}/LP-{outcome.name()}')
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
  
  output[['exposure_file_A']] = renderPrint({ exposure.A.file() })
  output[['exposure_file_B']] = renderPrint({ exposure.B.file() })
  output[['exposure_file_C']] = renderPrint({ exposure.C.file() })
  output[['exposure_file_D']] = renderPrint({ exposure.D.file() })
  output[['exposure_file_E']] = renderPrint({ exposure.E.file() })
  output[['outcome_file']] = renderPrint({ outcome.file() })
  
  observeEvent(input[['use_demo_data']], {
    
    shiny::removeUI(selector = '#upload_exposure_data_here_C', immediate = TRUE)
    shiny::removeUI(selector = '#upload_exposure_data_here_D', immediate = TRUE)
    shiny::removeUI(selector = '#upload_exposure_data_here_E', immediate = TRUE)
    exposure.box.n(c('A', 'B', 'C'))
    exposure.box.o(c('D', 'E'))
    exposure.A.file('ieu-a-299.vcf.gz')
    exposure.A.filepath(glue('demo/ieu-a-299.vcf.gz'))
    shiny::updateTextInput(session = session, inputId = 'sample_size_exposure_A', label = 'Sample size', value = 187167)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure_A', label = 'Trait name', value = 'HDL cholesterol')
    shiny::updateTextInput(session = session, inputId = 'trait_id_exposure_A', label = 'Trait ID', value = 'ieu-a-299')
    shinyjs::hide('exposure_A')
    shinyjs::hide('use_demo_data')
    
    exposure.B.file('ieu-a-300.vcf.gz')
    exposure.B.filepath(glue('demo/ieu-a-300.vcf.gz'))
    shiny::updateTextInput(session = session, inputId = 'sample_size_exposure_B', label = 'Sample size', value = 173082)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure_B', label = 'Trait name', value = 'LDL cholesterol')
    shiny::updateTextInput(session = session, inputId = 'trait_id_exposure_B', label = 'Trait ID', value = 'ieu-a-300')
    shinyjs::hide('exposure_B')
    
    idd = 'C'
    shiny::insertUI(
      selector = '#upload_exposure_data_here',
      where = 'afterBegin',
      immediate = TRUE,
      ui = fluidRow(
        id = glue::glue('upload_exposure_data_here_{idd}'),
        bs4Dash::box(
          id = glue::glue('step_upload_exposure_data_{idd}'),
          title = HTML(glue::glue("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Choose exposure data ({idd}).</strong>")),
          status = 'warning',
          width = 6,
          verbatimTextOutput(glue::glue('exposure_file_{idd}'))
        ),
        bs4Dash::box(
          id = glue::glue('step_upload_exposure_data_information_{idd}'),
          title = HTML(glue::glue("<strong>&nbsp;Information for exposure ({idd})</strong>")),
          status = 'warning',
          width = 6,
          collapsed = FALSE,
          collapsible = TRUE,
          fluidRow(
            column(4, tags$div(title = 'Sample size.', shiny::textInput(inputId = glue::glue('sample_size_exposure_{idd}'), label = 'Sample size', value = 177861))),
            column(4, tags$div(title = 'Case size.', shiny::textInput(inputId = glue::glue('case_size_exposure_{idd}'), label = 'Case size', value = NULL))),
            column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = glue::glue('trait_name_exposure_{idd}'), label = 'Trait Name', value = 'Triglycerides'))),
            column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = glue::glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = 'ieu-a-302')))
          )
        )
      )
    )
    
    exposure.C.file('ieu-a-302.vcf.gz')
    exposure.C.filepath(glue('demo/ieu-a-302.vcf.gz'))
    shinyjs::hide('add_exposure')
    shinyjs::hide('delete_exposure')
    
    exposure.box.status(3)
    if (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) {
      shinyjs::show('next_upload_data')
    }
    
    outcome.file('ieu-a-7.vcf.gz')
    outcome.filepath(glue('demo/ieu-a-7.vcf.gz'))
    shiny::updateTextInput(session = session, inputId = 'trait_name_outcome', label = 'Trait name', value = 'Coronary heart disease')
    shiny::updateTextInput(session = session, inputId = 'trait_id_outcome', label = 'Trait ID', value = 'ieu-a-7')
    shinyjs::hide('outcome')
    shinyjs::hide('use_demo_data')
    if (exposure.box.status() == 3 || exposure.box.status() == length(exposure.box.n())) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['exposure_A']], {
    idd = 'A'
    shinyjs::hide('next_upload_data')
    exposure.A.file(input[[glue('exposure_{idd}')]]$name)
    exposure.A.filepath(input[[glue('exposure_{idd}')]]$datapath)
    shiny::updateTextInput(session = session, inputId = glue('trait_name_exposure_{idd}'), label = 'Trait name', value = 'exposure_A')
    shiny::updateTextInput(session = session, inputId = glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = strsplit(input[[glue('exposure_{idd}')]]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    if ((!is.null(input[['exposure_file_A']]) || !is.null(exposure.A.file())) && (!is.null(input[['exposure_file_B']]) || !is.null(exposure.B.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file()))) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['exposure_B']], {
    idd = 'B'
    shinyjs::hide('next_upload_data')
    exposure.B.file(input[[glue('exposure_{idd}')]]$name)
    exposure.B.filepath(input[[glue('exposure_{idd}')]]$datapath)
    shiny::updateTextInput(session = session, inputId = glue('trait_name_exposure_{idd}'), label = 'Trait name', value = 'exposure_B')
    shiny::updateTextInput(session = session, inputId = glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = strsplit(input[[glue('exposure_{idd}')]]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    exposure.box.status(2)
    if ((!is.null(input[['exposure_file_A']]) || !is.null(exposure.A.file())) && (!is.null(input[['exposure_file_B']]) || !is.null(exposure.B.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && exposure.box.status() == length(exposure.box.n())) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['exposure_C']], {
    idd = 'C'
    shinyjs::hide('next_upload_data')
    exposure.C.file(input[[glue('exposure_{idd}')]]$name)
    exposure.C.filepath(input[[glue('exposure_{idd}')]]$datapath)
    shiny::updateTextInput(session = session, inputId = glue('trait_name_exposure_{idd}'), label = 'Trait name', value = 'exposure_C')
    shiny::updateTextInput(session = session, inputId = glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = strsplit(input[[glue('exposure_{idd}')]]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    exposure.box.status(3)
    if ((!is.null(input[['exposure_file_A']]) || !is.null(exposure.A.file())) && (!is.null(input[['exposure_file_B']]) || !is.null(exposure.B.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && exposure.box.status() == length(exposure.box.n())) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['exposure_D']], {
    idd = 'D'
    shinyjs::hide('next_upload_data')
    exposure.D.file(input[[glue('exposure_{idd}')]]$name)
    exposure.D.filepath(input[[glue('exposure_{idd}')]]$datapath)
    shiny::updateTextInput(session = session, inputId = glue('trait_name_exposure_{idd}'), label = 'Trait name', value = 'exposure_D')
    shiny::updateTextInput(session = session, inputId = glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = strsplit(input[[glue('exposure_{idd}')]]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    exposure.box.status(4)
    if ((!is.null(input[['exposure_file_A']]) || !is.null(exposure.A.file())) && (!is.null(input[['exposure_file_B']]) || !is.null(exposure.B.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && exposure.box.status() == length(exposure.box.n())) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['exposure_E']], {
    idd = 'E'
    shinyjs::hide('next_upload_data')
    exposure.E.file(input[[glue('exposure_{idd}')]]$name)
    exposure.E.filepath(input[[glue('exposure_{idd}')]]$datapath)
    shiny::updateTextInput(session = session, inputId = glue('trait_name_exposure_{idd}'), label = 'Trait name', value = 'exposure_E')
    shiny::updateTextInput(session = session, inputId = glue('trait_id_exposure_{idd}'), label = 'Trait ID', value = strsplit(input[[glue('exposure_{idd}')]]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    exposure.box.status(5)
    if ((!is.null(input[['exposure_file_A']]) || !is.null(exposure.A.file())) && (!is.null(input[['exposure_file_B']]) || !is.null(exposure.B.file())) && (!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && exposure.box.status() == length(exposure.box.n())) {
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
    if ((!is.null(input[['exposure_file_A']]) || !is.null(exposure.A.file())) && (!is.null(input[['exposure_file_B']]) || !is.null(exposure.B.file())) && exposure.box.status() == length(exposure.box.n())) {
      shinyjs::show('next_upload_data')
    }
  })
  
  observeEvent(input[['next_upload_data']], {
    if (!is.null(exposure.A.filepath()) && !is.null(exposure.B.filepath()) && !is.null(outcome.filepath())) {
      updatebs4TabItems(
        inputId = 'sidebarmenu',
        session = session,
        selected = 'tab-siv'
      )
      shinyjs::show('tab-siv-all')
      shinyjs::show('confirm_select_ivs')
      shinyjs::hide('next_select_ivs')
    } else {
      if (is.null(exposure.A.filepath()) && is.null(exposure.B.filepath())) {
        spsComps::shinyCatch({ base::stop('Exposure Data Not Selected!') }, trace_back = FALSE)
      }
      if (is.null(outcome.filepath())) {
        spsComps::shinyCatch({ base::stop('Outcome Data Not Selected!') }, trace_back = FALSE)
      }
    }
  })
  
  observeEvent(input[['confirm_select_ivs']], {
    
    exposure_data_random(as.numeric(Sys.time()))
    con.vis.status(NULL)
    shinyjs::hide('confirm_select_ivs')
    shinyjs::hide('tab-siv-all')
    
    shiny::removeUI(selector = '#step_select_vis_tbl', immediate = TRUE)
    shiny::insertUI(
      selector = '#tab-siv-all',
      where = 'beforeBegin',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_select_vis_tbl',
        title = HTML('Instruments of exposure data'),
        status = 'success',
        width = 12,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data', exposure_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Find instruments from uploaded data, please be patient...'))
    )
    
    output[[paste0('exposure_data', exposure_data_random())]]  <- DT::renderDT({
      clump.dat = data.frame()
      for (exposure in unique(vcf.data.e()$group)) {
        spsComps::shinyCatch({
          e.data = base::subset(vcf.data.e(), pval.exposure < shiny::isolate(input[['p1']]))
          ee.data = e.data[e.data$group == exposure, ]
          c.data <- ieugwasr::ld_clump(
            clump_kb = isolate({ as.numeric(input[['clump_kb']]) }),
            clump_r2 = isolate({ as.numeric(input[['clump_r2']]) }),
            clump_p = isolate({ as.numeric(input[['clump_p']]) }),
            pop = isolate({ input[['pop']] }),
            dplyr::tibble(rsid = ee.data$SNP, pval = ee.data$pval.exposure, id = ee.data$id.exposure),
            plink_bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
            bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['pop']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
          )
          cc.data = base::merge(ee.data, c.data, by.x = 'SNP', by.y = 'rsid') %>% dplyr::select(-pval, -id)
          clump.dat = rbind(clump.dat, cc.data)
        })
      }
      exposure_data_clumo(clump.dat)
      exposure_data_clump(clump.dat)

      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      f.data = exposure_data_clump()
      if (nrow(f.data) > 0) { con.vis.status('AL-BER-T') }
      if (!is.null(con.vis.status())) {
        shinyjs::show('next_select_ivs')
        shinyjs::show('tab-siv-all')
      }
      f.data %>%
        dplyr::select(c('group', dplyr::everything())) %>%
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  'instrumental_variables', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  'instrumental_variables', title = 'instrumental_variables', exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = 'instrumental_variables', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'instrumental_variables', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_select_vis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_ivs_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    observeEvent(input$table_ivs_loaded, {
      if (!is.null(con.vis.status())) {
        shinyjs::show('next_select_ivs')
        shinyjs::show('tab-siv-all')
      }
    })
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
      selected = 'tab-mrdat'
    )
    shinyjs::show('tab-mrdat-all')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mmr_sessionInfo')
    shinyjs::hide('show_codes')
    shinyjs::show('confirm_format_data')
  })
  
  output[['confounding-ui']] <- renderUI({
    f.data = exposure_data_clump()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = 'confounding',
        label = 'Select SNPs (instrumental variables) for further analysis',
        choices = unique(f.data$SNP),
        selected = unique(f.data$SNP),
        optionsCount = 7,
        noOfDisplayValues = 50,
        multiple = TRUE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      )
    }
  })
  
  observeEvent(input[['confirm_select_ivs_con']], {
    e.file.tmp = exposure.A.filepath()
    e.file.vi = glue('{dirname(e.file.tmp)}/LP-{exposure.name()}.vi')
    exposure.vi.filepath(e.file.vi)
    spsComps::shinyCatch({
      e.vi = isolate(input[['confounding']])
      all_ivs(e.vi)
      f.data = exposure_data_clump() %>% dplyr::filter(SNP %in% e.vi)
      exposure_data_clump(f.data)
      write.table(x = e.vi, file = e.file.vi, quote = FALSE, sep = '\t', col.names = FALSE, row.names = FALSE)
    })
    shinyjs::hide('confirm_select_ivs_con')
    shinyjs::show('next_select_ivs_con')
    shinyjs::show('download_select_ivs_con')
  })
  
  observeEvent(input[['confirm_format_data']], {
    
    format_data_random(as.numeric(Sys.time()))
    
    shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
    shinyjs::hide('confirm_format_data')
    shinyjs::hide('next_format_data')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('tab-2mr-all')
    shiny::removeUI(selector = '#step_format_data_tbl_box', immediate = TRUE)
    shiny::removeUI(selector = '#step_format_data_tbl2', immediate = TRUE)
    shiny::removeUI(selector = '#step_format_data_tbl2_box', immediate = TRUE)
    
    if (length(exposure.box.n()) == 2) {
      shiny::insertUI(
        selector = '#tab-mrdat-all',
        where = 'beforeEnd',
        immediate = TRUE,
        ui =  bs4Dash::tabBox(
          id = 'step_format_data_tbl',
          title = NULL,
          width = 12,
          selected = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
          status = 'success',
          solidHeader = FALSE,
          type = 'tabs',
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_A', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_B')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_B', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_outcome')]], ' (Outcome)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('outcome_data', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          )
        )
      )
    } else if (length(exposure.box.n()) == 3) {
      shiny::insertUI(
        selector = '#tab-mrdat-all',
        where = 'beforeEnd',
        immediate = TRUE,
        ui =  bs4Dash::tabBox(
          id = 'step_format_data_tbl',
          title = NULL,
          width = 12,
          selected = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
          status = 'success',
          solidHeader = FALSE,
          type = 'tabs',
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_A', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_B')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_B', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_C')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_C', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_outcome')]], ' (Outcome)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('outcome_data', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          )
        )
      )
    } else if (length(exposure.box.n()) == 4) {
      shiny::insertUI(
        selector = '#tab-mrdat-all',
        where = 'beforeEnd',
        immediate = TRUE,
        ui =  bs4Dash::tabBox(
          id = 'step_format_data_tbl',
          title = NULL,
          width = 12,
          selected = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
          status = 'success',
          solidHeader = FALSE,
          type = 'tabs',
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_A', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_B')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_B', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_C')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_C', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_D')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_D', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_outcome')]], ' (Outcome)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('outcome_data', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          )
        )
      )
    } else {
      shiny::insertUI(
        selector = '#tab-mrdat-all',
        where = 'beforeEnd',
        immediate = TRUE,
        ui =  bs4Dash::tabBox(
          id = 'step_format_data_tbl',
          title = NULL,
          width = 12,
          selected = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
          status = 'success',
          solidHeader = FALSE,
          type = 'tabs',
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_A')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_A', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_B')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_B', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_C')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_C', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_D')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_D', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_exposure_E')]], ' (Exposure)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data_E', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          ),
          tabPanel(
            title = paste0(input[[glue('trait_name_outcome')]], ' (Outcome)'),
            withSpinner({ DT::dataTableOutput(outputId = paste0('outcome_data', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading data, please be patient...')
          )
        )
      )
    }

    shiny::insertUI(
      selector = '#tab-mrdat-all',
      where = 'beforeEnd',
      immediate = TRUE,
      ui =  bs4Dash::box(
        id = 'step_format_data_tbl2',
        title = HTML('Formatted data for MVMR'),
        status = 'success',
        width = 12,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('mvmr_data', format_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Format data for MVMR analysis, please be patient...'))
    )

    spsComps::shinyCatch({
      suppressWarnings({
        c.data = ieugwasr::ld_clump(
          clump_kb = isolate({ as.numeric(input[['clump_kb']]) }),
          clump_r2 = isolate({ as.numeric(input[['clump_r2']]) }),
          clump_p = isolate({ as.numeric(input[['clump_p']]) }),
          pop = isolate({ input[['pop']] }),
          dplyr::tibble(rsid = unique(exposure_data_clump()$SNP)),
          plink_bin = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = '/tools/plink/plink', no = 'E:/tools/plink/plink.exe'),
          bfile = ifelse(test = Sys.info()['sysname'] == 'Linux', yes = paste0('/references/plink/1kg.v3/', isolate({ input[['pop']] })), no = paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
        )
      })
      o.data = subset(vcf.data.o(), pval.outcome > shiny::isolate(input[['p1']]))
      o.data = base::merge(c.data, o.data, by.x = 'rsid', by.y = 'SNP') %>% dplyr::select(-pval, -id)
      o.data$SNP = o.data$rsid; o.data$rsid = NULL
      if (TRUE) {
        o.file.tmp = outcome.filepath()
        o.file.vi = glue('{dirname(o.file.tmp)}/LP-o.file.tmp.vi')
        outcome.vi.filepath(o.file.vi)
        write.table(x = o.data$SNP, file = o.file.vi, quote = FALSE, sep = '\t', col.names = FALSE, row.names = FALSE)
      }
      e.data = data.frame()
      for (idd in exposure.box.n()) {
        spsComps::shinyCatch({
          if (Sys.info()['sysname'] == 'Linux') {
            olp.vi = outcome.vi.filepath()
            e.file.tmp = eval(parse(text = glue('exposure.{idd}.filepath()')))
            e.file.filter = glue('{dirname(e.file.tmp)}/LP-{idd}{idd}.vcf.gz')
            system(glue('{BCFTOOLS} index -t {e.file.tmp}'), intern = FALSE)
            system(glue('{BCFTOOLS} view -i "ID=@{olp.vi}" {e.file.tmp} -Oz -o {e.file.filter}'), intern = FALSE)
            e.file = e.file.filter
          } else {
            e.file = eval(parse(text = glue('exposure.{idd}.filepath()')))
          }
          vcfRT = VariantAnnotation::readVcf(e.file)
        })
        e.data.ALL = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
        assign(glue('e.data.{idd}'), base::merge(o.data, e.data.ALL, by.x = 'SNP', by.y = 'SNP'))
        eval(parse(text = glue("e.data.{idd}$id.exposure = ifelse(test = is.null(isolate(input[[glue('trait_id_exposure_{idd}')]])) || isolate(input[[glue('trait_id_exposure_{idd}')]]) == '', yes = 'id.exposure', no = isolate(input[[glue('trait_id_exposure_{idd}')]]))")))
        eval(parse(text = glue("e.data.{idd}$exposure = ifelse(test = is.null(isolate(input[[glue('trait_name_exposure_{idd}')]])) || isolate(input[[glue('trait_name_exposure_{idd}')]]) == '', yes = 'name.exposure', no = isolate(input[[glue('trait_name_exposure_{idd}')]]))")))
        eval(parse(text = glue("exposure.{idd}.mr.data(e.data.{idd} %>% dplyr::select(c('SNP', 'id.exposure', 'exposure', 'chr.exposure', 'pos.exposure', 'effect_allele.exposure', 'other_allele.exposure', 'beta.exposure', 'se.exposure', 'pval.exposure', 'eaf.exposure', 'mr_keep.exposure')))")))
        e.data = rbind(e.data, eval(parse(text = glue('e.data.{idd}'))))
      }
      o.data$id.outcome = ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']]))
      o.data$outcome = ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']]))
      outcome.mr.data(o.data %>% dplyr::select(c('SNP', 'id.outcome', 'outcome', 'chr.outcome', 'pos.outcome', 'effect_allele.outcome', 'other_allele.outcome', 'beta.outcome', 'se.outcome', 'pval.outcome', 'eaf.outcome', 'mr_keep.outcome')))
      e.data = e.data[, c('SNP', 'exposure', 'id.exposure', 'effect_allele.exposure', 'other_allele.exposure', 'eaf.exposure', 'beta.exposure', 'se.exposure', 'pval.exposure')]
      mvmr.hdat(TwoSampleMR::mv_harmonise_data(exposure_dat = e.data, outcome_dat = outcome.mr.data(), harmonise_strictness = 2))
    })

    # Exposure A
    output[[paste0('exposure_data_A', format_data_random())]]  <- DT::renderDT({
      idd = 'A'
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      exposure.A.mr.data() %>% 
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible',modifier = list(page = 'current'))),
                list(extend = 'excel', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })

    # Exposure B
    output[[paste0('exposure_data_B', format_data_random())]]  <- DT::renderDT({
      idd = 'B'
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      exposure.B.mr.data() %>% 
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    # Exposure C
    output[[paste0('exposure_data_C', format_data_random())]]  <- DT::renderDT({
      idd = 'C'
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      exposure.C.mr.data() %>% 
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    # Exposure D
    output[[paste0('exposure_data_D', format_data_random())]]  <- DT::renderDT({
      idd = 'D'
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      exposure.D.mr.data() %>% 
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    # Exposure E
    output[[paste0('exposure_data_E', format_data_random())]]  <- DT::renderDT({
      idd = 'E'
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      exposure.E.mr.data() %>% 
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.exposure')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.exposure')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.exposure = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.exposure = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('instrumental_variables_exposure_{idd}'), title = input[[glue('trait_name_exposure_{idd}')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'excel', filename = glue('instrumental_variables_exposure_{idd}'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    # Outcome
    output[[paste0('outcome_data', format_data_random())]]  <- DT::renderDT({
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      outcome.mr.data() %>% 
        formattable::formattable(
          x = .,
          list(
            area(col = c('se.outcome')) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c('eaf.outcome')) ~ color_tile("#8ABCD1", "#ED2F6A"),
            beta.outcome = formatter('span', style = x ~ style(color = ifelse(x < 0, 'green', 'red'))),
            pval.outcome = color_bar('#FA614B', fun = unit.scale)
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
            list(extend = 'copy', filename =  glue('instrumental_variables_outcome'), title = input[[glue('trait_name_outcome')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('instrumental_variables_outcome'), title = input[[glue('trait_name_outcome')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = glue('instrumental_variables_outcome'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = glue('instrumental_variables_outcome'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    # Formatted MVMR data
    output[[paste0('mvmr_data', format_data_random())]]  <- DT::renderDT({
      
      e.name.a = input[['trait_name_exposure_A']]
      e.name.b = input[['trait_name_exposure_B']]
      e.name.c = input[['trait_name_exposure_C']]
      e.name.d = input[['trait_name_exposure_D']]
      e.name.e = input[['trait_name_exposure_E']]
      o.name = input[['trait_name_outcome']]
      
      h.data = mvmr.hdat()
      MRMVInputObject <- MendelianRandomization::mr_mvinput(
        bx = h.data$exposure_beta,
        bxse = h.data$exposure_se,
        by = h.data$outcome_beta,
        byse = h.data$outcome_se,
        exposure = c(e.name.a, e.name.b, e.name.c, e.name.d, e.name.e),
        outcome = o.name
      )
      
      dat.a = data.frame(SNP = rownames(MRMVInputObject@betaX))
      dat.b = as.data.frame(MRMVInputObject@betaX); colnames(dat.b) = paste0(MRMVInputObject@exposure, '.beta')
      dat.c = as.data.frame(MRMVInputObject@betaXse); colnames(dat.c) = paste0(MRMVInputObject@exposure, '.se')
      dat.d = data.frame(A = MRMVInputObject@betaY, B = MRMVInputObject@betaYse); colnames(dat.d) = c(paste0(MRMVInputObject@outcome, '.beta'), paste0(MRMVInputObject@outcome, '.se'))
      MRMVInput.data = cbind(dat.a, dat.b, dat.c, dat.d)
      
      mvmr.obj(MRMVInputObject)
      mvmr.data(MRMVInput.data)

      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      mvmr.data() %>% as.data.frame() %>%
        formattable::formattable(
          x = ., list(
            area(col = c(glue('{o.name}.beta'))) ~ color_tile("#DeF7E9", "#71CA97"),
            area(col = c(glue('{o.name}.se'))) ~ color_tile("#8ABCD1", "#ED2F6A")
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
            list(extend = 'copy', filename =  glue('mvmr_formatted_data'), title = input[[glue('MVMR')]], exportOptions = list(modifier = list(page = 'current'))),
            list(extend = 'print', filename =  glue('mvmr_formatted_data'), title = input[[glue('MVMR')]], exportOptions = list(modifier = list(page = 'current'))),
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = glue('mvmr_formatted_data'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = glue('mvmr_formatted_data'), title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data')),
          lengthMenu = list(c(10, 30, 50, 100, -1), c('10', '30', '50', '100', 'All'))))
    })
    
    observe({
      jsCode <- "setInterval(function() { if ($('#step_format_data_tbl2').find('tbody tr').length > 0) { Shiny.onInputChange('table_mvmr_data_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })
    
    observeEvent(input$table_mvmr_data_loaded, {
      shinyjs::show('next_format_data')
    })
  })
  
  observeEvent(input[['next_format_data']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-2mr'
    )
    shinyjs::show('tab-2mr-all')
    shinyjs::show('confirm_mvmr_analysis')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent(input[['mvmr_sessionInfo']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-r-session'
    )
    shinyjs::show('tab-r-session-all')
  })
  
  observeEvent(input[['mvmr_sessionInfo']], {
    
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
      utils::sessionInfo()
    })
    
    shinyjs::show('show_codes')
  })
  
  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {
    
    '# Load exposure data'
    A <- data.frame()
    B <- data.frame()
    C <- data.frame()
    D <- data.frame()
    E <- data.frame()
    if (!is.null(..(exposure.A.filepath()))) {
      A <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.A.filepath())), type = 'exposure')
      A$group <- ..(input[[glue('trait_name_exposure_A')]])
    }
    if (!is.null(..(exposure.B.filepath()))) {
      B <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.B.filepath())), type = 'exposure')
      B$group <- ..(input[[glue('trait_name_exposure_B')]])
    }
    if (!is.null(..(exposure.C.filepath()))) {
      C <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.C.filepath())), type = 'exposure')
      C$group <- ..(input[[glue('trait_name_exposure_C')]])
    }
    if (!is.null(..(exposure.D.filepath()))) {
      D <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.D.filepath())), type = 'exposure')
      D$group <- ..(input[[glue('trait_name_exposure_D')]])
    }
    if (!is.null(..(exposure.E.filepath()))) {
      E <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.E.filepath())), type = 'exposure')
      E$group <- ..(input[[glue('trait_name_exposure_E')]])
    }
    vcf.dat <- rbind(A, B, C, D, E)
    '# LD clump (Each Expousre)'
    clump.dat <- data.frame()
    for (exposure in unique(vcf.dat$group)) {
      spsComps::shinyCatch({
        e.data = base::subset(vcf.dat, pval.exposure < ..(unname(shiny::isolate(input[['p1']]))))
        ee.data = e.data[e.data$group == exposure, ]
        c.data = ieugwasr::ld_clump(
          clump_kb = ..(as.numeric(input[['clump_kb']])),
          clump_r2 = ..(as.numeric(input[['clump_r2']])),
          clump_p = ..(as.numeric(input[['clump_p']])),
          pop = ..(input[['pop']]),
          dplyr::tibble(rsid = ee.data$SNP, pval = ee.data$pval.exposure, id = ee.data$id.exposure),
          plink_bin = 'your/path/to/plink',
          bfile = 'your/path/to/1kg.v3/'
          # plink_bin = 'E:/tools/plink/plink.exe',
          # bfile = ..(paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
        )
        cc.data = base::merge(ee.data, c.data, by.x = 'SNP', by.y = 'rsid') %>% dplyr::select(-pval, -id)
        clump.dat = rbind(clump.dat, cc.data)
      })
    }
    '# remove confounders'
    confounders <- ..(setdiff(exposure_data_clumo()$SNP, all_ivs()))
    '# Filter'
    all_ivs <- setdiff(clump.dat$SNP, confounders)
    clump.dat <- clump.dat %>% dplyr::filter(SNP %in% all_ivs)
    '# LD clump (All Expousres)'
    c.data <- ieugwasr::ld_clump(
      clump_kb = ..(as.numeric(input[['clump_kb']])),
      clump_r2 = ..(as.numeric(input[['clump_r2']])),
      clump_p = ..(as.numeric(input[['clump_p']])),
      pop = ..(input[['pop']]),
      dplyr::tibble(rsid = unique(clump.dat$SNP)),
      plink_bin = 'your/path/to/plink',
      bfile = 'your/path/to/1kg.v3/'
      # plink_bin = 'E:/tools/plink/plink.exe',
      # bfile = ..(paste0('E:/references/plink/1kg.v3/', isolate({ input[['pop']] })))
    )
    '# Load outcome data'
    vcf.dat <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(outcome.filepath())), type = 'outcome')
    o.data <- subset(vcf.dat, pval.outcome > ..(unname(shiny::isolate(input[['p1']]))))
    o.data <- base::merge(c.data, o.data, by.x = 'rsid', by.y = 'SNP') %>% dplyr::select(-pval, -id)
    o.data$SNP <- o.data$rsid; o.data$rsid = NULL
    '# Load exposure data base on outcome data'
    e.data = data.frame()
    exposure.A.filepath <- ..(exposure.A.filepath())
    exposure.B.filepath <- ..(exposure.B.filepath())
    exposure.C.filepath <- ..(exposure.C.filepath())
    exposure.D.filepath <- ..(exposure.D.filepath())
    exposure.E.filepath <- ..(exposure.E.filepath())
    trait_id_exposure_A <- ..(input[['trait_id_exposure_A']])
    trait_id_exposure_B <- ..(input[['trait_id_exposure_B']])
    trait_id_exposure_C <- ..(input[['trait_id_exposure_C']])
    trait_id_exposure_D <- ..(input[['trait_id_exposure_D']])
    trait_id_exposure_E <- ..(input[['trait_id_exposure_E']])
    trait_name_exposure_A <- ..(input[['trait_name_exposure_A']])
    trait_name_exposure_B <- ..(input[['trait_name_exposure_B']])
    trait_name_exposure_C <- ..(input[['trait_name_exposure_C']])
    trait_name_exposure_D <- ..(input[['trait_name_exposure_D']])
    trait_name_exposure_E <- ..(input[['trait_name_exposure_E']])
    
    for (idd in ..(exposure.box.n())) {
      vcfRT = readVcf(eval(parse(text = glue('exposure.{idd}.filepath'))))
      e.data.ALL = gwasglue::gwasvcf_to_TwoSampleMR(vcf = vcfRT, type = 'exposure')
      assign(glue('e.data.{idd}'), base::merge(o.data, e.data.ALL, by.x = 'SNP', by.y = 'SNP'))
      eval(parse(text = glue("e.data.{idd}$id.exposure = trait_id_exposure_{idd}")))
      eval(parse(text = glue("e.data.{idd}$exposure = trait_name_exposure_{idd}")))
      eval(parse(text = glue("e.data.{idd} <- e.data.{idd} %>% dplyr::select(c('SNP', 'id.exposure', 'exposure', 'chr.exposure', 'pos.exposure', 'effect_allele.exposure', 'other_allele.exposure', 'beta.exposure', 'se.exposure', 'pval.exposure', 'eaf.exposure', 'mr_keep.exposure'))")))
      e.data = rbind(e.data, eval(parse(text = glue('e.data.{idd}'))))
    }
    '# Harmonise exposure and outcome for multivariable MR'
    o.data$id.outcome <- ..(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])))
    o.data$outcome <- ..(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])))
    o.data <- o.data %>% dplyr::select(c('SNP', 'id.outcome', 'outcome', 'chr.outcome', 'pos.outcome', 'effect_allele.outcome', 'other_allele.outcome', 'beta.outcome', 'se.outcome', 'pval.outcome', 'eaf.outcome', 'mr_keep.outcome'))
    e.data <- e.data[, c('SNP', 'exposure', 'id.exposure', 'effect_allele.exposure', 'other_allele.exposure', 'eaf.exposure', 'beta.exposure', 'se.exposure', 'pval.exposure')]
    mvmr.hdat <- TwoSampleMR::mv_harmonise_data(exposure_dat = e.data, outcome_dat = o.data, harmonise_strictness = 2)
    '# Formatted MVMR data'
    e.name.a <- ..(input[['trait_name_exposure_A']])
    e.name.b <- ..(input[['trait_name_exposure_B']])
    e.name.c <- ..(input[['trait_name_exposure_C']])
    e.name.d <- ..(input[['trait_name_exposure_D']])
    e.name.e <- ..(input[['trait_name_exposure_E']])
    o.name <- ..(input[['trait_name_outcome']])
    
    MRMVInputObject <- MendelianRandomization::mr_mvinput(
      bx = mvmr.hdat$exposure_beta,
      bxse = mvmr.hdat$exposure_se,
      by = mvmr.hdat$outcome_beta,
      byse = mvmr.hdat$outcome_se,
      exposure = c(e.name.a, e.name.b, e.name.c, e.name.d, e.name.e),
      outcome = o.name
    )
    dat.a <- data.frame(SNP = rownames(MRMVInputObject@betaX))
    dat.b <- as.data.frame(MRMVInputObject@betaX); colnames(dat.b) = paste0(MRMVInputObject@exposure, '.beta')
    dat.c <- as.data.frame(MRMVInputObject@betaXse); colnames(dat.c) = paste0(MRMVInputObject@exposure, '.se')
    dat.d <- data.frame(A = MRMVInputObject@betaY, B = MRMVInputObject@betaYse); colnames(dat.d) = c(paste0(MRMVInputObject@outcome, '.beta'), paste0(MRMVInputObject@outcome, '.se'))
    MRMVInput.data <- cbind(dat.a, dat.b, dat.c, dat.d)
    'MVMR analysis'
    if (..(input[['mvmr_methods']]) == 'mr_mvivw') {
      MRMVObject <- MendelianRandomization::mr_mvivw(
        object = MRMVInputObject,
        model = ..(input$mr_mvivw_model),
        robust = ..(input$mr_mvivw_robust),
        correl = ..(input$mr_mvivw_correl), 
        distribution = ..(input$mr_mvivw_distribution),
        alpha = ..(input$mr_mvivw_alpha)
      )
      method <- 'mr_mvivw'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError
        pval = MRMVObject@Pvalue
        lo_ci = MRMVObject@CILower
        up_ci = MRMVObject@CIUpper
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
    } else if (..(input[['mvmr_methods']]) == 'mr_mvegger') {
      MRMVObject <- MendelianRandomization::mr_mvegger(
        object = MRMVInputObject, 
        orientate = ..(input$mr_mvegger_orientate), 
        correl = ..(input$mr_mvegger_correl), 
        distribution = ..(input$mr_mvegger_distribution),
        alpha =  ..(input$mr_mvegger_alpha)
      )
      method <- 'mr_mvegger'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError.Est
        pval = MRMVObject@Pvalue.Est
        lo_ci = MRMVObject@CILower.Est
        up_ci = MRMVObject@CIUpper.Est
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
    } else if (..(input[['mvmr_methods']]) == 'mr_mvlasso') {
      if (..(input$mr_mvlasso_lambda) == 0) {
        lambda <- numeric(0)
      } else {
        lambda <- .(input$mr_mvlasso_lambda)
      }
      MRMVObject <- MendelianRandomization::mr_mvlasso(
        object = MRMVInputObject, 
        orientate = ..(input$mr_mvlasso_orientate), 
        distribution = ..(input$mr_mvlasso_distribution),
        alpha =  ..(input$mr_mvlasso_alpha),
        lambda = lambda
      )
      method <- 'mr_mvlasso'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError
        pval = MRMVObject@Pvalue
        lo_ci = MRMVObject@CILower
        up_ci = MRMVObject@CIUpper
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
    } else if (..(input[['mvmr_methods']]) == 'mr_mvmedian') {
      MRMVObject <- MendelianRandomization::mr_mvmedian(
        object = MRMVInputObject, 
        distribution = ..(input$mr_mvmedian_distribution),
        alpha = ..(input$mr_mvmedian_alpha),
        iterations = ..(as.numeric(input$mr_mvmedian_iterations)),
        seed = ..(as.numeric(input$mr_mvmedian_seed))
      )
      method <- 'mr_mvmedian'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError
        pval = MRMVObject@Pvalue
        lo_ci = MRMVObject@CILower
        up_ci = MRMVObject@CIUpper
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
    } else {
      mvmr.dat. <- data.frame()
      MRMVObject <- MendelianRandomization::mr_mvivw(
        object = MRMVInputObject,
        model = ..(input$mr_mvivw_model),
        robust = ..(input$mr_mvivw_robust),
        correl = ..(input$mr_mvivw_correl), 
        distribution = ..(input$mr_mvivw_distribution),
        alpha = ..(input$mr_mvivw_alpha)
      )
      method <- 'mr_mvivw'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError
        pval = MRMVObject@Pvalue
        lo_ci = MRMVObject@CILower
        up_ci = MRMVObject@CIUpper
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
      if (is.null(MRMVObject)) {
        mvmr.dat <- data.frame(
          exposure = NA,
          method = method,
          nsnp = NA,
          b = NA,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = NA,
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      } else {
        mvmr.dat <- data.frame(
          exposure = MRMVObject@Exposure,
          method = method,
          nsnp = length(MRMVInputObject@snps),
          b = MRMVObject@Estimate,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = exp(MRMVObject@Estimate),
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      }
      mvmr.dat. <- rbind(mvmr.dat., mvmr.dat)
      
      MRMVObject <- MendelianRandomization::mr_mvegger(
        object = MRMVInputObject, 
        orientate = ..(input$mr_mvegger_orientate), 
        correl = ..(input$mr_mvegger_correl), 
        distribution = ..(input$mr_mvegger_distribution),
        alpha =  ..(input$mr_mvegger_alpha)
      )
      method <- 'mr_mvegger'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError.Est
        pval = MRMVObject@Pvalue.Est
        lo_ci = MRMVObject@CILower.Est
        up_ci = MRMVObject@CIUpper.Est
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
      if (is.null(MRMVObject)) {
        mvmr.dat <- data.frame(
          exposure = NA,
          method = method,
          nsnp = NA,
          b = NA,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = NA,
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      } else {
        mvmr.dat <- data.frame(
          exposure = MRMVObject@Exposure,
          method = method,
          nsnp = length(MRMVInputObject@snps),
          b = MRMVObject@Estimate,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = exp(MRMVObject@Estimate),
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      }
      mvmr.dat. <- rbind(mvmr.dat., mvmr.dat)
      
      if (..(input$mr_mvlasso_lambda) == 0) {
        lambda <- numeric(0)
      } else {
        lambda <- ..(input$mr_mvlasso_lambda)
      }
      MRMVObject <- MendelianRandomization::mr_mvlasso(
        object = MRMVInputObject, 
        orientate = ..(input$mr_mvlasso_orientate), 
        distribution = ..(input$mr_mvlasso_distribution),
        alpha =  ..(input$mr_mvlasso_alpha),
        lambda = lambda
      )
      method <- 'mr_mvlasso'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError
        pval = MRMVObject@Pvalue
        lo_ci = MRMVObject@CILower
        up_ci = MRMVObject@CIUpper
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
      if (is.null(MRMVObject)) {
        mvmr.dat <- data.frame(
          exposure = NA,
          method = method,
          nsnp = NA,
          b = NA,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = NA,
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      } else {
        mvmr.dat <- data.frame(
          exposure = MRMVObject@Exposure,
          method = method,
          nsnp = length(MRMVInputObject@snps),
          b = MRMVObject@Estimate,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = exp(MRMVObject@Estimate),
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      }
      mvmr.dat. <- rbind(mvmr.dat., mvmr.dat)
      
      MRMVObject <- MendelianRandomization::mr_mvmedian(
        object = MRMVInputObject, 
        distribution = ..(input$mr_mvmedian_distribution),
        alpha = ..(input$mr_mvmedian_alpha),
        iterations = ..(as.numeric(input$mr_mvmedian_iterations)),
        seed = ..(as.numeric(input$mr_mvmedian_seed))
      )
      method <- 'mr_mvmedian'
      if (is.null(MRMVObject)) {
        se = NA
        pval = NA
        lo_ci = NA
        up_ci = NA
        or_lci95 = NA
        or_uci95 = NA
      } else {
        se = MRMVObject@StdError
        pval = MRMVObject@Pvalue
        lo_ci = MRMVObject@CILower
        up_ci = MRMVObject@CIUpper
        or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
        or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
      }
      if (is.null(MRMVObject)) {
        mvmr.dat <- data.frame(
          exposure = NA,
          method = method,
          nsnp = NA,
          b = NA,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = NA,
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      } else {
        mvmr.dat <- data.frame(
          exposure = MRMVObject@Exposure,
          method = method,
          nsnp = length(MRMVInputObject@snps),
          b = MRMVObject@Estimate,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = exp(MRMVObject@Estimate),
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      }
      mvmr.dat. <- rbind(mvmr.dat., mvmr.dat)
    }
    
    if (..(input[['mvmr_methods']]) == 'mr_all') {
      mvmr.dat <- mvmr.dat.
    } else {
      if (is.null(MRMVObject)) {
        mvmr.dat <- data.frame(
          exposure = NA,
          method = method,
          nsnp = NA,
          b = NA,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = NA,
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      } else {
        mvmr.dat <- data.frame(
          exposure = MRMVObject@Exposure,
          method = method,
          nsnp = length(MRMVInputObject@snps),
          b = MRMVObject@Estimate,
          se = se,
          pval = pval,
          lo_ci = lo_ci,
          up_ci = up_ci,
          or = exp(MRMVObject@Estimate),
          or_lci95 = or_lci95,
          or_uci95 = or_uci95
        )
      }
    }
    '# F-statistic'
    e.name.a <- ..(input[['trait_name_exposure_A']])
    e.name.b <- ..(input[['trait_name_exposure_B']])
    e.name.c <- ..(input[['trait_name_exposure_C']])
    e.name.d <- ..(input[['trait_name_exposure_D']])
    e.name.e <- ..(input[['trait_name_exposure_E']])
    FF <- MVMR::strength_mvmr(MRMVInputObject, gencov = 0)
    colnames(FF) <- c(e.name.a, e.name.b, e.name.c, e.name.d, e.name.e)
    "# Cochran's Q statistic"
    PL <- MVMR::pleiotropy_mvmr(r_input = MRMVInputObject, gencov = 0)
    presso.tab <- data.frame(
      Qstat = PL$Qstat,
      Qpval = PL$Qpval
    )
    "# Forest Plot"
    MRanalysisBase::forest.mv(dat = mvmr.dat %>% na.omit())
  })
  
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(glue)
        library(dplyr)
        library(ggplot2)
        library(ieugwasr)
        library(gwasglue)
        library(MVMR)
        library(TwoSampleMR)
        library(MendelianRandomization)
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
  
  output[['2mr-ui']] <- renderUI({
    f.data = mvmr.data()
    obj = c('mr_all', 'mr_mvivw', 'mr_mvegger', 'mr_mvlasso', 'mr_mvmedian')
    name = c('Use all methods', 'Multivariable inverse-variance weighted method', 'Multivariable MR-Egger method', 'Multivariable MR-Lasso method', 'Multivariable median-based method')
    if (nrow(f.data) > 0) {
      tags$div(
        shinyWidgets::virtualSelectInput(
          inputId = 'mvmr_methods',
          label = 'Select MVMR methods',
          choices = setNames(obj, paste0(name, ' (', obj, ')')),
          selected = 'mr_all',
          optionsCount = 7,
          noOfDisplayValues = 16,
          multiple = FALSE,
          inline = TRUE,
          showValueAsTags = TRUE,
          search = TRUE
        ),
        conditionalPanel(
          condition = paste0('input.', 'mvmr_methods == ', '"mr_mvivw"'),
          tags$div(
            title = 'What type of model should be used: "default", "random" or "fixed". The random-effects model ("random") is a multiplicative random-effects model, allowing overdispersion in the weighted linear regression (the residual standard error is not fixed to be 1, but is not allowed to take values below 1). The fixed-effect model ("fixed") sets the residual standard error to be 1. The "default" setting is to use a fixed-effect model with 3 genetic variants or fewer, and otherwise to use a random-effects model.',
            selectInput(inputId = 'mr_mvivw_model', label = 'model', choices = list('default' = 'default', 'random' = 'random', 'fixed' = 'fixed'), selected = 'default')
          ),
          tags$div(
            title = 'Indicates whether robust regression using the lmrob() function from the package robustbase should be used in the method rather than standard linear regression (lm).',
            shiny::checkboxInput(inputId = 'mr_mvivw_correl', label = 'robust', value = FALSE)
          ),
          tags$div(
            title = 'If the genetic variants are correlated, then this correlation can be accounted for. The matrix of correlations between must be provided in the MRMVInput object: the elements of this matrix are the correlations between the individual variants (diagonal elements are 1). If a correlation matrix is specified in the MRMVInput object, then correl is set to TRUE.',
            shiny::checkboxInput(inputId = 'mr_mvivw_robust', label = 'correl', value = FALSE)
          ),
          tags$div(
            title = 'The type of distribution used to calculate the confidence intervals. Options are "normal" (default) or "t-dist".',
            selectInput(inputId = 'mr_mvivw_distribution', label = 'distribution', choices = list('normal' = 'normal', 't-dist' = 't-dist'), selected = 'normal')
          ),
          tags$div(
            title = 'The significance level used to calculate the confidence interval. The default value is 0.05.',
            sliderInput(inputId = 'mr_mvivw_alpha', label = 'alpha', min = 0.01, max = 1, value = 0.05, step = 0.01)
          )
        ),
        conditionalPanel(
          condition = paste0('input.', 'mvmr_methods == ', '"mr_mvegger"'),
          tags$div(
            title = 'The risk factor that genetic associations are orientated to. The univariable and multivariable versions of MR-Egger are both sensitive to the choice of parameterization of the genetic associations - which allele the associations are orientated with respect to (in other words, which allele is the effect allele). For univariable MR-Egger, this is resolved by setting the genetic associations with the exposure all to be positive. In multivariable MR-Egger, we have to choose which of the exposures to orientate the genetic associations to. The default option is 1, meaning that genetic associations with the first exposure are set to be positive.',
            sliderInput(inputId = 'mr_mvegger_orientate', label = 'orientate', min = 1, max = length(exposure.box.n()), value = 1, step = 1)
          ),
          tags$div(
            title = 'If the genetic variants are correlated, then this correlation can be accounted for. The matrix of correlations between must be provided in the MRInput object: the elements of this matrix are the correlations between the individual variants (diagonal elements are 1). If a correlation matrix is specified in the MRInput object, then correl is set to TRUE.',
            shiny::checkboxInput(inputId = 'mr_mvegger_correl', label = 'robust', value = FALSE)
          ),
          tags$div(
            title = 'The type of distribution used to calculate the confidence intervals. Options are "normal" (default) or "t-dist".',
            selectInput(inputId = 'mr_mvegger_distribution', label = 'distribution', choices = list('normal' = 'normal', 't-dist' = 't-dist'), selected = 'normal')
          ),
          tags$div(
            title = 'The significance level used to calculate the confidence interval. The default value is 0.05.',
            sliderInput(inputId = 'mr_mvegger_alpha', label = 'alpha', min = 0.01, max = 1, value = 0.05, step = 0.01)
          )
        ),
        conditionalPanel(
          condition = paste0('input.', 'mvmr_methods == ', '"mr_mvlasso"'),
          tags$div(title = 'The risk factor that genetic associations are orientated to. The default option is 1, meaning that genetic associations with the first risk factor are set to be positive.',
            sliderInput(inputId = 'mr_mvlasso_orientate', label = 'orientate', min = 1, max = length(exposure.box.n()), value = 1, step = 1)
          ),
          tags$div(
            title = 'The type of distribution used to calculate the confidence intervals. Options are "normal" (default) or "t-dist".',
            selectInput(inputId = 'mr_mvlasso_distribution', label = 'distribution', choices = list('normal' = 'normal', 't-dist' = 't-dist'), selected = 'normal')
          ),
          tags$div(
            title = 'The significance level used to calculate the confidence intervals. The default value is 0.05.',
            sliderInput(inputId = 'mr_mvlasso_alpha', label = 'alpha', min = 0.01, max = 1, value = 0.05, step = 0.01)
          ),
          tags$div(
            title = 'The value of the tuning parameter used by the lasso procedure which controls the level of sparsity. If not specified, the tuning parameter will be calculated by the heterogeneity stopping rule.',
            sliderInput(inputId = 'mr_mvlasso_lambda', label = 'lambda', min = 0, max = 1, value = 0, step = 0.01)
          )
        ),
        conditionalPanel(
          condition = paste0('input.', 'mvmr_methods == ', '"mr_mvmedian"'),
          tags$div(
            title = 'The type of distribution used to calculate the confidence intervals. Options are "normal" (default) or "t-dist".',
            selectInput(inputId = 'mr_mvmedian_distribution', label = 'distribution', choices = list('normal' = 'normal', 't-dist' = 't-dist'), selected = 'normal')
          ),
          tags$div(
            title = 'The significance level used to calculate the confidence intervals. The default value is 0.05.',
            sliderInput(inputId = 'mr_mvmedian_alpha', label = 'alpha', min = 0.01, max = 1, value = 0.05, step = 0.01)
          ),
          tags$div(
            title = 'The number of bootstrap samples to generate when calculating the estimated standard error. The default value is 10000.',
            textInput(inputId = 'mr_mvmedian_iterations', label = 'iterations', value = 10000, placeholder = 'The number of bootstrap samples to generate when calculating the estimated standard error.')
          ),
          tags$div(
            title = 'The random seed to use when generating the bootstrap samples (for reproducibility). The default value is 314159265. If set to NA, the random seed will not be set (for example, if the function is used as part of a larger simulation).',
            textInput(inputId = 'mr_mvmedian_seed', label = 'seed', value = 314159265, placeholder = 'The random seed to use when generating the bootstrap samples (for reproducibility).')
          )
        )
      )
    }
  })
  
  observeEvent(input[['confirm_mvmr_analysis']], {
    
    mr_analysis_random(as.numeric(Sys.time()))
    
    shinyjs::hide('confirm_mvmr_analysis')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
    db.inter = tryCatch(as.numeric(isolate(input$mr_mvmedian_iterations)), error = function(e) NA)
    db.seed = tryCatch(as.numeric(isolate(input$mr_mvmedian_seed)), error = function(e) NA)
    db.status = !is.na(db.inter) && db.inter > 0 && !is.na(db.seed) && db.seed > 0
    
    if (db.status) {
      shiny::insertUI(
        selector = '#tab-2mr-all',
        where = 'beforeBegin',
        immediate = TRUE,
        ui =  bs4Dash::box(
          id = 'step_mvmr_analysis_tbl',
          title = HTML('Causal Effect Estimates (MVMR)'),
          status = 'success',
          width = 12,
          collapsed = FALSE,
          collapsible = TRUE,
          background = NULL,
          withSpinner({ DT::dataTableOutput(outputId = paste0('mvmr_result', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform MVMR analysis, please be patient...'))
      )
      
      shiny::insertUI(
        selector = '#tab-2mr-all',
        where = 'afterEnd',
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
          )
        )
      )
      
      shiny::insertUI(
        selector = '#tab-2mr-all',
        where = 'afterEnd',
        immediate = TRUE,
        ui =  bs4Dash::box(
          id = 'step_mvmr_analysis_f_tbl',
          title = HTML('F-statistic'),
          status = 'success',
          width = 12,
          collapsed = FALSE,
          collapsible = TRUE,
          background = NULL,
          withSpinner({ DT::dataTableOutput(outputId = paste0('mvmr_result_F', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform MVMR analysis, please be patient...'))
      )
      
      shiny::insertUI(
        selector = '#tab-2mr-all',
        where = 'afterEnd',
        immediate = TRUE,
        ui =  bs4Dash::box(
          id = 'step_mvmr_analysis_pleiotropy_tbl',
          title = HTML("Cochran's Q statistic"),
          status = 'success',
          width = 12,
          collapsed = FALSE,
          collapsible = TRUE,
          background = NULL,
          withSpinner({ DT::dataTableOutput(outputId = paste0('mvmr_result_pleiotropy', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform MVMR analysis, please be patient...'))
      )
      
      if (input[['mvmr_methods']] == 'mr_mvivw') {
        MRMVObject = MendelianRandomization::mr_mvivw(
          object = mvmr.obj(),
          model = isolate(input$mr_mvivw_model),
          robust = isolate(input$mr_mvivw_robust),
          correl = isolate(input$mr_mvivw_correl), 
          distribution = isolate(input$mr_mvivw_distribution),
          alpha = isolate(input$mr_mvivw_alpha)
        )
        method = 'mr_mvivw'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError
          pval = MRMVObject@Pvalue
          lo_ci = MRMVObject@CILower
          up_ci = MRMVObject@CIUpper
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
      } else if (input[['mvmr_methods']] == 'mr_mvegger') {
        MRMVObject = MendelianRandomization::mr_mvegger(
          object = mvmr.obj(), 
          orientate = isolate(input$mr_mvegger_orientate), 
          correl = isolate(input$mr_mvegger_correl), 
          distribution = isolate(input$mr_mvegger_distribution),
          alpha =  isolate(input$mr_mvegger_alpha)
        )
        method = 'mr_mvegger'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError.Est
          pval = MRMVObject@Pvalue.Est
          lo_ci = MRMVObject@CILower.Est
          up_ci = MRMVObject@CIUpper.Est
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
      } else if (input[['mvmr_methods']] == 'mr_mvlasso') {
        if (isolate(input$mr_mvlasso_lambda) == 0) {
          lambda = numeric(0)
        } else {
          lambda = isolate(input$mr_mvlasso_lambda)
        }
        MRMVObject = MendelianRandomization::mr_mvlasso(
          object = mvmr.obj(), 
          orientate = isolate(input$mr_mvlasso_orientate), 
          distribution = isolate(input$mr_mvlasso_distribution),
          alpha =  isolate(input$mr_mvlasso_alpha),
          lambda = lambda
        )
        method = 'mr_mvlasso'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError
          pval = MRMVObject@Pvalue
          lo_ci = MRMVObject@CILower
          up_ci = MRMVObject@CIUpper
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
      } else if (input[['mvmr_methods']] == 'mr_mvmedian') {
        MRMVObject = MendelianRandomization::mr_mvmedian(
          object = mvmr.obj(), 
          distribution = isolate(input$mr_mvmedian_distribution),
          alpha = isolate(input$mr_mvmedian_alpha),
          iterations = as.numeric(isolate(input$mr_mvmedian_iterations)),
          seed = as.numeric(isolate(input$mr_mvmedian_seed))
        )
        method = 'mr_mvmedian'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError
          pval = MRMVObject@Pvalue
          lo_ci = MRMVObject@CILower
          up_ci = MRMVObject@CIUpper
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
      } else {
        mvmr.dat. = data.frame()
        MRMVObject = MendelianRandomization::mr_mvivw(
          object = mvmr.obj(),
          model = isolate(input$mr_mvivw_model),
          robust = isolate(input$mr_mvivw_robust),
          correl = isolate(input$mr_mvivw_correl), 
          distribution = isolate(input$mr_mvivw_distribution),
          alpha = isolate(input$mr_mvivw_alpha)
        )
        method = 'mr_mvivw'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError
          pval = MRMVObject@Pvalue
          lo_ci = MRMVObject@CILower
          up_ci = MRMVObject@CIUpper
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
        if (is.null(MRMVObject)) {
          mvmr.dat = data.frame(
            exposure = NA,
            method = method,
            nsnp = NA,
            b = NA,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = NA,
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        } else {
          mvmr.dat = data.frame(
            exposure = MRMVObject@Exposure,
            method = method,
            nsnp = length(mvmr.obj()@snps),
            b = MRMVObject@Estimate,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = exp(MRMVObject@Estimate),
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        }
        mvmr.dat. = rbind(mvmr.dat., mvmr.dat)
        
        MRMVObject = MendelianRandomization::mr_mvegger(
          object = mvmr.obj(), 
          orientate = isolate(input$mr_mvegger_orientate), 
          correl = isolate(input$mr_mvegger_correl), 
          distribution = isolate(input$mr_mvegger_distribution),
          alpha =  isolate(input$mr_mvegger_alpha)
        )
        method = 'mr_mvegger'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError.Est
          pval = MRMVObject@Pvalue.Est
          lo_ci = MRMVObject@CILower.Est
          up_ci = MRMVObject@CIUpper.Est
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
        if (is.null(MRMVObject)) {
          mvmr.dat = data.frame(
            exposure = NA,
            method = method,
            nsnp = NA,
            b = NA,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = NA,
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        } else {
          mvmr.dat = data.frame(
            exposure = MRMVObject@Exposure,
            method = method,
            nsnp = length(mvmr.obj()@snps),
            b = MRMVObject@Estimate,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = exp(MRMVObject@Estimate),
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        }
        mvmr.dat. = rbind(mvmr.dat., mvmr.dat)
        
        if (isolate(input$mr_mvlasso_lambda) == 0) {
          lambda = numeric(0)
        } else {
          lambda = isolate(input$mr_mvlasso_lambda)
        }
        MRMVObject = MendelianRandomization::mr_mvlasso(
          object = mvmr.obj(), 
          orientate = isolate(input$mr_mvlasso_orientate), 
          distribution = isolate(input$mr_mvlasso_distribution),
          alpha =  isolate(input$mr_mvlasso_alpha),
          lambda = lambda
        )
        method = 'mr_mvlasso'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError
          pval = MRMVObject@Pvalue
          lo_ci = MRMVObject@CILower
          up_ci = MRMVObject@CIUpper
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
        if (is.null(MRMVObject)) {
          mvmr.dat = data.frame(
            exposure = NA,
            method = method,
            nsnp = NA,
            b = NA,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = NA,
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        } else {
          mvmr.dat = data.frame(
            exposure = MRMVObject@Exposure,
            method = method,
            nsnp = length(mvmr.obj()@snps),
            b = MRMVObject@Estimate,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = exp(MRMVObject@Estimate),
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        }
        mvmr.dat. = rbind(mvmr.dat., mvmr.dat)
        
        MRMVObject = MendelianRandomization::mr_mvmedian(
          object = mvmr.obj(), 
          distribution = isolate(input$mr_mvmedian_distribution),
          alpha = isolate(input$mr_mvmedian_alpha),
          iterations = as.numeric(isolate(input$mr_mvmedian_iterations)),
          seed = as.numeric(isolate(input$mr_mvmedian_seed))
        )
        method = 'mr_mvmedian'
        if (is.null(MRMVObject)) {
          se = NA
          pval = NA
          lo_ci = NA
          up_ci = NA
          or_lci95 = NA
          or_uci95 = NA
        } else {
          se = MRMVObject@StdError
          pval = MRMVObject@Pvalue
          lo_ci = MRMVObject@CILower
          up_ci = MRMVObject@CIUpper
          or_lci95 = exp(MRMVObject@Estimate - 1.96 * se)
          or_uci95 = exp(MRMVObject@Estimate + 1.96 * se)
        }
        if (is.null(MRMVObject)) {
          mvmr.dat = data.frame(
            exposure = NA,
            method = method,
            nsnp = NA,
            b = NA,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = NA,
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        } else {
          mvmr.dat = data.frame(
            exposure = MRMVObject@Exposure,
            method = method,
            nsnp = length(mvmr.obj()@snps),
            b = MRMVObject@Estimate,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = exp(MRMVObject@Estimate),
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        }
        mvmr.dat. = rbind(mvmr.dat., mvmr.dat)
      }
      
      if (input[['mvmr_methods']] == 'mr_all') {
        mvmr.dat = mvmr.dat.
      } else {
        if (is.null(MRMVObject)) {
          mvmr.dat = data.frame(
            exposure = NA,
            method = method,
            nsnp = NA,
            b = NA,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = NA,
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        } else {
          mvmr.dat = data.frame(
            exposure = MRMVObject@Exposure,
            method = method,
            nsnp = length(mvmr.obj()@snps),
            b = MRMVObject@Estimate,
            se = se,
            pval = pval,
            lo_ci = lo_ci,
            up_ci = up_ci,
            or = exp(MRMVObject@Estimate),
            or_lci95 = or_lci95,
            or_uci95 = or_uci95
          )
        }
      }
      
      mvmr.result(mvmr.dat)
      
      output[[paste0('mvmr_result', mr_analysis_random())]] <- DT::renderDT({
        
        if  (nrow(mvmr.result()) > 0) {
          shinyjs::show('download_mvmr_analysis')
          shinyjs::show('mvmr_sessionInfo')
        }
        
        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        mvmr.result() %>%
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
            dom = 'Brt',
            scrollX = TRUE,
            paging = FALSE,
            buttons = list(
              list(extend = 'copy', filename =  'mvmr_result', title = 'mvmr_result', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'mvmr_result', title = 'mvmr_result', exportOptions = list(modifier = list(page = 'current'))),
              list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = 'mvmr_result', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                  list(extend = 'excel', filename = 'mvmr_result', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                ),
                text = 'Download data'))))
      })
      
      output[[paste0('mvmr_result_F', mr_analysis_random())]] <- DT::renderDT({
        
        e.name.a = input[['trait_name_exposure_A']]
        e.name.b = input[['trait_name_exposure_B']]
        e.name.c = input[['trait_name_exposure_C']]
        e.name.d = input[['trait_name_exposure_D']]
        e.name.e = input[['trait_name_exposure_E']]
        FF = MVMR::strength_mvmr(mvmr.obj(), gencov = 0)
        colnames(FF) = c(e.name.a, e.name.b, e.name.c, e.name.d, e.name.e)
        
        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        as.data.frame(FF) %>%
          formattable::formattable(
            x = .) -> dt
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
              list(extend = 'copy', filename =  'mvmr_result_F', title = 'mvmr_result_F', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'mvmr_result_F', title = 'mvmr_result_F', exportOptions = list(modifier = list(page = 'current'))),
              list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = 'mvmr_result_F', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                  list(extend = 'excel', filename = 'mvmr_result_F', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                ),
                text = 'Download data'))))
      })
      
      
      output[[paste0('mvmr_result_pleiotropy', mr_analysis_random())]] <- DT::renderDT({
        
        PL = MVMR::pleiotropy_mvmr(r_input = mvmr.obj(), gencov = 0)
        presso.tab <- data.frame(
          Qstat = PL$Qstat,
          Qpval = PL$Qpval
        )
        
        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        presso.tab %>%
          formattable::formattable(
            x = .) -> dt
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
              list(extend = 'copy', filename =  'mvmr_result_pleiotropy', title = 'mvmr_result_pleiotropy', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'mvmr_result_pleiotropy', title = 'mvmr_result_pleiotropy', exportOptions = list(modifier = list(page = 'current'))),
              list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = 'mvmr_result_pleiotropy', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                  list(extend = 'excel', filename = 'mvmr_result_pleiotropy', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                ),
                text = 'Download data'))))
      })
      
      output[[paste0('forest_plot', mr_analysis_random())]] <- renderUI({
        fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'forest_plot_draw', width = '720px', height = '400px')))
      })
      
      output[['forest_plot_draw']] <- renderPlot({
        spsComps::shinyCatch({
          MRanalysisBase::forest.mv(dat = mvmr.result() %>% na.omit())
        }, trace_back = FALSE)
      })
      
      observe({
        jsCode <- "setInterval(function() { if ($('#step_mvmr_analysis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_mvmr_result_loaded', true); clearInterval(this); }}, 100);"
        runjs(jsCode)
      })
      
      observeEvent(input$table_mvmr_result_loaded, {
        shinyjs::hide('confirm_mvmr_analysis')
        shinyjs::show('download_mvmr_analysis')
        shinyjs::show('mvmr_sessionInfo')
      })
    }
  })
  
  observeEvent(input[['mvmr_methods']], {
    shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
    shinyjs::show('confirm_mvmr_analysis')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
    shinyjs::hide('tab-r-session-all')
  })
  
  observeEvent( {
    input$mr_mvivw_model
    input$mr_mvivw_robust
    input$mr_mvivw_correl
    input$mr_mvivw_distribution
    input$mr_mvivw_alpha
    input$mr_mvegger_orientate
    input$mr_mvegger_correl
    input$mr_mvegger_distribution
    input$mr_mvegger_alpha
    input$mr_mvlasso_orientate
    input$mr_mvlasso_distribution
    input$mr_mvlasso_alpha
    input$mr_mvlasso_lambda
    input$mr_mvmedian_distribution
    input$mr_mvmedian_alpha
    input$mr_mvmedian_iterations
    input$mr_mvmedian_seed
  }, {
    shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
    shinyjs::show('confirm_mvmr_analysis')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  output[['download_mvmr_analysis']] <- downloadHandler(
    filename = function() { glue::glue('io.MVMR.{format(Sys.time(), "%y%m%d%H%M%S")}.zip') },
    content = function(zipfilename) {
      spsComps::shinyCatch({
        temp_dir <- tempdir()
        
        # Forest Plot
        message(glue('{Sys.time()} add Forest Plot.'))
        width = ifelse(test = is.null(input[['forest_plot_draw_size']]$width), yes = 720 / 60, no = input[['forest_plot_draw_size']]$width / 60)
        height = ifelse(test = is.null(input[['forest_plot_draw_size']]$height), yes = 400 / 60, no = input[['forest_plot_draw_size']]$height / 60)
        forest_plot.pdf.file = file.path(temp_dir, 'forest_plot.pdf')
        ggplot2::ggsave(filename = forest_plot.pdf.file, plot = MRanalysisBase::forest.mv(dat = mvmr.result() %>% na.omit()), device = 'pdf', width = width, height = height)
        
        files = c('exposure_A.csv', 'exposure_B.csv', 'outcome.csv', 'mvmr_result.csv', 'forest_plot.pdf')
        
        exposure_A.csv.file = file.path(temp_dir, 'exposure_A.csv')
        exposure_B.csv.file = file.path(temp_dir, 'exposure_B.csv')
        exposure_C.csv.file = file.path(temp_dir, 'exposure_C.csv')
        exposure_D.csv.file = file.path(temp_dir, 'exposure_D.csv')
        exposure_E.csv.file = file.path(temp_dir, 'exposure_E.csv')
        write.csv(x = exposure.A.mr.data(), file = exposure_A.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        write.csv(x = exposure.B.mr.data(), file = exposure_B.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        if (!is.null(exposure.C.mr.data())) {
          write.csv(x = exposure.C.mr.data(), file = exposure_C.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
          files = c(files, 'exposure_C.csv')
        }
        if (!is.null(exposure.D.mr.data())) {
          write.csv(x = exposure.D.mr.data(), file = exposure_D.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
          files = c(files, 'exposure_D.csv')
        }
        if (!is.null(exposure.E.mr.data())) {
          write.csv(x = exposure.E.mr.data(), file = exposure_E.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
          files = c(files, 'exposure_E.csv')
        }

        outcome.csv.file = file.path(temp_dir, 'outcome.csv')
        write.csv(x = outcome.mr.data(), file = outcome.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        mvmr_result.csv.file = file.path(temp_dir, 'mvmr_result.csv')
        write.csv(x = mvmr.result(), file = mvmr_result.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        
        # Zip files
        zip::zip(zipfile = zipfilename, files = files, root = temp_dir)
        
        message(glue('{Sys.time()} Download all results.'))
      }, trace_back = FALSE)
    }, contentType = 'application/zip')
  
  output[['download_select_ivs_con']] <- downloadHandler(
    filename = function() { glue::glue('io.albert.MVMR.IVs.{format(Sys.time(), "%y%m%d%H%M%S")}.txt') },
    content = function(ivfile) {
      spsComps::shinyCatch({
        write.table(x = isolate(input[['confounding']]), file = ivfile, quote = FALSE, row.names = FALSE, col.names = FALSE)
        base::message(glue('{Sys.time()} Download all Instruments.'))
      }, trace_back = FALSE)
    })
  
  observeEvent({
    input[['sample_size_exposure_A']]
    input[['sample_size_exposure_B']]
    input[['sample_size_exposure_C']]
    input[['sample_size_exposure_D']]
    input[['sample_size_exposure_E']]
    input[['case_size_exposure_A']]
    input[['case_size_exposure_B']]
    input[['case_size_exposure_C']]
    input[['case_size_exposure_D']]
    input[['case_size_exposure_E']]
    input[['trait_name_exposure_A']]
    input[['trait_name_exposure_B']]
    input[['trait_name_exposure_C']]
    input[['trait_name_exposure_D']]
    input[['trait_name_exposure_E']]
    input[['trait_id_exposure_A']]
    input[['trait_id_exposure_B']]
    input[['trait_id_exposure_C']]
    input[['trait_id_exposure_D']]
    input[['trait_id_exposure_E']]
    input[['sample_size_outcome']]
    input[['case_size_outcome']]
    input[['trait_name_outcome']]
    input[['trait_id_outcome']]
  }, {
    shinyjs::hide('tab-siv-all')
    shinyjs::hide('tab-con-all')
    shinyjs::hide('tab-mrdat-all')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('tab-r-session-all')
    shinyjs::hide('step_select_vis_tbl')
    shinyjs::hide('next_select_ivs')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('next_format_data')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_format_data_tbl_box', immediate = TRUE)
    shiny::removeUI(selector = '#step_format_data_tbl2', immediate = TRUE)
    shiny::removeUI(selector = '#step_format_data_tbl2_box', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
  })
  
  observeEvent( {
    input[['p1']]
    input[['clump_r2']]
    input[['clump_kb']]
    input[['clump_p']]
    input[['pop']]
  }, {
    shinyjs::hide('tab-con-all')
    shinyjs::hide('tab-mrdat-all')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('step_select_vis_tbl')
    shinyjs::show('confirm_select_ivs')
    shinyjs::hide('confirm_select_ivs_con')
    shinyjs::hide('confirm_format_data')
    shinyjs::hide('confirm_mvmr_analysis')
    shinyjs::hide('next_select_ivs')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('next_format_data')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
  })
  
  observeEvent({
    input[['confounding']]
  }, {
    shinyjs::show('confirm_select_ivs_con')
    shinyjs::hide('tab-mrdat-all')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('confirm_format_data')
    shinyjs::hide('confirm_mvmr_analysis')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('next_format_data')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_mvmr_analysis')
    shinyjs::hide('mvmr_sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_format_data_tbl_box', immediate = TRUE)
    shiny::removeUI(selector = '#step_format_data_tbl2', immediate = TRUE)
    shiny::removeUI(selector = '#step_format_data_tbl2_box', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_f_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_mvmr_analysis_pleiotropy_tbl', immediate = TRUE)
    shiny::removeUI(selector = '#step_2mr_analysis_plot_box', immediate = TRUE)
  })
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.analysis.mr_mvmr_local, server = server.analysis.mr_mvmr_local)
