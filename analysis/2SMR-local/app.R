#' @title
#'
#' @description
#'

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

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

options(shiny.maxRequestSize = tryCatch(if (Sys.getenv("MRANALYSIS_MAX_SIZE") == '') 100000 * 1024^2 else eval(parse(text = Sys.getenv("MRANALYSIS_MAX_SIZE"))), error = function(e) 100000 * 1024^2))

# ui
ui.analysis.mr_two_sample_local <- function() {

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
            column(12, h5("Step 6. Collect Information About the Current R Session."))
          ),
          tags$div(
            class = 'btn-albert',
            shiny::actionButton(inputId = 'show_codes', label = 'Show codes', class = 'btn-danger btn-floatsession')
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
            html. <- system.file('extdata', 'HTML/intro.analysis.2SMR-local.html', package = 'MRanalysisBase')
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
              id = 'step_upload_exposure_data',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 1. Choose exposure data.</strong>"),
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
              )
            ),
            tags$div(
              class = 'btn-albert',
              shiny::actionButton(inputId = 'next_upload_data', label = 'Next', class = 'btn-danger btn-float')
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
            ),
            bs4Dash::box(
              id = 'step_upload_outcome_data',
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 2. Choose outcome data.</strong>"),
              status = 'warning',
              width = 6,
              shiny::fileInput(
                inputId = 'outcome',
                accept = c('.vcf.gz'),
                label = 'Upload a file for outcome.',
                multiple = FALSE
              ),
              verbatimTextOutput('outcome_file'),
              column(12,
                     div(style = "display: flex; justify-content: flex-start; margin-top: 15px;",
                         shiny::downloadButton(outputId = 'download_demo_data', label = 'Download demo data', class = 'btn-success')
                     )
              )
            ),
            bs4Dash::box(
              id = 'step_upload_outcome_data_information',
              title = HTML("<strong>&nbsp;Information for outcome</strong>"),
              status = 'warning',
              width = 6,
              collapsed = FALSE,
              collapsible = TRUE,
              fluidRow(
                column(8, tags$div(title = 'Trait Name', shiny::textInput(inputId = 'trait_name_outcome', label = 'Trait Name', value = NULL))),
                column(4, tags$div(title = 'Trait ID', shiny::textInput(inputId = 'trait_id_outcome', label = 'Trait ID', value = NULL)))
              )
            ),
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
              title = HTML("<strong><i class='fa-solid fa-play'></i>&nbsp;&nbsp;&nbsp;Step 3.3. Calculate R<sup>2</sup> and F value.</strong>"),
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
            bs4Dash::box(
              id = 'step_select_ivs2',
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

        # ++++++++++++++++++++++++ tabName = 'tab-2mr' ++++++++++++++++++++++++
        bs4TabItem(
          tabName = 'tab-2mr',
          fluidRow(
            id = 'tab-2mr-all',
            column(12, h5("Step 5. Perform two-sample Mendelian randomization.")),
            bs4Dash::box(
              id = 'step_2mr_analysis',
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
              shiny::actionButton(inputId = '2mr_sessionInfo', label = 'Session Info', class = 'btn-danger btn-floatsession')
            )
          )
        ) # end bs4TabItem
      )
    ),
    controlbar = NULL,
    scrollToTop = TRUE,
    title = '2SMR'
  )
}


server.analysis.mr_two_sample_local <- function(input, output, session) {

  # Download Demo data
  output[['download_demo_data']] <- downloadHandler(
    filename <- function(){ paste0('demo-', as.numeric(Sys.time()), '.zip') },
    content <- function(file){
      file.copy(glue("demo/demo.zip"), file)
    }, contentType = NULL)

  shinyjs::hide('tab-siv-tip')
  shinyjs::hide('tab-siv-all')
  shinyjs::hide('tab-con-all')
  shinyjs::hide('tab-2mr-tip')
  shinyjs::hide('tab-2mr-all')
  shinyjs::hide('tab-r-session-all')
  shinyjs::hide('next_select_ivs')
  shinyjs::hide('next_select_ivs_con')
  shinyjs::hide('download_select_ivs_con')
  shinyjs::hide('download_2mr_analysis')
  shinyjs::hide('2mr_sessionInfo')
  shinyjs::hide('show_codes')

  tempdir. <- reactiveVal(tempdir())
  exposure.file <- reactiveVal(NULL)
  exposure.filepath <- reactiveVal(NULL)
  exposure.vi.filepath <- reactiveVal(NULL)
  outcome.file <- reactiveVal(NULL)
  outcome.filepath <- reactiveVal(NULL)
  exposure_data <- reactiveVal(TRUE)
  exposure_data_clump <- reactiveVal(TRUE)
  exposure_data_r2f <- reactiveVal(TRUE)
  hat_data <- reactiveVal(TRUE)
  mr_odds_res <- reactiveVal(TRUE)
  mr_odds_res_het <- reactiveVal(TRUE)
  mr_odds_res_pleio <- reactiveVal(TRUE)
  mr_odds_res_presso <- reactiveVal(NULL)
  all_ivs <- reactiveVal(TRUE)
  con.vis.status <- reactiveVal(TRUE)
  exposure_data_random <- reactiveVal(as.numeric(Sys.time()))
  mr_analysis_random <- reactiveVal(as.numeric(Sys.time()))

  db.sample.size.e <- reactive({ isolate(input[['sample_size_exposure']]) })
  db.case.size.e <- reactive({ isolate(input[['case_size_exposure']]) })

  observeEvent(input[['reset']], {
    shinyjs::refresh()
  })

  iv <- InputValidator$new()
  iv$add_rule('clump_r2', sv_gte(0))
  iv$add_rule('clump_kb', sv_gte(0))
  iv$add_rule('clump_p', sv_between(0, 1))
  iv$add_rule('sample_size_exposure', sv_gt(0))
  iv$add_rule('f', sv_gte(0))
  iv$enable()

  output[['exposure_file']] = renderPrint({ exposure.file() })
  output[['outcome_file']] = renderPrint({ outcome.file() })

  observeEvent(input[['use_demo_data']], {
    exposure.file('ieu-a-2.vcf.gz')
    exposure.filepath('demo/ieu-a-2.vcf.gz')
    shiny::updateTextInput(session = session, inputId = 'sample_size_exposure', label = 'Sample size', value = 339224)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure', label = 'Trait name', value = 'Body mass index')
    shiny::updateTextInput(session = session, inputId = 'trait_id_exposure', label = 'Trait ID', value = 'ieu-a-2')
    outcome.file('ieu-a-7.vcf.gz')
    outcome.filepath('demo/ieu-a-7.vcf.gz')
    shiny::updateTextInput(session = session, inputId = 'trait_name_outcome', label = 'Trait name', value = 'Coronary heart disease')
    shiny::updateTextInput(session = session, inputId = 'trait_id_outcome', label = 'Trait ID', value = 'ieu-a-7')
    shinyjs::hide('exposure')
    shinyjs::hide('outcome')
    db.sample.size.e.c <- tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    if (!is.null(input[['exposure_file']]) || !is.null(exposure.file()) && !is.null(input[['outcome_file']]) || !is.null(outcome.file()) && !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0) {
      shinyjs::show('next_upload_data')
    }
  })

  observeEvent(input[['exposure']], {
    shinyjs::hide('next_upload_data')
    exposure.file(input[['exposure']]$name)
    exposure.filepath(input[['exposure']]$datapath)
    shiny::updateTextInput(session = session, inputId = 'trait_name_exposure', label = 'Trait name', value = 'exposure')
    shiny::updateTextInput(session = session, inputId = 'trait_id_exposure', label = 'Trait ID', value = strsplit(input[['exposure']]$name, '\\.')[[1]][1])
    shinyjs::hide('use_demo_data')
    db.sample.size.e.c <- tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    if ((!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0) {
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
    db.sample.size.e.c <- tryCatch(as.numeric(db.sample.size.e()), error = function(e) NA)
    if ((!is.null(input[['exposure_file']]) || !is.null(exposure.file())) && !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0) {
      shinyjs::show('next_upload_data')
    }
  })

  observeEvent(input[['sample_size_exposure']], {
    shinyjs::hide('next_upload_data')
    db.sample.size.e.c <- tryCatch(as.numeric(input[['sample_size_exposure']]), error = function(e) NA)
    if ((!is.null(input[['outcome_file']]) || !is.null(outcome.file())) && (!is.null(input[['exposure_file']]) || !is.null(exposure.file())) && !is.na(db.sample.size.e.c) && db.sample.size.e.c > 0) {
      shinyjs::show('next_upload_data')
    }
  })

  observeEvent(input[['next_upload_data']], {

    allids = c(exposure.file(), outcome.file())
    if (base::anyDuplicated(allids) > 0) {
      spsComps::shinyCatch({ base::stop('Duplicate samples have been used, please check!') }, trace_back = FALSE)
    } else {
      if(!is.null(exposure.filepath()) && !is.null(outcome.filepath())){
        updatebs4TabItems(
          inputId = 'sidebarmenu',
          session = session,
          selected = 'tab-siv'
        )
        shinyjs::show('tab-siv-tip')
        shinyjs::show('tab-siv-all')
        shinyjs::show('confirm_select_ivs')
        shinyjs::hide('next_select_ivs')
      } else {
        if (is.null(exposure.filepath())) {
          spsComps::shinyCatch({ base::stop('Exposure Data Not Selected!') }, trace_back = FALSE)
        }
        if (is.null(outcome.filepath())) {
          spsComps::shinyCatch({ base::stop('Outcome Data Not Selected!') }, trace_back = FALSE)
        }
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
    shinyjs::show('tab-2mr-tip')
    shinyjs::show('tab-2mr-all')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('2mr_sessionInfo')
    shinyjs::hide('show_codes')
    shinyjs::show('confirm_2mr_analysis')
  })

  observeEvent(input[['2mr_sessionInfo']], {
    updatebs4TabItems(
      inputId = 'sidebarmenu',
      session = session,
      selected = 'tab-r-session'
    )
    shinyjs::show('tab-r-session-all')
  })

  output[['download_2mr_analysis']] <- downloadHandler(
    filename = function() { glue::glue('io.2sMR.{format(Sys.time(), "%y%m%d%H%M%S")}.zip') },
    content = function(zipfilename) {
      spsComps::shinyCatch({
        temp_dir <- tempdir()

        # Forest Plot
        message(glue('{Sys.time()} add Forest Plot.'))
        width = ifelse(test = is.null(input[['forest_plot_draw_size']]$width), yes = 720 / 60, no = input[['forest_plot_draw_size']]$width / 60)
        height = ifelse(test = is.null(input[['forest_plot_draw_size']]$height), yes = 400 / 60, no = input[['forest_plot_draw_size']]$height / 60)
        forest_plot.pdf.file = file.path(temp_dir, 'forest_plot.pdf')
        pdf(file = forest_plot.pdf.file, width = width, height = height)
        MRanalysisBase::forest.x(dat = mr_odds_res() %>% na.omit(), shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(4, 1), vline = 1, xrang = NULL, text.layout = c(0, 6.5, 6.4, 9, 10))
        dev.off()

        # Scatter Plot
        message(glue('{Sys.time()} add Scatter Plot.'))
        width = ifelse(test = is.null(input[['scatter_plot_draw_size']]$width), yes = 720 / 100, no = input[['scatter_plot_draw_size']]$width / 100)
        height = ifelse(test = is.null(input[['scatter_plot_draw_size']]$height), yes = 720 / 100, no = input[['scatter_plot_draw_size']]$height / 100)
        scatter_plot.pdf.file = file.path(temp_dir, 'scatter_plot.pdf')
        gg = TwoSampleMR::mr_scatter_plot(mr_odds_res() %>% na.omit() %>% dplyr::filter(!method %in% c('Unweighted regression', 'Simple mode (NOME)', 'Weighted mode (NOME)')), hat_data())
        gg = gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text =  element_text(size = 14)
        )
        ggplot2::ggsave(filename = scatter_plot.pdf.file, plot = gg, device = 'pdf', width = width, height = height)

        # Single SNP Plot
        message(glue('{Sys.time()} add Single SNP Plot.'))
        width = ifelse(test = is.null(input[['single_snp_plot_draw_size']]$width), yes = 1000 / 100, no = input[['single_snp_plot_draw_size']]$width / 100)
        height = ifelse(test = is.null(input[['single_snp_plot_draw_size']]$height), yes = 1600 / 100, no = input[['single_snp_plot_draw_size']]$height / 100)
        single_snp_plot.pdf.file = file.path(temp_dir, 'single_snp_plot.pdf')
        # gg = TwoSampleMR::mr_forest_plot(TwoSampleMR::mr_singlesnp(hat_data(), all_method = isolate(input[['2mr-methods']])[isolate(input[['2mr-methods']]) != c('mr_wald_ratio', 'mr_raps')]))
        gg = TwoSampleMR::mr_forest_plot(TwoSampleMR::mr_singlesnp(hat_data()))
        gg = gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text.x =  element_text(size = 14),
          axis.text.y =  element_text(size = 14)
        )
        ggplot2::ggsave(filename = single_snp_plot.pdf.file, plot = gg, device = 'pdf', width = width, height = height)

        # Leave-one-out Plot
        message(glue('{Sys.time()} add Leave-one-out Plot.'))
        width = ifelse(test = is.null(input[['leave_one_out_plot_draw_size']]$width), yes = 1000 / 100, no = input[['leave_one_out_plot_draw_size']]$width / 100)
        height = ifelse(test = is.null(input[['leave_one_out_plot_draw_size']]$height), yes = 1600 / 100, no = input[['leave_one_out_plot_draw_size']]$height / 100)
        leave_one_out_plot.pdf.file = file.path(temp_dir, 'leave_one_out_plot.pdf')
        gg = TwoSampleMR::mr_leaveoneout_plot(leaveoneout_results = TwoSampleMR::mr_leaveoneout(hat_data()))
        gg =             gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.text.x =  element_text(size = 16),
          axis.text.y =  element_text(size = 16)
        )
        ggplot2::ggsave(filename = leave_one_out_plot.pdf.file, plot = gg, device = 'pdf', width = width, height = height)

        # Funnel Plot
        message(glue('{Sys.time()} add Funnel Plot.'))
        width = ifelse(test = is.null(input[['funnel_draw_size']]$width), yes = 720 / 100, no = input[['funnel_draw_size']]$width / 100)
        height = ifelse(test = is.null(input[['funnel_draw_size']]$height), yes = 720 / 100, no = input[['funnel_draw_size']]$height / 100)
        funnel_plot.pdf.file = file.path(temp_dir, 'funnel_plot.pdf')
        gg = TwoSampleMR::mr_funnel_plot(singlesnp_results = TwoSampleMR::mr_singlesnp(hat_data()))
        gg = gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x =  element_text(size = 16),
          axis.text.y =  element_text(size = 16)
        )
        ggplot2::ggsave(filename = funnel_plot.pdf.file, plot = gg, device = 'pdf', width = width, height = height)

        # Instruments for use in MR from the exposure data
        instruments.csv.file = file.path(temp_dir, 'instruments.csv')
        instruments.csv.F.file = file.path(temp_dir, 'instruments.F.csv')
        f.data = exposure_data_r2f(); v.data = exposure_data_r2f()
        if (input[['caiculate_f_statitics_mode']]) {
          f.data = f.data[f.data$F > isolate(input[['f']]), ]
        }
        f.data$id.exposure = ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']]))
        f.data$exposure = ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']]))
        v.data$F = NULL
        v.data$R2 = NULL
        write.csv(x = v.data, file = instruments.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')
        write.csv(x = f.data, file = instruments.csv.F.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')

        # Causal Effect Estimates
        mr_results.csv.file = file.path(temp_dir, 'mr_results.csv')
        write.csv(x = mr_odds_res(), file = mr_results.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')

        # Heterogeneity Tests
        mr_heterogeneity.csv.file = file.path(temp_dir, 'mr_heterogeneity.csv')
        write.csv(x = mr_odds_res_het(), file = mr_heterogeneity.csv.file, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')

        # Horizontal Pleiotropy
        mr_pleiotropy.csv.file1 = file.path(temp_dir, 'mr_pleiotropy_megger.csv')
        write.csv(x = mr_odds_res_pleio(), file = mr_pleiotropy.csv.file1, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')

        mr_pleiotropy.csv.file2 = file.path(temp_dir, 'mr_pleiotropy_presso.csv')
        write.csv(x = mr_odds_res_presso(), file = mr_pleiotropy.csv.file2, quote = FALSE, row.names = FALSE, fileEncoding = 'UTF-8')

        # Zip files
        zip::zip(zipfile = zipfilename, files = c('forest_plot.pdf', 'scatter_plot.pdf', 'single_snp_plot.pdf', 'leave_one_out_plot.pdf', 'funnel_plot.pdf', 'instruments.csv', 'instruments.F.csv', 'mr_results.csv', 'mr_heterogeneity.csv', 'mr_pleiotropy_megger.csv', 'mr_pleiotropy_presso.csv'), root = temp_dir)

        message(glue('{Sys.time()} Download all results.'))
      }, trace_back = FALSE)
    }, contentType = 'application/zip')

  output[['download_select_ivs_con']] <- downloadHandler(
    filename = function() { glue::glue('io.albert.2sMR.IVs.{format(Sys.time(), "%y%m%d%H%M%S")}.txt') },
    content = function(ivfile) {
      spsComps::shinyCatch({
        write.table(x = isolate(input[['confounding']]), file = ivfile, quote = FALSE, row.names = FALSE, col.names = FALSE)
        base::message(glue('{Sys.time()} Download all Instruments.'))
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

  vcf.data.o <- reactive({
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
        withSpinner({ DT::dataTableOutput(outputId = paste0('exposure_data', exposure_data_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Loading instruments from uploaded data, please be patient...'))
    )

    output[[paste0('exposure_data', exposure_data_random())]] <- DT::renderDataTable({
      spsComps::shinyCatch({
        e.data = subset(vcf.data.e(), pval.exposure < shiny::isolate(input[['p1']]))
        exposure_data(e.data)
      })

      if (!is.null(exposure_data())) {
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
      }

      if (!is.null(exposure_data())) {
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
          f.data = base::transform(m.data, R2 = (2 * (beta.exposure^2) * eaf.exposure * (1 - eaf.exposure)) / (2 * beta.exposure^2 * eaf.exposure * (1 - eaf.exposure) + 2 * se.exposure^2 * N * eaf.exposure * (1 - eaf.exposure)))
          f.data = base::transform(f.data, F = R2 * (N - 2) / (1 - R2))
          exposure_data_r2f(f.data)
        })
      }

      if (!is.null(exposure_data())) {
        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        f.data = exposure_data_r2f()
        if (nrow(f.data) > 0) { con.vis.status('A-LB-E-RT') }
        if (!is.null(con.vis.status())) {
          shinyjs::show('next_select_ivs')
          shinyjs::show('tab-siv-all')
        }
        if (input[['caiculate_f_statitics_mode']]) {
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
      }
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

  observeEvent(input[['confirm_select_ivs_con']], {
    e.file.tmp = exposure.filepath()
    e.file.vi = glue('{dirname(e.file.tmp)}/LP-{basename(e.file.tmp)}.vi')
    exposure.vi.filepath(e.file.vi)
    spsComps::shinyCatch({
      e.vi = isolate(input[['confounding']])
      all_ivs(e.vi)
      write.table(x = e.vi, file = e.file.vi, quote = FALSE, sep = '\t', col.names = FALSE, row.names = FALSE)
    })
    shinyjs::hide('confirm_select_ivs_con')
    shinyjs::show('next_select_ivs_con')
    shinyjs::show('download_select_ivs_con')
  })

  observeEvent(input[['confirm_2mr_analysis']], {

    shinyjs::hide('confirm_2mr_analysis')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('2mr_sessionInfo')
    shinyjs::hide('show_codes')
    shiny::removeUI(selector = '#step_2mr_analysis_tbl', immediate = TRUE)
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
        withSpinner({ DT::dataTableOutput(outputId = paste0('mr_data', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform Causal Effect Estimate, please wait...'))
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
        title = HTML('Horizontal Pleiotropy (MR-Egger regression intercept)'),
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
      ui =  bs4Dash::box(
        id = 'step_2mr_analysis_presso_tbl',
        title = HTML('Horizontal Pleiotropy (MR-PRESSO)'),
        status = 'success',
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        background = NULL,
        withSpinner({ DT::dataTableOutput(outputId = paste0('mr_data_presso', mr_analysis_random()))  }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Test for horizontal pleiotropy in MR analysis. This may take a few minutes, please wait...'))
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
          title = 'Scatter Plot',
          withSpinner({ uiOutput(outputId = paste0('scatter_plot', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Create scatter plot with lines showing the causal estimate for different MR tests, please wait...')
        ),
        tabPanel(
          title = 'Single SNP Plot',
          withSpinner({ uiOutput(outputId = paste0('single_snp_plot', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Draw Forest plot of each SNP, please wait...')
        ),
        tabPanel(
          title = 'Funnel Plot',
          withSpinner({ uiOutput(outputId = paste0('funnel_plot', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform leave one out sensitivity analysis, please wait...')
        ),
        tabPanel(
          title = 'Leave-one-out Plot',
          withSpinner({ uiOutput(outputId = paste0('leave_one_out_plot', mr_analysis_random())) }, type = 8, color = '#EE3F4D', size = 1, color.background = '#2F2F35', caption = 'Perform two sample MR on each SNP individually, please wait...')
        )
      )
    )

    output[[paste0('mr_data', mr_analysis_random())]] <- DT::renderDataTable({

      shinyjs::hide('step_2mr_analysis')
      shinyjs::hide('step_2mr_analysis_het_tbl')
      shinyjs::hide('step_2mr_analysis_pleio_tbl')
      shinyjs::hide('step_2mr_analysis_presso_tbl')
      shinyjs::hide('step_2mr_analysis_plot_box')
      shinyjs::hide('confirm_2mr_analysis')
      shinyjs::hide('download_2mr_analysis')
      shinyjs::hide('2mr_sessionInfo')
      shinyjs::hide('show_codes')

      spsComps::shinyCatch({
        f.data = exposure_data_r2f()
        f.data = f.data %>% dplyr::filter(SNP %in% all_ivs())
        o.data = subset(vcf.data.o(), pval.outcome > shiny::isolate(input[['p1']]))
        o.data = merge(o.data, f.data, by.x = 'SNP', by.y = 'SNP') %>% dplyr::filter(SNP %in% all_ivs())

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
        hat_data(h.dat)
      })

      observeEvent(input$step_2mr_analysis_tbl, {
        shinyjs::hide('confirm_2mr_analysis')
        shinyjs::show('step_2mr_analysis')
        shinyjs::show('step_2mr_analysis_het_tbl')
        shinyjs::show('step_2mr_analysis_pleio_tbl')
        shinyjs::show('step_2mr_analysis_presso_tbl')
        shinyjs::show('step_2mr_analysis_plot_box')
        shinyjs::show('download_2mr_analysis')
        shinyjs::show('2mr_sessionInfo')
      })

      spsComps::shinyCatch({

        res.mr = data.frame()
        for (method in isolate(input[['2mr-methods']])) {
          res.mr.tmp = try({TwoSampleMR::mr(
            hat_data(),
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
        mr_odds_res(mr.odds)

        heter.tab <- TwoSampleMR::mr_heterogeneity(h.dat); mr_odds_res_het(heter.tab)
        pleio.tab <- TwoSampleMR::mr_pleiotropy_test(h.dat); mr_odds_res_pleio(pleio.tab)
      })

      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      mr_odds_res() %>%
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
                list(extend = 'csv', filename = 'mr_result', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))

    })

    output[[paste0('mr_data_het', mr_analysis_random())]] <- DT::renderDataTable({
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      mr_odds_res_het() %>%
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
                list(extend = 'csv', filename = 'mr_result_het', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result_het', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))
    })

    output[[paste0('mr_data_pleio', mr_analysis_random())]] <- DT::renderDataTable({
      unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
      mr_odds_res_pleio() %>%
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
                list(extend = 'csv', filename = 'mr_result_pleio', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                list(extend = 'excel', filename = 'mr_result_pleio', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
              ),
              text = 'Download data'))))
    })

    output[[paste0('mr_data_presso', mr_analysis_random())]] <- DT::renderDataTable({
      if (!is.null(hat_data())) {
        presso <- TwoSampleMR::run_mr_presso(hat_data())
        presso.tab <- data.frame(
          id.exposure =	rep(ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']])), 2),
          id.outcome = rep(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])), 2),
          outcome	= rep(ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']])), 2),
          exposure = rep(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])), 2),
          RSSobs = c(presso[[1]]$`MR-PRESSO results`$`Global Test`$RSSobs, '-'),
          'Distortion Coefficient' = c('-', presso[[1]]$`MR-PRESSO results`$`Distortion Test`$`Distortion Coefficient`),
          Pvalue = c(presso[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue, presso[[1]]$`MR-PRESSO results`$`Distortion Test`$Pvalue)
        )
        mr_odds_res_presso(presso.tab)

        unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
        mr_odds_res_presso() %>%
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
              list(extend = 'copy', filename =  'mr_result_presso', title = 'mr_result_presso', exportOptions = list(modifier = list(page = 'current'))),
              list(extend = 'print', filename =  'mr_result_presso', title = 'mr_result_presso', exportOptions = list(modifier = list(page = 'current'))),
              list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = 'mr_result_presso', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current'))),
                  list(extend = 'excel', filename = 'mr_result_presso', title = NULL, exportOptions = list(columns = ':visible', modifier = list(page = 'current')))
                ),
                text = 'Download data'))))
      }
    })

    observe({
      jsCode <- "setInterval(function() { if ($('#step_2mr_analysis_tbl').find('tbody tr').length > 0) { Shiny.onInputChange('table_2mr_analysis_loaded', true); clearInterval(this); }}, 100);"
      runjs(jsCode)
    })

    output[[paste0('forest_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'forest_plot_draw', width = '800px', height = '400px')))
    })

    output[[paste0('scatter_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'scatter_plot_draw', width = '720px', height = '720px')))
    })

    output[[paste0('single_snp_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'single_snp_plot_draw', width = '1000px', height = '1600px')))
    })

    output[[paste0('leave_one_out_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'leave_one_out_plot_draw', width = '1000px', height = '1600px')))
    })


    output[[paste0('funnel_plot', mr_analysis_random())]] <- renderUI({
      fluidRow(shinyjqui::jqui_resizable(plotOutput(outputId = 'funnel_plot_draw', width = '720px', height = '720px')))
    })


    output[['forest_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({
        MRanalysisBase::forest.x(dat = mr_odds_res() %>% na.omit(), shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(4, 1), vline = 1, xrang = NULL, text.layout = c(0, 6.5, 6.4, 9, 10))
      }, trace_back = FALSE)
    })

    output[['scatter_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({
        gg = TwoSampleMR::mr_scatter_plot(mr_odds_res() %>% na.omit() %>% dplyr::filter(!method %in% c('Unweighted regression', 'Simple mode (NOME)', 'Weighted mode (NOME)')), hat_data())
        gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text =  element_text(size = 16)
        )
      }, trace_back = FALSE)
    })

    output[['single_snp_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({
        # gg = TwoSampleMR::mr_forest_plot(TwoSampleMR::mr_singlesnp(hat_data(), all_method = isolate(input[['2mr-methods']])[isolate(input[['2mr-methods']]) != c('mr_wald_ratio', 'mr_raps')]))
        gg = TwoSampleMR::mr_forest_plot(TwoSampleMR::mr_singlesnp(hat_data()))
        gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.text.x =  element_text(size = 16),
          axis.text.y =  element_text(size = 16)
        )
      }, trace_back = FALSE)
    })

    output[['leave_one_out_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({
        gg <- TwoSampleMR::mr_leaveoneout_plot(leaveoneout_results = TwoSampleMR::mr_leaveoneout(hat_data()))
        gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.text.x =  element_text(size = 16),
          axis.text.y =  element_text(size = 16)
        )
      }, trace_back = FALSE)
    })

    output[['funnel_plot_draw']] <- renderPlot({
      spsComps::shinyCatch({
        gg <- TwoSampleMR::mr_funnel_plot(singlesnp_results = TwoSampleMR::mr_singlesnp(hat_data()))
        gg[[1]] + theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x =  element_text(size = 16),
          axis.text.y =  element_text(size = 16)
        )
      }, trace_back = FALSE)
    })

    observeEvent(input$table_2mr_analysis_loaded, {
      shinyjs::show('download_2mr_analysis')
    })
  })

  output[['confounding-ui']] <- renderUI({
    f.data = exposure_data_r2f()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = 'confounding',
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

  output[['2mr-ui']] <- renderUI({
    f.data = exposure_data_r2f()
    if (!is.null(f.data)) {
      shinyWidgets::virtualSelectInput(
        inputId = '2mr-methods',
        label = 'Select MR methods',
        choices = setNames(TwoSampleMR::mr_method_list()$obj, paste0(TwoSampleMR::mr_method_list()$name, ' (', TwoSampleMR::mr_method_list()$obj, ')')),
        selected = subset(TwoSampleMR::mr_method_list(), use_by_default)$obj,
        optionsCount = 7,
        noOfDisplayValues = 16,
        multiple = TRUE,
        inline = TRUE,
        showValueAsTags = TRUE,
        search = TRUE
      )
    }
  })

  observeEvent(input[['2mr_sessionInfo']], {

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
      # xfun::session_info(c('dplyr', 'ggplot2', 'DT', 'gwasglue', 'ieugwasr', 'TwoSampleMR', 'VariantAnnotation'))
      utils::sessionInfo()
    })

    shinyjs::show('show_codes')
  })

  #+++++++++++++++++++++++++++++++  Show Code Start ++++++++++++++++++++++++++++
  output[['workflow_code']] <- metaRender(renderText, {

    '# Load exposure data'
    vcf.data.e <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(exposure.filepath())), type = 'exposure')
    '# Filter exposure data'
    e.data <- subset(vcf.data.e, pval.exposure < ..(unname(shiny::isolate(input[['p1']]))))
    '# LD clump.'
    '# See details in https://mrcieu.github.io/ieugwasr/articles/local_ld.html'
    c.data <- ieugwasr::ld_clump(
      clump_kb = ..(isolate({ as.numeric(input[['clump_kb']]) })),
      clump_r2 = ..(isolate({ as.numeric(input[['clump_r2']]) })),
      clump_p = ..(isolate({ as.numeric(input[['clump_p']]) })),
      pop = ..(isolate({ input[['pop']] })),
      dplyr::tibble(rsid = e.data$SNP, pval = e.data$pval.exposure, id = e.data$id.exposure),
      plink_bin = 'your/path/to/plink',
      bfile = 'your/path/to/1kg.v3/'
    )
    m.data <- base::merge(e.data, c.data, by.x = 'SNP', by.y = 'rsid')
    '# Caucualte F statistics and R2'
    N <- ..(ifelse(test = is.na(as.numeric(db.sample.size.e())), yes = 'please enter the sample size of exposure', no = as.numeric(db.sample.size.e())))
    f.data <- base::transform(m.data, R2 = (2 * (beta.exposure^2) * eaf.exposure * (1 - eaf.exposure)) / (2 * beta.exposure^2 * eaf.exposure * (1 - eaf.exposure) + 2 * se.exposure^2 * N * eaf.exposure * (1 - eaf.exposure)))
    f.data <- base::transform(f.data, F = R2 * (N - 2) / (1 - R2))
    f.data <- f.data[f.data$F > ..(isolate(input[['f']])), ]
    '# remove confounders'
    confounders <- ..(setdiff(exposure_data_r2f()$SNP, all_ivs()))
    '# Load outcome data'
    vcf.data.o <- gwasglue::gwasvcf_to_TwoSampleMR(vcf = readVcf(..(outcome.filepath())), type = 'outcome')
    '# Filter'
    all_ivs <- setdiff(f.data$SNP, confounders)
    f.data <- f.data %>% dplyr::filter(SNP %in% all_ivs)
    o.data <- subset(vcf.data.o, pval.outcome > ..(unname(shiny::isolate(input[['p1']]))))
    o.data <- merge(o.data, f.data, by.x = 'SNP', by.y = 'SNP') %>% dplyr::filter(SNP %in% all_ivs)
    f.data$Phenotype = ..(ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']])))
    o.data$Phenotype = ..(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])))
    '# Format data'
    e.dat <- TwoSampleMR::format_data(
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
    '# Harmonise the alleles and effects between the exposure and outcome'
    h.dat <- TwoSampleMR::harmonise_data(exposure_dat = e.dat, outcome_dat = o.dat)
    h.dat$id.exposure <- ..(ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']])))
    h.dat$id.outcome <- ..(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])))
    '# Perform MR analysis'
    res.mr <- data.frame()
    for (method in ..(isolate(input[['2mr-methods']]))) {
      res.mr.tmp = try({TwoSampleMR::mr(
        h.dat,
        method_list = method
      )}, silent = TRUE)
      if (!'try-error' %in% class(res.mr.tmp)) {
        res.mr = rbind(res.mr, res.mr.tmp)
      } else {
        base::warning(glue('The "{method}" method is not suitable; an error occurred, skipping this method.'))
      }
    }
    '# Generate odds ratios'
    mr.odds <- TwoSampleMR::generate_odds_ratios(res.mr)
    mr.odds <- mr.odds %>% dplyr::select(id.exposure, id.outcome, exposure, outcome, everything())
    '# Get heterogeneity statistics'
    heter.tab <- TwoSampleMR::mr_heterogeneity(h.dat)
    '# Test for horizontal pleiotropy in MR analysis'
    pleio.tab <- TwoSampleMR::mr_pleiotropy_test(h.dat)
    presso <- TwoSampleMR::run_mr_presso(h.dat)
    presso.tab <- data.frame(
      id.exposure =	..(rep(ifelse(test = is.null(isolate(input[['trait_id_exposure']])) || isolate(input[['trait_id_exposure']]) == '', yes = 'id.exposure', no = isolate(input[['trait_id_exposure']])), 2)),
      id.outcome = ..(rep(ifelse(test = is.null(isolate(input[['trait_id_outcome']])) || isolate(input[['trait_id_outcome']]) == '', yes = 'id.outcome', no = isolate(input[['trait_id_outcome']])), 2)),
      outcome	= ..(rep(ifelse(test = is.null(isolate(input[['trait_name_exposure']])) || isolate(input[['trait_name_exposure']]) == '', yes = 'name.exposure', no = isolate(input[['trait_name_exposure']])), 2)),
      exposure = ..(rep(ifelse(test = is.null(isolate(input[['trait_name_outcome']])) || isolate(input[['trait_name_outcome']]) == '', yes = 'name.outcome', no = isolate(input[['trait_name_outcome']])), 2)),
      RSSobs = c(presso[[1]]$`MR-PRESSO results`$`Global Test`$RSSobs, '-'),
      'Distortion Coefficient' = c('-', presso[[1]]$`MR-PRESSO results`$`Distortion Test`$`Distortion Coefficient`),
      Pvalue = c(presso[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue, presso[[1]]$`MR-PRESSO results`$`Distortion Test`$Pvalue)
    )
    '# plot'
    TwoSampleMR::mr_funnel_plot(singlesnp_results = TwoSampleMR::mr_singlesnp(h.dat))
    TwoSampleMR::mr_leaveoneout_plot(leaveoneout_results = TwoSampleMR::mr_leaveoneout(h.dat))
    TwoSampleMR::mr_forest_plot(TwoSampleMR::mr_singlesnp(h.dat))
    TwoSampleMR::mr_scatter_plot(mr.odds %>% na.omit() %>% dplyr::filter(!method %in% c('Unweighted regression', 'Simple mode (NOME)', 'Weighted mode (NOME)')), h.dat)
    MRanalysisBase::forest.x(dat = mr.odds %>% na.omit(), shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(4, 1), vline = 1, xrang = NULL, text.layout = c(0, 6.5, 6.4, 9, 10))
  })


  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(dplyr)
        library(ieugwasr)
        library(gwasglue)
        library(TwoSampleMR)
        library(VariantAnnotation)
        library(MRanalysisBase)
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
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('step_select_vis_tbl')
    shinyjs::show('confirm_select_ivs')
    shinyjs::hide('confirm_select_ivs_con')
    shinyjs::hide('confirm_2mr_analysis')
    shinyjs::hide('next_select_ivs')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('2mr_sessionInfo')
    shinyjs::hide('show_codes')
  })

  observeEvent({
    input[['sample_size_exposure']]
    input[['case_size_exposure']]
    input[['trait_name_exposure']]
    input[['trait_id_exposure']]
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
    shinyjs::hide('2mr_sessionInfo')
    shinyjs::hide('show_codes')
  })

  observeEvent({
    input[['confounding']]
  }, {
    shinyjs::show('confirm_select_ivs_con')
    shinyjs::hide('tab-2mr-all')
    shinyjs::hide('next_select_ivs_con')
    shinyjs::hide('download_select_ivs_con')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('2mr_sessionInfo')
    shinyjs::hide('show_codes')
  })

  observeEvent({
    input[['2mr-methods']]
  }, {
    shinyjs::show('confirm_2mr_analysis')
    shinyjs::hide('step_2mr_analysis_tbl')
    shinyjs::hide('step_2mr_analysis_het_tbl')
    shinyjs::hide('step_2mr_analysis_pleio_tbl')
    shinyjs::hide('step_2mr_analysis_presso_tbl')
    shinyjs::hide('step_2mr_analysis_plot_box')
    shinyjs::hide('download_2mr_analysis')
    shinyjs::hide('2mr_sessionInfo')
    shinyjs::hide('show_codes')
    shinyjs::hide('tab-r-session-all')
  })
}

# Use for standalone testing,
# comment out the following line when used as a component.
# shinyApp(ui = ui.analysis.mr_two_sample_local, server = server.analysis.mr_two_sample_local, options = list(host = '10.0.12.2', port = 3390))
shinyApp(ui = ui.analysis.mr_two_sample_local, server = server.analysis.mr_two_sample_local)
