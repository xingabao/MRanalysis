#' @title
#'
#' @description
#' 
# Load Global
source('global.R')
source('up.R')
source('left.R')
source('src/i18n.R')

# load R packages
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(forcats)))
suppressMessages(suppressWarnings(library(ggdag)))

options(shiny.maxRequestSize = 1 * 1024 ^ 2)

ui.gallery <- function() {
  
  www = 'www/data'
  dag_demo <<- data.table::fread(glue('{www}/demo.csv'))
  
  fluidPage(
    useShinyjs(),
    web.statistic.baidu,
    web.statistic.google,
    tags$head(
      includeCSS("www/css/shiny-style.css"),
      HTML("<html lang='en'>"),
      tags$title('DAG Plot'),
      tags$link(rel = "shortcut icon", href = "img/favicon.ico"),
    ),
    tags$style(
      HTML(".col-sm-1 { width:3.5%; !important }")
    ),
    wellPanel(
      column(width = 1, uiOutput(outputId = "left_edit")),
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
               tags$a(href = glue("data/demo.png"), target = "_blank", tags$img(src = glue("data/demo.png"), width = "auto", height = "100", alt = "ZOOM IN")),
               HTML('</div>')
             ),
             import_ui_demo(
               id = 'show_data',
               from = c('env', 'file', 'copypaste', 'url'),
               file_extensions = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")
             ),
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.2}</strong></div>")),
             tags$div(id = 'variable-des',  
                      HTML(glue("<span class='help-block m-b-none var-select' style='color:#EE0000'>The input file for this application can directly use the MR results exported from MMR analysis (<a href=\"{MR.HOME}analysis/2SMR-api/\" target=\"_blank\">API</a> or <a href=\"{MR.HOME}analysis/2SMR-local/\" target=\"_blank\">LOCAL</a>), which simultaneously includes two-sample MR results for \"Exposure to Outcome\", \"Exposure to Mediation\", and \"Mediation to Outcome.\"</span><br>")),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>BETA</span>, <span class='var-num'>one</span>. This represents the effect size estimate of the genetic variant on the outcome or exposure in the MR analysis.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>SE</span>, <span class='var-num'>one</span>. The standard error (SE) is a measure of the precision of the beta estimate.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>lo_ci</span>, <span class='var-num'>one</span>, Lower Confidence Interval. The lower bound of the confidence interval (typically 95%) for the beta estimate.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>up_ci</span>, <span class='var-num'>one</span>, Upper Confidence Interval. The upper bound of the confidence interval (typically 95%) for the beta estimate.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>PVAL</span>, <span class='var-num'>one</span>. The p-value associated with the beta coefficient.</span>"),
                      HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Group</span>, <span class='var-num'>one</span>. This column indicates the specific analysis group in the MR study. In multi-step MR, you must contain three groups: \"<strong style='color:#3B4992'>E2O</strong>\" or \"<strong style='color:#3B4992'>Exposure to Outcome</strong>\", \"<strong style='color:#EE0000'>E2M</strong>\" or \"<strong style='color:#EE0000'>Exposure to Mediation</strong>\" and \"<strong style='color:#008B45'>M2O</strong>\" or \"<strong style='color:#008B45'>Mediation to Outcome</strong>\".</span>")
             ),
             uiOutput(outputId = 'ui_aesthetics'),
             
             HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> {step.3}</strong></div>")),
             uiOutput(outputId = 'is_add_plot_buttom'),
             
      ),
      column(width = 7, 
             column(width = 12, uiOutput(outputId = 'modify_plot_params')),
             tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
             column(width = 12,
                    mainPanel(
                      fluidRow(div(id = 'io-albert-gg', style = 'width:61vw;overflow-x:scroll;height:680px;overflow-y:scroll;display:none', uiOutput(outputId = 'io.albert.gg')))
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
    filename <- function(){ paste('mmr_result.csv') },
    content <- function(file){
      file.copy(glue("{www}/demo.csv"), file)
    }, contentType = NULL)
  
  # Import Data
  imported <- import_server_demo(
    id = "show_data", 
    choices = c('dag_demo'), 
    return_class = "tbl_df",
    read_fns = list(tsv = function(file) { 
      data.table::fread(fread) 
    }))

  # Choose Variable
  output[['ui_aesthetics']] <- renderUI({
    aesthetics <- c('BETA', 'SE', 'lo_ci', 'up_ci', 'PVAL', 'Group')
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
        # selected = list(
        #   BETA = find.first(var_choices, c('b')),
        #   SE = find.first(var_choices, c('se')),
        #   lo_ci = find.first(var_choices, c('lo_ci')),
        #   up_ci = find.first(var_choices, c('up_ci')),
        #   PVAL = find.first(var_choices, c('pval')),
        #   Group = find.first(var_choices, c('group'))
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
    !is.null(input[["dragvars"]]$target$BETA) &&
      !is.null(input[["dragvars"]]$target$SE) &&
      !is.null(input[["dragvars"]]$target$lo_ci) &&
      !is.null(input[["dragvars"]]$target$up_ci) &&
      !is.null(input[["dragvars"]]$target$PVAL) &&
      !is.null(input[["dragvars"]]$target$Group)
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
        fluidRow(column(width = 6, sliderInput(inputId = "WIDTH", label = h5("Width"), value = 1080,  min = 100, max = 3000, step = 10)),
                 column(width = 6, sliderInput(inputId = "HEIGHT", label = h5("Height"), value = 480, min = 100, max = 3000, step = 10))
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

  # left ui
  output[['left_edit']] <- renderUI({
    if (display.status()) {
      tags$div(
        tags$style(
          HTML(".sw-dropdown { margin-bottom:10px }")
        ),
        tags$br(),
        tags$br(),
        left.ui.title(),
        left.ui.text(),
      )
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
        input[['dragvars']]$target$BETA,
        input[['dragvars']]$target$SE,
        input[['dragvars']]$target$lo_ci,
        input[['dragvars']]$target$up_ci,
        input[['dragvars']]$target$PVAL,
        input[['dragvars']]$target$Group
      )}, silent = QUITE)
    if(! "try-error" %in% class(try.c)) {
      try.c
    }
  })
  
  # DEBOUNCE
  db.exposure_size <- debounce(reactive({ input[['exposure_size']] * 12 }), DEBOUNCE.A)
  db.mediator_size <- debounce(reactive({ input[['mediator_size']] * 12 }), DEBOUNCE.A)
  db.outcome_size <- debounce(reactive({ input[['outcome_size']] * 12 }), DEBOUNCE.A)
  db.total_effect_size <- debounce(reactive({ input[['total_effect_size']] * 12 }), DEBOUNCE.A)
  db.indirect_effect_size <- debounce(reactive({ input[['indirect_effect_size']] * 12 }), DEBOUNCE.A)
  db.beta1_size <- debounce(reactive({ input[['beta1_size']] * 12 }), DEBOUNCE.A)
  db.beta2_size <- debounce(reactive({ input[['beta2_size']] * 12 }), DEBOUNCE.A)
  db.width <- debounce(reactive({ input[['WIDTH']] }), DEBOUNCE.A)
  db.height <- debounce(reactive({ input[['HEIGHT']] }), DEBOUNCE.A)
  
  # 绘图部分
  palette. <- reactiveVal(NULL)
  
  plot.reactive <- function() {
    
    message(glue('{Sys.time()} Starts Running'))
    
    # define a function for DAG plot
    dag.m <- function(m.t, e.t, o.t, e.m.t, m.o.t, e.o.d.t, e.o.i.t) {
      suppressMessages(suppressWarnings(library(ggdag)))
      suppressMessages(suppressWarnings(library(ggplot2)))
      dag <- dagify(
        m ~ e, o ~ e + m, exposure = "e", outcome = "o", labels = c(m = m.t, e = e.t, o = o.t),
        coords = list(
          x = c(e = 1, m = 2, o = 3),
          y = c(e = 1, m = 2, o = 1)
        )) %>% tidy_dagitty() %>% 
        dplyr::mutate(colour = ifelse(name == "e", "CE", ifelse(name == "m", "CM", 'CO')))
      
      gg <- dag %>% ggplot(aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend)
      ) + geom_dag_point(aes(colour = colour)) +
        geom_dag_edges() +
        theme_dag()
      
      size = 6
      gg <- gg + guides(fill = "none", color = "none")
      gg <- gg + annotate(
        geom = "text", 
        label = e.t, 
        x = 0.9, 
        y = 0.78,
        lineheight = input[['exposure_lineheight']], 
        angle = input[['exposure_angle']], 
        size = db.exposure_size(), 
        hjust = input[['exposure_hjust']],
        vjust = input[['exposure_vjust']],
        color = input[['exposure_colour']]
      )
      gg <- gg + annotate(
        geom = "text", 
        label = m.t, 
        x = 2, 
        y = 2.18, 
        lineheight = input[['mediator_lineheight']], 
        angle = input[['mediator_angle']], 
        size = db.mediator_size(), 
        hjust = input[['mediator_hjust']],
        vjust = input[['mediator_vjust']],
        color = input[['mediator_colour']]
      )
      gg <- gg + annotate(
        geom = "text", 
        label = o.t,
        x = 3.1, 
        y = 0.78, 
        lineheight = input[['outcome_lineheight']], 
        angle = input[['outcome_angle']], 
        size = db.outcome_size(), 
        hjust = input[['outcome_hjust']],
        vjust = input[['outcome_vjust']],
        color = input[['outcome_colour']]
      )
      gg <- gg + annotate(
        geom = "text", 
        label = e.m.t,
        x = 1.5,
        y = 1.6, 
        lineheight = input[['beta1_lineheight']], 
        angle = input[['beta1_angle']], 
        size = db.beta1_size(), 
        hjust = input[['beta1_hjust']],
        vjust = input[['beta1_vjust']],
        color = input[['beta1_colour']]
      )
      gg <- gg + annotate(
        geom = "text", 
        label = m.o.t,
        x = 2.5,
        y = 1.6, 
        lineheight = input[['beta2_lineheight']], 
        angle = input[['beta2_angle']], 
        size = db.beta2_size(), 
        hjust = input[['beta2_hjust']],
        vjust = input[['beta2_vjust']],
        color = input[['beta2_colour']]
      )
      gg <- gg + annotate(
        geom = "text", 
        label = e.o.i.t,
        x = 2,
        y = 1.15, 
        lineheight = input[['indirect_effect_lineheight']], 
        angle = input[['indirect_effect_angle']], 
        size = db.indirect_effect_size(), 
        hjust = input[['indirect_effect_hjust']],
        vjust = input[['indirect_effect_vjust']],
        color = input[['indirect_effect_colour']]
      )
      gg <- gg + annotate(
        geom = "text", 
        label = e.o.d.t,
        x = 2,
        y = 0.9, 
        lineheight = input[['total_effect_lineheight']], 
        angle = input[['total_effect_angle']], 
        size = db.total_effect_size(), 
        hjust = input[['total_effect_hjust']],
        vjust = input[['total_effect_vjust']],
        color = input[['total_effect_colour']]
      )
      gg
    }
    
    # 加载数据集
    # ------------------------------------------------------------------------------------------------------------
    i.data <- i.data()
    colnames(i.data) <- c('b', 'se', 'lo_ci', 'up_ci', 'pval', 'group')
    
    if (input[['self_palette']]) {
      pal.col <- c.palette(input[['palette_bin']])[1:3]
      shinyWidgets::updateColorPickr(session = session, inputId = 'exposure_col', label = 'Exposure', value = c.palette(input[['palette_bin']])[1])
      shinyWidgets::updateColorPickr(session = session, inputId = 'mediator_col', label = 'Mediator', value = c.palette(input[['palette_bin']])[2])
      shinyWidgets::updateColorPickr(session = session, inputId = 'outcome_col', label = 'Outcome', value = c.palette(input[['palette_bin']])[3])
    } else {
      pal.col <- c(input[['exposure_col']], input[['mediator_col']], input[['outcome_col']])
    }
    
    EO.P <- i.data %>% filter(group %in% c('E2O', 'Exposure to Outcome'))
    EM.P <- i.data %>% filter(group %in% c('E2M', 'Exposure to Mediation'))
    MO.P <- i.data %>% filter(group %in% c('M2O', 'Mediation to Outcome'))
    
    b.mediation <- EM.P$b * MO.P$b
    b.direct <- EO.P$b - b.mediation
    se.mediation <- sqrt(EM.P$b^2 * MO.P$se^2 + MO.P$b^2 * EM.P$se^2)
    Z <- b.mediation/se.mediation
    m.q <- 2 * pnorm(
      q = abs(Z),
      lower.tail = FALSE
    )
    lo_ci.mediation <- b.mediation - 1.96 * se.mediation
    up_ci.mediation <- b.mediation + 1.96 * se.mediation
    
    MM.P <- data.frame(
      pval = m.q,
      se.mediation = se.mediation,
      b.mediation = b.mediation,
      lo_ci.mediation = lo_ci.mediation,
      up_ci.mediation = up_ci.mediation
    )
    
    EM.PP <- ifelse(test = EM.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(EM.P$pval, 4)}"))
    MO.PP <- ifelse(test = MO.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(MO.P$pval, 4)}"))
    EO.PP <- ifelse(test = EO.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(EO.P$pval, 4)}"))
    MM.PP <- ifelse(test = MM.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(MM.P$pval, 4)}"))
    
    e.name <- "Feeling lonely"
    m.name <- "Cigarettes smoked per day"
    o.name <- "Major depressive disorder in trauma-unexposed individuals"
    e.name <- "Exposure"
    m.name <- "Mediator"
    o.name <- "Outcome"
    if (input[['exposure_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'exposure_text', label = "Exposure", value = e.name), DEBOUNCE.L) }
    if (input[['mediator_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'mediator_text', label = "Mediator", value = m.name), DEBOUNCE.L) }
    if (input[['outcome_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'outcome_text', label = "Outcome", value = o.name), DEBOUNCE.L) }
    e.name <- if (input[['exposure_text']] == '') { e.name } else { input[['exposure_text']] }
    m.name <- if (input[['mediator_text']] == '') { m.name } else { input[['mediator_text']] }
    o.name <- if (input[['outcome_text']] == '') { o.name } else { input[['outcome_text']] }
    MM.P$b.direct <- EO.P$b - MM.P$b.mediation
    prop <- round((MM.P$b.mediation/(MM.P$b.mediation + MM.P$b.direct)) * 100, 3)
    
    e.m.t <- glue("beta1: {round(EM.P$b, 4)} ({round(EM.P$lo_ci, 4)} to {round(EM.P$up_ci, 4)})\n{EM.PP}")
    m.o.t <- glue("beta2: {round(MO.P$b, 4)} ({round(MO.P$lo_ci, 4)} to {round(MO.P$up_ci, 4)})\n{MO.PP}")
    e.o.d.t = glue("Total effect: {round(EO.P$b, 4)} ({round(EO.P$lo_ci, 4)} to {round(EO.P$up_ci, 4)}), {EO.PP}")
    e.o.i.t = glue("Indirect effect: {round(MM.P$b.mediation, 4)} ({round(MM.P$lo_ci.mediation, 4)} to {round(MM.P$up_ci.mediation, 4)}), {MM.PP}\nProp: {prop}%")
    if (input[['beta1_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'beta1_text', label = "beta1", value = e.m.t), DEBOUNCE.L) }
    if (input[['beta2_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'beta2_text', label = "beta2", value = m.o.t), DEBOUNCE.L) }
    if (input[['total_effect_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'total_effect_text', label = "Total Effect", value = e.o.d.t), DEBOUNCE.L) }
    if (input[['indirect_effect_text']] == '') { debounce(shiny::updateTextAreaInput(session = session, inputId = 'indirect_effect_text', label = "Indirect Effect", value = e.o.i.t), DEBOUNCE.L) }
    beta1.title <- if (input[['beta1_text']] == '') { e.m.t } else { input[['beta1_text']] }
    beta2.title <- if (input[['beta2_text']] == '') { m.o.t } else { input[['beta2_text']] }
    total.effect.text <- if (input[['total_effect_text']] == '') { e.o.d.t } else { input[['total_effect_text']] }
    indirect.effect.text <- if (input[['indirect_effect_text']] == '') { e.o.i.t } else { input[['indirect_effect_text']] }

    gg <- dag.m(
      m.t = m.name, e.t = e.name, o.t = o.name, e.m.t = beta1.title,
      m.o.t = beta2.title,
      e.o.d.t = total.effect.text,
      e.o.i.t = indirect.effect.text
    ) + scale_color_manual(values = pal.col)
    
    message(glue('{Sys.time()} Plot Success'))
    gg
  }
  
  # 同时支持自定义图片大小
  # ------------------------------------------------------------------------------------------------------------
  observeEvent(input[['show_data-confirm']], {
    
    shinyjs::hide('io-albert-gg')
    shinyjs::hide('introbox-download')
    shinyjs::hide('variable-des')
    shinyWidgets::updateSwitchInput(session = session, inputId = 'plot_interactively', value = FALSE)
    
    observeEvent(input[['plot_button']], {
      
      req(db.exposure_size(), cancelOutput = TRUE)
      req(db.mediator_size (), cancelOutput = TRUE)
      req(db.outcome_size(), cancelOutput = TRUE)
      req(db.total_effect_size(), cancelOutput = TRUE)
      req(db.indirect_effect_size (), cancelOutput = TRUE)
      req(db.beta1_size(), cancelOutput = TRUE)
      req(db.beta2_size(), cancelOutput = TRUE)
      
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
  thisp <- 'dag_plot'
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
    '# Define a function for DAG Plot'
    dag.m <- function(m.t, e.t, o.t, e.m.t, m.o.t, e.o.d.t, e.o.i.t) {
      dag <- dagify(
        m ~ e, o ~ e + m, exposure = "e", outcome = "o", labels = c(m = m.t, e = e.t, o = o.t),
        coords = list(
          x = c(e = 1, m = 2, o = 3),
          y = c(e = 1, m = 2, o = 1)
        )) %>% tidy_dagitty() %>% 
        dplyr::mutate(colour = ifelse(name == "e", "CE", ifelse(name == "m", "CM", 'CO')))
      
      gg <- dag %>% ggplot(aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend)
      ) + geom_dag_point(aes(colour = colour)) +
        geom_dag_edges() +
        theme_dag()
      
      size = 6
      gg <- gg + guides(fill = "none", color = "none")
      gg <- gg + annotate(
        geom = "text", 
        label = e.t, 
        x = 0.9, 
        y = 0.78,
        lineheight = ..(input[['exposure_lineheight']]), 
        angle = ..(input[['exposure_angle']]), 
        size = ..(db.exposure_size()), 
        hjust = ..(input[['exposure_hjust']]),
        vjust = ..(input[['exposure_vjust']]),
        color = ..(input[['exposure_colour']])
      )
      gg <- gg + annotate(
        geom = "text", 
        label = m.t, 
        x = 2, 
        y = 2.18, 
        lineheight = ..(input[['mediator_lineheight']]), 
        angle = ..(input[['mediator_angle']]), 
        size = ..(db.mediator_size()), 
        hjust = ..(input[['mediator_hjust']]),
        vjust = ..(input[['mediator_vjust']]),
        color = ..(input[['mediator_colour']])
      )
      gg <- gg + annotate(
        geom = "text", 
        label = o.t,
        x = 3.1, 
        y = 0.78, 
        lineheight = ..(input[['outcome_lineheight']]), 
        angle = ..(input[['outcome_angle']]), 
        size = ..(db.outcome_size()), 
        hjust = ..(input[['outcome_hjust']]),
        vjust = ..(input[['outcome_vjust']]),
        color = ..(input[['outcome_colour']])
      )
      gg <- gg + annotate(
        geom = "text", 
        label = e.m.t,
        x = 1.5,
        y = 1.6, 
        lineheight = ..(input[['beta1_lineheight']]), 
        angle = ..(input[['beta1_angle']]), 
        size = ..(db.beta1_size()), 
        hjust = ..(input[['beta1_hjust']]),
        vjust = ..(input[['beta1_vjust']]),
        color = ..(input[['beta1_colour']])
      )
      gg <- gg + annotate(
        geom = "text", 
        label = m.o.t,
        x = 2.5,
        y = 1.6, 
        lineheight = ..(input[['beta2_lineheight']]), 
        angle = ..(input[['beta2_angle']]), 
        size = ..(db.beta2_size()), 
        hjust = ..(input[['beta2_hjust']]),
        vjust = ..(input[['beta2_vjust']]),
        color = ..(input[['beta2_colour']])
      )
      gg <- gg + annotate(
        geom = "text", 
        label = e.o.i.t,
        x = 2,
        y = 1.15, 
        lineheight = ..(input[['indirect_effect_lineheight']]), 
        angle = ..(input[['indirect_effect_angle']]), 
        size = ..(db.indirect_effect_size()), 
        hjust = ..(input[['indirect_effect_hjust']]),
        vjust = ..(input[['indirect_effect_vjust']]),
        color = ..(input[['indirect_effect_colour']])
      )
      gg <- gg + annotate(
        geom = "text", 
        label = e.o.d.t,
        x = 2,
        y = 0.9, 
        lineheight = ..(input[['total_effect_lineheight']]), 
        angle = ..(input[['total_effect_angle']]), 
        size = ..(db.total_effect_size()), 
        hjust = ..(input[['total_effect_hjust']]),
        vjust = ..(input[['total_effect_vjust']]),
        color = ..(input[['total_effect_colour']])
      )
      gg
    }
    '# Load your own data. Here use "data.table::fread" and "demo.csv" for example.'
    data <- data.table::fread('E:/BaiduNetdiskWorkspace/005.Bioinformatics/MRanalysis/gallery/dag-mmr/www/data/demo.csv')
    # data <- data.table::fread('path/to/demo.csv')
    '# Prepare plotting data'
    i.data <- ..(if ((class(imported$code()) == 'name')[1]) { quote(data %>% ungroup()) } else { imported$code() } ) %>% dplyr::select(
      ..(input[['dragvars']]$target$BETA),
      ..(input[['dragvars']]$target$SE),
      ..(input[['dragvars']]$target$lo_ci),
      ..(input[['dragvars']]$target$up_ci),
      ..(input[['dragvars']]$target$PVAL),
      ..(input[['dragvars']]$target$Group)
    )
    colnames(i.data) <- c('b', 'se', 'lo_ci', 'up_ci', 'pval', 'group')
    '# Set palette'
    pal.col <- ..(
      if (input[['self_palette']]) {
        c.palette(input[['palette_bin']])[1:3]
      } else {
        c(input[['exposure_col']], input[['mediator_col']], input[['outcome_col']])
      }
    )
    '# Arrange Data'
    EO.P <- i.data %>% filter(group %in% c('E2O', 'Exposure to Outcome'))
    EM.P <- i.data %>% filter(group %in% c('E2M', 'Exposure to Mediation'))
    MO.P <- i.data %>% filter(group %in% c('M2O', 'Mediation to Outcome'))
    
    b.mediation <- EM.P$b * MO.P$b
    b.direct <- EO.P$b - b.mediation
    se.mediation <- sqrt(EM.P$b^2 * MO.P$se^2 + MO.P$b^2 * EM.P$se^2)
    Z <- b.mediation/se.mediation
    m.q <- 2 * pnorm(
      q = abs(Z),
      lower.tail = FALSE
    )
    lo_ci.mediation <- b.mediation - 1.96 * se.mediation
    up_ci.mediation <- b.mediation + 1.96 * se.mediation
    
    MM.P <- data.frame(
      pval = m.q,
      se.mediation = se.mediation,
      b.mediation = b.mediation,
      lo_ci.mediation = lo_ci.mediation,
      up_ci.mediation = up_ci.mediation
    )
    
    EM.PP <- ifelse(test = EM.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(EM.P$pval, 4)}"))
    MO.PP <- ifelse(test = MO.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(MO.P$pval, 4)}"))
    EO.PP <- ifelse(test = EO.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(EO.P$pval, 4)}"))
    MM.PP <- ifelse(test = MM.P$pval < 1e-04, yes = "P < 0.0001", no = glue("p = {round(MM.P$pval, 4)}"))
    
    e.name <- ..(if (input[['exposure_text']] == '') { e.name } else { input[['exposure_text']] })
    m.name <- ..(if (input[['mediator_text']] == '') { m.name } else { input[['mediator_text']] })
    o.name <- ..(if (input[['outcome_text']] == '') { o.name } else { input[['outcome_text']] })
    
    MM.P$b.direct <- EO.P$b - MM.P$b.mediation
    prop <- round((MM.P$b.mediation/(MM.P$b.mediation + MM.P$b.direct)) * 100, 3)
    
    beta1.title <- ..(if (input[['beta1_text']] == '') { e.m.t } else { input[['beta1_text']] })
    beta2.title <- ..(if (input[['beta2_text']] == '') { m.o.t } else { input[['beta2_text']] })
    total.effect.text <- ..(if (input[['total_effect_text']] == '') { e.o.d.t } else { input[['total_effect_text']] })
    indirect.effect.text <- ..(if (input[['indirect_effect_text']] == '') { e.o.i.t } else { input[['indirect_effect_text']] })
    
    '# Draw Plot'
    dag.m(
      m.t = m.name, e.t = e.name, o.t = o.name, e.m.t = beta1.title,
      m.o.t = beta2.title,
      e.o.d.t = total.effect.text,
      e.o.i.t = indirect.effect.text
    ) + scale_color_manual(values = pal.col)
  })
  
  # Show Plot Codes
  observeEvent(input[['show_codes']], {
    code <- shinymeta::expandChain(
      quote({
        library(glue)
        library(dplyr)
        library(ggdag)
        library(ggplot2)
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
      code = as.character(xfun::session_info(c('dplyr', 'glue', 'stringr', 'ggplot2', 'ggdag'))),
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
    tags$h3('What is a DAG Plot?'),
    tags$p(style = 'text-align:left', "A DAG plot in mediation analysis refers to a Directed Acyclic Graph (DAG), which is used to visually represent causal relationships between variables. In the context of mediation analysis, a DAG plot shows how an independent variable (exposure) affects a dependent variable (outcome), potentially through a mediator."),
    tags$hr(),
    tags$h3('Key Components of a DAG in Mediation Analysis:'),
    tags$a(href = glue("data/demo.png"), target="_blank", tags$img(src = glue("data/demo.png"), width = "100%", height = "auto", alt = "ZOOM IN")),
    tags$p(style = 'text-align:left', tags$strong("Nodes (Circles)"), ': Each node represents a variable in the analysis. In this case: 1) The left node represents the exposure (e.g., "Feeling lonely"). 2) The middle node represents the mediator (e.g., "Cigarettes smoked per day"). 3) The right node represents the outcome (e.g., "Major depressive disorder").'),
    tags$p(style = 'text-align:left', tags$strong("Arrows (Edges)"), ': Arrows indicate the direction of potential causal relationships between variables.'),
    tags$p(style = 'text-align:left', tags$strong("Effect Estimates (Beta coefficients)"), ': The beta coefficients (e.g., beta1, beta2) quantify the strength of the association between the variables. beta1: The effect of the exposure on the mediator. beta2: The effect of the mediator on the outcome.'),
    tags$p(style = 'text-align:left', tags$strong("Direct Effect"), ':The effect of the exposure on the outcome that is not mediated by the mediator. This is shown by the arrow directly linking "Feeling lonely" to "Major depressive disorder."'),
    tags$p(style = 'text-align:left', tags$strong("Indirect Effect"), ': The portion of the effect of the exposure on the outcome that occurs through the mediator. This is calculated as the product of beta1 and beta2 and is represented by the two arrows flowing through the mediator.'),
    tags$p(style = 'text-align:left', tags$strong("Total Effect"), ': The sum of the direct and indirect effects, representing the overall impact of the exposure on the outcome.'),
    tags$p(style = 'text-align:left', tags$strong("Proportion Mediated"), ': The proportion of the total effect that is mediated through the mediator is often reported. In this case, 6.7% of the total effect is mediated by "Cigarettes smoked per day."'),
    size = "l", easyClose = TRUE, fade = TRUE,
    footer = modalButton("Close (Esc)"))
}

# Use for standalone testing, 
# comment out the following line when used as a component.
shinyApp(ui = ui.gallery, server = server.gallery)