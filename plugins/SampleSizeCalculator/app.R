#' @title
#'
#' @description
#' 

suppressMessages(suppressMessages(library(bslib)))
suppressMessages(suppressMessages(library(shinymeta)))

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

ui.alb <- function() {
  tagList(
    navbarPage(
      title = suppressWarnings(tags$img(src = '/XINGABAO/img/mr_logo.png', title = "MRanalysis", width = "250px")),
      theme = bslib::bs_theme(version = 5, bootswatch = "journal"),
      tabPanel(
        tags$h4("Sample Size Calculator (Continuous outcome)"),
        fluidPage(
          titlePanel("Sample Size Calculator (Continuous outcome)"),
          sidebarLayout(
            sidebarPanel(
              numericInput("power3", "Power:", value = 0.8, step = 0.01),
              numericInput("alpha3", "Alpha:", value = 0.05),
              numericInput("byx3", "Byx:", value = 0.2, step = 0.01),
              numericInput("r23", "R2:", value = 0.028, step = 0.01),
              shiny::actionButton(inputId = "submitSampleSizeContinuous", label = "Submit", class = 'btn-danger btn-floatsession')
            ),
            mainPanel(
              tags$head(MRanalysisBase::web.statistic.baidu, tags$style(
                HTML(
                  ".panel {
                    padding: 20px;
                    border-radius: 10px;
                    box-shadow: rgba(0, 0, 0, 0.1) 0px 5px 15px;
                    margin-bottom: 20px;
                  }
                  h3 {
                    margin-top: 0;
                  }"
                )
              )),
              tags$head(tags$link(rel = "shortcut icon", href = '/XINGABAO/img/favicon.ico')),
              div(
                class = "panel",
                h3("Result"),
                tableOutput("SampleSizeContinuousResultTable")
              ),
              div(
                class = "panel",
                h3("Parameter interpretation"),
                p("Power: Study the probability of being able to detect the true effect"),
                p("Alpha: Type-I error rate, defaults to 0.05"),
                p("βyx: Regression coefficient, representing the potential causal relationship between exposure and outcome, i.e. the beta value after MR analysis"),
                p("R2: Explanation of exposure by instrumental variables,The input required is the joint R2, which can be obtained by directly summing the R2 of each of the exposed SNPs to obtain the joint R2, while ensuring that there are no linkage disequilibrium (LD)"),
                img(src = "/XINGABAO/img/R2.png", width = "100%", height = "auto")
              )
            )
          ),
        )
      ),
      tabPanel(
        tags$h4("Sample Size Calculator (Binary outcome)"),
        fluidPage(
          titlePanel("Sample Size Calculator (Binary outcome)"),
          sidebarLayout(
            sidebarPanel(
              numericInput("power4", "Power:", value = 0.8, step = 0.01),
              numericInput("alpha4", "Alpha:", value = 0.05),
              numericInput("k4", "K:", value = 0.8, step = 0.01),
              numericInput("or4", "OR:", value = 1.23, step = 0.01),
              numericInput("r24", "R2:", value = 0.02, step = 0.01),
              shiny::actionButton(inputId = "submitSampleSizeBinary", label = "Submit", class = 'btn-danger btn-floatsession')
            ),
            mainPanel(
              tags$head(tags$style(
                HTML(
                  ".panel {
                    padding: 20px;
                    border-radius: 10px;
                    box-shadow: rgba(0, 0, 0, 0.1) 0px 5px 15px;
                    margin-bottom: 20px;
                  }
                  h3 {
                    margin-top: 0;
                  }"
                )
              )),
              div(
                class = "panel",
                h3("Result"),
                tableOutput("SampleSizeBinaryResultTable")
              ),
              div(
                class = "panel",
                h3("Parameter interpretation"),
                p("Power: Study the probability of being able to detect the true effect"),
                p("Alpha: Type-I error rate, defaults to 0.05"),
                p("K: The ratio of cases to controls, for example, if there are 3400 cases and 5000 controls, then the K = 3400/5000 = 0.68."),
                p("OR: odds ratio:The effect of the exposure factor on the outcome, i.e. the multiplicative increase in the odds of the outcome occurring when the exposure factor is increased by one unit."),
                p("R2: Explanation of exposure by instrumental variables,The input required is the joint R2, which can be obtained by directly summing the R2 of each of the exposed SNPs to obtain the joint R2, while ensuring that there are no linkage disequilibrium(LD)"),
                img(src = "/XINGABAO/img/R2.png", width = "100%", height = "auto"),
              )
            )
          ),
        )
      ),
      tabPanel(
        tags$h4("Citation"),
        fluidPage(
          mainPanel(
            tags$head(tags$style(
              HTML(
                ".panel {
                    padding: 20px;
                    border-radius: 10px;
                    box-shadow: rgba(0, 0, 0, 0.1) 0px 5px 15px;
                    margin-bottom: 20px;
                  }
                  h3 {
                    margin-top: 0;
                  }"
              )
            )),
            div(class = "panel",
                p("1. Stephen Burgess, Sample size and power calculations in Mendelian randomization with a single instrumental variable and a binary outcome, International Journal of Epidemiology, Volume 43, Issue 3, June 2014, Pages 922–929, https://doi.org/10.1093/ije/dyu005"),
                p("2. Stephen Burgess, Simon G Thompson, CRP CHD Genetics Collaboration, Avoiding bias from weak instruments in Mendelian randomization studies, International Journal of Epidemiology, Volume 40, Issue 3, June 2011, Pages 755–764, https://doi.org/10.1093/ije/dyr036"),
            )
          )
        )
      )
    )
  )
}

server.ert <- function(input, output, session) {
  output$SampleSizeContinuousResultTable <- renderTable({
    if(!is.null(input$submitSampleSizeContinuous) && input$submitSampleSizeContinuous > 0){
      isolate({
        ps_beta_based <- function(b1, rsq, sig = 0.05, pow = 0.8) {
          res.exact <- (qnorm(1 - sig/2) + qnorm(pow))^2 / (b1^2 * rsq)
          res <- ceiling(res.exact/100) * 100
          return(data.frame(Parameter = "Sample Size", Value = as.integer(res)))
        }
        
        results <- ps_beta_based(
          pow = input$power3,
          sig = input$alpha3,
          rsq = input$r23,
          b1 = input$byx3
        )
        return(results)
      })
    } else {
      return(data.frame(Parameter = "Sample Size", Value = as.integer(0)))
      
    }
  })
  
  output$SampleSizeBinaryResultTable <- renderTable({
    if(!is.null(input$submitSampleSizeBinary) && input$submitSampleSizeBinary > 0){
      isolate(
        {
          ps_binary <-  function(b1, rsq, ratio, sig = 0.05, pow = 0.8) {
            res.exact = (qnorm(1 - sig/2) + qnorm(pow))^2/b1^2/rsq/(ratio/(1 + ratio))/((1/(1 + ratio)))
            res <- ceiling(res.exact/100) * 100
            data.frame(Parameter = "Sample Size", Value = as.integer(res))
          }
          results <- ps_binary(
            pow = input$power4,
            sig = input$alpha4,
            rsq = input$r24,
            ratio = input$k4,
            b1 = log(input$or4)
          )
          return(results)
        }
      )
    } else {
      data.frame(
        Parameter = c("Sample Size"),
        Value = as.integer(0)
      )
    }
  })
}

# Use for standalone testing, 
# comment out the following line when used as a component.
# shinyApp(ui = ui.alb, server = server.ert, options = list(host = '10.0.12.2', port = 3390))
shinyApp(ui = ui.alb, server = server.ert)