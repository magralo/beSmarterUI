
library(shiny)
# Define UI for Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility
image<- img(src="BEsmarterLogo.png", height = 200, width = "90%") #Local variable
#file<- fileInput("file", label = h3("File input"))



###############################################
## Ui


## Load General conditions, packages and other stuff necesary for run the app UI
source(file.path("genUI.R"),  local = TRUE)$value


ui <- navbarPage(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx"),
                 
                 sidebarLayout( 
                   sidebarPanel(h1(a(em(strong("BEsmarter",style = "color:light blue")),
                                     href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx")),
                                h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
                                image,
                                h4("Professor Andres Ramirez Hassan"),
                                h4(span("besmarter.org@gmail.com", style = "color:blue"))),
                   mainPanel(h3(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx")," is a team of friends from ", a("Universidad EAFIT", href ="http://www.eafit.edu.co/Paginas/index.aspx"), " (Medellin, Colombia) that promotes research, teaching and encoding of Bayesian Econometrics with social responsibility."
                   ),
                   h3("Bayesian Econometrics allows establishing a framework that simultaneously unifies decision theory, statistical inference, and probability theory under a single philosophically and mathematically consistent structure."),
                   br(),
                   h3(strong("VISION")),
                   h4(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx"), "envisions worldwide econometric research, teaching and applications based on a Bayesian framework that:") ,
                   h4(em("inspire"), " new econometric ideas,"),
                   h4(em("create"), " a user friendly environment  for applications of Bayesian econometrics,"),
                   h4(em("transform"), " classic econometric research, teaching and applications,"),
                   h4("and where one of the main concerns of science is to solve social problems."),
                   br(),
                   h3(strong("MISSION")),
                   h4(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx"), "leads and excels in the generation and dissemination of Bayesian Econometric knowledge through research, teaching and software.")
                   )),         
  #title = "Split app code across multiple files",
  # include the UI for each tab
  #source(file.path("ui", "main"),  local = TRUE)$value
  source(file.path("ui", "presentation.R"),  local = TRUE)$value,
  source(file.path("ui", "univariate.R"),  local = TRUE)$value,
  source(file.path("ui", "multivariate.R"),  local = TRUE)$value,
  source(file.path("ui", "hierarchical.R"),  local = TRUE)$value,
  source(file.path("ui", "nonpar.R"),  local = TRUE)$value,
  source(file.path("ui", "BMAGLM.R"),  local = TRUE)$value,
  source(file.path("ui", "help.R"),  local = TRUE)$value
)

#################################
## Server

#################################################################################

#rm(list=ls())

## Load General conditions, packages and other stuff necesary for run the app 
source(file.path("genServer.R"),  local = TRUE)$value


server <- function(input, output, session) {
  # Include the logic (server) for each tab
 #source(file.path("server", "main"),  local = TRUE)$value
  source(file.path("server", "presentation.R"),  local = TRUE)$value
  source(file.path("server", "univariate.R"),  local = TRUE)$value
  source(file.path("server", "multivariate.R"),  local = TRUE)$value
  source(file.path("server", "hierarchical.R"),  local = TRUE)$value
  source(file.path("server", "nonpar.R"),  local = TRUE)$value
  source(file.path("server", "BMAGLM.R"),  local = TRUE)$value
  source(file.path("server", "help.R"),  local = TRUE)$value
  }

shinyApp(ui = ui, server = server)