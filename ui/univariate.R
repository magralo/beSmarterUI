tabPanel("Univariate Models",
         sidebarLayout(
           sidebarPanel(
             radioButtons("M11", "Models",
                          c("No Selection"="m110","Normal"="m111", "Logit"="m112","Probit"="m113","Multinomial(Mixed) Probit"="m114","Multinomial(Mixed) Logit"="m115","Ordered Probit"="m116","Negative Binomial(Poisson)"="m117","Tobit"="m118","Quantile"="m119")
             ),
             #h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image
           ),
           mainPanel(fluidRow(column(5,file1m),column(2,filech1m),column(2,rb1m)),fluidRow(column(4,it1),column(4,it2),column(4,it3)),DUI11,HT,go11,BE,DL11,DLP11,verbatimTextOutput("summary11"),pplot11)),

  uiOutput("univariate")
)