tabPanel("Bayesian Model Average",
         sidebarLayout(
           sidebarPanel(
             radioButtons("M51", "Bayesian Model Average",
                          c("Spatial Autoregressive Panel Data"="SARPD", "Spatial Moving Average Panel Data"="SMAPD","Spatial Autoregressive Moving Average Panel Data"="SARARPD")
             ),
             h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.eafit.edu.co/docentes-investigadores/Paginas/andres-ramirez.aspx")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image,
             h4("Professor Andres Ramirez Hassan"),
             h4("Mateo Graciano Londono"),
             h4(span("besmarter.org@gmail.com", style = "color:blue"))),
           mainPanel()                      
         ),
         uiOutput("bayes")
         )
