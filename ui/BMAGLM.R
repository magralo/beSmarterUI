tabPanel("Bayesian Model Averaging",
         sidebarLayout(
           sidebarPanel(
             radioBMA,
             h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.besmarter-team.org/")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image
           ),
           mainPanel(CONDBMA)
         )
)