

##preamble

## required packages
library(truncnorm)
library(shiny)
library(rhandsontable)
library(coda)
library(bayesm)
library(MCMCpack)
library(MASS)
library(bayesboot)
library(ivbma)
library(BMA)
library(DT)
library(Matrix)


# 
# packages <- c("shiny","coda","bayesm","MCMCpack","MASS","bayesboot")
# # new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]  ## Allow installation of missing packages
# # if(length(new.packages)) install.packages(new.packages)
#     lapply(packages, library, character.only=TRUE)

##
# Define UI for Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility
#image<- img(src="BEsmarterLogo.png", height = 200, width = 450) #Local variable
#setwd("~/Desktop/BS-UI")
image<- img(src="BEsmarterLogo.png", height = 200, width = "90%") #Local variable

#file<- fileInput("file", label = h3("File input"))

##### 1. Univariate Models: First NavBar#####
file1m<- fileInput('file1', 'Choose File',
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv'))
filech1m<- checkboxInput('header1', 'Header', TRUE)
rb1m<- radioButtons('sep1', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')
###################################
##### 2. Multivariate Models: Second NavBar#####

file2m<- fileInput('file2', 'Choose File',
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv'))
filech2m<- checkboxInput('header2', 'Header', TRUE)
rb2m<- radioButtons('sep2', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')

#######################################################

##### 3. Longitudinal Models: Forth NavBar#####
file3m<- fileInput('file3', 'Choose File',
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv'))
filech3m<- checkboxInput('header3', 'Header', TRUE)
rb3m<- radioButtons('sep3', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')
###################################

##### 4. Non-Parametric Models: Forth NavBar#####
file4m<- fileInput('file4', 'Choose File',
                   accept=c('text/csv', 
                            'text/comma-separated-values,text/plain', 
                            '.csv'))
filech4m<- checkboxInput('header4', 'Header', TRUE)
rb4m<- radioButtons('sep4', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')
###################################

#####Univariate#################
it1<- sliderInput("it", 
                  "MCMC Iterations:", 
                  value = 10000,
                  min = 10000, 
                  max = 100000,
                  step = 10000)
it2<- sliderInput("burnin", 
                  "Burn-in Sample:", 
                  value = 1000,
                  min = 1000, 
                  max = 10000,
                  step = 1000)

it3<- selectInput("keep", "Thinning parameter:", 
                  choices = c("1", "10", "20", "50", "100"), selected = "1")

#####Multivariate#################
it1MV<- sliderInput("itMV", 
                  "MCMC Iterations:", 
                  value = 10000,
                  min = 10000, 
                  max = 100000,
                  step = 10000)
it2MV<- sliderInput("burninMV", 
                  "Burn-in Sample:", 
                  value = 1000,
                  min = 1000, 
                  max = 10000,
                  step = 1000)

it3MV<- selectInput("keepMV", "Thinning parameter:", 
                  choices = c("1", "10", "20", "50", "100"), selected = "1")

#####Hierarchical#################
it1HM<- sliderInput("itHM", 
                    "MCMC Iterations:", 
                    value = 10000,
                    min = 10000, 
                    max = 100000,
                    step = 10000)
it2HM<- sliderInput("burninHM", 
                    "Burn-in Sample:", 
                    value = 1000,
                    min = 1000, 
                    max = 10000,
                    step = 1000)

it3HM<- selectInput("keepHM", "Thinning parameter:", 
                    choices = c("1", "10", "20", "50", "100"), selected = "1")

######Nonparametric: Bootstrap########
it1BB<- sliderInput("itBB", 
                  "MCMC Iterations:", 
                  value = 1000,
                  min = 1000, 
                  max = 10000,
                  step = 1000)

BBr2<- sliderInput("BBr2", 
                    "S_1:", 
                    value = 1000,
                    min = 1000, 
                    max = 10000,
                    step = 1000)


HT<- helpText("Click the button (Go!) after importing the dataset and selecting the model to update the value displayed in the main panel.")
BE<- helpText("Warning: Be patient this may take several minutes!!!")

##### 1.1 #########
DUI11<- uiOutput("ui11")
go11<- actionButton("goButton11", "Go!")
DL11<- downloadButton('download11', 'Download Posterior Chains')
DLP11<- downloadButton('multiDownload11', 'Download Posterior Graphs')
pplot11<- plotOutput("plot11", height = 1)

##### 2.1 #########
DUI21<- uiOutput("ui21")
go21<- actionButton("goButton21", "Go!")
DL21<- downloadButton('download21', 'Download Posterior Chains')
DLP21<- downloadButton('multiDownload21', 'Download Posterior Graphs')
pplot21<- plotOutput("plot21", height = 1)

##### 3.1 #########
DUI31<- uiOutput("ui31")
go31<- actionButton("goButton31", "Go!")
DL31<- downloadButton('download31', 'Download Posterior Chains')
DLP31<- downloadButton('multiDownload31', 'Download Posterior Graphs')
pplot31<- plotOutput("plot31", height = 1)

##### 4.2 #########
FormulaM42<- textInput("Formula42", "Main Equation", value = "")
DUI42<- uiOutput("ui42")
go42<- actionButton("goButton42", "Go!")
DL42<- downloadButton('download42', 'Download Posterior Chains')
DLP42<- downloadButton('multiDownload42', 'Download Posterior Graphs')
pplot42<- plotOutput("plot42", height = 1)


##### BMA GLM ####
radioBMA=radioButtons("radioBMA", "Bayesian Model Average",
                      c("No selection"="NS",
                        "Normal data"="NBMA", 
                        "Binomial data (Logit)"="LBMA",
                        "Real positive data (Gamma)"="GBMA",
                        "Count data (Poisson)"="PBMA")
)

CONDBMA<- uiOutput("CONDBMA")
