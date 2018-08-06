

##preamble
#rm(list=ls())
library(matrixcalc)
## required packages
# 
# packages <- c("shiny","coda","bayesm","MCMCpack","MASS","bayesboot")
# # new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]  ## Allow installation of missing packages
# # if(length(new.packages)) install.packages(new.packages)
#     lapply(packages, library, character.only=TRUE)

    #### Auxiliary ############
    
    source("Models/SumDiagPlots.R")
    source("Models/Xcreate.R")
    
    #### Models ########
    source("Models/Normal.R")
    source("Models/Probit.R")
    source("Models/MultProbit.R")
    source("Models/MultLogit.R")
    source("Models/Oprobit.R")
    source("Models/NegBin.R")
    source("Models/InstVar.R")
    source("Models/MultiReg.R")
    source("Models/SUR.R")
    source("Models/BiVarProbit.R")

    
    path<-getwd()
    unlink(file.path(path,"Posterior Graphs"),recursive=TRUE)
    
    
    ##########Univarate Models###############
    
    FormulaM1A<- textInput("Formula1a", "Main Equation", value = "")
    HTForm<- helpText("Introduce Formula. Example: y~x1+x2 where y is the dependent variable, and x1 and x2 are independent variables. Model specification includes the constant term by default.")
    HTFormOP<- helpText("Introduce Formula. Example: y~x1+x2-1 where y is the dependent variable, and x1 and x2 are independent variables. Model specification has to omit intercept then it is required -1 at the end of the formula.")
    
    FormulaM2A<- textInput("Formula2a", "Main Equation", value = "")
    HTFormy<- helpText("Introduce Formula. Example: y~x1+x2 where y is the dependent variable, and x1 and x2 are independent variables. The first variable of the right hand side has to be the variable with endogeneity problems. Model specification includes the constant term by default.")
    FormulaM2B<- textInput("Formula2b", "Instrumental Equation", value = "")
    HTFormZ<- helpText("Introduce Formula. Example: x1~z1+z2 where x1 is the independent variable with problems, and z1 and z2 are instruments. Model specification includes the constant term by default.")
    
    m116numAlt<- sliderInput("m116numAlt", "Number of alternatives", value = 3,min = 3,max = 20,step = 1)
    PMean<- textInput("PMeanL", "Prior Mean Vector: Location Parameters", value = "")
    HTM<- helpText("Introduce prior mean vector location parameters. Example: c(0,0)")
    PVar<- textInput("PVarL", "Prior Covariance Matrix: Location Parameters", value = "")
    HTV<- helpText("Introduce prior covariances location parameters by row. It has to be symmetric. Example: c(100,0,0,100)")
    Psh<-textInput("PShL", "Prior Shape Parameter: Variance Parameter", value = "")
    HTsh<- helpText("Introduce Prior shape Parameter. Example: 0.001")
    Psc<- textInput("PScL", "Prior Scale Parameter: Variance Parameter", value = "")
    HTsc<- helpText("Introduce prior scale parameter. Example: 0.001")
    PMeanCut<- textInput("PMeanLcut", "Prior Mean Vector: Cut Points", value = "")
    HTMCut<- helpText("Introduce prior mean vector (Dimension: alternatives - 2). Example: c(0,0)")
    PVarCut<- textInput("PVarLcut", "Prior Covariance Matrix: Cut Points", value = "")
    HTVCut<- helpText("Introduce prior covariances by row. It has to be symmetric. Example: c(1,0,0,1)")
    Psh1<-textInput("PShL", "Prior Shape Parameter: Dispersion Parameter", value = "")
    Psc1<- textInput("PScL", "Prior Scale Parameter: Dispersion Parameter", value = "")
    PMeanY<- textInput("PMeanLY", "Prior Mean Vector: Location Parameters Main Equation", value = "")
    HTMY<- helpText("Introduce prior mean vector of main equation. Example: c(0,0)")
    PVarY<- textInput("PVarLY", "Prior Covariance Matrix: Location Parameters Main Equation", value = "")
    HTVY<- helpText("Introduce prior covariances of main equation by row. It has to be symmetric. Example: c(100,0,0,100)")
    PMeanZ<- textInput("PMeanLZ", "Prior Mean Vector: Location Parameters Instrumental Equation", value = "")
    HTMZ<- helpText("Introduce prior mean vector of instrumental equation. Example: c(0,0)")
    PVarZ<- textInput("PVarLZ", "Prior Covariance Matrix: Location Parameters Instrumental Equation", value = "")
    HTVZ<- helpText("Introduce prior covariances of intrumental equation by row. It has to be symmetric. Example: c(100,0,0,100)")
    PshIW<-textInput("PShLIW", "Degrees of freedom: Inverse Wishart", value = "")
    HTshIW<- helpText("Introduce degrees of freedom Inverse Wishart distribution (integer greater than 2). Example: 3")
    PVarIW<- textInput("PVarLIW", "Scale Matrix: Inverse Wishart", value = "")
    HTVIW<- helpText("Introduce scale matrix Inverse Wishart distribution by row. It has to be a symmetric 2 x 2 matrix. Example: c(1,0,0,1)")
    Below<- textInput("Below11", "Left Censoring Point:", value = "")
    HTBelow<- helpText("The point at which the dependent variable is censored from below. The default is zero. To censor from above only, specify that below = -Inf. Example: 0")
    Above<- textInput("Above11", "Right Censoring Point:", value = "")
    HTAbove<- helpText("The point at which the dependent variable is censored from above. To censor from below only, use the default value of Inf. Example: Inf")
    tau<- textInput("tau11", "Quantile:", value = "")
    HTtau<- helpText("The quantile of interest. Must be between 0 and 1. The default value of 0.5 corresponds to median regression. Example: 0.5")
    
    FormulaM1B<- textInput("Formula1b", "Main Equation", value = "")
    HTFormMP<- helpText("Introduce Formula. Example: y~x11+x12+x13+x2 where y is the categorical dependent variable (y=1,2,...), x11, x12 and x13 are alternative specific independent variables (Warning: introduce first these variables!!!), and x2 is a Non-alternative specific variables. Model specification includes the constant term by default.")
    MultPnn<-textInput("MultPLnn", "Base Alternative", value = "")
    HTMultPnn<- helpText("Integer indicating the base alternative. Example: 3")
    MultPy<-textInput("MultPLy", "Number of choice categorical alternatives", value = "3")
    HTMultPy<- helpText("Integer indicating the number of choice alternatives. Example: 3")
    MultPXA<-textInput("MultPLXA", "Number of alternative specific variables", value = "1")
    HTMultPXA<- helpText("Integer indicating the number of alternative specific regressors. Example: 1. Warning: If there is none write 0!!!")
    MultPXD<-textInput("MultPLXD", "Number of Non-Alternative specific variables", value = "1")
    HTMultPXD<- helpText("Integer indicating the number of Non-alternative specific regressors without intercept inclusion. Example: 2. Warning: if there is none write 0!!!")
    
    PMeanMP<- textInput("PMeanLMP", "Prior Mean Vector: Location Parameters", value = "")
    HTMMP<- helpText("Introduce prior mean vector location parameters. One for each alternative regressor and p-1 for each Non-specific regressor (Warning: intercept is a Non-specific regresor!!!), where p is the number of alternatives. Example: c(0,0,0,0,0)")
    PVarMP<- textInput("PVarLMP", "Prior Covariance Matrix: Location Parameters", value = "")
    HTVMP<- helpText("Introduce prior covariances location parameters by row. It has to be symmetric. Example: c(100,0,0,0,100,0,0,0,100)")
    PshIWMP<-textInput("PShLIWMP", "Degrees of freedom: Inverse Wishart", value = "")
    HTshIWMP<- helpText("Introduce degrees of freedom Inverse Wishart distribution (integer greater than number of alternatives plus one). Example: 4")
    PVarIWMP<- textInput("PVarLIWMP", "Scale Matrix: Inverse Wishart", value = "")
    HTVIWMP<- helpText("Introduce scale matrix Inverse Wishart distribution by row. It has to be a symmetric (p-1) x (p-1) matrix where p is the number of alternatives. Example: c(1,0,0,1)")
    HTPtst<- helpText("Select degrees of freedom Multivariate t-student distribution (proposal distribution Metropolis-Hastings algorithm). Default: 6")
    it4<- selectInput("nu", "Degrees of Fredom: Multivariate t-Student:", 
                      choices = c("3", "4", "5", "6", "7", "8", "9", "10"), selected = "6")
    NegBinAlpha=numericInput('NegBinAlpha','alpha',value = 2.93,min = 0.01)
    
    NegBinBeta=numericInput('NegBinBeta','Beta',value = 1,min = 0.01)
    NegBinBetaCB=checkboxInput('NegBinBetaCB','Use default value for Beta? ',value = TRUE)
    NegBinBetaHT<- helpText(" The default value for this parameter is 1/sqrt(# Regressors) ")
    
    OprobitS=numericInput('OprobitS','S',value = 1,min = 0.01)
    OprobitSCB=checkboxInput('OprobitSCB','Use default value for S? ',value = TRUE)
    OprobitSHT<- helpText(" The default value for this parameter is 1/sqrt(# options-2) ")
    
    
    LogitTune=numericInput('LogitTune','Tune Parameter',value = 1.1,min = 0.01)
    
    #######Multivariate Models################
    #Simple Multivariate Equations
    HTEndVarNum<-helpText("Data set should have first endogeneous variables by columns starting at first column, then exogeneous variables. If it is required, you should include explicitly an intercept, that is, a column with 1's in your data set.")
    EndVarNumY<-textInput("EndVarNumnY", "Number Endogeneous Variables: m", value = "2")
    HTEndVarNumY<- helpText("Integer indicating number of endogeneous variables. Example: 2")
    ExVarNumX<-textInput("ExVarNumnX", "Number Exogeneous Variables: k", value = "3")
    HTExVarNumX<- helpText("Integer indicating number of exogeneous variables. Example: 3")
    DegreeFred<-textInput("DegFredMV", "Number of degrees of freedom in Inverse Wishart distribution", value = "5")
    HTDegreeFred<- helpText("This integer must be at least equal to m-1.")
    
    #SUR
    HTEndVarNumSUR<-helpText("Data set should have first endogeneous variables by columns starting at first column, then exogeneous variables by equation. If it is required, you should include explicitly intercepts, that is, a column with 1's in your data set for each equation.")
    EndVarNumYSUR<-textInput("EndVarNumnYSUR", "Number Endogeneous Variables (equations): m", value = "2")
    HTEndVarNumYSUR<- helpText("Integer indicating number of endogeneous variables. Example: 2")
    ExVarNumXSUR<-textInput("ExVarNumnXSUR", "TOTAL number Exogeneous Variables: k. This is the sum of ALL exogeneous variables, if it is required including intercepts, over ALL equations", value = "5")
    HTExVarNumXSUR<- helpText("Integer indicating TOTAL number of exogeneous variables. Example: 5")
    DegreeFredSUR<-textInput("DegFredMVSUR", "Number of degrees of freedom in Inverse Wishart distribution", value = "5")
    HTDegreeFredSUR<- helpText("This integer must be at least equal to m-1.")
    
    
    #Bivariate Probit:
    HTDataBVProbit<-helpText("Data set has to be organized by individual. First column should be individual identifier, then endogeneous variable, and then exogeneous variables. If it is required, you should include explicitly an intercept, that is, a column with 1's in your data set.")
    HTExVarNumXBVProbit<- helpText("Integer indicating number of exogeneous variables. Example: 3")
    ExVarNumXBVProbit<-textInput("ExVarNumnXBVProbit", "Number of exogeneous Variables: k. If it is required include intercept", value = "3")
    HTNBVProbit<-helpText("Integer indicating number of individuals. Example: 500")
    NBVProbit<-textInput("NnBVProbit", "Number of individuals: n.", value = "500")
    HTMeanRhoBVProbit<-helpText("Number indicating prior mean of correlation coefficient. It should be between -1 and 1. Example: 0")
    MeanRhoBVProbit<-textInput("MeanRhonBVProbit", "Prior mean correlation coefficient.", value = "0")
    HTVarRhoBVProbit<-helpText("Number indicating prior variance of correlation coefficient. Example: 100")
    VarRhoBVProbit<-textInput("VarRhonBVProbit", "Prior variance correlation coefficient.", value = "100")
    HTTuningBVProbit<-helpText("Tuning parameter in Random Walk Metropolis-Hastings algorithm. This parameter controls rejection rate. Example: 0.25")
    TuningBVProbit<-textInput("TuningnBVProbit", "Tuning parameter.", value = "0.25")
    
    
    #####Longitudinal Hierarchical Models#########
    FormulaM3A<- textInput("Formula3a", "Main Equation: Fixed Effects", value = "")
    HTForm3A<- helpText("Introduce Formula. Example: y~x1+x2 where y is the dependent variable, and x1 and x2 are independent variables associated with fixed effects. Model specification includes the constant term by default.")
    FormulaM3B<- textInput("Formula3b", "Main Equation: Random Effects", value = "")
    HTForm3B<- helpText("Introduce Formula. Example: ~x3+x4 where x3 and x4 are independent variables associated with random effects. Model specification includes the constant term by default.")
    Group<- textInput("VarGroup", "Name of grouping variable", value = "")
    HTGroup<- helpText("Introduce name of grouping variable in data. This defines the hierarchical structure in the model.")
    
    PMeanFix<- textInput("PMeanLFix", "Prior Mean Vector: Fixed Effects", value = "")
    HTMFix<- helpText("Introduce prior mean vector of fixed effects. Example: c(0,0)")
    PVarFix<- textInput("PVarLFix", "Prior Covariance Matrix: Fixed Effects", value = "")
    HTVFix<- helpText("Introduce prior covariances fixed effects by row. It has to be symmetric. Example: c(100,0,0,100)")
    PshIWMPRandom<-textInput("PShLIWMPRandom", "Shape parameter: Inverse Wishart", value = "")
    HTshIWMPRandom<- helpText("Introduce degrees of freedom Inverse Wishart distribution on variance for the random effects (integer greater or equal to number of regressors in the random components including the constant). Example: 3")
    PVarIWMPRandom<- textInput("PVarLIWMPRandom", "Scale Matrix: Inverse Wishart", value = "")
    HTVIWMPRandom<- helpText("Introduce scale matrix Inverse Wishart on variance for the random effects distribution by row. It has to be a symmetric matrix. Example: c(1,0,0,0,1,0,0,0,1)")
    PshHier<-textInput("PShLHier", "Prior Shape Paramete: Variance Parameter", value = "")
    HTshHier<- helpText("Introduce Prior shape Parameter. Example: 0.001")
    PscHier<- textInput("PScLHier", "Prior Scale Parameter: Variance Parameter", value = "")
    HTscHier<- helpText("Introduce prior scale parameter. Example: 0.001")
    
    
    #####Non-Parametric Models##############
    #####Bayesian Boostrap##########
    FormulaM42<- textInput("Formula42", "Main Equation", value = "")
    