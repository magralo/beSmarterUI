###AUXiliar functions
MC3.REGm=function (all.y, all.x, num.its, M0.var = NULL, M0.out = NULL, 
                   outs.list = NULL, outliers = TRUE, PI = 0.1 * (length(all.y) < 
                                                                    50) + 0.02 * (length(all.y) >= 50), K = 7, nu = NULL, 
                   lambda = NULL, phi = NULL) 
{
  num.its=max(100,num.its)
  cl <- match.call()
  all.x <- data.frame(all.x)
  if (is.null(M0.var)) 
    M0.var <- rep(TRUE, ncol(all.x))
  if ((sum(M0.var) == 0)) 
    stop("\nInput error:  M0.var cannot be null model")
  if (outliers) {
    if (is.null(outs.list)) {
      outs.list <- out.ltsreg(all.x, all.y, 2)
    }
    if (length(outs.list) == 0) 
      outliers <- FALSE
  }
  if (outliers) {
    if (is.null(M0.out)) {
      M0.out <- rep(TRUE, length(outs.list))
    }
  }
  if ((length(M0.out) != length(outs.list)) || (length(M0.var) != 
                                                dim(all.x)[2])) 
    stop("\nInput error: M0.*** is not the right length")
  if (is.null(nu) | is.null(lambda) | is.null(phi)) {
    r2 <- summary(lm(all.y ~ ., data = data.frame(all.y, 
                                                  all.x)))$r.squared
    if (r2 < 0.9) {
      new.nu <- 2.58
      new.lambda <- 0.28
      new.phi <- 2.85
    }
    else {
      new.nu <- 0.2
      new.lambda <- 0.1684
      new.phi <- 9.2
    }
    if (is.null(nu)) 
      nu <- new.nu
    if (is.null(lambda)) 
      lambda <- new.lambda
    if (is.null(phi)) 
      phi <- new.phi
  }
  var.names <- colnames(all.x)
  var.numbers <- 1:ncol(all.x)
  outlier.numbers <- outs.list
  Ys <- scale(all.y)
  Xs <- scale(all.x)
  g <- list()
  g$flag <- 1
  g$M0.var <- M0.var
  g$M0.out <- M0.out
  if (is.null(outs.list)) 
    g$outcnt <- 0
  else g$outcnt <- sum(outs.list)
  g$big.list <- matrix(0, 1, 4)
  g$big.list[1, 1] <- sum(2^((0:(length(g$M0.var) - 1))[g$M0.var])) + 
    1
  if (sum(g$M0.out) != 0) 
    g$big.list[1, 2] <- sum(2^((0:(length(g$M0.out) - 1))[g$M0.out])) + 
    1
  else g$big.list[1, 2] <- 1
  if (g$outcnt != 0) 
    g$big.list[1, 3] <- (dim(Ys)[1] - sum(g$M0.out)) * log(1 - 
                                                             PI) + sum(g$M0.out) * log(PI) + MC3.REG.logpost(Ys, 
                                                                                                             Xs, g$M0.var, sum(g$M0.var), outs.list[g$M0.out], 
                                                                                                             K, nu, lambda, phi)
  else g$big.list[1, 3] <- MC3.REG.logpost(Ys, Xs, g$M0.var, 
                                           sum(g$M0.var), outs.list[g$M0.out], K, nu, lambda, phi)
  withProgress(message = 'Making calculations', value = 0, {
    for (i in 1:num.its){ 
      
      
      g <- For.MC3.REG(i, g, Ys, Xs, PI, K,  nu, lambda, phi, outs.list)
      
      incProgress(1/i, detail = paste('Doing iteration', i))
    }
    
  })
  var.matrix <- matrix(as.logical(rep(g$big.list[, 1] - 1, 
                                      rep(length(g$M0.var), length(g$big.list[, 1])))%/%2^(0:(length(g$M0.var) - 
                                                                                                1))%%2), ncol = length(g$M0.var), byrow = TRUE)
  n.var <- length(g$M0.var)
  ndx <- 1:n.var
  Xn <- rep("X", n.var)
  labs <- paste(Xn, ndx, sep = "")
  colnames(var.matrix) <- var.names
  postprob <- exp(g$big.list[, 3])/(sum(exp(g$big.list[, 3])))
  visits <- g$big.list[, 4]
  if (length(outs.list) != 0) {
    out.matrix <- matrix(as.logical(rep(g$big.list[, 2] - 
                                          1, rep(length(outs.list), length(g$big.list[, 2])))%/%2^(0:(length(outs.list) - 
                                                                                                        1))%%2), ncol = length(outs.list), byrow = TRUE)
    colnames(out.matrix) <- outs.list
  }
  else out.matrix <- NULL
  ordr <- order(-postprob)
  result <- list(post.prob = postprob[ordr], variables = var.matrix[ordr, 
                                                                    , drop = FALSE], outliers = out.matrix[ordr, , drop = FALSE], 
                 visit.count = visits[ordr], var.numbers = var.numbers, 
                 outlier.numbers = outlier.numbers, var.names = var.names, 
                 n.models = length(postprob[ordr]), PI = PI, K = K, nu = nu, 
                 lambda = lambda, phi = phi, call = cl)
  class(result) <- "mc3"
  return(result)
}

ivbmaM=function (Y, X, Z, W, s = 1000, b = round(s/10), full = FALSE, 
                 odens = min(c(5000, s - b)), print.every = round(s/10), run.diagnostics = FALSE) 
{
  
  
  D <- NULL
  D$Y <- Y
  D$U <- cbind(as.matrix(Z), as.matrix(W))
  D$Z <- as.matrix(Z)
  D$V <- cbind(as.matrix(X), as.matrix(W))
  D$W <- as.matrix(W)
  D$X <- as.matrix(X)
  D$r <- dim(D$X)[2]
  theta <- ivbma:::ivbma.init(D, full)
  results <-ivbma::: ivbma.results.init(D, odens, run.diagnostics)
  which.save <- round(seq(b + 1, s, length = odens))
  save.loc <- 1
  next.save <- which.save[save.loc]
  #print(paste("Running IVBMA for", s, "iterations", Sys.time()))
  withProgress(message = 'Making calculations', value = 0, {
    for (i in 1:s) {
      #if (i%%print.every == 0) 
      #print(paste("On Iteration", i, Sys.time()))
      theta <-ivbma::: ivbma.sample.theta(theta, D, full)
      if (i == next.save) {
        results$rho[save.loc, ] <- theta$rho
        results$lambda[, , save.loc] <- theta$lambda
        results$Sigma[, , save.loc] <- theta$Sigma
        results$L[save.loc, ] <- theta$L
        results$M[, , save.loc] <- theta$M
        save.loc <- save.loc + 1
        next.save <- which.save[save.loc]
        if (run.diagnostics) {
          dd <- ivbma.diagnostics(theta, D)
          results$Sargan <- results$Sargan + dd[1]/odens
          results$Bayesian.Sargan[i] <- dd[2]
        }
      }
      if (i > b) {
        results$rho.bar <- results$rho.bar + theta$rho/(s - 
                                                          b)
        results$lambda.bar <- results$lambda.bar + theta$lambda/(s - 
                                                                   b)
        results$L.bar <- results$L.bar + theta$L/(s - b)
        results$M.bar <- results$M.bar + theta$M/(s - b)
        results$Sigma.bar <- results$Sigma.bar + theta$Sigma/(s - 
                                                                b)
      }
      incProgress(1/i, detail = paste('Doing iteration', i))
    }
  })
  class(results) <- c("ivbma", class(results))
  return(results)
}


##load librarys

##Upload user data
fileBMA<- fileInput('fileBMA', 'Choose File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv'))

filechBMA<- checkboxInput('headerBMA', 'Header', TRUE)


rbBMA<- radioButtons('sepBMA', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     selected=',')
uploadBMA=fluidRow(column(6,fileBMA),column(3,filechBMA),column(3,rbBMA))


fileBMAI<- fileInput('fileBMAI', 'Choose File (Instruments)',
                     accept=c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv'))

filechBMAI<- checkboxInput('headerBMAI', 'Header', TRUE)


rbBMAI<- radioButtons('sepBMAI', 'Separator',
                      c(Comma=',',
                        Semicolon=';',
                        Tab='\t'),
                      selected=',')
uploadBMAI=fluidRow(column(6,fileBMAI),column(3,filechBMAI),column(3,rbBMAI))
numEnd=numericInput('numEnd', '# of Endogenous variables', value=1, min = 1, max = 20, step = 1)
iterMC3=numericInput('iterMC3', '# of Iterations', value=10000, min = 100, step = 1000)
iterMC3en=numericInput('iterMC3en', '# of Iterations', value=10000, min = 100, step = 1000)




summaryBMA=dataTableOutput('summaryBMA')
summaryBMA2=dataTableOutput('summaryBMA2')

#conditional for normal models

normaltcond<- uiOutput("normaltcond")#because inputs may vary according to the selected type

normalT=radioButtons("normalT", "Which type do you want to perform?",
                     c("BIC"="1",
                       "MC3"="2", 
                       "Intrumental variable"="3")
)
goBMAN1<- actionButton("goBMAN1", "Go!")
goBMAN2<- actionButton("goBMAN2", "Go!")
goBMAN3<- actionButton("goBMAN3", "Go!")

BMA_OR=numericInput('BMA_OR','OR',value = 50,min = 10)

CNBMA=fluidPage( normalT,uploadBMA,normaltcond,br(),br(),summaryBMA)



#conditional for logit models

goBMAL<- actionButton("goBMAL", "Go!")
CLBMA=fluidPage(uploadBMA,br(),BMA_OR,h6("logit"),goBMAL,br(),br(),summaryBMA)

#conditional for Poisson models
goBMAP<- actionButton("goBMAP", "Go!")
CPBMA=fluidPage(uploadBMA,br(),BMA_OR,h6("Poisson"),goBMAP,br(),br(),summaryBMA)

#conditional for GAMMA models
goBMAG<- actionButton("goBMAG", "Go!")
CGBMA=fluidPage(uploadBMA,br(),BMA_OR,h6("Gamma"),goBMAG,br(),br(),summaryBMA)

DLBIC<- downloadButton('DLBIC', 'Download results using BIC')
DLMC3<- downloadButton('DLMC3', 'Download results using MC3')
DLMC3en<- downloadButton('DLMC3en', 'Download results using MC3')
normalDW<- uiOutput("normalDW")#because inputs may vary according to the selected type




  ##CONDITIONALS UI
  output$normalDW <- renderUI({
    
    switch(input$normalT,
           "1"=DLBIC,
           "2"=DLMC3,
           "3"=DLMC3en
    )
    
  })
  
  
  output$CONDBMA <- renderUI({
    switch(input$radioBMA,
           "NS"=fluidPage(),
           "NBMA"=fluidPage(CNBMA,br(),br(),normalDW),
           "LBMA"=fluidPage(CLBMA,br(),br(),DLBIC),
           "GBMA"=fluidPage(CGBMA,br(),br(),DLBIC),
           "PBMA"=fluidPage(CPBMA,br(),br(),DLBIC)
    )   
  })
  
  
  
  output$normaltcond <- renderUI({
    
    switch(input$normalT,
           "1"=fluidPage(br(),BMA_OR,h6("Using BIC approximation"),
                         goBMAN1,
                         br()),
           "2"=fluidPage(h6("Performing MC3"),
                         iterMC3,
                         goBMAN2,
                         br()),
           "3"=fluidPage(uploadBMAI,numEnd,h6("Intrumental variable"),
                         withMathJax(helpText(" $$Y=[X W]\\beta + \\epsilon_y$$"),
                                     helpText(" $$X=[Z W]\\delta + \\epsilon_x$$"),
                                     helpText(" First file should include a matrix [Y X W], the second one includes only the instruments [Z]")),
                         iterMC3en,
                         goBMAN3,
                         summaryBMA2,br()
           )
    )   
  })
  
  ####Lectura de datos
  dataInputBMA <- reactive({
    inFile1 <- input$fileBMA
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header=input$headerBMA, sep=input$sepBMA)
  })
  dataInputBMAI <- reactive({
    inFile1 <- input$fileBMAI
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header=input$headerBMAI, sep=input$sepBMAI)
  })
  
  
  #####variable de comunicaci??n
  rvBMA <- reactiveValues(
    results=NULL,
    results2=NULL,
    obj=NULL,
    objBIC=NULL,
    objMC3=NULL,
    objEN=NULL,
    type=NULL
  )
  
  output$DLBIC <- downloadHandler(
    filename = 'BIC.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("BMAmodel.csv", "results.csv")
      obj=rvBMA$obj
      aux=summary(obj)
      #print(dim(aux))
      #print(nrow(aux)-5))
      aux11=matrix(as.numeric(aux[1:(nrow(aux)-5),1:3]),ncol=3)
      colnames(aux11)=c("PIP","EV","SD")
      rownames(aux11)=rownames(aux)[1:(nrow(aux)-5)]
      write.csv(aux11, file = fs[1], sep =",")
      write.csv(rbind(obj$which,obj$postprob), file = fs[2], sep =",")
      
      print (fs)
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )
  
  output$DLMC3 <- downloadHandler(
    filename = 'MC3.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("BMAmodel.csv", "results.csv")
      
      write.csv(rvBMA$results, file = fs[1], sep =",")
      obj=rvBMA$objMC3
      aux=cbind(obj$variables,obj$post.prob)
      colnames(aux)=c(colnames(obj$variables),"PostProb")
      rownames(aux)=paste('model',1:nrow(aux))
      write.csv(aux, file = fs[2], sep =",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )
  
  output$DLMC3en <- downloadHandler(
    filename = 'MC3iv.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("BMAmodel1.csv", "BMAmodel2.csv","chains.csv")
      
      write.csv(rvBMA$results, file = fs[1], sep =",")
      write.csv(rvBMA$results2, file = fs[2], sep =",")
      obj=rvBMA$objEN
      aux=rbind(obj$lambda[,1,],
                t(obj$rho),
                obj$Sigma[1,1,],
                obj$Sigma[1,2,],
                obj$Sigma[2,2,])
      rownames(aux)=c(paste('lambda',1:nrow(obj$lambda[,1,])),paste('rho',1:nrow(t(obj$rho))),"Sigma_11","Sigma_12","Sigma_22")
      write.csv(aux, file = fs[3], sep =",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )
  
  
  
  
  output$summaryBMA <- renderDataTable({ 
    if(!is.null(rvBMA$results)){
      
      A=rvBMA$results
      
      dt=datatable(A,options = list(
        pageLength = nrow(A)),caption = rvBMA$type)
      dt
      
    }else{
      a=matrix(c("No results yet","Upload data and click the go button"))
      dt=datatable(a)
      dt
      
      
    }
    
  })
  
  output$summaryBMA2 <- renderDataTable({ 
    if(!is.null(rvBMA$results2)){
      
      A=rvBMA$results2
      
      dt=datatable(A,options = list(
        pageLength = nrow(A)),caption = "Gaussian family Instrumental variable (Objective model)")
      dt
      
    }else{
      
      
    }
    
  })
  
  observeEvent(input$goBMAN1, { 
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Gaussian family using BIC approximation"
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bicreg(x=X, y=Y, strict = FALSE, OR = input$BMA_OR,maxCol = (hasta+1))
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
      rvBMA$objBIC=aux
    }
  })
  observeEvent(input$goBMAN2, { 
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Gaussian family using MC3"
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      y=Y
      x=X
      print(sapply(X,mean))
      print(mean(Y))
      ayuda=MC3.REGm(Y, X, num.its=input$iterMC3,rep(TRUE,dim(X)[2]), outliers = FALSE )
      
      pmp=ayuda$post.prob
      
      which=ayuda$variables
      
      nModels=ayuda$n.models
      
      nreg=ncol(x)
      betaModels=matrix(rep(0,nreg*nModels),nModels,nreg)
      
      BMAbeta=rep(0,nreg)
      BMAvar=rep(0,nreg)
      PIP=BMAbeta
      
      for (i in 1:nModels){
        included=which[i,]
        xred=as.matrix(x[,included])
        
        model=lm(y ~ 0+xred)
        beta=model$coefficients##lm ()
        betaModels[i,included]=beta
        BMAbeta[included]=BMAbeta[included]+beta*pmp[i]#here a i get the bma point estimate
        PIP[included]=PIP[included]+pmp[i]###here i get the posterior inclusion probability
        
        ###This is following the formula for the variance
        esd=coef(summary(model))[, "Std. Error"]
        esd=esd^2
        suma=esd+beta^2
        BMAvar[included]=BMAvar[included]+suma*pmp[i]
        ###
      }
      
      BMAvar=BMAvar-BMAbeta^2##E(x^2)-E(x)^2
      
      BMAsd=(BMAvar^0.5)
      
      table=cbind(PIP,BMAbeta,BMAsd)
      colnames(table)=c("PIP","Est Value","Sd")
      rownames(table)=colnames(x)
      
      rvBMA$results=table
      rvBMA$objMC3=ayuda
    }
  })
  
  
  observeEvent(input$goBMAN3, { 
    if (is.null(dataInputBMA())||is.null(dataInputBMAI())){
      return()
    }else{
      rvBMA$type="Gaussian family Instrumental variable (Instrumental variable stage)"
      YX=dataInputBMA()
      Y=YX[,1]
      numEndo=input$numEnd
      
      X=YX[,2:(1+numEndo)]
      W=YX[,-(1:(1+numEndo))]
      Z=dataInputBMAI()
      
      aux <- ivbmaM(Y, X, Z, W, s = input$iterMC3en)
      
      PIP2=aux$M.bar
      exp2=aux$lambda.bar
      
      tabla2 = PIP2[,1]
      cols=""
      for (i in 1 :ncol(PIP2)){
        cols=c(cols,paste("EV",i), paste("PIP",i))
        tabla2=cbind(tabla2,exp2[,i],PIP2[,i])
      }
      tabla2=tabla2[,-1]
      
      
      colnames(tabla2)=cols[-1]
      rownames(tabla2)=c(colnames(Z),colnames(YX)[-(1:(1+numEndo))])
      
      
      PIP=aux$L.bar
      exp=aux$rho.bar
      
      tabla1=cbind(exp,PIP)
      
      colnames(tabla1) = c("EV","PIP")
      rownames(tabla1) = colnames(YX)[-1]
      #hasta=dim(X)[2]
      #aux <- bicreg(x=X, y=Y, strict = FALSE, OR = 50,maxCol = (hasta+1))
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=tabla2#the generic table goes after the conditional panel
      rvBMA$results2=tabla1
      rvBMA$objEN=aux
    }
  })
  
  
  
  
  observeEvent(input$goBMAL, { 
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Logistic using BIC approximation"
      if ((sum(dataInputBMA()[,1]==1) +sum(dataInputBMA()[,1]==0))<length(dataInputBMA()[,1])){
        rvBMA$results<-matrix("Responce variable is not as expected")
        return()
      }
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bic.glm(x=X, y=Y, strict = FALSE, OR = input$BMA_OR,maxCol = (hasta+1), glm.family=binomial())
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
      
    }
  })
  observeEvent(input$goBMAP, { 
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Poisson using BIC approximation"
      if (sum(is.integer(dataInputBMA()[,1]) )<1){
        rvBMA$results<-matrix("Responce variable is not as expected")
        return()
      }
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bic.glm(x=X, y=Y, strict = FALSE, OR = input$BMA_OR,maxCol = (hasta+1), glm.family=poisson())
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
    }
  })
  observeEvent(input$goBMAG, { 
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Gamma using BIC approximation"
      if (sum(dataInputBMA()[,1]>=0)<length(dataInputBMA()[,1])){
        rvBMA$results<-matrix("Responce variable is not as expected")
        return()
      }
      
      
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bic.glm(x=X, y=Y, strict = FALSE, OR = input$BMA_OR,maxCol = (hasta+1),  glm.family=Gamma(link="log"))
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
    }
  })
  
  
  
  
  
  
  
  
  
  
