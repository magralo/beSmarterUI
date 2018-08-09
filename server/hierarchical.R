getRegs=function(f){ #function that get the variables given a formula
  if(f==""){
    return("null")
  }
  nv=unlist(gregexpr(pattern ='~',f))
  f=substr(f, nv+1,nchar(f))
  regs="hola"
  desde=1
  i=1
  while(i<=nchar(f)){
    if(substr(f, i,i)=='+'){
      regs=c(regs,substr(f, desde,i-1))
      desde=i+1
    }
    i=i+1
  }
  regs=c(regs,substr(f, desde,nchar(f)))
  
  regs=regs[-1]
  return(regs)
}

isRegs=function(f,rn){
  rnf=getRegs(f)
  bool=sum((rnf==rn)*1)
  return(bool==length(rn))
}

sim=function(DF){
  if(nrow(DF)>1){
    DF1=DF
    DF1[lower.tri(DF, diag = TRUE)]= 0
    DF=DF1+t(DF1)+diag(diag(as.matrix(DF)))
  }
  return(DF)
}



#output$hierarchical <- renderUI({

    ######Data NavBar 3. Models #########
  dataInput3 <- reactive({
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    read.csv(inFile3$datapath, header=input$header3, sep=input$sep3)
  })
  
  ######Formulas NavBar 3. Models ######### 
  
  sumtextM3a <- reactive({
    model.formula(input$Formula3a,dataInput3())
  })
  
  sumtextM3b <- reactive({
    model.formula(input$Formula3b,dataInput3())
  })
  
  
  ####### Output UI #####
  
  ##### 3.1 ########
  output$ui31 <- renderUI({
    if (input$M31=='m310'){
      return()}
    else{    
      switch(input$M31,
             "m311" = isolate(wellPanel(fluidRow(column(3,FormulaM3A),column(9,HTForm3A)),
                                        fluidRow(column(3,FormulaM3B),column(9,HTForm3B)),
                                        fluidRow(column(3,Group),column(9,HTGroup)),
                                        #fluidRow(column(3,PMeanFix),column(3,PVarFix)),
                                        helpText("Introduce prior mean vector of fixed effects."),
                                        rHandsontableOutput("hotPmeanH"),
                                        helpText("Introduce prior covariance matrix of fixed effects."),
                                        rHandsontableOutput("hotPvarH"),
                                        fluidRow(column(3,PshHier),column(3,PscHier)),
                                        #fluidRow(column(3,HTMFix),column(3,HTVFix),column(3,HTshHier),column(3,HTscHier)),
                                        fluidRow(column(3,HTshHier),column(3,HTscHier)),
                                        #fluidRow(column(3,PshIWMPRandom),column(3,PVarIWMPRandom)),
                                        #fluidRow(column(3,HTshIWMPRandom),column(3,HTVIWMPRandom)),
                                        fluidRow(column(3,PshIWMPRandom)),
                                        fluidRow(column(3,HTshIWMPRandom)),
                                        helpText("Introduce scale matrix Inverse Wishart on variance for the random effects distribution."),
                                        rHandsontableOutput("hotPvarH2")
                                        
                                        )),
             "m312" = isolate(wellPanel(fluidRow(column(3,FormulaM3A),column(9,HTForm3A)),
                                        fluidRow(column(3,FormulaM3B),column(9,HTForm3B)),
                                        fluidRow(column(3,Group),column(9,HTGroup)),
                                        #fluidRow(column(3,PMeanFix),column(3,PVarFix),column(3,PshHier),column(3,PscHier)),
                                        helpText("Introduce prior mean vector of fixed effects."),
                                        rHandsontableOutput("hotPmeanH"),
                                        helpText("Introduce prior covariance matrix of fixed effects."),
                                        rHandsontableOutput("hotPvarH"),
                                        fluidRow(column(3,PshHier),column(3,PscHier)),
                                        #fluidRow(column(3,HTMFix),column(3,HTVFix),column(3,HTshHier),column(3,HTscHier)),
                                        fluidRow(column(3,HTshHier),column(3,HTscHier)),
                                        #fluidRow(column(3,PshIWMPRandom),column(3,PVarIWMPRandom)),
                                        #fluidRow(column(3,HTshIWMPRandom),column(3,HTVIWMPRandom)),
                                        fluidRow(column(3,PshIWMPRandom)),
                                        fluidRow(column(3,HTshIWMPRandom)),
                                        helpText("Introduce scale matrix Inverse Wishart on variance for the random effects distribution."),
                                        rHandsontableOutput("hotPvarH2")
                                        )),
             "m313" = isolate(wellPanel(fluidRow(column(3,FormulaM3A),column(9,HTForm3A)),
                                        fluidRow(column(3,FormulaM3B),column(9,HTForm3B)),
                                        fluidRow(column(3,Group),column(9,HTGroup)),
                                        #fluidRow(column(3,PMeanFix),column(3,PVarFix),column(3,PshHier),column(3,PscHier)),
                                        helpText("Introduce prior mean vector of fixed effects."),
                                        rHandsontableOutput("hotPmeanH"),
                                        helpText("Introduce prior covariance matrix of fixed effects."),
                                        rHandsontableOutput("hotPvarH"),
                                        fluidRow(column(3,PshHier),column(3,PscHier)),
                                        #fluidRow(column(3,HTMFix),column(3,HTVFix),column(3,HTshHier),column(3,HTscHier)),
                                        fluidRow(column(3,HTshHier),column(3,HTscHier)),
                                        #fluidRow(column(3,PshIWMPRandom),column(3,PVarIWMPRandom)),
                                        #fluidRow(column(3,HTshIWMPRandom),column(3,HTVIWMPRandom)),
                                        fluidRow(column(3,PshIWMPRandom)),
                                        fluidRow(column(3,HTshIWMPRandom)),
                                        helpText("Introduce scale matrix Inverse Wishart on variance for the random effects distribution."),
                                        rHandsontableOutput("hotPvarH2")
                                        ))
      )
    }
  })
  
  
  
  
  output$hotPmeanH=renderRHandsontable({
    
    if(is.null(input$hotPmeanH) ){
      nv = 1
      f=input$Formula3a
      
      DF=data.frame("Prior mean"=rep(0,nv))
      print(f)
      rownames(DF)=getRegs(f)
      
      
    }else{
      DF=hot_to_r(input$hotPmeanH)
      rn=rownames(DF)[-1]
      
      f=input$Formula3a
      if(!identical(rn,getRegs(f))){
        f=input$Formula3a
        nv=unlist(gregexpr(pattern ='~',f))
        
        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=rep(0,nv))
          rownames(DF)="null"
        }else{
          
          
            regs=getRegs(f)
            DF=data.frame("Prior mean"=rep(0,length(regs)+1))
            rownames(DF)=c("cte",regs)
          
          
          
        }
        
      }
      
    }
    DF$Prior.mean=as.numeric(DF$Prior.mean)
    
    rhandsontable(DF)%>% 
      hot_col("Prior.mean",format="0.01")
    
  })
  
 
  
  output$hotPvarH=renderRHandsontable({
    
    if(is.null(input$hotPvarH) ){
      nv = 1
      f=input$Formula3a
      
      DF=data.frame("Prior mean"=0)
      
        rownames(DF)=getRegs(f)
        colnames(DF)=getRegs(f)
      
      
    }else{
      DF=hot_to_r(input$hotPvarH)
      
        rn=rownames(DF)[-1]
      
      f=input$Formula3a
      if(!identical(rn,getRegs(f))){
        f=input$Formula3a
        nv=unlist(gregexpr(pattern ='~',f))
        
        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{
          
          regs=getRegs(f)
          
          
            DF=data.frame(diag(length(regs)+1))
            rownames(DF)=c("cte",getRegs(f))
            colnames(DF)=c("cte",getRegs(f))
          
          
        }
        
      }
      
    }
    DF=sim(DF)
    x=as.matrix(DF)
    bool=is.positive.semi.definite(x, tol=1e-8)
    if(bool){
      rv$warningSDP=""
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col < row) {
                 td.style.background = 'black';
                 } 
                 }")
    }else{
      showNotification("Watch out! the red covariance matrix is not positive semi definite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 
                 }")
      
    }
    
    })
  
  
  output$hotPvarH2=renderRHandsontable({
    
    if(is.null(input$hotPvarH2) ){
      nv = 1
      f=input$Formula3b
      
      DF=data.frame("Prior mean"=0)
      
      rownames(DF)=getRegs(f)
      colnames(DF)=getRegs(f)
      
      
    }else{
      DF=hot_to_r(input$hotPvarH2)
      
      rn=rownames(DF)[-1]
      
      f=input$Formula3b
      if(!identical(rn,getRegs(f))){
        f=input$Formula3b
        nv=unlist(gregexpr(pattern ='~',f))
        
        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{
          
          regs=getRegs(f)
          
          
          DF=data.frame(diag(length(regs)+1))
          rownames(DF)=c("cte",getRegs(f))
          colnames(DF)=c("cte",getRegs(f))
          
          
        }
        
      }
      
    }
    DF=sim(DF)
    x=as.matrix(DF)
    bool=is.positive.semi.definite(x, tol=1e-8)
    if(bool){
      rv$warningSDP=""
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col < row) {
                 td.style.background = 'black';
                 } 
                 }")
    }else{
      showNotification("Watch out! the red covariance matrix is not positive semi definite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 
                 }")
      
    }
    
    })
  
  # output$hotPvarH2=renderRHandsontable({
  #   
  #   if(is.null(input$hotPvarH2) ){
  #     nv = 1
  #     f=input$Formula3b
  #     
  #     DF=data.frame("Prior mean"=0)
  #     rownames(DF)=getRegs(f)
  #     colnames(DF)=getRegs(f)
  #   }else{
  #     DF=hot_to_r(input$hotPvarH2)
  #     rn=rownames(DF)
  #     f=input$Formula3b
  #     if(!identical(rn,getRegs(f))){
  #       f=input$Formula3b
  #       nv=unlist(gregexpr(pattern ='~',f))
  #       
  #       if(nv==-1){
  #         nv = 1
  #         DF=data.frame("Prior mean"=0)
  #         rownames(DF)="null"
  #       }else{
  #         
  #         regs=getRegs(f)
  #         DF=data.frame(diag(length(regs)))
  #         rownames(DF)=regs
  #         colnames(DF)=regs
  #         
  #       }
  #       
  #     }
  #     
  #   }
  #   rhandsontable(sim(DF))%>%
  #     hot_cols(renderer = "
  #              function (instance, td, row, col, prop, value, cellProperties) {
  #              Handsontable.renderers.TextRenderer.apply(this, arguments);
  #              if (col < row) {
  #              td.style.background = 'black';
  #              } 
  #              }")
  #   
  # })
  # 
  
  ######## 3.1 Models: Posterior Chains#########
  Posteriors31 <- eventReactive(input$goButton31, {
    showNotification("Trying to run the Gibbs sampler. It may be long, keep cool :)")
    
    # if(input$PMeanLFix==""){BmeanFix<-0}
    # else{
    #   tas<- sub("[c]",",",isolate(input$PMeanLFix))
    #   tas1<- sub("[(]",",",tas)
    #   tas2<- sub("[)]",",",tas1)
    #   s3<- strsplit(tas2,",")
    #   s4<- c(sapply(s3, as.numeric))
    #   BmeanFix<- s4[!is.na(s4)]
    # }
    BmeanFix<- hot_to_r(input$hotPmeanH)[,1]
    BvarFix<- solve(as.matrix(hot_to_r(input$hotPvarH)))
    # if(input$PVarLFix==""){BvarFix<-1.0e3}
    # else{
    #   tas3<- sub("[c]",",",isolate(input$PVarLFix))
    #   tas4<- sub("[(]",",",tas3)
    #   tas5<- sub("[)]",",",tas4)
    #   s5<- strsplit(tas5,",")
    #   s6<- c(sapply(s5, as.numeric))
    #   s6<- s6[!is.na(s6)]
    #   n1<- ncol(sumtextM3a()$X) 
    #   BvarFix<- solve(matrix(s6,byrow=TRUE,n1,n1))
    # }
    
    if(input$PShLHier==""){aHier<-0.001}
    else{
      aHier<- isolate(as.numeric(input$PShLHier))
    }
    
    if(input$PScLHier==""){bHier<-0.001}
    else{
      bHier<- isolate(as.numeric(input$PScLHier))
    }
    
    # if(input$PVarLIWMPRandom==""){VMPRandom<-diag(ncol(sumtextM3b()$X))}
    # else{
    #   tas3iwMP<- sub("[c]",",",isolate(input$PVarLIWMPRandom))
    #   tas4iwMP<- sub("[(]",",",tas3iwMP)
    #   tas5iwMP<- sub("[)]",",",tas4iwMP)
    #   s5iwMP<- strsplit(tas5iwMP,",")
    #   s6iwMP<- c(sapply(s5iwMP, as.numeric))
    #   s6iwMP<- s6iwMP[!is.na(s6iwMP)]
    #   nRan<- ncol(sumtextM3b()$X)
    #   VMPRandom<- matrix(s6iwMP,byrow=TRUE,nRan,nRan)
    # }
    VMPRandom<- (as.matrix(hot_to_r(input$hotPvarH2)))
    
    if(input$PShLIWMPRandom==""){nuMPRandom<-ncol(sumtextM3b()$X)}
    else{
      nuMPRandom<- isolate(as.numeric(input$PShLIWMPRandom))
    }
    
    if(input$M31=='m310')
      return()
    else {
      args <- switch(input$M31,
                     "m311" = list(fixed=input$Formula3a, random=input$Formula3b, group=input$VarGroup, data=dataInput3(), 
                                   burnin=input$burnin, mcmc=input$it, thin=as.numeric(input$keep), verbose=0, seed=NA,
                                   beta.start=NA, sigma2.start=NA, Vb.start=NA, mubeta=BmeanFix, Vbeta=BvarFix, r=nuMPRandom, R=VMPRandom, nu=aHier, delta=bHier),
                     "m312" = list(fixed=input$Formula3a, random=input$Formula3b, group=input$VarGroup, data=dataInput3(), 
                                   burnin=input$burnin, mcmc=input$it, thin=as.numeric(input$keep), verbose=0, seed=NA,
                                   beta.start=NA, sigma2.start=NA, Vb.start=NA, mubeta=BmeanFix, Vbeta=BvarFix, r=nuMPRandom, R=VMPRandom, nu=aHier, delta=bHier, FixOD=0),
                     "m313" = list(fixed=input$Formula3a, random=input$Formula3b, group=input$VarGroup, data=dataInput3(), 
                                   burnin=input$burnin, mcmc=input$it, thin=as.numeric(input$keep), verbose=0, seed=NA,
                                   beta.start=NA, sigma2.start=NA, Vb.start=NA, mubeta=BmeanFix, Vbeta=BvarFix, r=nuMPRandom, R=VMPRandom, nu=aHier, delta=bHier, FixOD=0)
      )}
    
    if (input$M31 == 'm311') {
      do.call(MCMChregress, args)}
    else {
      if (input$M31 == 'm312') {
        do.call(MCMChlogit, args)}
      else {
        if (input$M31 == 'm313') {
          do.call(MCMChpoisson, args)}
      }
    }
    showNotification("Done!")
    
  })
  
   ####### 3.1 Models: Download Posterior Chains##########
  
  output$download31 <- downloadHandler(
    filename = function() { 
      paste("Posterior Chains", '.csv', sep='') 
    },
    
    content = function(file) {
      
      if(input$M31=='m310')
        content<- return()
      
      switch(input$M31,
             "m311" = post31<- cbind(Posteriors31()$mcmc),
             "m312" = post31<- cbind(Posteriors31()$mcmc),
             "m313" = post31<- cbind(Posteriors31()$mcmc))      
      write.csv(post31, file)
    }
  )
  
  
  ####### 3.1 Models: Summary Posterior Chains##########
  
  output$summary31 <- renderPrint({
    if(input$M31=='m310'){
      return()}   
    else{
      switch(input$M31,
             "m311" = SumDiagHier(Posteriors31()$mcmc),
             "m312" = SumDiagHier(Posteriors31()$mcmc),
             "m313" = SumDiagHier(Posteriors31()$mcmc))
    }
  })
  
  
  
  ####### 3.1 Models: Graphs Posterior Chains##########  
  output$plot31 <- renderPlot({
    unlink(file.path(path,"Posterior Graphs"),recursive=TRUE)
    dir.create(file.path(path,"Posterior Graphs"),showWarnings = FALSE)
    setwd(file.path(path,"Posterior Graphs"))
    
    graphs31<- function(post31){
      nc<-ncol(post31)
      for (i in 1:nc) {
        pdf(paste("Density Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot(post31[,i])
        dev.off()
        setEPS()
        postscript(paste("Density Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot(post31[,i])
        dev.off()
        pdf(paste("Trace Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.trace(post31[,i])
        dev.off()
        setEPS()
        postscript(paste("Trace Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.trace(post31[,i])
        dev.off()
        pdf(paste("Autocorrelation Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.corr(post31[,i])
        dev.off()
        setEPS()
        postscript(paste("Autocorrelation Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.corr(post31[,i])
        dev.off()
      }
    }
    switch(input$M31,
           "m311" = graphs31(Posteriors31()$mcmc),
           "m312" = graphs31(Posteriors31()$mcmc),
           "m313" = graphs31(Posteriors31()$mcmc))
    setwd("..")
  })
  
  output$multiDownload31 <- downloadHandler(
    filename = function() {
      paste("Posterior Graphs", "zip", sep=".")
    },
    
    content = function(file) {
      zip(zipfile=file, files='Posterior Graphs')
    },
    contentType = "application/zip"
  )
  
  
#})

