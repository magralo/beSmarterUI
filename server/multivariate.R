getRegs=function(f){
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

sim=function(DF){
  if(nrow(DF)>1){
    DF1=DF
    DF1[lower.tri(DF, diag = TRUE)]= 0
    DF=DF1+t(DF1)+diag(diag(as.matrix(DF)))
  }
  return(DF)
}

ListSUR<-function(dat,m,ki){
  st<-c(m,m+cumsum(ki))
  listreg<-NULL
  for(i in 1:m){
    listreg[[i]]=list(y=dat[,i],X=as.matrix(dat[,c((st[i]+1):st[i+1])]))
  }
  return(listreg)
}

  ######Data NavBar 2. Models #########
  dataInput2 <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    read.csv(inFile2$datapath, header=input$header2, sep=input$sep2)
  })
  
  
  ######Formulas NavBar 2. Models ######### 
  
  sumtextM2a <- reactive({
    model.formula(input$Formula2a,dataInput2())
  })
  
  sumtextM2b <- reactive({
    model.formula1(input$Formula2b,dataInput2())
  })
  
  
  
  ####### Output UI #####
  
  ##### 2.1 ########
  output$ui21 <- renderUI({
    if (input$M21=='m210'){
      return()}
    else{    
      switch(input$M21,
             "m211" = isolate(wellPanel(fluidRow(column(12,HTEndVarNum)),
                                        fluidRow(column(3,EndVarNumY),column(3,ExVarNumX)),
                                        fluidRow(column(3,HTEndVarNumY),column(3,HTExVarNumX)),
                                        helpText("Prior mean matrix (k x m): Introduce prior mean location parameters. Each equation per column."),
                                        rHandsontableOutput("hotPMeanMV"),
                                        helpText("Prior covariance matrix (k x k): Introduce prior covariance matrix. It should be symmetric positive semidefinite."),
                                        rHandsontableOutput("hotPVarMV"),
                                        fluidRow(column(3,DegreeFred)),
                                        fluidRow(column(3,HTDegreeFred)),
                                        helpText("Scale matrix (m x m): Introduce prior scale matrix for inverse Wishart distribution. It should be symmetric positive semidefinite."),
                                        rHandsontableOutput("hotPVarMVWis")
   )),
   "m212" = isolate(wellPanel(fluidRow(column(12,HTEndVarNumSUR)),
                              fluidRow(column(3,EndVarNumYSUR),column(3,ExVarNumXSUR)),
                              fluidRow(column(3,HTEndVarNumYSUR),column(3,HTExVarNumXSUR)),
                              helpText("Introduce number of regressors by equation (ki). You should take into account intercepts, if you included them in your data set."),
                              rHandsontableOutput("RegEquat"),
                              helpText("Prior mean vector (1 x k): Introduce prior mean location parameters by equation."),
                              rHandsontableOutput("hotPMeanMVSUR"),
                              helpText("Prior covariance matrix (k x k): Introduce prior covariance matrix. It should be symmetric positive semidefinite."),
                              rHandsontableOutput("hotPVarMVSUR"),
                              fluidRow(column(3,DegreeFredSUR)),
                              fluidRow(column(3,HTDegreeFredSUR)),
                              helpText("Scale matrix (m x m): Introduce prior scale matrix for inverse Wishart distribution. It should be symmetric positive semidefinite."),
                              rHandsontableOutput("hotPVarMVWisSUR")
                              
   )),
   "m213" = isolate(wellPanel(fluidRow(column(3,FormulaM2A),column(9,HTFormy)),
                              fluidRow(column(3,FormulaM2B),column(9,HTFormZ)),
                              helpText("Introduce prior mean vector location parameters (Main equation)"),
                              rHandsontableOutput("hotPmeanY"),
                              helpText("Introduce prior covariances location parameters by row. It has to be symmetric (Main equation)"),
                              rHandsontableOutput("hotPvarY"), 
                              helpText("Introduce prior mean vector location parameters (Instrumental equation)"),
                              rHandsontableOutput("hotPmeanZ"),
                              helpText("Introduce prior covariances location parameters by row. It has to be symmetric (Instrumental equation)"),
                              rHandsontableOutput("hotPvarZ"), 
                              helpText("Introduce scale matrix Inverse Wishart distribution"),
                              rHandsontableOutput("hotPIW"),
                              fluidRow(column(3,PshIW)),
                              fluidRow(column(3,HTshIW)))),
   "m214" = isolate(wellPanel(fluidRow(column(12,HTDataBVProbit)),
                              fluidRow(column(3,NBVProbit),column(3,ExVarNumXBVProbit)),
                              fluidRow(column(3,HTNBVProbit),column(3,HTExVarNumXBVProbit)),
                              helpText("Introduce prior mean vector location parameters"),
                              rHandsontableOutput("hotPmeanBVProbit"),
                              helpText("Introduce prior covariances location parameters. It has to be symmetric"),
                              rHandsontableOutput("hotPvarBVProbit"),
                              fluidRow(column(3,MeanRhoBVProbit),column(3,VarRhoBVProbit)),
                              fluidRow(column(3,HTMeanRhoBVProbit),column(3,HTVarRhoBVProbit)),
                              fluidRow(column(3,TuningBVProbit)),
                              fluidRow(column(3,HTTuningBVProbit))))
)
    }
  })

  #######SUR#########  
  output$RegEquat=renderRHandsontable({
    if(is.null(input$RegEquat)){
      nYs<-as.numeric(input$EndVarNumnYSUR)
      DF<-data.frame(matrix(rep(2,input$EndVarNumnYSUR),1,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-"ki"
    }else{
      DF<-hot_to_r(input$RegEquat)
      nYs<-as.numeric(input$EndVarNumnYSUR)
      if(ncol(DF)!=nYs){
        DF<-data.frame(matrix(rep(2,input$EndVarNumnYSUR),1,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-"ki"
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    check<-sum(DF,na.rm=TRUE)!=input$ExVarNumnXSUR
    if(check){
      showNotification("Watch out! The red table means that sum of regressors by equation (ki) is not equal to TOTAL number of regressors (k)")
      rhandsontable(DF)%>%
        hot_cols(colnames(DF),format="0") %>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 td.style.background = 'red';
                 }")
    }else{
      showNotification("Good! The green table means that sum of regressors by equation (ki) is equal to TOTAL number of regressors (k). You can continue!")
      rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0") %>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 td.style.background = 'green';
                 }")
    }
  })
  
  output$hotPMeanMVSUR=renderRHandsontable({
    if(is.null(input$hotPMeanMVSUR)){
      nbetas<-as.numeric(sum(hot_to_r(input$RegEquat)))
      nYs<-as.numeric(input$EndVarNumnYSUR)
      ki<-as.numeric(hot_to_r(input$RegEquat))
      DF<-data.frame(matrix(0,1,nbetas))
      colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
    }else{
      DF<-hot_to_r(input$hotPMeanMVSUR)
      nbetas<-as.numeric(sum(hot_to_r(input$RegEquat)))
      if(ncol(DF)!=nbetas){
        nYs<-as.numeric(input$EndVarNumnYSUR)
        ki<-as.numeric(hot_to_r(input$RegEquat))
        DF<-data.frame(matrix(0,1,nbetas))
        colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0.000")
    })
  
  output$hotPVarMVSUR=renderRHandsontable({
    if(is.null(input$hotPVarMVSUR)){
      nbetas<-as.numeric(sum(hot_to_r(input$RegEquat)))
      DF<-data.frame(diag(100.00,nbetas))
      nYs<-as.numeric(input$EndVarNumnYSUR)
      ki<-as.numeric(hot_to_r(input$RegEquat))
      colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
      rownames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMVSUR)
      nbetas<-as.numeric(sum(hot_to_r(input$RegEquat)))
      nYs<-as.numeric(input$EndVarNumnYSUR)
      ki<-as.numeric(hot_to_r(input$RegEquat))
      if(ncol(DF)!=nbetas){
        DF<-data.frame(diag(100.00,nbetas))
        colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
        rownames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 }")
    }
    })
  
  output$hotPVarMVWisSUR=renderRHandsontable({
    if(is.null(input$hotPVarMVWisSUR)){
      nYs<-as.numeric(input$EndVarNumnYSUR)
      nu<-as.numeric(input$DegFredMVSUR)
      DF<-data.frame(nu*diag(1.00,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("y",1:nYs,sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMVWisSUR)
      nYs<-as.numeric(input$EndVarNumnYSUR)
      nu<-as.numeric(input$DegFredMVSUR)
      if(ncol(DF)!=nYs){
        DF<-data.frame(nu*diag(1.00,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("y",1:nYs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red scale matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 }")
    }
    })
 
  ##########Multivariate regression######### 
  output$hotPMeanMV=renderRHandsontable({
    if(is.null(input$hotPMeanMV)){
      nYs<-as.numeric(input$EndVarNumnY)
      nXs<-as.numeric(input$ExVarNumnX)
      DF<-data.frame(matrix(0.000,nXs,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("x",1:nXs,sep="")
    }else{
      DF<-hot_to_r(input$hotPMeanMV)
      nYs<-as.numeric(input$EndVarNumnY)
      nXs<-as.numeric(input$ExVarNumnX)
      if(ncol(DF)!=nYs | nrow(DF)!=nXs){
        DF<-data.frame(matrix(0.000,nXs,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("x",1:nXs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0.000")
  })
  
  output$hotPVarMV=renderRHandsontable({
    if(is.null(input$hotPVarMV)){
      nXs<-as.numeric(input$ExVarNumnX)
      DF<-data.frame(diag(100.00,nXs))
      colnames(DF)<-paste("Beta",1:nXs,sep="")
      rownames(DF)<-paste("Beta",1:nXs,sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMV)
      nXs<-as.numeric(input$ExVarNumnX)
      if(ncol(DF)!=nXs){
        DF<-data.frame(diag(100.00,nXs))
        colnames(DF)<-paste("Beta",1:nXs,sep="")
        rownames(DF)<-paste("Beta",1:nXs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 }")
    }
  })
  
  output$hotPVarMVWis=renderRHandsontable({
    if(is.null(input$hotPVarMVWis)){
      nYs<-as.numeric(input$EndVarNumnY)
      nu<-as.numeric(input$DegFredMV)
      DF<-data.frame(nu*diag(1.00,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("y",1:nYs,sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMVWis)
      nYs<-as.numeric(input$EndVarNumnY)
      nu<-as.numeric(input$DegFredMV)
      if(ncol(DF)!=nYs){
        DF<-data.frame(nu*diag(1.00,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("y",1:nYs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red scale matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 }")
    }
    })
  
  ###########Instrumental variable##################
  output$hotPmeanY=renderRHandsontable({
    
    if(is.null(input$hotPmeanY) ){
      nv = 1
      f=input$Formula2a
      
      DF=data.frame("Prior mean"=rep(0,nv))
      rownames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPmeanY)
      rn=rownames(DF)[-1]
      f=input$Formula2a
      if(!identical(rn,getRegs(f))){
        f=input$Formula2a
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
      hot_col("Prior.mean",format="0.000") 
  })
  
  output$hotPvarY=renderRHandsontable({
    
    if(is.null(input$hotPvarY) ){
      nv = 1
      f=input$Formula2a
      
      DF=data.frame("Prior mean"=0)
      rownames(DF)=getRegs(f)
      colnames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPvarY)
      rn=rownames(DF)[-1]
      f=input$Formula2a
      if(!identical(rn,getRegs(f))){
        f=input$Formula2a
        nv=unlist(gregexpr(pattern ='~',f))
        
        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{
          
          regs=getRegs(f)
          DF=data.frame(100*diag(length(regs)+1))
          rownames(DF)=c("cte",regs)
          colnames(DF)=c("cte",regs)
          
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 
                 }")
      
    }
    
    })
  
  output$hotPmeanZ=renderRHandsontable({
    
    if(is.null(input$hotPmeanZ) ){
      nv = 1
      f=input$Formula2b
      
      DF=data.frame("Prior mean"=rep(0,nv))
      rownames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPmeanZ)
      rn=rownames(DF)[-1]
      f=input$Formula2b
      if(!identical(rn,getRegs(f))){
        f=input$Formula2b
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
      hot_col("Prior.mean",format="0.000")
    
  })
  
  output$hotPvarZ=renderRHandsontable({
    
    if(is.null(input$hotPvarZ) ){
      nv = 1
      f=input$Formula2b
      
      DF=data.frame("Prior mean"=0)
      rownames(DF)=getRegs(f)
      colnames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPvarZ)
      rn=rownames(DF)[-1]
      f=input$Formula2b
      if(!identical(rn,getRegs(f))){
        f=input$Formula2b
        nv=unlist(gregexpr(pattern ='~',f))
        
        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{
          
          regs=getRegs(f)
          DF=data.frame(100*diag(length(regs)+1))
          rownames(DF)=c("cte",regs)
          colnames(DF)=c("cte",regs)
          
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 
                 }")
    }
  })
  
  
  output$hotPIW=renderRHandsontable({
    
    if(is.null(input$hotPIW) ){
      DF=data.frame("Main"=c(1,0),"Inst"=c(0,1))
      rownames(DF)=colnames(DF)
    }else{
      DF=hot_to_r(input$hotPIW)
      
      
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 
                 }")
    }
  })
  
  #########Bivariate Probit###########
  
  output$hotPmeanBVProbit=renderRHandsontable({
    if(is.null(input$hotPmeanBVProbit)){
      nYs<-2
      nXs<-as.numeric(input$ExVarNumnXBVProbit)
      DF<-data.frame(matrix(0.000,nXs,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("x",1:nXs,sep="")
    }else{
      DF<-hot_to_r(input$hotPmeanBVProbit)
      nYs<-2
      nXs<-as.numeric(input$ExVarNumnXBVProbit)
      if(ncol(DF)!=nYs | nrow(DF)!=nXs){
        DF<-data.frame(matrix(0.000,nXs,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("x",1:nXs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0.000")
  })
  
  output$hotPvarBVProbit=renderRHandsontable({
    if(is.null(input$hotPvarBVProbit)){
      nbetas<-2*as.numeric(input$ExVarNumnXBVProbit)
      DF<-data.frame(diag(100.00,nbetas))
      nYs<-2
      ki<-as.numeric(input$ExVarNumnXBVProbit)
      colnames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
      rownames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
    }else{
      DF<-hot_to_r(input$hotPvarBVProbit)
      nbetas<-2*as.numeric(input$ExVarNumnXBVProbit)
      nYs<-2
      ki<-as.numeric(input$ExVarNumnXBVProbit)
      if(ncol(DF)!=nbetas){
        DF<-data.frame(diag(100.00,nbetas))
        colnames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
        rownames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 
                 td.style.background = 'red';
                 }")
    }
    })

  
  
  ######## 2.1 Models: Posterior Chains#########
  
  Posteriors21 <- eventReactive(input$goButton21, {

    if(input$M21=='m211'){
      Bbar<- hot_to_r(input$hotPMeanMV)
      A<- solve(as.matrix(hot_to_r(input$hotPVarMV)))
      nu<- isolate(as.numeric(input$DegFredMV))
      V<- as.matrix(hot_to_r(input$hotPVarMVWis))
    }
    
    if(input$M21=='m212'){
      betabar<- c(hot_to_r(input$hotPMeanMVSUR))
      A<- solve(as.matrix(hot_to_r(input$hotPVarMVSUR)))
      nu<- isolate(as.numeric(input$DegFredMVSUR))
      V<- as.matrix(hot_to_r(input$hotPVarMVWisSUR))
      ki<- hot_to_r(input$RegEquat)
      nYs<-isolate(as.numeric(input$EndVarNumnYSUR))
      SURlist<-ListSUR(dataInput2(),nYs,ki)
    }
    
    if(input$M21=='m213'){
      Bmeany= hot_to_r(input$hotPmeanY)[,1]
      Bvary<- solve(as.matrix(hot_to_r(input$hotPvarY)))
      Bmeanz<- hot_to_r(input$hotPmeanZ)[,1]
      Bvarz<- solve(as.matrix(hot_to_r(input$hotPvarZ)))
      V<- solve(as.matrix(hot_to_r(input$hotPIW)))

      if(input$PShLIW==""){nu<-NULL}
      else{
        nu<- isolate(as.numeric(input$PShLIW)) 
      }
    }
    
    if(input$M21=='m214'){
      betabar<- c(hot_to_r(input$hotPmeanBVProbit))
      A<- as.matrix(hot_to_r(input$hotPvarBVProbit))
      rhobar<- isolate(as.numeric(input$MeanRhonBVProbit))
      v<- isolate(as.numeric(input$VarRhonBVProbit))
      tun<- isolate(as.numeric(input$TuningnBVProbit))
      nxs<- isolate(as.numeric(input$ExVarNumnXBVProbit))
      nind<- isolate(as.numeric(input$NnBVProbit))
      Dat<- XcreateMP(nxs=nxs,nind=nind,Data=dataInput2())
    }
    
    MCMC<- list(R=input$itMV,keep=as.numeric(input$keepMV),burnin=input$burninMV)
    
    if(input$M21=='m210')
      return()
    else{
      args <- switch(input$M21,
                     "m211" = list(Data=dataInput2(),m=as.numeric(input$EndVarNumnY),k=as.numeric(input$ExVarNumnX),Prior=list(Bbar=Bbar,A=A,nu=nu,V=V),MCMC),
                     "m212" = list(Data=list(regdata=SURlist), Mcmc=MCMC, Prior=list(betabar=betabar,A=A,nu=nu,V=V)),
                     "m213" = list(list(z=as.matrix(sumtextM2b()$Z),w=as.matrix(sumtextM2a()$X[,-2]),x=sumtextM2b()$x,y=sumtextM2a()$y),list(md=Bmeanz,Ad=Bvarz,mbg=Bmeany,Abg=Bvary,nu=nu,V=V),MCMC),
                     "m214" = list(Data=Dat,Prior=list(betabar=betabar,A=A,rhobar=rhobar,v=v,tun=tun), Mcmc=MCMC)
      )
    }
    if (input$M21 == 'm211') {
      do.call(MultiReg, args)}
    else {
      if (input$M21 == 'm212') {
        do.call(SUR, args)}
        else {
          if (input$M21 == 'm213') {
            do.call(InstVar, args)}
          else {
            if (input$M21 == 'm214'){
              do.call(BivProbit, args)
            }
          }
        }
      }
  })

  
  ####### 2.1 Models: Download Posterior Chains##########
  
  output$download21 <- downloadHandler(
    filename = function() { 
      paste("Posterior Chains", '.csv', sep='') 
    },
    
    content = function(file) {
      
      if(input$M21=='m110')
        content<- return()
      
      switch(input$M21,
             "m211" = post21<- cbind(Posteriors21()$bet,Posteriors21()$Sig),
             "m212" = post21<- cbind(Posteriors21()$bet,Posteriors21()$Sig),
             "m213" = post21<- cbind(Posteriors21()$betadraw,Posteriors21()$deltadraw,Posteriors21()$gammadraw,Posteriors21()$Sigmadraw),
             "m214" = post21<- cbind(Posteriors21()$bet,Posteriors21()$rho))
      
      write.csv(post21, file)
    }
  )
  
  
  ####### 2.1 Models: Summary Posterior Chains##########
  output$summary21 <- renderPrint({
    if(input$M21=='m210'){
      return()}
    
    else{
      switch(input$M21,
             "m211" = SumDiagMultiReg(Posteriors21()$bet[,],Posteriors21()$Sig[,]),
             "m212" = SumDiagSUR(Posteriors21()$bet[,],Posteriors21()$Sig[,]),
             "m213" = SumDiagInstVar(Posteriors21()$betadraw[],Posteriors21()$deltadraw[,],Posteriors21()$gammadraw[,],Posteriors21()$Sigmadraw[,]),
             "m214" = SumDiagBVProbit(Posteriors21()$bet[,],Posteriors21()$rho[]))
    }
  })
  
  
  ####### 2.1 Models: Summary Posterior Chains##########
  
  output$plot21 <- renderPlot({
    unlink(file.path(path,"Posterior Graphs"),recursive=TRUE)
    dir.create(file.path(path,"Posterior Graphs"),showWarnings = FALSE)
    setwd(file.path(path,"Posterior Graphs"))
    
    graphs21<- function(post21){
      nc<-ncol(post21)
      for (i in 1:nc) {
        pdf(paste("Density Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot(post21[,i])
        dev.off()
        setEPS()
        postscript(paste("Density Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot(post21[,i])
        dev.off()
        pdf(paste("Trace Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.trace(post21[,i])
        dev.off()
        setEPS()
        postscript(paste("Trace Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.trace(post21[,i])
        dev.off()
        pdf(paste("Autocorrelation Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.corr(post21[,i])
        dev.off()
        setEPS()
        postscript(paste("Autocorrelation Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.corr(post21[,i])
        dev.off()
      }
    }
    switch(input$M21,
           "m211" = graphs21(cbind(Posteriors21()$bet[,],Posteriors21()$Sig[,])),
           "m212" = graphs21(cbind(Posteriors21()$bet[,],Posteriors21()$Sig[,])),
           "m213" = graphs21(cbind(Posteriors21()$betadraw[],Posteriors21()$deltadraw[,],Posteriors21()$gammadraw[,],Posteriors21()$Sigmadraw[,])),
           "m214" = graphs21(cbind(Posteriors21()$bet[,],Posteriors21()$rho[])))
    setwd("..")
  })
  
  output$multiDownload21 <- downloadHandler(
    filename = function() {
      paste("Posterior Graphs", "zip", sep=".")
    },
    
    content = function(file) {
      zip(zipfile=file, files='Posterior Graphs')
    },
    contentType = "application/zip"
  )
