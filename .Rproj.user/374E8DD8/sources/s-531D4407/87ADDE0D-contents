library(shiny)
library(readr)
library(dplyr)
library(factoextra)
library(readxl)
library(dplyr)
library(caTools)
library(plotrix)
library(corrplot)

library(plotly)
library(MLmetrics)
library(DT)
library(FactoMineR)
library(ggplot2)
library(grid)
library(doBy)
library(shinyWidgets)
library(gridExtra)
library(plyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(agricolae)
library(MASS)
library(googleVis)
shinyServer(
  function(input, output) { 
    
    output$filedf <- DT::renderDataTable({
      if(is.null(input$file)){return ()}
      input$file # the file input data frame object that contains the file attributes
    })
    
    # Extract the file path for file
    output$filedf2 <- DT::renderDataTable({
      if(is.null(input$file)){return ()}
      input$file$datapath # the file input data frame object that contains the file attributes
    })
    
    ## Below code to display the structure of the input file object
    output$fileob <- renderPrint({
      if(is.null(input$file)){return ()}
      str(input$file)
    })
    
    ## Side bar seelect input widget coming through renderUI()
    # Following code displays the seelect input widget with the list of file loaded by the user
    output$selectfile <- renderUI({
      if(is.null(input$file)) {return()}
      list(hr(), 
           helpText("Select the files for which you need to see data and summary stats"),
           selectInput("Select", "Select", choices=input$file$name)
      )
      
    })
    
    ## Summary Stats code ##
    # this reactive output contains the summary of the dataset and display the summary in table format
    output$summ <- renderPrint({
      if(is.null(input$file)){return()}
      summary(read.table(file=input$file$datapath[input$file$name==input$Select], 
                         sep=input$sep, 
                         header = input$header, 
                         stringsAsFactors = input$stringAsFactors))})
    
    #############
    data2 <- reactive({
      read.table(file=input$file$datapath[input$file$name==input$Select], 
                 sep=input$sep, 
                 header = input$header, 
                 stringsAsFactors = input$stringAsFactors)
      
    })
    
    
    
    output$table <- DT::renderDataTable ({
      
      data2()
    })
    
    
    ###########
    output$tb <- renderUI({
      if(is.null(input$file)) {return()}
      else
        tabsetPanel(
          tabPanel("Input File Object DF ", tableOutput("filedf"), tableOutput("filedf2")),
          tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
          
          tabPanel("Summary Stats", verbatimTextOutput("summ")),
          tabPanel("Dataset",DT::dataTableOutput("table")))
    })
    #############thtwo
    output$filed <- renderTable({
      if(is.null(input$files)){return ()}
      input$files # the file input data frame object that contains the file attributes
    })
    
    # Extract the file path for file
    output$filedf3 <- renderTable({
      if(is.null(input$files)){return ()}
      input$files$datapath # the file input data frame object that contains the file attributes
    })
    
    ## Below code to display the structure of the input file object
    output$filest <- renderPrint({
      if(is.null(input$files)){return ()}
      str(input$files)
    })
    
    ## Side bar select input widget coming through renderUI()
    
    output$seelect <- renderUI({
      if(is.null(input$files)) {return()}
      list(hr(), 
           helpText("Select the files for which you need to see data and summary stats"),
           selectInput("Selecte", "Select", choices=input$files$name)
      )
      
    })
    ## Summary Stats code ##
    # this reactive output contains the summary of the dataset and display the summary in table format
    output$sume <- renderPrint({
      if(is.null(input$files)){return()}
      summary(read.table(input$files$datapath[input$files$name==input$Selecte], 
                         sep=input$sep2, 
                         header = input$header2, 
                         stringsAsFactors = input$stringAsFactors2))})
    
    datahedo <- reactive({
      read.table(input$files$datapath[input$files$name==input$Selecte], 
                 sep=input$sep2, 
                 header = input$header2, 
                 stringsAsFactors = input$stringAsFactors2)
      
    })
    
    
    
    output$tablehedo <- DT::renderDataTable ({
      
      datahedo()
    })
    
    
    output$affiche<- renderUI({
      if(is.null(input$files)) {return()}
      else
        tabsetPanel(
          tabPanel("Input File Object DF ", tableOutput("filed"), tableOutput("filedf3")),
          tabPanel("Input File Object Structure", verbatimTextOutput("filest")),
          
          tabPanel("Summary Stats", verbatimTextOutput("sume")),
          tabPanel("Dataset",DT::dataTableOutput("tablehedo")))
    })
    #############
    
    #mean table 
    output$mtab<-DT::renderDataTable({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(senso)
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    final})
    
    
    #scree
    output$scree<-renderPlot({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(senso)
      
      
      final=summaryBy(.~produit , data = senso[,-c(1,2)] , FUN = c(mean) , na.rm= T)
      rownames(final)<-final[,1]
      final<-final[,-1]
      res.pca2<-PCA(final,scale.unit = TRUE)
      fviz_screeplot(res.pca2,addlabels=T)})
    #les coor sur 2 axes
    output$table2<-DT::renderDataTable({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(senso)
      final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
      rownames(final)<-final[,1]
      final<-final[,-1]
      res.pca<-PCA(final,scale.unit = TRUE)
      round(res.pca$var$cor[,1:2],2)}) 
    #projection sur 2 axes
    
    output$Projection<-renderPlot({senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    fviz_pca_var(res.pca,axes=c(1,2),geom=c("arrow", "text"),col.var = "purple")})
    ##############################################
    
    output$table3<-DT::renderDataTable({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(senso)
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    datadeux=as.data.frame(c())
    datadeux<-cbind(names(round(sort(res.pca$var$contrib[,1]),3)) ,as.data.frame(round(sort(res.pca$var$contrib[,1]),3)),as.data.frame(round(sort(res.pca$var$contrib[,2]),3)))
    colnames(datadeux)<-c("Les variables","Contribution on the first axe","Contribution on the second axe")
    
    datadeux}) 
    output$plot2<-renderPlot( {senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    fviz_pca_contrib(res.pca, choice = "var", axes = 2)})
    ################
    output$plot1<-renderPlot( {senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    fviz_pca_contrib(res.pca, choice = "var", axes = 1)})
    ###################
    
    output$table4<-DT::renderDataTable({senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(senso)
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    datatrois=as.data.frame(c())
    datatrois<-cbind(names(round(sort(res.pca$ind$contrib[,1]),3)) ,as.data.frame(round(sort(res.pca$ind$contrib[,1]),3)),as.data.frame(round(sort(res.pca$ind$contrib[,2]),3)))
    colnames(datatrois)<-c("Les indiables","Contribution on the first axe","Contribution on the second axe")
    datatrois}) 
    ##################
    output$plot3<-renderPlot({senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE) 
    fviz_pca_contrib(res.pca, choice = "ind", axes = 2)})
    ########
    output$plot4<-renderPlot({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    fviz_pca_contrib(res.pca, choice = "ind", axes = 1)})
    ##########
    output$plot5<-renderPlot( {senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    fviz_pca_ind(res.pca, choice = "ind", axes = c(1,2))})
    #############
    output$plot6<-renderPlot(  {senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
    fviz_pca_var(res.pca,col.var="cos2")+scale_color_gradient2(low="green",mid="blue",high="red",midpoint=0.4)}
    )
    #elipse et biplot
    #fviz_pca_ind(res.pca, geom = c("point","text"), habillage = 1, addEllipses = TRUE) + theme_classic()
    #fviz_pca_biplot(res.pca,axes=c(1,2),geom=c("point","text","arrow"),habillage = 1)
    ####################carto
    
    
    ##########end cartp
    graph<-reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    e1=ggplot(senso,aes(senso$seance,senso[,input$type]))+geom_boxplot()
    e2=ggplot(senso,aes(senso$juge,senso[,input$type]))+geom_boxplot()
    e3=ggplot(senso,aes(senso$produit,senso[,input$type]))+geom_boxplot()
    
    g=grid.arrange(e1,e2,e3)
    
    })
    
    output$graph<-renderPlot({plot(graph())})
    graph2<-reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    melted_senso=melt(senso,id.vars = c("juge","produit"),measure.vars = 4:8)
    
    if (input$js == "produit") {
      ggplot(melted_senso,aes(x=produit,y=value,color=produit))+geom_boxplot()+facet_wrap(~variable)
    }
    else if(input$js == "juge"){
      ggplot(melted_senso,aes(x=juge,y=value,color=juge))+geom_boxplot()+facet_wrap(~variable)
      
    }
    else {melted_data=melt(senso,id.vars = c("juge","seance"),measure.vars=4:8)
    ggplot(melted_data,aes(x = seance,y=value,color=seance))+geom_boxplot()+facet_wrap(~variable)
    }
    
    })
    output$graphe<-renderPlot({plot(graph2())})
    
    anovv<-reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    senso=na.omit(senso )
    lm1=lm(senso[,input$deux] ~ senso[,input$un] , data=na.omit(senso ))
    #lm3=lm(input$un ~  input$deux,data= na.omit(senso[,c(1,2,3)]))
    aov3=aov(lm1,data=na.omit(senso ))
    b= summary(aov3)
    b
    })
    output$trois<-renderPrint({anovv()})
    hedhy<- reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    lm2=lm(senso[,input$deux] ~juge*produit*seance,data=senso)
    aov1=aov(lm2,data=senso)
    k=summary(aov1)
    k})
    output$quatre<-renderPrint({hedhy()})
    
    eltuc<- reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    if (input$troiss == "produit") {
      lm4<-lm(senso[,input$quatree]~ produit,senso)
      aov4<-aov(lm4)
      
      tuq1<-HSD.test(aov4,"produit",senso,group = TRUE)
    }
    else if(input$troiss == "juge"){
      
      lm4<-lm(senso[,input$quatree]~ juge ,senso)
      aov4<-aov(lm4)
      
      tuq1<-HSD.test(aov4,"juge",senso,group = TRUE)
      
    }
    else {   lm4<-lm(senso[,input$quatree]~ seance ,senso)
    aov4<-aov(lm4)
    
    tuq1<-HSD.test(aov4,"seance",senso,group = TRUE)
    
    }
    
    
    
    tuq1
    
    })
    output$cinq<-renderPrint({eltuc()})
    
    
    output$six<-renderPlot({plot(eltuc())})
    
    
    resumee <-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      
      if(input$factee=="mean"){
      if (input$v == "produit") {
        final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
        rownames(final)<-final[,1]
        final<-final[,-1]
        final     
      }
      else if(input$v == "juge"){
        
        
        final=summaryBy(.~juge , data = senso[, -c(1,3)] , FUN = c(mean) , na.rm= T)
        rownames(final)<-final[,1]
        final<-final[,-1]
        final
        
      }
      else {   
        final=summaryBy(.~seance , data = senso[, -c(2,3)] , FUN = c(mean) , na.rm= T)
        rownames(final)<-final[,1]
        final<-final[,-1]
        final
      }}else{
        if (input$v == "produit") {
          final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(sd) , na.rm= T)
          rownames(final)<-final[,1]
          final<-final[,-1]
          final     
        }
        else if(input$v == "juge"){
          
          
          final=summaryBy(.~juge , data = senso[, -c(1,3)] , FUN = c(sd) , na.rm= T)
          rownames(final)<-final[,1]
          final<-final[,-1]
          final
          
        }
        else {   
          final=summaryBy(.~seance , data = senso[, -c(2,3)] , FUN = c(sd) , na.rm= T)
          rownames(final)<-final[,1]
          final<-final[,-1]
          final
        }
        
      }
      
      
      
    })
    output$thenya<- DT::renderDataTable({resumee()})
    
    datastat<-reactive({
      df<-data.frame(data2()[,-c(1:3)])
      df=na.omit(df)
      cdata2<-do.call(data.frame,list(
        moyenne=apply(df,2,mean),
        ecrtype=apply(df,2,sd),
        mediane=apply(df,2,median)
        
        
      ))})
    
    output$stable<- DT::renderDataTable ({datastat()})
    
    
    stack<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      p= ggplot(senso , aes(x=produit , y=senso[,input$varr] , fill=senso[,input$varr2]))+ geom_bar(width = 1 , stat = "identity")+ ggtitle("Stacked Histogram ")
      p
    })
    output$stacked<-renderPlot({stack()})
    piee<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      essai<-table(senso[,input$w])
      essai<-as.data.frame(essai)
      P= pie3D(essai$Freq,radius=0.9,labels=essai$Var1,explode=0.1,main="frequency of variable levels")
      
    })
    output$pieee<-renderPlot({piee()})
    quatreplot<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      bp <- ggplot(senso, aes(x=senso[,input$produit], y=senso[,input$variablee], color=senso[,input$produit])) +
        geom_boxplot() + 
        theme(legend.position = "none")
      # Créer un dot plot
      # Ajouter la moyenne et l'écart type
      dp <- ggplot(senso, aes(x=senso[,input$produit], y=senso[,input$variablee], fill=senso[,input$produit])) +
        geom_dotplot(binaxis='y', stackdir='center')+
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="pointrange", color="red")+
        theme(legend.position = "none")
      # Créer un violin plot
      vp <- ggplot(senso, aes(x=senso[,input$produit], y=senso[,input$variablee])) +
        geom_violin()+
        geom_boxplot(width=0.05)
      # Créer un stripchart
      sc <- ggplot(senso, aes(x=senso[,input$produit], y=senso[,input$variablee], color=senso[,input$produit], shape=senso[,input$produit])) + scale_shape_manual(values=seq(0,15))+
        geom_jitter(position=position_jitter(0.2))+
        theme(legend.position = "none") +
        theme_gray()
      grid.arrange(bp, dp, vp, sc, ncol=2, nrow = 2)
      
    })
    output$quatrep<-renderPlot({ quatreplot()})
    cartoo<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      hedo<- datahedo()
      
      
      senso <- as.data.frame(senso)
      tableau.moyen=summaryBy(.~produit,senso[,-c(1:2)],FUN=c(mean),na.rm=T)
      senso.hedo=cbind.data.frame(tableau.moyen,t(hedo[,c(2:5)]),t(hedo[,c(152:155)]),row.names=1)
      senso.hedo
      res.hs = PCA(senso.hedo,quali.sup = c(1),graph=T)
      fviz_pca_ind(res.hs)
    })  
    output$cartoo<-renderPlot({cartoo()})
    cartoo2<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      hedo<-datahedo()
      
      
      senso <- as.data.frame(senso)
      tableau.moyen=summaryBy(.~produit,senso[,-c(1:2)],FUN=c(mean),na.rm=T)
      senso.hedo=cbind.data.frame(tableau.moyen,t(hedo[,c(2:5)]),t(hedo[,c(152:155)]),row.names=1)
      senso.hedo
      res.hs = PCA(senso.hedo,quali.sup = c(1),graph=T)
      fviz_pca_var(res.hs, col.var="contrib", gradient.cols = c("white", "green", "red"),ggtheme = theme_minimal())
    }) 
    output$cartoo2<-renderPlot({cartoo2()})
    cartoo3<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
      hedo<-datahedo()
      
      
      senso <- as.data.frame(senso)
      tableau.moyen=summaryBy(.~produit,senso[,-c(1:2)],FUN=c(mean),na.rm=T)
      senso.hedo=cbind.data.frame(tableau.moyen,t(hedo[,c(2:5)]),t(hedo[,c(152:155)]),row.names=1)
      senso.hedo
      res.hs = PCA(senso.hedo,quali.sup = c(1),graph=T)
      biplot<-fviz_pca_biplot(res.hs)
      biplot}) 
    output$cartoo3<-renderPlot({cartoo3()})
    pltlt<-reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(senso)
    
    if (input$fa == "produit") {
      maVar=senso %>% group_by(produit,seance) %>% summarise_all(funs(mean))
      
      maVar_melt=melt(maVar,id.vars = c("produit"),measure.vars = 4:8)
      p6=ggplot(maVar_melt,aes(x = produit,y=value,color=produit))+geom_boxplot()+facet_wrap(~variable)
      ggplotly(p6)  
      
    }
    else if(input$fa == "juge"){
      maVar2=senso %>% group_by(juge,seance) %>% summarise_all(funs(mean))
      
      maVar2_data=melt(maVar2,id.vars = c("juge","seance"),measure.vars=4:8)
      
      maVar_melt=melt(maVar2,id.vars = c("juge"),measure.vars = 4:8)
      p6=ggplot(maVar_melt,aes(x =juge ,y=value,color=juge))+geom_boxplot()+facet_wrap(~variable)
      ggplotly(p6)
    }
    else {   
      maVar2=senso %>% group_by(juge,seance) %>% summarise_all(funs(mean))
      
      maVar2_data=melt(maVar2,id.vars = c("seance"),measure.vars=4:8)
      p6=ggplot( maVar2_data,aes(x = seance,y=value,color=seance))+geom_boxplot()+facet_wrap(~variable)
      ggplotly(p6)
    }
    
    
    
    })
    output$esmha<-renderPlotly({pltlt()})
    
    plotth<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(senso)
      
      
      
      pal <-  colorRampPalette("red")
      p <- plot_ly(data = senso, x = ~produit, type='bar' ,y = ~Olait)%>%
        add_trace(y=~ juge,colors=pal) %>%
        layout( barmode = 'group')
      
      
      
      aval <- list()
      for(step in 1:22){
        aval[[step]] <-list(visible = FALSE,
                            name = colnames(senso)[step+3],
                            x=senso$produit,
                            y=senso[,step+3],
                            juge=senso$juge)
      }
      steps <- list()
      p <- plot_ly()
      
      p <- add_bars(p,x=aval[1][[1]]$x,y=aval[1][[1]]$y, color = ~aval[1][[1]]$juge, type = 'bar', hoverinfo= 'name')
      for (i in 1:22) {
        step <- list(args = list('visible', rep(FALSE, length(aval)/12)),
                     method = 'restyle', label = colnames(senso)[i+3])
        step$args[[2]][i] = TRUE  
        steps[[i]] = step
      }  
      p <- p %>%
        layout(sliders = list(list(active = 1,
                                   currentvalue = list(prefix = "variable "),
                                   steps = steps)))
    })
    output$tokhrej<-renderPlotly({plotth()})
    #####lesDownload
    #2
    output$down<-downloadHandler(
      filename=function(){
        paste("plot2",input$var3,sep=".")},
      content = function(file){
        if(input$var3=="png")
          png(file)
        else
          pdf(file)
        plot(graph2())
         dev.off() 
      }
    
    )
    #1
    output$var4<-downloadHandler(
      filename=function(){
        paste("plot1",input$va,sep=".")},
      content = function(file){
        if(input$va=="png")
          png(file)
        else
          pdf(file)
        plot(graph())
        dev.off() 
      }
      
    )
    #3
    output$var5<-downloadHandler(
      filename=function(){
        paste("plot3",input$var10,sep=".")},
      content = function(file){
        if(input$var10=="png")
          png(file)
        else
          pdf(file)
       stack()
        dev.off() 
      }
      
    )
    #4
    output$var60<-downloadHandler(
      filename=function(){
        paste("plot4",input$var6,sep=".")},
      content = function(file){
        if(input$var6=="png")
          png(file)
        else
          pdf(file)
        piee()
        dev.off() 
      }
    
    )
    #5
    output$var88<-downloadHandler(
      filename=function(){
        paste("plot5",input$var8,sep=".")},
      content = function(file){
        if(input$var8=="png")
          png(file)
        else
          pdf(file)
        quatreplot()
        dev.off() 
      }
      
    )
    
    
  ############
    pies<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(senso)
      maVar=senso %>% group_by(produit,seance) %>% summarise_all(funs(mean))
      
      
      maVar_melt=melt(maVar,id.vars = c("produit"),measure.vars = 4:8)
     d= plot_ly(maVar_melt, labels = ~produit, values = ~value, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(produit),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE)
     d
    })
    output$piess<-renderPlotly({pies()})
    
    
    
    clustt<-reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    hedo<- datahedo()
    
    
    senso <- as.data.frame(senso)
    tableau.moyen=summaryBy(.~produit,senso[,-c(1:2)],FUN=c(mean),na.rm=T)
    senso.hedo=cbind.data.frame(tableau.moyen,t(hedo[,c(2:5)]),t(hedo[,c(152:155)]),row.names=1)
     
    res.hs = PCA(senso.hedo,quali.sup = c(1),graph=T)
    res.hs
     #plot(res, axes=c(1,2), choice="3D.map")
 
     
    })
    output$clus<-renderPlot({
      res.hs<-clustt()
      oui<-HCPC(res.hs,graph = F)
      
    plot(oui, axes=c(1,2), choice="3D.map")

    })
    output$clus2<-renderPlot({
      res.hs<-clustt()
      res<-HCPC(res.hs,graph = F)
      
       fviz_cluster(res)
      
    })
    courbe<-reactive({
      senso<-data2()
      senso<-na.omit(senso)
      senso[,2]=as.factor(senso[,2])
      senso[,1]=as.factor(senso[,1])
      senso=as.data.frame(na.omit(senso))
       if(input$var2=="juge"){
      p=ggplot(senso,aes(x=senso$juge,y=senso[, input$var1], label="oui"))+
        geom_point(aes(color=senso$juge),size=2)+
        geom_smooth()
      ggplotly(p)
      }else if(input$var2=="seance"){
     
      
       d=ggplot(senso,aes(x=senso$seance,y=senso[, input$var1], label="oui"))+
        geom_point(aes(color=senso$seance),size=2)+
        geom_smooth()
      ggplotly(d)}
      else{
        d=ggplot(senso,aes(x=senso$produit,y=senso[, input$var1], label="oui"))+
          geom_point(aes(color=senso$produit),size=2)+
          geom_smooth()
        ggplotly(d)}
      
      
    })
    output$courbee<-renderPlotly({courbe()})
    #conrib dwer
    output$corr<-renderPlot({
      senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    
    
    final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    rownames(final)<-final[,1]
    final<-final[,-1]
    res.pca<-PCA(final,scale.unit = TRUE)
   d= corrplot(res.pca$var$contrib, is.corr=FALSE) 
   d
    })
    ####TRY
    eltuc2<- reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    if (input$troiss3 == "produit" && input$troiss2==" " && input$troiss1==" ") {
      lm4<-lm(senso[,input$quatree1]~ produit,senso)
      aov4<-aov(lm4)
      
      tuq1<-HSD.test(aov4,"produit",senso,group = TRUE)
      tuq1
    }
    else if(input$troiss2 == "juge" && input$troiss1==" "&& input$troiss3==" "){
      
      lm4<-lm(senso[,input$quatree1]~ juge ,senso)
      aov4<-aov(lm4)
      
      tuq1<-HSD.test(aov4,"juge",senso,group = TRUE)
      tuq1
      
    }
    else if(input$troiss1 == "seance" && input$troiss2==" " && input$troiss3==" ") {   
      lm4<-lm(senso[,input$quatree1]~seance ,senso)
    aov4<-aov(lm4)
    
    tuq1<-HSD.test(aov4,"seance",senso,group = TRUE)
    tuq1
    }
    else if (input$troiss3 == "produit" && input$troiss1=="seance" && input$troiss2==" ") {
      lm4<-lm(senso[,input$quatree1]~produit+seance,senso)
      aov4<-aov(lm4)
      
      tuq1<-HSD.test(aov4,c("produit","seance"),senso,group = TRUE)
      tuq1
    }
    else if(input$troiss3 == "produit" && input$troiss1=="seance" && input$troiss2=="juge"){
      
      lm4<-lm(senso[,input$quatree1]~juge+produit+seance ,senso)
      aov4<-aov(lm4)
      
      tuq1<-HSD.test(aov4,c("produit","seance","juge"),senso,group = TRUE)
      tuq1
    }
    else if(input$troiss1 == "seance" && input$troiss2=="juge" && input$troiss3==" ") {   
      lm3<-lm(senso[,input$quatree1]~seance+juge ,senso)
    aov3<-aov(lm3)
    
    tuq3<-HSD.test(aov3,c( "seance","juge"),senso,group = TRUE)
    tuq3
    }
    else if(input$troiss3 == "produit" && input$troiss2=="juge" && input$troiss1==" ") {   
      lm5<-lm(senso[,input$quatree1]~produit+juge ,senso)
      aov10<-aov(lm5)
      
      tuq10<-HSD.test(aov10,c( "produit","juge"),senso,group = TRUE)
      tuq10
    }  
    
    
    
    
    
    })
    output$cinq2<-renderPrint({eltuc2()})
    output$six2<-renderPlot({plot(eltuc2())})
    
    
    #########end TRY
    output$one<-downloadHandler(
      filename=function(){
        paste("scree.png")},
      content = function(file){
         
          png(file)
        senso<-data2()
        senso<-na.omit(senso)
        senso[,2]=as.factor(senso[,2])
        senso[,1]=as.factor(senso[,1])
        senso=as.data.frame(senso)
        
        
        final=summaryBy(.~produit , data = senso[,-c(1,2)] , FUN = c(mean) , na.rm= T)
        rownames(final)<-final[,1]
        final<-final[,-1]
        res.pca2<-PCA(final,scale.unit = TRUE)
        fviz_screeplot(res.pca2,addlabels=T)
        dev.off()})
    output$three<-downloadHandler(
      filename=function(){
        paste("graph","png",sep=".")},
      content = function(file){
        
        png(file)
        senso<-data2()
        senso<-na.omit(senso)
        senso[,2]=as.factor(senso[,2])
        senso[,1]=as.factor(senso[,1])
        senso=as.data.frame(na.omit(senso))
        
        
        final=summaryBy(.~produit , data = senso[, -c(1:2)] , FUN = c(mean) , na.rm= T)
        rownames(final)<-final[,1]
        final<-final[,-1]
        res.pca<-PCA(final,scale.unit = TRUE)
        fviz_pca_var(res.pca,axes=c(1,2),geom=c("arrow", "text"),col.var = "purple")
        dev.off()})
    output$five<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    output$sixx<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    output$four<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    output$eight<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    output$nine<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    output$ten<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    output$eleven<-downloadHandler(
      filename=function(){
        paste("graph.png")},
      content = function(file){
        
        png(file)})
    
    eltuc22<- reactive({ senso<-data2()
    senso<-na.omit(senso)
    senso[,2]=as.factor(senso[,2])
    senso[,1]=as.factor(senso[,1])
    senso=as.data.frame(na.omit(senso))
    if (input$troiss33 == "produit" && input$troiss22==" " && input$troiss11==" ") {
      lm4<-lm(senso[,input$quatree11]~ produit,senso)
      aov4<-aov(lm4)
      aov4
       
    }
    else if(input$troiss22 == "juge" && input$troiss11==" "&& input$troiss33==" "){
      
      lm4<-lm(senso[,input$quatree11]~ juge ,senso)
      aov4<-aov(lm4)
      
      aov4
      
    }
    else if(input$troiss11 == "seance" && input$troiss22==" " && input$troiss33==" ") {   
      lm4<-lm(senso[,input$quatree11]~seance ,senso)
      aov4<-aov(lm4)
      aov4
      
    }
    else if (input$troiss33 == "produit" && input$troiss11=="seance" && input$troiss22==" ") {
      lm4<-lm(senso[,input$quatree11]~produit+seance,senso)
      aov4<-aov(lm4)
   avo4
    }
    else if(input$troiss33 == "produit" && input$troiss11=="seance" && input$troiss22=="juge"){
      
      lm4<-lm(senso[,input$quatree11]~juge+produit+seance ,senso)
      aov4<-aov(lm4)
      
      aov4
    }
    else if(input$troiss11 == "seance" && input$troiss22=="juge" && input$troiss33==" ") {   
      lm3<-lm(senso[,input$quatree11]~seance+juge ,senso)
      aov3<-aov(lm3)
      
      aov3
    }
    else if(input$troiss33 == "produit" && input$troiss22=="juge" && input$troiss11==" ") {   
      lm5<-lm(senso[,input$quatree11]~produit+juge ,senso)
      aov10<-aov(lm5)
      
      aov10
    }  
    
    
    
    
    
    })
    output$cinq22<-renderPrint({eltuc22()})
    
  })