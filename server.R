#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(caret)
library(ROCR)
library(pROC)
library(rlist)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(plyr)
library(DMwR)
library(ggthemes)

### data initial loading
model_dir = "models"
data_dir = "data"
saved_models = list.files(model_dir)

### load data
data_initial=read.csv(paste(data_dir,"bank-additional-full.csv",sep="/"), header = TRUE, sep = ";")
data_original =  select(data_initial,-duration)
data_preprocess = data_original

### load models
for(file in saved_models) {
  load(paste(model_dir,file,sep="/"))
}

best_model = model_glmnet_mannual_feature_selection


### preprocessing data

data_preprocess$age_group = cut2(data_preprocess$age,c(20,30,40,50,60))
data_preprocess$age_group = mapvalues(data_preprocess$age_group, from = levels(data_preprocess$age_group), to = c("teens", "20s", "30s", "40s", "50s", "seniors"))
data_preprocess = data_preprocess[,colnames(data_preprocess) != "age"]

data_preprocess$pdays_cat = factor(ifelse(data_preprocess$pdays!=999,"contacted_before","not_contacted_before"))
data_preprocess = select(data_preprocess,-pdays)

data_preprocess$campaign_cat = factor(ifelse(data_preprocess$campaign>3 , ">3",data_preprocess$campaign))
data_preprocess = select(data_preprocess,-campaign)

data_preprocess$emp.var.rate.cat = factor(cut2(data_preprocess$emp.var.rate,c(-1.8,-0.1)))
data_preprocess = select(data_preprocess,-emp.var.rate)

for(col in c("cons.conf.idx","cons.price.idx","euribor3m","nr.employed")) {
  newCol = paste(col,".cat",sep="")
  data_preprocess[[newCol]]=factor(cut2(data_preprocess[,col],c(quantile(data_preprocess[,col], 0.25, na.rm=TRUE)[[1]],
                                          mean(data_preprocess[,col]),
                                          quantile(data_preprocess[,col], 0.75, na.rm=TRUE)[[1]])))
  data_preprocess = select(data_preprocess,-col)
}

set.seed(42)
split = createDataPartition(data_preprocess$y, times = 1, p=0.25, list = F)
test = data_preprocess[-split,]
train = data_preprocess[split,]
train_x = train[,names(train)!="y"]
train_y = train$y
folds <- createFolds(train_y, k = 5)

shinyServer(function(input, output,session) {
  data <- reactive({
    switch(input$datasetOption,
           "Original" = data_original,
           "Preprocessed" = data_preprocess)
  })
  
  costChange = reactiveValues(maxThreshold = 0.05)
  
  
  observe({
    updateSelectInput(session, "var", choices = names(data()))
    xy = c(input$call_cost, input$gain_success)
    if (!is.null(xy)){
      roi_list = list()
      max_roi_vec = c()
      max_thres_vec = c()
      for (fold in folds) {
        cvdata = train[fold,]
        p = predict(best_model, cvdata, type = "prob")[,2]
        roi = data.frame(thres=c(),roi=c())
        for(thres in seq(0.15,1,0.01)) {
          pred = ifelse(p>thres,"yes","no")
          cost = input$call_cost * sum(pred=="yes")
          gain = input$gain_success * sum(cvdata$y=="yes" & pred=="yes")
          roi<-rbind(roi, data.frame(thres=thres, roi=gain-cost))
        }
        roi_list = list.append(roi_list,roi)
        max_roi_vec = c(max_roi_vec,max(roi$roi))
        max_thres_vec = c(max_thres_vec,roi$thres[which.max(roi$roi)])
      }
      max_thres_mean = mean(max_thres_vec)
      costChange$maxThreshold <- max_thres_mean
    }

  })

  output$data_table <- DT::renderDataTable({
    data <- switch(input$datasetOption,
                   "Original" = data_original,
                   "Preprocessed" = data_preprocess)
  })
  
  output$summary <- renderPrint({
    data <- switch(input$datasetOption,
                   "Original" = data_original,
                   "Preprocessed" = data_preprocess)
    summary(data)
  })
  
  output$correlationGraph <- renderPlot({
    data <- switch(input$datasetOption,
                   "Original" = data_original,
                   "Preprocessed" = data_preprocess)
    data_num=data[,sapply(data, is.numeric)]
    data_num=cbind(data_num,y=as.integer(data$y))
    cor_result=rcorr(as.matrix(data_num))
    ggcorrplot(cor_result$r, hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 3, 
               method="circle", 
               colors = c("tomato2", "white", "springgreen3"), 
               title="Correlogram of data_num", 
               ggtheme=theme_bw)
    
  })
  
  output$distPlot <- renderPlot({
    data <- switch(input$datasetOption,
                   "Original" = data_original,
                   "Preprocessed" = data_preprocess)
    col <- input$var
    if(is.numeric(data[,col])){
      p3 = ggplot(data, aes(x=y, y=data[,col])) + 
        geom_boxplot(fill="slateblue", alpha=0.2) + 
        xlab("Target?") + ylab(col)
      
      df_temp = data.frame(table(data[,col],data$y))
      colnames(df_temp) =c(col,"y","freq")
      df_temp[,col] = as.numeric(df_temp[,col])
      p1 = ggplot(df_temp, aes(x=df_temp[,col], y=freq,fill=y)) + geom_area() + 
        labs(x=col,y="freq")
      
      df_temp = data.frame(prop.table(table(data[,col],data$y),1))
      colnames(df_temp) =c(col,"y","freq")
      df_temp[,col] = as.numeric(df_temp[,col])
      p2 = ggplot(df_temp, aes(x=df_temp[,col], y=freq,fill=y)) + geom_area() + 
        labs(x=col,y="freq%")
      
      p = ggarrange(p1, p2, p3, ncol = 3, nrow = 1, common.legend = TRUE)

      print(p)
      
    } else {
      p1 = ggplot(data, aes(data[,col], fill = y)) + geom_bar(position="fill") +
           labs(x = "Target?", y = col) +
           theme(axis.text.x=element_text(angle = -90, hjust = 0))
      p2 = ggplot(data, aes(data[,col], fill = y)) + geom_bar() +
           labs(x = "Target?", y = col) +
           theme(axis.text.x=element_text(angle = -90, hjust = 0))
      p3 = ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE)
      print(p3)
    }

  })
  
  output$var_description <- renderText({
    text <- switch(input$var, 
                   "age" = "Customer's age (numeric)",
                   "age_group" = "Proccessed customer's age into categorical: teens, 20s, 30s, 40s, 50s and seniors",
                   "job" = "Type of job (categorical)",
                   "marital" = "Marital Status (categorical)", 
                   "education" = "Education (categorical)",
                   "default" = "Has credit in default? (categorical)",
                   "housing" = "Has housing loan? (categorical)",
                   "loan" = "Has personal loan? (categorical)",
                   "contact" = "Last contact's communication type (categorical)",
                   "month" = "Last contact month of year (categorical)",
                   "day_of_week" = "Last contact day of the week (categorical)" ,
                   "campaign" = "Number of contacts performed during this campaign and for this client (numeric, includes last contact)",
                   "campaign_cat" = "Proccessed numeric attribute campaign to categorical: 1,2,3 and >3",
                   "pdays" = "Number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)",
                   "pdays_cat" = "Proccessed numeric attribute pdays to categorical: contacted before and not contacted before",
                   "previous" = "Number of contacts performed before this campaign and for this client (numeric)",
                   "poutcome" = "Outcome of the previous marketing campaign (categorical)",
                   "emp.var.rate" = "Employment variation rate - quarterly indicator (numeric)",
                   "cons.price.idx" = "Consumer price index - monthly indicator (numeric)",
                   "cons.conf.idx" = "Consumer confidence index - monthly indicator (numeric)",
                   "euribor3m" = "Euribor 3 month rate - daily indicator (numeric)",
                   "nr.employed" = "Number of employees - quarterly indicator (numeric)",
                   "emp.var.rate.cat" = "Proccessed numeric attribute emp.var.rate to categorical",
                   "cons.price.idx.cat" = "Proccessed numeric attribute cons.price.idx to categorical",
                   "cons.conf.idx.cat" = "Proccessed numeric attribute cons.conf.idx to categorical",
                   "euribor3m.cat" = "Proccessed numeric attribute euribor3m to categorical",
                   "nr.employed.cat" = "Proccessed numeric attribute nr.employed to categorical")
  })
  
  output$modelPlot <- renderPlot({
    model_list = list(rf=model_rf,
                      glmnet=model_glmnet,
                      glmnet_pca=model_glmnet_pca,
                      glmnet_manual_feature=model_glmnet_mannual_feature_selection,
                      rpart=model_rpart,
                      nb=model_nb,
                      stack=model_stack,
                      glm_step_AIC = model_glm_stepAIC
    )
    resamps <- resamples(model_list)
    summary(resamps, metric = "ROC")
    dotplot(resamps, metric = "ROC")

  })
  
  output$model_details <- DT:: renderDataTable({
    results <- switch(input$model,
                    "RPart" = model_rpart$results,
                    "Naive Bayes" = model_nb$results,
                    "Random Forest" = model_rf$results,
                    "GLM Net" = model_glmnet$results,
                    "GLM PCA" = model_glmnet_pca$results,
                    "GLM Manual Feature Selection" = model_glmnet_mannual_feature_selection$results,
                    "Stack" = model_stack$results,
                    "GLM Step AIC" = model_glm_stepAIC$results )
  })
  
  
  output$threshold <- renderPlot({
    roi_list = list()
    max_roi_vec = c()
    max_thres_vec = c()
    for (fold in folds) {
      cvdata = train[fold,]
      p = predict(best_model, cvdata, type = "prob")[,2]
      roi = data.frame(thres=c(),roi=c())
      for(thres in seq(0.15,1,0.01)) {
        pred = ifelse(p>thres,"yes","no")
        cost = input$call_cost * sum(pred=="yes")
        gain = input$gain_success * sum(cvdata$y=="yes" & pred=="yes")
        roi<-rbind(roi, data.frame(thres=thres, roi=gain-cost))
      }
      roi_list = list.append(roi_list,roi)
      max_roi_vec = c(max_roi_vec,max(roi$roi))
      max_thres_vec = c(max_thres_vec,roi$thres[which.max(roi$roi)])
    }
    max_roi_mean = mean(max_roi_vec)
    max_thres_mean = mean(max_thres_vec)
    ggplot(roi_list[[1]],aes(x=thres,y=roi)) +
      geom_line() +
      geom_line(data=roi_list[[2]],color="red") +
      geom_line(data=roi_list[[3]],color="green") +
      geom_line(data=roi_list[[4]],color="purple") +
      geom_line(data=roi_list[[5]],color="yellow") +
      labs(title ="Decision Threshold vs ROI", x = "threshold(0~100%)", y = "ROI") +
      annotate("text", x = 0.9, y = max_roi_mean,
               label = c(paste("Max ROI mean:",max_roi_mean,"\nthreshold mean:",max_thres_mean)) , color="orange", size=5 , angle=0)
    
  })
 
  p = predict(best_model, test, type = "prob")[,2]
  
  output$auc <- renderPlot({
    
    plot.roc(test$y,p,print.auc=T)
  })
  
  output$matrix <- renderPrint({
    confusionMatrix(as.factor(ifelse(p>costChange$maxThreshold,"yes","no")),test$y,mode="prec_recall")
  })
  
  output$roi <- renderTable({
    pred = ifelse(p>costChange$maxThreshold,"yes","no")
    costByPredict = input$call_cost * sum(pred=="yes")
    gainByPredict = input$gain_success * sum(test$y=="yes" & pred=="yes")
    ROIByPredict = gainByPredict - costByPredict
    costCallAll = input$call_cost * dim(test)[1]
    gainCallAll = input$gain_success * sum(test$y=="yes")
    ROIByCallAll = gainCallAll- costCallAll

    ROITable <- data.frame(
      Name = c("Cost Caused By Predict",
               "Gain Earned By Predic",
               "ROI Earned By Predict",
               "Cost Caused By Call All",
               "Gain Earned By Call All",
               "ROI Earned By Call All"),
      Value = as.character(c(costByPredict,gainByPredict,ROIByPredict, costCallAll,gainCallAll,ROIByCallAll)),
      stringsAsFactors = FALSE)
  })
  
  output$importantAttributes <- renderPlot({
    imp    <- varImp(best_model)$importance
    varImportance <- data.frame(Variables = row.names(imp),
                                Importance = imp$Overall)
    # Create a rank variable based on importance
    rankImportance <- varImportance %>%
      mutate(Rank = paste0('#',dense_rank(desc(Importance))))
    rankImportance = rankImportance[order(rankImportance$Importance,decreasing = T),]
    rankImportance = rankImportance[1:10,]
    # Use ggplot2 to visualize the relative importance of variables
    ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                               y = Importance, fill = Importance)) +
      geom_bar(stat='identity') +
      geom_text(aes(x = Variables, y = 0.5, label = Rank),
                hjust=0, vjust=0.55, size = 4, colour = 'red') +
      labs(x = 'Variables') +
      coord_flip() +
      theme_few()
  })
  

})
