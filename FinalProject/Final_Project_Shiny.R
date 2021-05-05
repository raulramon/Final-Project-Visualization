library(shiny)
library(tidyverse)
library(plotly)
library(gifski)
library(gganimate)
theme_set(theme_bw())

#Read Data: CTG
data <- read.csv("C:/Users/17875/Downloads/CTG.csv", header = TRUE)
#svmPer <- read.csv("svmpPerfomance_df.csv")
#svmPer


data$NSP <- as.factor(data$NSP)
data$Tendency <- as.factor(data$Tendency)


ui <- fluidPage(
 
  navbarPage(title = strong(h1("Final Project", h2("Cardiotogram Fetal Heart Rate and Uterine Contraction"))),
             
             div(
             
             HTML("<img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/heart_rateP.jpg>")
             
             ),
            
             br(),
             
    tabPanel("Introduction", 
             br(),
             mainPanel(
            p("CTG Data: This is a fetal heart rate (FHR) and Uterine contraction (UC) 
                       features on cardiotocograms"),
             p("A 2,126 fetal Cardiotocograms (CTGs) automatically processed and 
               diagnostic features measured"),
            p("CTGs classified by thrww expert obstetricians and consensus classification 
              label as Normal, Suspect, or Pathologic."),
            
    br(),
    br(),
    p("CTG DATA"),
    br(),
    p("LB - FHR baseline (beats pre minute)"),
    p("AC - # of accelerations per second"),
    p("FM - # of fetal movement per second"),
    p("UC - # of uterrine contractions per second"),
    p("DL - # of light decelerations per second"),
    p("DS - # of severe desacelerations per second"),
    p("DP - # of prolongued decelerations per second"),
    div("ASTV - percentage of time with abnormal short term variability", style = "color:red"),
    p("MSTV - mean value short term variability"),
    div("ALTV - percentage of time with abnormal long term variability", style = "color:red"),
    p("MLTV - mean value of long term variability"),
    p("Width - width of FHR histogram"),
    div("Min - minimum of FHR histogram", style = "color:red"),
    div("Max - Maximum of FHR histogram", style = "color:red"),
    p("Nmax - # of histogram peaks"),
    p("Mode - histogram mode"),
    p("Mean - histogram mean"),
    p("Variance - histogram variance"),
    p("Tendency - histogram tendency"),
    div("NSP - fetal state class code(N = normal; S = suspect; P = pathologic", style = "color:blue"),
    
    )),
             
    
    
    tabPanel("Methodology",
             
             mainPanel(
             p("Methodology:"),
             br(),
             strong("Random Forest"), 
             p(" Was developed by aggregating trees, it's multiple desicion trees"),
             p("Is a classification or regression, can avoid overfitting"),
             p("Can manage a big amount of variables 'features'"),
             p("Work for variables selection base on importance"),
             p("Random Forest give you a probability that belong to class"),
             br(),
             strong("Support Vector Machine (SVM)"),p("gives you distance to the boundary"),
             p("The probability still need to convert"),
             p("SVM gives you 'support vectors', 
               that is points in each class closest to the boundary between classes."),
             p("SVM models perform better on sparse data"),
             p("SVM will generally perform better on linear dependencies"),
             br(),
             strong("Neural Network:"), 
             p("A neural network is a series of algorithms that 
               endeavors to recognize underlying relationships in a set of data 
               through a process that mimics the way the human brain operates."),
             p("neural networks refer to systems of neurons"),
             p("Neural networks can adapt to changing input"),
             p("network generates the best possible result without needing to redesign the output"),
             br(),
             strong("Deep Neural Network:"), p("A deep neural network is a neural network with 
               a certain level of complexity"),
             p("Is a neural network with more than two layers"),
             p("Deep neural networks use sophisticated mathematical modeling to process data"),
             )),
    
             
             
    tabPanel("Plots", 
          
             sidebarPanel(selectInput(inputId = "x",
                                       label = "Explanatory Variable (x)",
                                      choices = list("LB - FHR baseline (beats pre minute)" = "LB",
                                                  "AC # of accelerations per second" = "AC",
                                                  "FM # of fetal movement per second" = "FM",
                                                  "UC # of uterrine contractions per second" = "UC",
                                                  "DL # of light decelerations per second" = "DL",
                                                  "DS # of severe desacelerations per second" = "DS",
                                                  "DP # of prolongued decelerations per second" = "DP",
                                                  "ASTV percentage of time with abnormal short term variability" = "ASTV",
                                                  "MSTV mean value short term variability" = "MSTV",
                                                  "ALTV percentage of time with abnormal long term variability" = "ALTV",
                                                  "Width width of FHR histogram" = "Width",
                                                  "Min minimum of FHR histogram" = "Min", 
                                                  "Max Maximum of FHR histogram" = "Max", 
                                                  "Nmax # of histogram peaks" = "Nmax",
                                                  "Mode histogram mode" = "Mode",
                                                  "Mean histogram mean" = "Mean",
                                                  "Variance histogram variance" = "Variance")
                                                  ),
                          
                                      
                          selectInput(inputId = "y",
                                      label = "Response Variable (y)",
                                      choices = list("LB - FHR baseline (beats pre minute)" = "LB",
                                                  "AC # of accelerations per second" = "AC",
                                                  "FM # of fetal movement per second" = "FM",
                                                  "UC # of uterrine contractions per second" = "UC",
                                                  "DL # of light decelerations per second" = "DL",
                                                  "DS # of severe desacelerations per second" = "DS",
                                                  "DP # of prolongued decelerations per second" = "DP",
                                                  "ASTV percentage of time with abnormal short term variability" = "ASTV",
                                                  "MSTV mean value short term variability" = "MSTV",
                                                  "ALTV percentage of time with abnormal long term variability" = "ALTV",
                                                  "Width width of FHR histogram" = "Width",
                                                  "Min minimum of FHR histogram" = "Min", 
                                                  "Max Maximum of FHR histogram" = "Max", 
                                                  "Nmax # of histogram peaks" = "Nmax",
                                                  "Mode histogram mode" = "Mode",
                                                  "Mean histogram mean" = "Mean",
                                                  "Variance histogram variance" = "Variance")
                                      
                                      
                                    )),
                          
              sidebarPanel(    
             mainPanel("Plot"),
             plotlyOutput(outputId = "theplot",
                          width = 1100,
                          height = 750)
             )),
    
    
    
    tabPanel("Result", 
             navlistPanel(tabPanel("Deep Neural Network",
                         sidebarPanel(
                           div(HTML("<left><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/DNn_confMatrix_Accuracy.jpg>"))
                         ),
                                   
                                   div(HTML("<center><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/DNn_KerasSeq_50_3layer.png>
                                  </br> <img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/DNnK8_3layer.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/DNnK_50_8_3layer.png>
                                            </center>"))),
                                   
                                   
                          
                          
                          
                          tabPanel("Neural Network",
                                   
                                   
                                   div(HTML("<center><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/NeuralNet_1hidden.jpg>
                                  </br> <img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/NeuralNet_5hidden.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/NeuralNet2_1hidden.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/NeuralNet5_Rhidden.jpg>
                                            </center>"))),
                          
                          tabPanel("Support Vector Machine",
                                   
                                   
                                   div(HTML("<center><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/SVMRadial_ALTV_ASTV.jpg>
                                  </br> <img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/SVMLinear_ALTV_ASTV.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/SVMPolynomial_ALTV_ASTV.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/SVMSigmoid_ALTV_ASTV.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/SVMPerformance_CostEpsilonError.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/SVM_Best_Radial.jpg>
                                            </center>")),
                                   plotOutput(outputId = "svmplot")
                                   ),
                          
                          tabPanel("Random Forest",
                                   
                                   
                                   div(HTML("<center><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/ErroRateRF.jpg>
                                  </br> <img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/OOBError_Mtry.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/TreesizeRf300_hist.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/T10VImp_RF.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/RF_ParDepen_ASTV1.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/RF_ParDepen_ASTV2.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/RF_ParDepen_ASTV3.jpg>
                                  </br><img src=https://raw.githubusercontent.com/raulramon/Final-Project-Visualization/main/Images/MultDimScaP_ProxMatrix_RF.jpg>
                                            </center>")))),
             
            
             
                          
                          
                          )
    
)
  
)


server <- function(input, output){
  
  output$theplot <- renderPlotly({
   
    names_of_features <- c(input$x, input$y, "NSP")
data %>% 
      select(names_of_features[1], names_of_features[2], names_of_features[3]) %>%
      transmute(x = data[,names_of_features[1]], y = data[,names_of_features[2]]
                , class = data[,names_of_features[3]]) %>%
      ggplot(mapping = aes(x = x,
                            y = y,
                           color = class )) + 
      geom_point() +
  labs(title = paste(input$x, "VS", input$y, "Colored by NSP"),
       x = paste(input$x),
       y = paste(input$y))



  })
  
  output$svmplot <- renderImage({
    
  p <- svmPer %>%
    ggplot(aes(dispersion, error, size = epsilon, colour = cost)) +
      geom_point(alpha = 0.7, show.legend = FALSE)+
      labs(title = "Error dispersion for SVM best model: {frame_time}", x = "Dispersion", y = "Error") +
      transition_time(dispersion) +
      ease_aes('linear')
    
   anim_save('svmPlot.gif', animate(p))
   list(src = 'svmplot.gif',
        contentType = 'image/gif',
        width =600,
        height =500,
        alt = "Error Dispersion for SVM best Model NSP dependant")
   
  })
  
}

shinyApp(ui = ui, server = server)

# Neural Network
# Deep Learning Neural Network
# Random Forest
# SVM
# AutoML model