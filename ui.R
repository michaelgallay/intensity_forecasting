
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)




ui <- navbarPage("Intensity Forecast ",
                 
                 

                 
                 
tabPanel("Early / Late", 
         
         titlePanel(title=div(img(height = 70, width = 54,src="Y:/WFM - forecast/analysis/int_forecast/rbc_logo2.png"), "Collections Intensity Summary")),
         
         
         
         
         sidebarLayout(
            sidebarPanel(
                h3("Instructions"),
                
                p("This application is made to assit you in your intensity forecasts."),
                
                br(),
                
                p("You can navigate in the different tabs to set your parameters for each teams."),
                
                br(), 
                
                p("The summary for ", strong("Early and Late stage "), "in the current tab is updated in real time based on the results of all different teams. "),
                
                br(), 
                
                p("Similar to the graph and table you see on the right the month", strong(" m"), " refers to the current month intensity forecast and all month", strong(" m+"), "to future month based on forecasted volume."),
                
                br(),

                p("The forecasted volume can be changed by adjusting the Seasonal Factors for each team."),
                
                br(),
                
                p("The following", strong("save"),  " and", strong( "load"), " options allows you to save you parameters and load presets. Just enter the name with no extension. Parameters will be saved to a .csv file."),
                
                br(),
                
                p("Becareful if you choose a name already existing, data will be overwritten"),
                
                
            #textInput to name file to save parameters    
                
            textInput(inputId = "save_file_name", label = ""),
                        
            actionButton(inputId = "save", label = "save"), 
            
            #Select input to load parameters to environment 
            
            
            
            selectInput(inputId = "selectl", label = "", choices = dir("param") ), 
            
           
            actionButton(inputId = "load", label = "load")
                      
                
                             
                ),
             
            mainPanel(plotOutput('col_plot'), tableOutput('summar')))
         
            
             
            
            
                 
         ),
                 
                 
                       


                 
                 
tabPanel("MXELO",

  # Application title
  

  # Sidebar with a slider input for number of bins
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("volume_elo",
                  "Average weekly volume:",
                  min = 210000,
                  max = 400000,
                  value = 273495),
      sliderInput("aht_elo",
                  "AHT (seconds):",
                  min = 80,
                  max = 150,
                  value = 119),
      sliderInput("shrink_me",
                  "Shrinkage ME:",
                  min = 10,
                  max = 60,
                  value = 25),
      sliderInput("shrink_mt",
                  "Shrinkage MT:",
                  min = 10,
                  max = 60,
                  value = 40),
      numericInput("staff_me", 
                   "Staff ME:",
                   min = 0,
                   value = 50),
      numericInput("staff_mt", 
                   "Staff MT:",
                   min = 0,
                   value = 26),
      numericInput("ntr_me", 
                   "NTR ME:", 
                   min = 0,
                   value = 5),
      numericInput("ntr_mt", 
                   "NTR MT:", 
                   min = 0,
                   value = 6)
      
      
    ),

    # Show a plot of forecasted intensity + table / main panel
    mainPanel(
        tabsetPanel(
            tabPanel(
            
                "Overview",
                plotOutput(outputId = 'elo_plot'),
                tableOutput('values')),
                
             tabPanel("Seasonal Factors",
                      
                      
                      numericInput("elo_1", 
                                   "month +1:",
                                   step = 0.01,
                                   value = 0.9889),
                      numericInput("elo_2", 
                                   "month +2:",
                                   step = 0.01,
                                   value = 1.0242),
                      numericInput("elo_3", 
                                   "month +3:",
                                   step = 0.01,
                                   value = 1.016),
                      numericInput("elo_4", 
                                   "month +4:",
                                   step = 0.01,
                                   value = 1.1448),
                      numericInput("elo_5", 
                                   "month +5:",
                                   step = 0.01,
                                   value = 0.8904)
                      
                      )   
    
     ) # close tabset panel
    ) # close main panel
  ) #close sidebarlayout
), #closing tabpanel
tabPanel("MXEMB",
         
         sidebarLayout(
             sidebarPanel(
             
                 sliderInput("volume_emb", 
                             "Average weekly volume:",
                             min = 35000,
                             max = 60000,
                             value = 53637),
                 
                 sliderInput("aht_emb", 
                             "AHT (seconds):",
                             min = 140,
                             max = 220,
                             value = 197), 
                 
                 sliderInput("ah_emb", 
                             "Agent Hours (per week):", 
                             min = 200,
                             max = 700,
                             value = 416)
             
             
             ), 
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("overview", plotOutput(outputId = "emb_plot"), tableOutput('emb_pred') ),
                     #plotOutput(outputId = 'elo_plot'),
                     #tableOutput('values')),
                     
                     tabPanel("Seasonal Factors",
                              numericInput("emb_1", 
                                           "month +1:",
                                           step = 0.01,
                                           value = 0.9772),
                              numericInput("emb_2", 
                                           "month +2:",
                                           step = 0.01,
                                           value = 1.001),
                              numericInput("emb_3", 
                                           "month +3:",
                                           step = 0.01,
                                           value = 1.1182),
                              numericInput("emb_4", 
                                           "month +4:",
                                           step = 0.01,
                                           value = 1.073),
                              numericInput("emb_5", 
                                           "month +5:",
                                           step = 0.01,
                                           value = 0.9106))
                              
                     
                     
                     
                     
                     )))
        
         ),

tabPanel("MXEHB",
         
         sidebarLayout(
             sidebarPanel(
                 
                 sliderInput("volume_ehb", 
                             "Average weekly volume:",
                             min = 5000,
                             max = 20000,
                             value = 10143),
                 
                 sliderInput("aht_ehb", 
                             "AHT (seconds):",
                             min = 140,
                             max = 250,
                             value = 194), 
                 
                 sliderInput("ah_ehb", 
                             "Agent Hours (per week):", 
                             min = 50,
                             max = 300,
                             value = 138)
                 
                 
             ), 
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("overview", plotOutput(outputId = "ehb_plot"), tableOutput('ehb_pred') ),
                     #plotOutput(outputId = 'elo_plot'),
                     #tableOutput('values')),
                     
                     tabPanel("Seasonal Factors",
                              numericInput("ehb_1", 
                                           "month +1:",
                                           step = 0.01,
                                           value = 0.9101),
                              numericInput("ehb_2", 
                                           "month +2:",
                                           step = 0.01,
                                           value = 1.0538),
                              numericInput("ehb_3", 
                                           "month +3:",
                                           step = 0.01,
                                           value = 1.0137),
                              numericInput("ehb_4", 
                                           "month +4:",
                                           step = 0.01,
                                           value = 1.0577),
                              numericInput("ehb_5", 
                                           "month +5:",
                                           step = 0.01,
                                           value = 0.8945))
                     
                     
                     
                     
                     
                 )))
         
),


tabPanel("MXLLB",
         
         sidebarLayout(
             sidebarPanel(
                 
                 sliderInput("volume_llb", 
                             "Average weekly volume:",
                             min = 20000,
                             max = 80000,
                             value = 42852),
                 
                 sliderInput("aht_llb", 
                             "AHT (seconds):",
                             min = 140,
                             max = 400,
                             value = 236), 
                 
                 sliderInput("ah_llb", 
                             "Agent Hours (per week):", 
                             min = 100,
                             max = 800,
                             value = 513)
                 
                 
             ), 
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("overview", plotOutput(outputId = "llb_plot"), tableOutput('llb_pred') ),
                     #plotOutput(outputId = 'elo_plot'),
                     #tableOutput('values')),
                     
                     tabPanel("Seasonal Factors",
                              numericInput("llb_1", 
                                           "month +1:",
                                           step = 0.01,
                                           value = 1.0273),
                              numericInput("llb_2", 
                                           "month +2:",
                                           step = 0.01,
                                           value = 1.0287),
                              numericInput("llb_3", 
                                           "month +3:",
                                           step = 0.01,
                                           value = 1.0126),
                              numericInput("llb_4", 
                                           "month +4:",
                                           step = 0.01,
                                           value = 1.1195),
                              numericInput("llb_5", 
                                           "month +5:",
                                           step = 0.01,
                                           value = 0.9383))
                     
                     
                     
                     
                     
                 )))
         
),

tabPanel("MXLMB",
         
         sidebarLayout(
             sidebarPanel(
                 
                 sliderInput("volume_lmb", 
                             "Average weekly volume:",
                             min = 25000,
                             max = 45000,
                             value = 33871),
                 
                 sliderInput("aht_lmb", 
                             "AHT (seconds):",
                             min = 190,
                             max = 300,
                             value = 270), 
                 
                 sliderInput("ah_lmb", 
                             "Agent Hours (per week):", 
                             min = 100,
                             max = 800,
                             value = 481)
                 
                 
             ), 
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("overview", plotOutput(outputId = "lmb_plot"), tableOutput('lmb_pred') ),
                     #plotOutput(outputId = 'elo_plot'),
                     #tableOutput('values')),
                     
                     tabPanel("Seasonal Factors",
                              numericInput("lmb_1", 
                                           "month +1:",
                                           step = 0.01,
                                           value = 1.0383),
                              numericInput("lmb_2", 
                                           "month +2:",
                                           step = 0.01,
                                           value = 1.0031),
                              numericInput("lmb_3", 
                                           "month +3:",
                                           step = 0.01,
                                           value = 1.0192),
                              numericInput("lmb_4", 
                                           "month +4:",
                                           step = 0.01,
                                           value = 1.1572),
                              numericInput("lmb_5", 
                                           "month +5:",
                                           step = 0.01,
                                           value = 0.9442))
                     
                     
                     
                     
                     
                 )))
         
),

tabPanel("MXLHB",
         
         sidebarLayout(
             sidebarPanel(
                 
                 sliderInput("volume_lhb", 
                             "Average weekly volume:",
                             min = 5000,
                             max = 10000,
                             value = 8027),
                 
                 sliderInput("aht_lhb", 
                             "AHT (seconds):",
                             min = 250,
                             max = 400,
                             value = 331), 
                 
                 sliderInput("ah_lhb", 
                             "Agent Hours (per week):", 
                             min = 100,
                             max = 300,
                             value = 180)
                 
                 
             ), 
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("overview", plotOutput(outputId = "lhb_plot"), tableOutput('lhb_pred') ),
                     #plotOutput(outputId = 'elo_plot'),
                     #tableOutput('values')),
                     
                     tabPanel("Seasonal Factors",
                              numericInput("lhb_1", 
                                           "month +1:",
                                           step = 0.01,
                                           value = 0.9626),
                              numericInput("lhb_2", 
                                           "month +2:",
                                           step = 0.01,
                                           value = 0.97),
                              numericInput("lhb_3", 
                                           "month +3:",
                                           step = 0.01,
                                           value = 1.0433),
                              numericInput("lhb_4", 
                                           "month +4:",
                                           step = 0.01,
                                           value = 1.154),
                              numericInput("lhb_5", 
                                           "month +5:",
                                           step = 0.01,
                                           value = 0.8742))
                     
                     
                     
                     
                     
                 )))
         
)





)

































