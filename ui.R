suppressWarnings(library(shiny))
suppressWarnings(library(markdown))

shinyUI <- fluidPage(
        titlePanel ("Data_Science_Capstone: Words Prediction Project"),
        p("This app gives a predicted word based on the word the users input"),
        HTML("<strong>Author: Nhi Truong</strong>"),
        br(),
        HTML("<strong>Date: 03/24/2021 </strong>"),
        br(),
        
        sidebarLayout(
                sidebarPanel(
                        h3("Instructions:"),
                        h4("1. The users input a word or words in the text box"),
                        h4("2. The predicted next word will show in cyan as below"),
                        br()
                ),
        mainPanel(
                #tabsetPanel(
                        tabPanel("Predict words",
                        textInput("inputString", h3("Your input word(s):"),
                                  value = ""),
                        h3("The Predicted Next Word:"),
                        h4(em(span (textOutput("outtext"), style= "color:darkcyan")))),
        
                        #tabPanel("Top bi-grams",
                        #br(),
                        #img(src="bipic.png", height=500, width=700))
                
                        # )
        
                        )
                )
        )
        
        
                                   
                                          
                                            