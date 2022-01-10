
# this is a Shiny web application. 
# you can run the application by clicking the 'Run App' button above.

# find out more about building applications with Shiny here: at http://shiny.rstudio.com/

# install and load packages ----------------------------------


#install.packages("lme4") https://cran.r-project.org/web/packages/lme4/lme4.pdf
#install.packages("shinythemes") https://rstudio.github.io/shinythemes/
#install.packages("rsconnect") hosting on shinyapps.io
# recently updated R / R Studio: issue with ggplot > pillar:
#install.packages("devtools")
#devtools::install_github("r-lib/pillar")
#install.packages("ggplot2")

library(shiny)
library(lme4)
library(shinythemes)
library(rsconnect)
library(devtools)
library(pillar)
library(ggplot2)

# connecting to shinyapps.io --------------------------------------

# first need to create an account at https://www.shinyapps.io
# after the account is created, go to Account > Tokens > Show
# this will give you the below line of code but for your account

# authorize the shinyapps.io account
#rsconnect::setAccountInfo(name='hernandezb', token='', secret='')

# deploying the app in shinyapps.io
# run the app, in the top right corner, there is a publish button. Use this to deploy the app as a webpage


# Define UI --------------------------------------
ui <- fluidPage(theme = shinytheme("paper"), # fluid pages automatically adjust to the browser window size
    
        # creating a row for the header
        # this allows it to be fixed across pages
        fluidRow( 
        
        # adds a panel at the top of the page
        # this will not change when you move pages
        titlePanel(
          HTML('<p><img src="logo.png" width=300 height=30/></p>')),
        
      ), # fluidRow           

      
      # creates a new row which will have multiple pages
      fluidRow(
        
        # creating a navigation bar design --------------------------------------
        navbarPage("", id = "navbar",
        
                   
        # designing page 1 --------------------------------------
        tabPanel(title = "Page 1", value = "page1",
            
              # uses a layout with sidebar (left sidebar + main panel)
              # sidebar with a slider input for number of bins 
              sidebarLayout(
                
                  sidebarPanel(
                    
                      selectInput("sample1", label = "Sample", 
                                  choices = list("All Polls" = 1,
                                  "Polls of likely or registered voters" = 2, 
                                  "Polls of adults" = 3), selected = 1),
                      
                      textOutput("print")
                      
                  ), # sidebarPanel
          
                  
                  # show a plot of the generated distribution
                  mainPanel(
                    
                    plotOutput("scatter1")
                         
                  ) # mainPanel
              
            ), # sidebarLayout
            
            fluidRow(
              
              column(2,
                
                actionButton('nextButton', 'Next Page') 
              
              ) # column
            ) # fluidRow
            
        ), # tabPanel
        
        
        # designing page 2 -------------------------------------- 
        tabPanel(title = "Page 2", value = "page2", 
            
            # uses a more fluid layout 
            # stacks rows of element
            # use column() to add columns within a given row 
            # there's a total of 12 rows
            fluidRow(
              
              # to get the input on the right side, we create 8 empty columns
              # alternatively could use offset = 8 in the second column (see comment)
              column(8,
                
              ), # column
              
              # the input goes in the last 4 columns
              column(4, # offset = 8,
                     selectInput("sample2", label = "Sample", 
                              choices = list("All Polls" = 1,
                              "Polls of likely or registered voters" = 2, 
                              "Polls of adults" = 3), selected = 1)
            
              ) # column
              
              
              
            ), # fluidRow
            
            # in the next row we add the plot
            fluidRow(
              
              column(12,
                     
                     plotOutput("scatter2")  
                     
                     
              ) # column 
              
            ) # fluidRow
        
        ) # tabPanel
        
        ) # navbarPage
      ) # fluidRow
) # fluidPage


# Define server ----------------------------------
server <- function(input, output, session) {
  
    observeEvent(input$nextButton, {
      
        updateTabsetPanel(session, "navbar", selected = "page2")
      
    }) # observeEvent
    
  
    # loading data ----------------------------------
    

    
    # data from: https://projects.fivethirtyeight.com/biden-approval-rating/?ex_cid=rrpromo
    poll <- read.csv("./approval_polllist.csv", header=TRUE)
    
    # recoding the subgroups to a variable called sample
    poll$sample[poll$subgroup=="All polls"] <- 1
    poll$sample[poll$subgroup=="Voters"] <- 2
    poll$sample[poll$subgroup=="Adults"] <- 3
    

    observeEvent(input$sample1, {
      
      print(input$sample1)
      
    }) # observeEvent
    
        
        output$scatter1 <- renderPlot({
          
          ggplot() +
            geom_point(data = poll[poll$sample==input$sample1,], aes(x = as.Date(enddate, "%m/%d/%Y"), 
                                                                     y = adjusted_approve), colour = '#34840C', size = 2, alpha = 1, shape = 16) +
            geom_point(data = poll[poll$sample==input$sample1,], aes(x = as.Date(enddate, "%m/%d/%Y"), 
                                                                     y = adjusted_disapprove), colour = '#FE640C', size = 2, alpha = 1, shape = 16) +
            xlab("") + ylab("")
      
        }) # renderPlot

      
        output$scatter2 <- renderPlot({
        
          ggplot() +
            geom_point(data = poll[poll$sample==input$sample2,], aes(x = as.Date(enddate, "%m/%d/%Y"), 
                                                                     y = adjusted_approve), colour = '#34840C', size = 2, alpha = 1, shape = 16) +
            geom_point(data = poll[poll$sample==input$sample2,], aes(x = as.Date(enddate, "%m/%d/%Y"), 
                                                                     y = adjusted_disapprove), colour = '#FE640C', size = 2, alpha = 1, shape = 16) +
            xlab("") + ylab("")
          
        }) # renderPlot
    
    
  
} # server



# Run the application 
shinyApp(ui = ui, server = server) # this line combines the ui and server


