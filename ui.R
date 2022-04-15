library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin="green",
                      #Application title
                      dashboardHeader(title = "Mental Health In Tech industry",titleWidth = 800),
                      # dashboard sidebar functions will be inserted here
                      dashboardSidebar(
                        
                        sidebarMenu(
                          menuItem("About",tabName ="about"),
                          menuItem("Analysis",tabName = "Analysis")
                          ),
                        radioButtons("Gender",
                                     label = "Select Gender:",
                                     choices = c('Female' = 'Female', 'Male'='Male','Others'= 'Others',"All" ="All"),
                                     selected = "All"),
                        sliderInput("Age",
                                    label = "Age range",
                                    min =  5,
                                    max = 100,
                                    step = 5,
                                    value = c(5,100),
                                    sep = ""),
                        radioButtons("CompanySize",
                                     label = "Select Company Size:",
                                     choices = c("6-25" = "6-25",
                                                 "26-100" = "26-100",
                                                 "500-1000" = "500-1000",
                                                 "More than 1000" = "More than 1000"),
                                     selected = "500-1000"),
                        uiOutput("typeSelectOutput"),
                        radioButtons("Leave",
                                     label = "Leave",
                                     choices =  c("Don't know" = "Don't know",
                                                  "Somewhat difficult" = "Somewhat difficult",
                                                  "Somewhat easy" = "Somewhat easy",
                                                  "Very difficult" = "Very difficult",
                                                  "Very easy" = "Very easy",
                                                  "None" = "None",
                                                  "All" ="All"
                                     ),
                                     selected = "All")
                        
                      ),
                      # functions that must go in the body of the dashboard.
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "about",
                                  h3("No of participants Country Wise"),
                                  plotOutput('participants'),
                                  h3("No of Participants Age wise"),
                                  plotOutput("Agewise")
                                  
                          ),
                  
                          tabItem(tabName = "Analysis",
                                  h3("Physical Health"),
                                  plotOutput('physicalhealth'),
                                  h3("Treatment Trend"),
                                  plotOutput('Treatmenttrend'),
                                  h3("Seek Help"),
                                  plotOutput("seekhelp")
                          )
                          
                        )
                      )
))
