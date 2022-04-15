
#######Importing Libraries ########

library('shiny')
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library('ggplot2')
library("DT")
library('dplyr')
library('markdown')
library('zoo')
library('tidyr')
library('rworldmap')
library('rnaturalearth')
library('rnaturalearthdata')

#######  Data Cleaning  ########
mental_health_survey <- read.csv("~/Sem3/CPSC/Lab2/Ankit_RShiny/survey.csv")

str(mental_health_survey)

#summary
summary(mental_health_survey)

#summary
sapply(mental_health_survey, function(x) sum(is.na(x)))


#replace missing values
mental_health_survey$state <- mental_health_survey$state %>% replace_na("NA")
mental_health_survey$work_interfere <- mental_health_survey$work_interfere %>% replace_na("Not available")
mental_health_survey$self_employed <- mental_health_survey$self_employed %>% replace_na("Not available")

#dropping Comments column 
mental_health_survey$comments <- NULL

# check for missing values
sapply(mental_health_survey, function(x) sum(is.na(x)))

####accuracy check ###
str(mental_health_survey)

# for numerical variables
summary(mental_health_survey)

#histogram
hist(mental_health_survey$Age,col ="blue")

#removing outliers
mental_health_survey<-mental_health_survey[!(mental_health_survey$Age < 0 | mental_health_survey$Age > 100),]


# for categorical variables

table(mental_health_survey['no_employees'])
table(mental_health_survey['Gender'])

#grouping the gender column
mental_health_survey$Gender <- as.character(mental_health_survey$Gender)
mental_health_survey$Gender <- tolower(mental_health_survey$Gender)

Male <- c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle')


Female <- c('female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail')

Others <- c('ostensibly male, unsure what that really means','others','a little about you','Agender','androgyne','fluid', 'genderqueer',  
            'guy (-ish) ^_^', 'male leaning androgynous','queer','p','all','trans-female', 'trans woman',
            'female (trans)','something kinda male?','neuter','queer/she/they', 'non-binary', 'nah', 'enby')


Gender_new <- as.vector(mental_health_survey$Gender)  
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Male) "Male" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Female) "Female" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Others) "Others" else x)

mental_health_survey$Gender<- Gender_new

#converting to factor
mental_health_survey$no_employees <- as.factor(mental_health_survey$no_employees)
mental_health_survey$treatment <- as.factor(mental_health_survey$treatment)
mental_health_survey$seek_help <- as.factor(mental_health_survey$seek_help)


#cleaned data
head(mental_health_survey)

#Age grouping
mental_health_survey$Age1<-cut(mental_health_survey$Age, c(0,20,30,50,100))



###### Server #######

shinyServer(function(input, output) {
  
  #Country list
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput","Select country",
                sort(unique(mental_health_survey$Country)),
                multiple = TRUE,
                selected = c("Canada","United States","United Kingdom"))
    
  })
  
  output$participants <- renderPlot({
    if(input$Gender =="All")
    {
      plot <- mental_health_survey %>% 
        filter(Country %in% input$typeInput) %>%
        ggplot(aes(x = Country, fill = Country)) + geom_bar()
    }
    else
    {
      plot <- mental_health_survey %>% 
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender)
        ggplot(aes(x = Country, fill = Country)) + geom_bar()
    }
    plot
  })
  
  output$Agewise <- renderPlot({
    if(input$Gender =="All")
    {
      plot <- mental_health_survey %>% 
        filter(Country %in% input$typeInput) %>%
        ggplot(aes(x =Age1,fill = Age1)) + geom_bar()
    }
    else
    {
      plot <- mental_health_survey %>% 
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        ggplot(aes(x =Age1,fill = Age1)) + geom_bar() 
    }
    plot
  })
  
  # physical health
  output$physicalhealth <- renderPlot({
    if(input$Leave == "All"){
      plot <- mental_health_survey %>% 
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "lightgreen")
    }
    else if(input$Leave == "Don't know"){
      plot <- mental_health_survey %>% 
        filter(leave == "Don't know") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "lightgreen")+
        xlab("No of employees") + 
        ylab("percent of employees ready to disscuss mental health concerns") 
    }
    else if(input$Leave == "Somewhat difficult"){
      plot <- mental_health_survey %>% 
        filter(leave == "Somewhat difficult") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "lightgreen")+
        xlab("No of employees") + 
        ylab("percent of employees ready to disscuss mental health concerns") 
    }
    else if(input$Leave == "Somewhat easy"){
      plot <- mental_health_survey %>% 
        filter(leave == "Somewhat easy") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "lightgreen")+
        xlab("No of employees") + 
        ylab("percent of employees ready to disscuss mental health concerns") 
    }
    else if(input$Leave == "Very difficult"){
      plot <- mental_health_survey %>% 
        filter(leave == "Very difficult") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "lightgreen")+
        xlab("No of employees") + 
        ylab("percent of employees ready to disscuss mental health concerns") 
    }
    else{
      plot <- mental_health_survey %>% 
        filter(leave == "very easy") %>%
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "lightgreen")+
        xlab("No of employees") + 
        ylab("percent of employees ready to disscuss mental health concerns") 
    }
    plot
    
  })    
  
  output$Treatmenttrend <- renderPlot({
    if(input$Gender =="All")
    {
      plot <- mental_health_survey %>% 
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x = no_employees, fill = treatment)) + geom_bar(position = "dodge")
    }
    else
    {
      plot <- mental_health_survey %>% 
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x = no_employees, fill = treatment)) + geom_bar(position = "dodge")
    }
    plot
  })
  
  output$seekhelp <- renderPlot({
    if(input$Gender =="All")
    {
      plot <- mental_health_survey %>% 
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")
    }
    else
    {
      plot <- mental_health_survey %>% 
        filter(Age>input$Age[1], Age<input$Age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == input$Gender) %>%
        filter(no_employees == input$CompanySize) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")
    }
    plot
  })
  
})



