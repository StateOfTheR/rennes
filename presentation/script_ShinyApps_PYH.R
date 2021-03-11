#------------------------------------#
#------- SOTR - Shiny Apps -----------
#-------- P.-Y. Hernvann ------------
#------------------------------------#

#------------------------#
# Charger les paquets ----
#------------------------# 

library(shiny)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)

remotes::install_github("allisonhorst/palmerpenguins")
data(penguins,package = "palmerpenguins")
penguins <- penguins %>%
  rename(bill_l = bill_length_mm, bill_d = bill_depth_mm, flip_l = flipper_length_mm, bm = body_mass_g)
penguins %>%
  print(n=2)

#__________________________________________________________________________________________

#------------------------#
# Niveau ZERO ------------
#------------------------# 

  #------------------------------------#
  ## Afficher le graph de base --------
  #------------------------------------#

ui <- fluidPage(
  titlePanel("Ma Donnée de Piafs"),
  sidebarLayout(
    sidebarPanel(
      "Pour l'instant, pas d'intéractivité, vous dîtes seulement à l'application de représenter une figure 'figée'."
    )
    ,
    mainPanel(
      "Ci-dessous notre jolie figure :",
      plotOutput("pengPlot")
    )
  )
)

server<- function(input, output) {
  output$pengPlot = renderPlot({
    penguins %>% 
      ggplot() + aes( x= bill_l, y=bill_d, col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+theme_light()
  })
}

shinyApp(ui, server)

  #------------------------------------#
  ## Avec Plotly --------
  #------------------------------------#

ui <- fluidPage(
  titlePanel("Ma Donnée de Piafs"),
  sidebarLayout(
    sidebarPanel(
      "Voilà la barre latérale qui permettra de rentrer des informations/instructions"
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot")
    )
  )
)

server<- function(input, output) {
  output$pengPlot = renderPlotly({
    penguins %>% 
      ggplot() + aes( x= bill_l, y=bill_d, col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()
  })
}

shinyApp(ui, server)

#__________________________________________________________________________________________

#------------------------#
# Niveau 1 ------------
#------------------------# 

ui <- fluidPage(
  titlePanel("Exploration de données Pingouins"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("select", label = h3("Select Species"),
                  choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" ="Gentoo"),
                  selected = "Adelie", multiple=T),
      sliderInput("slider", label = h3("Slider"), min = 2007, max = 2009, value = c(2007,2009))
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot")
    )
  )
)
server<- function(input, output) {
  output$pengPlot = renderPlotly({
    penguins %>% 
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%
      filter(year<=input$slider[2]) %>%
      ggplot() + aes( x= bill_l, y=bill_d,col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()
  })
}
shinyApp(ui, server)

#__________________________________________________________________________________________

#------------------------#
# Niveau 2 ------------
#------------------------# 

ui <- fluidPage(
  titlePanel("Exploration de données Pingouins"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("select", label = h3("Choisir une espèce"),
                  choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" ="Gentoo"),
                  selected = "Adelie", multiple=T),
      sliderInput("slider", label = h3("Choisir une période"), min = 2007, max = 2009, value = c(2007,2009)),
      checkboxGroupInput("checkbox1", label = "Draw regression", choices=c("Linear reg."="linear","Loess"="loess")),
      checkboxInput("checkbox2", label = "Represent uncertainty", value = FALSE)
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot")
    )
  )
)
server<- function(input, output) {
  output$pengPlot = renderPlotly({
    penguins %>% 
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%
      filter(year<=input$slider[2]) %>%
      ggplot() + aes( x= bill_l, y=bill_d,col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()->toplot
    
    if("linear"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'lm', se = input$checkbox2)
    }
    if("loess"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'loess', se = input$checkbox2)
    }
    toplot
    
  })
}
shinyApp(ui, server)


#__________________________________________________________________________________________

#------------------------#
# Niveau 3 ------------
#------------------------# 

ui <- fluidPage(
  titlePanel("Exploration de données Pingouins"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("select", label = h3("Choisir une espèce"),
                  choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" ="Gentoo"),
                  selected = "Adelie", multiple=T),
      sliderInput("slider", label = h3("Choisir une période"), min = 2007, max = 2009, value = c(2007,2009)),
      checkboxGroupInput("checkbox1", label = "Draw regression", choices=c("Linear reg."="linear","Loess"="loess")),
      checkboxInput("checkbox2", label = "Represent uncertainty", value = FALSE)
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot"),
      dataTableOutput("table")
      
    )
  )
)
server<- function(input, output) {
  
  penguinsBIS<-reactive({penguins%>% 
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%
      filter(year<=input$slider[2])})
  
  output$pengPlot = renderPlotly({
    
    penguinsBIS() %>% 
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%
      filter(year<=input$slider[2]) %>%
      ggplot() + aes( x= bill_l, y=bill_d,col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()->toplot
    
    if("linear"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'lm', se = input$checkbox2)
    }
    if("loess"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'loess', se = input$checkbox2)
    }
    
    toplot
    
  })
  
  output$table = renderDataTable({
    penguinsBIS()
  })
  
}
shinyApp(ui, server)



#__________________________________________________________________________________________

#------------------------#
# Niveau 4 ------------
#------------------------# 

ui <- fluidPage(
  titlePanel("Exploration de données Pingouins"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("spsel", label = h3("Choisir l'espèce"),
                  choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" ="Gentoo"), selected="Adelie", multiple=F),
      selectInput("islandsel", label = h3("Choisir l'île"),
                  choices = list("BISCOE" = "Biscoe", "DREAM" = "Dream", "TORGENSEN" ="Torgersen"), selected="Biscoe", multiple=F),
      selectInput("sexsel", label = h3("Choisir le sexe"),
                  choices = list("Males" = "male", "Femelles" = "female", "Unrecorded"=NA), selected="male", multiple=F)
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot")
    )
  )
)
server<- function(input, output, session) {
  
  observeEvent(input$spsel, {
    newislands <- as.character(unique(penguins$island[which(penguins$species%in%input$spsel)])) 
    updateSelectInput(session,"islandsel", "Choisir l'île", choices = c(newislands), selected=c(newislands)[1])
  })
  
  observeEvent(input$spsel, {
    newsex <- as.character(unique(penguins$sex[which(penguins$species%in%input$spsel & penguins$island%in%input$islandsel)]))
    updateSelectInput(session,"sexsel", "Choisir le sexe", choices = c(newsex), selected=c(newsex)[1])
  })
  
  observeEvent(input$islandsel, {
    newsex <- as.character(unique(penguins$sex[which(penguins$species%in%input$spsel & penguins$island%in%input$islandsel)])) 
    updateSelectInput(session,"sexsel", "Choisir le sexe", choices = c(newsex), selected=c(newsex)[1])
  })
  
  
  output$pengPlot = renderPlotly({
    penguins %>%
      filter(species%in%input$spsel)%>%
      filter(island%in%input$islandsel)%>%
      filter(sex%in%input$sexsel)%>%
      ggplot() + aes( x= flip_l, y=bm) + geom_point() + 
      labs(x = 'Flipper length') +
      labs(y = 'Body mass') +
      theme_light()
  })
  
}
shinyApp(ui, server)

#__________________________________________________________________________________________

#------------------------#
# Organize  shiny dashboard  ------------
#------------------------#

ui <- fluidPage(width=12,
                titlePanel("Exploration de données Pingouins"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               strong("Ma belle barre latérale"),
                               selectInput("select", label = h3("Choisir une espèce"),
                                           choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" ="Gentoo"),
                                           selected = "Adelie", multiple=T),
                               sliderInput("slider", label = h3("Choisir une période"), min = 2007, max = 2009, value = c(2007,2009)),
                               checkboxGroupInput("checkbox1", label = "Draw regression", choices=c("Linear reg."="linear","Loess"="loess")),
                               checkboxInput("checkbox2", label = "Represent uncertainty", value = FALSE)
                  )
                  ,
                  mainPanel(
                    "Panneau principal: ici sont représentées les sorties désirées",
                    tabsetPanel(
                      tabPanel("Graphiques",
                               
                               fluidRow(
                                 
                                 column(12,
                                        plotlyOutput("pengPlot1"),
                                        
                                        fluidRow(
                                          
                                          column(6,
                                                 plotlyOutput("pengPlot2")),
                                          
                                          column(6,
                                                 plotlyOutput("pengPlot3"))
                                          
                                        )
                                        
                                 )
                               )
                      ),
                      
                      tabPanel("Donnée",
                               dataTableOutput("table"))     
                      
                    )
                  )
                )
)
server<- function(input, output) {
  
  penguinsBIS<-reactive({penguins%>% 
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%
      filter(year<=input$slider[2])})
  
  output$pengPlot1 = renderPlotly({
    
    penguinsBIS() %>% 
      ggplot() + aes( x= bill_l, y=bill_d,col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()+
      theme(legend.position = "none") -> toplot
    
    if("linear"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'lm', se = input$checkbox2)
    }
    if("loess"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'loess', se = input$checkbox2)
    }
    
    toplot
    
  })
  
  output$pengPlot2 = renderPlotly({
    
    penguinsBIS() %>% 
      ggplot() + aes(x= bill_l, fill = species) + geom_histogram(stat="count", alpha=0.5,position = 'identity') + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Counts') +
      labs(color = "Species")+
      theme_light()+
      theme(legend.position = "none") -> toplot2
    
    toplot2
    
  })
  
  output$pengPlot3 = renderPlotly({
    
    penguinsBIS() %>% 
      ggplot() + aes(x= bill_d, fill = species) + geom_histogram(stat="count", alpha=0.5,position = 'identity') + 
      labs( x = 'Bill depth in mm') +
      labs(y = 'Counts') +
      labs(color = "Species")+
      theme_light() -> toplot2
    
    toplot2
    
  })
  
  output$table = renderDataTable({
    penguinsBIS()
  })
  
}
shinyApp(ui, server)


#__________________________________________________________________________________________

#------------------------#
# Shinytheme  ------------
#------------------------# 

shinyApp(
  ui = fluidPage(
    titlePanel("Theme 'united'"),
    theme = shinytheme("united"), 
    sidebarPanel(
      textInput("txt", "Theme 'united':", "text here"),
      sliderInput("slider", "Slider input:", 1, 100, 30),
      actionButton("action", "Button"),
      actionButton("action2", "Button2", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tab 1"),
        tabPanel("Tab 2")
      )
    )
  ),
  server = function(input, output) {}
)

#__________________________________________________________________________________________

#------------------------#
# Shinydashboard  ------------
#------------------------# 

body <- dashboardBody(
  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  ),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2")
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })
  }
)















