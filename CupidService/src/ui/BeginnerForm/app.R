#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- navbarPage(

    title = "Cupid",
    inverse = TRUE,
    
    # Sidebar with a slider input for number of bins 
    tabPanel(title = "Connectivity Test",
      div(id = "conForm",align="center",
          shinyjs::useShinyjs(),
        p(h3("Заполнение анкет")),
        p("Сначала нужно заполнить анкету про себя, затем про своего партнера"),
        
            numericInput("age", label = "Возраст", value = 18),
            
            radioButtons("gender", label = "Пол",
                         choices = list("М" = 1, "Ж" = 2), 
                         selected = 1),
            textInput("field", label = "Область, в которой вы работаете", placeholder = "Область"),
            
            textInput("undergra", label = "Университет, в котором обучались/учитесь", placeholder = "Название университета"),
            
            numericInput("mn_sat", label = "средний балл SAT", value = 0),
            
            sliderInput("imprace",
                        "Насколько для вас важно (по шкале от 1 до 10), чтобы человек, с которым вы встречаетесь, был того же расового/этнического происхождения?",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("imprelig",
                        "Насколько для вас важно (по шкале от 1 до 10), чтобы человек, с которым вы встречаетесь, был того же религиозного происхождения?",
                        min = 1,
                        max = 10,
                        value = 5),
        
            helpText("Насколько вам интересны следующие виды деятельности по шкале от 1 до 10?"),
            
            sliderInput("sports",
                        "Занятия спортом/легкая атлетика",
                        min = 1,
                        max = 10,
                        value = 5),
            
            sliderInput("tv_sports",
                        "Просмотр спортивных передач",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("exercise",
                        "Занятия бодибилдингом/физические нагрузки",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("museums",
                        "Музеи/галереи",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("art",
                        "Исскусство",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("hiking",
                        "Пешие прогулки/кемпинг",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("gaming",
                        "Гейминг",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("clubbing",
                        "Танцы",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("reading",
                        "Чтение",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("tv",
                        "Просмотр телевизора",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("theater",
                        "Театр",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("movies",
                        "Фильмы",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("music",
                        "Музыка",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("shopping",
                        "Шоппинг",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("yoga",
                        "Йога/медитация",
                        min = 1,
                        max = 10,
                        value = 5),
            
            uiOutput("doneButton")
      ),
      div(id = "conResult", align = "center",
        h3(textOutput("conResult"))
      )
    )
)

df = data.frame(sports = integer(), music = integer())

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveVal(df)
  
  output$table <- DT::renderDT(rv())
  
  output$doneButton <- renderUI(
    actionButton("doneButton", label = ifelse(nrow(rv()) < 1, "Отправить свою анкету", "Отправить анкету партнера"))
  )
  
  observeEvent(input$doneButton, {
    # Логика обработки введенных пользователем данных
    newdf = rv()
    print(newdf)
    newdf[nrow(newdf) + 1,] = c(input$sports, input$music)
    rv(newdf)
    df <<- rv()
    if (nrow(newdf) == 2) {
      shinyjs::hide(id = "conForm")
      output$conResult = renderText(
        "ЗДЕСЬ БУДЕТ ВЫВОДИТЬСЯ РЕЗУЛЬТАТ"
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
