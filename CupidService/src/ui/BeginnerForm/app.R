library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
#install.packages("randomForest")
library(randomForest)
library(stats)
#install.packages("themis")
#install.packages("rpart.plot")
#install.packages("rpart")
library(themis)
library(rpart.plot)

fitModel <- function() {
  #train ds preprocessing
  sd_fin = read_csv("../../../resources/speeddating_merged.csv")
  sd_fin = sd_fin %>% mutate(match = factor(match, levels = c(1, 0)))
  sd_fin_id = sd_fin
  sd_fin = sd_fin %>% select(-'...1', -id_1, -id_2)
  
  set.seed(18)
  split = initial_split(sd_fin)
  sd_Train = training(split)
  
  #weights
  table(sd_Train$match)
  table(sd_Train$match)[2]/table(sd_Train$match)[1]
  
  sdTrainPositive = sd_Train %>% filter(match == 1)
  
  sdTrainWeighted = sd_Train
  for (i in 1:3){
    sdTrainWeighted = rbind(sdTrainWeighted, sdTrainPositive)
  }
  sdTrainWeighted_trimmed = sdTrainWeighted %>% select(match, int_corr, age_dif, imprace_dif, imprelig_dif, sports_dif, tvsports_dif, exercise_dif, gaming_dif, clubbing_dif, reading_dif,  shopping_dif ,yoga_dif,  attr1_1_dif  ,sinc1_1_dif, 
                                                       intel1_1_dif, fun1_1_dif, amb1_1_dif,  shar1_1_dif, attr2_1_dif, sinc2_1_dif, intel2_1_dif,
                                                       fun2_1_dif, amb2_1_dif, shar2_1_dif)
  #model
  rf4 = rand_forest(mode = "classification") %>%
    set_engine('randomForest')
  
  wf_rf4 = workflow() %>%
    add_model(rf4) %>%
    add_formula(match ~ .)
  
  #fitting
  wf_rf4 = wf_rf4 %>% fit(sdTrainWeighted_trimmed)
  return(wf_rf4)
}


ui <- navbarPage(
    title = "Cupid",
    inverse = TRUE,
    
    tabPanel(title = "Connectivity Test",
      div(id = "conForm",align="center",
          
            shinyjs::useShinyjs(),
          
            p(h3("Заполнение анкет")),
            p("Сначала нужно заполнить анкету про себя, затем про своего партнера"),
        
            numericInput("age", label = "Возраст", value = 18, min = 18, max = 100),
            
            radioButtons("gender", label = "Пол",
                         choices = list("М" = 1, "Ж" = 2), 
                         selected = 1),
          
          sliderInput("imprace",
                      "Насколько для вас важно (по шкале от 1 до 10), чтобы человек, с которым вы встречаетесь, был того же расового/этнического происхождения?",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("imprelig",
                      "НасколькоНасколько для вас важно (по шкале от 1 до 10), чтобы человек, с которым вы встречаетесь, был того же религиозного происхождения?",
                      min = 1,
                      max = 10,
                      value = 5),
            
          helpText("Оцените насколько ВАМ интересны следующие активности по шкале от 1 до 10"),
          
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
          
          sliderInput("museums",
                      "Посещение музеев",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("art",
                      "Искусство",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("hiking",
                      "Походы",
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
                      "Кинематограф",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("concerts",
                      "Посещение концертов",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("music",
                      "Музыка",
                      min = 1,
                      max = 10,
                      value = 5),
            
          helpText("Насколько ВАМ важны следующие черты в вашем партнере? По шкале от 1 до 10"),
          
          sliderInput("attr",
                      "Привлекательность",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("sinc",
                      "Искренность",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("intel",
                      "Интеллект",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("fun",
                      "Чувство юмора",
                      min = 1,
                      max = 10,
                      value = 5),
          sliderInput("amb",
                      "Амбициозность",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("shar",
                      "Наличие общих интересов/хобби",
                      min = 1,
                      max = 10,
                      value = 5),
          
          helpText("Как думаете, насколько вашему партнеру важны следующие характеристики в вас? По шкале от 1 до 10"),
          
          sliderInput("attr_a",
                      "Привлекательность",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("sinc_a",
                      "Искренность",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("intel_a",
                      "Интеллект",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("fun_a",
                      "Чувство юмора",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("amb_a",
                      "Амбициозность",
                      min = 1,
                      max = 10,
                      value = 5),
          
          sliderInput("shar_a",
                      "Наличие общих интересов/хобби",
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

df = data.frame(
  age = integer(),
  gender = integer(),
  sports = integer(),
  tv_sports = integer(),
  exercise = integer(),
  gaming = integer(),
  clubbing = integer(),
  reading = integer(),
  shopping = integer(),
  yoga = integer(),
  museums = integer(),
  art = integer(),
  hiking = integer(),
  tv = integer(),
  theater = integer(),
  movies = integer(),
  concerts = integer(),
  music = integer(),
  attr = integer(),
  sinc = integer(),
  intel = integer(),
  fun = integer(),
  amb = integer(),
  shar = integer(),
  attr_a = integer(),
  sinc_a = integer(),
  intel_a = integer(),
  fun_a = integer(),
  amb_a = integer(),
  shar_a = integer(),
  imprace = integer(),
  imprelig = integer()
)

wf_rf4 = fitModel()

getPartnerInterestsArray <- function(input_df, i) {
  return(
    c(as.numeric(input_df[i,]$sports), as.numeric(input_df[i,]$tv_sports), as.numeric(input_df[i,]$exercise),
      as.numeric(input_df[i,]$gaming), as.numeric(input_df[i,]$clubbing), as.numeric(input_df[i,]$reading), as.numeric(input_df[i,]$shopping),
      as.numeric(input_df[i,]$yoga))
  )
}

calcIntCor <- function(input_df) {
  first_int = getPartnerInterestsArray(input_df, 1)
  second_int = getPartnerInterestsArray(input_df, 2)
  cor_res = cor(first_int, second_int)
  return(ifelse(is.na(cor_res), 0, cor_res))
}

mapInputDfToDiff <- function(input_df) {
  diff_df = data.frame(
    int_corr = integer(),
    age_dif = integer(),
    imprace_dif = integer(),
    imprelig_dif = integer(),
    sports_dif = integer(),
    tvsports_dif = integer(),
    exercise_dif = integer(),
    gaming_dif = integer(),
    clubbing_dif = integer(),
    reading_dif = integer(),
    shopping_dif = integer(),
    yoga_dif = integer(),
    attr1_1_dif = integer(),
    sinc1_1_dif = integer(), 
    intel1_1_dif= integer(),
    fun1_1_dif = integer(),
    amb1_1_dif = integer(),
    shar1_1_dif = integer(),
    attr2_1_dif = integer(), 
    sinc2_1_dif= integer(),
    intel2_1_dif = integer(),
    fun2_1_dif = integer(),
    amb2_1_dif = integer(),
    shar2_1_dif = integer()
  )
  
  diff_df[1,]$int_corr = calcIntCor(input_df)
  diff_df[1,]$age_dif = abs(as.numeric(input_df[1,]$age) - as.numeric(input_df[2,]$age))
  diff_df[1,]$imprace_dif = abs(as.numeric(input_df[1,]$imprace) - as.numeric(input_df[2,]$imprace))
  diff_df[1,]$imprelig_dif = abs(as.numeric(input_df[1,]$imprelig) - as.numeric(input_df[2,]$imprelig))
  diff_df[1,]$sports_dif = abs(as.numeric(input_df[1,]$sports) - as.numeric(input_df[2,]$sports))
  diff_df[1,]$tvsports_dif = abs(as.numeric(input_df[1,]$tv_sports) - as.numeric(input_df[2,]$tv_sports))
  diff_df[1,]$exercise_dif = abs(as.numeric(input_df[1,]$exercise) - as.numeric(input_df[2,]$exercise))
  diff_df[1,]$gaming_dif = abs(as.numeric(input_df[1,]$gaming) - as.numeric(input_df[2,]$gaming))
  diff_df[1,]$clubbing_dif = abs(as.numeric(input_df[1,]$clubbing) - as.numeric(input_df[2,]$clubbing))
  diff_df[1,]$reading_dif = abs(as.numeric(input_df[1,]$reading) - as.numeric(input_df[2,]$reading))
  diff_df[1,]$shopping_dif = abs(as.numeric(input_df[1,]$shopping) - as.numeric(input_df[2,]$shopping))
  diff_df[1,]$yoga_dif = abs(as.numeric(input_df[1,]$yoga) - as.numeric(input_df[2,]$yoga))
  diff_df[1,]$attr1_1_dif = abs(as.numeric(input_df[1,]$attr) - as.numeric(input_df[2,]$attr))
  diff_df[1,]$sinc1_1_dif = abs(as.numeric(input_df[1,]$sinc) - as.numeric(input_df[2,]$sinc))
  diff_df[1,]$intel1_1_dif = abs(as.numeric(input_df[1,]$intel) - as.numeric(input_df[2,]$intel))
  diff_df[1,]$fun1_1_dif = abs(as.numeric(input_df[1,]$fun) - as.numeric(input_df[2,]$fun))
  diff_df[1,]$amb1_1_dif = abs(as.numeric(input_df[1,]$amb) - as.numeric(input_df[2,]$amb))
  diff_df[1,]$shar1_1_dif = abs(as.numeric(input_df[1,]$shar) - as.numeric(input_df[2,]$shar))
  diff_df[1,]$attr2_1_dif = abs(as.numeric(input_df[1,]$attr_a) - as.numeric(input_df[2,]$attr_a))
  diff_df[1,]$sinc2_1_dif = abs(as.numeric(input_df[1,]$sinc_a) - as.numeric(input_df[2,]$sinc_a))
  diff_df[1,]$intel2_1_dif = abs(as.numeric(input_df[1,]$intel_a) - as.numeric(input_df[2,]$intel_a))
  diff_df[1,]$fun2_1_dif = abs(as.numeric(input_df[1,]$fun_a) - as.numeric(input_df[2,]$fun_a))
  diff_df[1,]$amb2_1_dif = abs(as.numeric(input_df[1,]$amb_a) - as.numeric(input_df[2,]$amb_a))
  diff_df[1,]$shar2_1_dif = abs(as.numeric(input_df[1,]$shar_a) - as.numeric(input_df[2,]$shar_a))
  
  return(diff_df)
}

calcConnectivity <- function(input_test) {
  predtest.rf4_prob = predict(wf_rf4, input_test, type = "prob")
  return(predtest.rf4_prob$.pred_1)
}

server <- function(input, output) {
  
  rv <- reactiveVal(df)
  
  output$table <- DT::renderDT(rv())
  
  output$doneButton <- renderUI(
    actionButton("doneButton", label = ifelse(nrow(rv()) < 1, "Отправить свою анкету", "Отправить анкету партнера"))
  )
  
  observeEvent(input$doneButton, {
    # Логика обработки введенных пользователем данных
    newdf = rv()
    #print(newdf)
    
    newdf[nrow(newdf) + 1,] = c(
      input$age, input$gender, input$sports, input$tv_sports, input$exercise, input$gaming, input$clubbing, input$reading, 
      input$shopping, input$yoga, input$museums, input$art, input$hiking, input$tv, input$theater, input$movies, input$concerts, input$music,
      input$attr, input$sinc, input$intel, input$fun, input$amb, input$shar, 
      input$attr_a, input$sinc_a, input$intel_a, input$fun_a, input$amb_a, input$shar_a, input$imprace, input$imprelig
    )
    
    rv(newdf)
    df <<- rv()
    if (nrow(newdf) == 2) {
      shinyjs::hide(id = "conForm")
      diff_df = mapInputDfToDiff(newdf)
      pred_prob = calcConnectivity(diff_df) * 100
      output$conResult = renderText(
        paste(
          "Ваш процент совместимости:\n",
          pred_prob, "%",
          sep=""
          )
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
