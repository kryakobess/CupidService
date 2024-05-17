library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(tidymodels)
library(purrr)
library(DBI)
#install.packages("randomForest")
library(randomForest)
library(stats)
#install.packages("themis")
#install.packages("rpart.plot")
#install.packages("rpart")
library(themis)
library(rpart.plot)


CONNECTIVITY_TEST_TAB_NAME = 'ConnectivityTest'
DATING_APP_TAB_NAME = 'Dating App'

sd_preload = read_csv("../../../resources/speeddating_merged.csv")
users_csv = read.csv("../../../resources/speeddating.csv")
load("../../../resources/rf_model_cupid.RData")


addSlider <- function(name, content) {
  return(
    sliderInput(name, content, min = 1, max = 10, value = 1)
  )
}

uiUserFeaturesForm <- function() {
  return(
    tagList( 
      numericInput("age", label = "Возраст", value = 18, min = 18, max = 100),
      
      radioButtons("gender", label = "Пол",
                   choices = list("М" = 1, "Ж" = 2), 
                   selected = 1),
      
      addSlider(
        "imprace",
        "Насколько для вас важно (по шкале от 1 до 10), чтобы человек, с которым вы встречаетесь, был того же расового/этнического происхождения?"
      ),
      
      addSlider(
        "imprelig",
        "НасколькоНасколько для вас важно (по шкале от 1 до 10), чтобы человек, с которым вы встречаетесь, был того же религиозного происхождения?"
      ),
      
      helpText("Оцените насколько ВАМ интересны следующие активности по шкале от 1 до 10"),
      
      addSlider("sports","Занятия спортом/легкая атлетика"),
      
      addSlider("tv_sports", "Просмотр спортивных передач"),
      
      addSlider("exercise", "Занятия бодибилдингом/физические нагрузки"),
      
      addSlider("gaming", "Гейминг"),
      
      addSlider("clubbing", "Танцы"),
      
      addSlider("reading", "Чтение"),
      
      addSlider("shopping", "Шоппинг"),
      
      addSlider("yoga", "Йога/медитация"),
      
      addSlider("museums", "Посещение музеев"),
      
      addSlider("art", "Искусство"),
      
      addSlider("hiking", "Походы"),
      
      addSlider("tv", "Просмотр телевизора"),
      
      addSlider("theater", "Театр"),
      
      addSlider("movies", "Кинематограф"),
      
      addSlider("go_out", "Прогулки"),
      
      addSlider("concerts", "Посещение концертов"),
      
      addSlider("music", "Музыка"),
      
      helpText("Насколько ВАМ важны следующие черты в вашем партнере? По шкале от 1 до 10"),
      
      addSlider("attr", "Привлекательность"),
      
      addSlider("sinc", "Искренность"),
      
      addSlider("intel", "Интеллект"),
      
      addSlider("fun", "Чувство юмора"),
      
      addSlider("amb","Амбициозность"),
      
      addSlider("shar", "Наличие общих интересов/хобби"),
      
      helpText("Как думаете, насколько вашему партнеру важны следующие характеристики в вас? По шкале от 1 до 10"),
      
      addSlider("attr_a", "Привлекательность"),
      
      addSlider("sinc_a", "Искренность"),
      
      addSlider("intel_a", "Интеллект"),
      
      addSlider("fun_a", "Чувство юмора"),
      
      addSlider("amb_a", "Амбициозность"),
      
      addSlider("shar_a", "Наличие общих интересов/хобби")
    )
  )
}

showConnectivityTest <- function() {
  return(
    tagList(
      div(id = "conForm",align="center",
          
          shinyjs::useShinyjs(),
          
          p(h3("Заполнение анкет")),
          h3("Сначала нужно заполнить анкету про себя, затем про своего партнера"),
          
          uiUserFeaturesForm(),
          
          uiOutput("doneButton")
      ),
      
      div(id = "conResult", align = "center",
          h3(textOutput("conResult")),
          uiOutput("continueFormButton")
      ),
      
      div(id = "conRegisterForm", align = "center",
          h3("Желаете зарегистрировать аккаунт?"),
          textInput("username", label = "Введите имя пользователя"),
          textInput("password", label = "Введите пароль"),
          actionButton("conRegisterButton", "Подтвердить регистрацию")
      )
    )
  )
}

showDatingApp <- function() {
  div(
    id = "dateApp", align="center",
    
    h3("Cupid"),
    
    actionButton("dateOpenRegistrationFormButton", "Зарегистрироваться"),
    
    actionButton("dateAuthenticateButton", "Войти"),
    
    #after click show forms:
    
    div(id = "dateRegistrationForm",
        h3("Регистрация"),
        uiOutput("dateRegistrationWarning"),
        textInput("dateRegistrationUsername", label = "Введите имя пользователя"),
        textInput("dateRegistrationPassword", label = "Введите пароль"),
        uiUserFeaturesForm(),
        actionButton("dateRegister", "Зарегистрироваться")
    ),
    
    #or
    
    div(id = "dateAuthWindow",
        h3("Войдите в аккаунт"),
        uiOutput("dateAuthWarning"),
        textInput("dateAuthUsername", label = "Имя пользователя"),
        textInput("dateAuthPassword", label = "Пароль"),
        actionButton("dateAuth", "Войти")
    ),
    
    div(id = "datingApp",
        tableOutput("datePredictedResult"),
        div(id = "DatingAppButtons", style="margin: 25px",
          uiOutput("dateDislikeButton"),
          uiOutput("dateLikeButton")
        )
    )
    
  )
}

ui <- navbarPage(
  title = "Cupid",
  inverse = TRUE,
  tabsetPanel(id = "appType",
              tabPanel(title = CONNECTIVITY_TEST_TAB_NAME, showConnectivityTest()),
              tabPanel(title = DATING_APP_TAB_NAME, showDatingApp())
  )
)

fitModel <- function() {
  #train ds preprocessing
  sd_fin = sd_preload
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

createEmptyInputDf <- function() {
  return(
    data.frame(
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
      go_out = integer(),
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
  )
}

wf_rf4 = fitModel()
mydb <- dbConnect(RSQLite::SQLite(), "../../../resources/cupid.db")

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
    go_out_dif = integer(),
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
  diff_df[1,]$go_out_dif = abs(as.numeric(input_df[1,]$go_out) - as.numeric(input_df[2,]$go_out))
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

saveUser <- function(username, password) {
  rs <- dbSendStatement(mydb, 'INSERT INTO User (username, password) VALUES (:username, :password)')
  dbBind(rs, params = list(username = username, password = password))
  return(rs)
}

getUserByUsername <- function(username) {
  rs <- dbSendQuery(mydb, 'SELECT * FROM USER WHERE username = :username')
  dbBind(rs, params = list(username = username))
  user = dbFetch(rs)
  
  return(user)
}

getAllUserFeaturesExceptOne <- function(id) {
  rs <- dbSendQuery(mydb, 'SELECT * FROM USER_FEATURES WHERE id != :id')
  dbBind(rs, params = list(id = id))
  userFeatures = dbFetch(rs)
  
  return(userFeatures)
}

getUserFeaturesById <- function(id) {
  rs <- dbSendQuery(mydb, 'SELECT * FROM USER_FEATURES WHERE id = :id')
  dbBind(rs, params = list(id = id))
  userFeatures = dbFetch(rs)
  
  return(userFeatures)
}

saveFeatures <- function(feature) {
  rs <- dbAppendTable(mydb, "USER_FEATURES", feature)
  #print(dbGetQuery(mydb, "SELECT * FROM USER_FEATURES"))
}

registerNewUser <- function(username, password, features) {
  existingUserCount = nrow(getUserByUsername(username))
  print(existingUserCount)
  print(getUserByUsername(username))
  if (existingUserCount != 0) {
    print("User already exists")
    return(NULL)
  }
  
  saveUser(username, password)
  savedUser = getUserByUsername(username)
  
  print("User saved")
  
  userFeatures = cbind(id = savedUser$id, features)
  
  saveFeatures(features)
  
  print("features saved")
  
  return(savedUser)
}

onDatingAppRegisterUser <- function(input, output) {
  features = createEmptyInputDf()
  
  features[1,] = c(
    input$age, input$gender, input$sports, input$tv_sports, input$exercise, input$gaming, input$clubbing, input$reading, 
    input$shopping, input$yoga, input$museums, input$art, input$hiking, input$tv, input$theater, input$movies, input$go_out,
    input$concerts, input$music,input$attr, input$sinc, input$intel, input$fun, input$amb, input$shar, 
    input$attr_a, input$sinc_a, input$intel_a, input$fun_a, input$amb_a, input$shar_a, input$imprace, input$imprelig
  )
  
  savedUser = registerNewUser(input$dateRegistrationUsername, input$dateRegistrationPassword, features)
  
  if (is.null(savedUser)) {
    output$dateRegistrationWarning = renderUI(
      h3(id = "dateRegistrationWarning", "Пользователь с таким именем уже существует.", style = "color:#DC143C")
    )
  }
  
  return(savedUser)
}

onDatingAppAuth <- function(input, output) {
  user = getUserByUsername(input$dateAuthUsername)
  if (input$dateAuthPassword != user$password) {
    output$dateAuthWarning = renderUI(
      h3(id = "dateAuthWarning", "Неверный пароль", style = "color:#DC143C")
    )
    return(NULL)
  }
  return(user)
}


mapUserFeaturesToModelDf <- function(user_features) {
  return(
    data.frame(id = user_features$id,  age = user_features$age, imprace = user_features$imprace,
               imprelig = user_features$imprelig, go_out = user_features$go_out , sports = user_features$sports,
               tvsports = user_features$tv_sports, exercise = user_features$exercise, gaming = user_features$gaming,
               clubbing = user_features$clubbing, reading = user_features$reading, shopping = user_features$shopping,
               yoga = user_features$yoga, attr1_1 = user_features$attr, sinc1_1 = user_features$sinc,
               intel1_1 = user_features$intel, fun1_1 = user_features$fun, amb1_1 = user_features$amb,
               shar1_1 = user_features$shar, attr2_1 = user_features$attr_a, sinc2_1 = user_features$sinc_a,
               intel2_1 = user_features$intel_a, fun2_1 = user_features$fun_a, amb2_1 = user_features$amb_a,
               shar2_1 = user_features$shar_a)
  )
}

getPredictions <- function(user) {
    user_features = getUserFeaturesById(user$id)
    user_info = mapUserFeaturesToModelDf(user_features)
    sd = users_csv
    sd_from_db = mapUserFeaturesToModelDf(getAllUserFeaturesExceptOne(user$id))
    sd_from_db = sd_from_db %>% mutate(iid = -id)
    print(sd_from_db)
    sd_difs_count = sd %>% select(iid, age, imprace, imprelig, go_out, sports, tvsports, exercise, gaming, clubbing,
                                  reading, shopping, yoga, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1, attr2_1,
                                  sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1)
    
    sd_difs_count = sd_difs_count %>% mutate(id = -1)
    
    sd_difs_count = rbind(sd_difs_count, sd_from_db)
    
    table(is.na(sd_difs_count))
    sd_difs_count = na.omit(sd_difs_count)
    
    user_info = user_info %>% mutate(attr1_1 = (attr1_1 - min(sd_difs_count$attr1_1))/(max(sd_difs_count$attr1_1) -
                                                                                         min(sd_difs_count$attr1_1)))
    user_info = user_info %>% mutate(attr2_1 = (attr2_1 - min(sd_difs_count$attr2_1))/(max(sd_difs_count$attr2_1) -
                                                                                         min(sd_difs_count$attr2_1)))
    
    user_info = user_info %>% mutate(sinc1_1 = (sinc1_1 - min(sd_difs_count$sinc1_1))/(max(sd_difs_count$sinc1_1) -
                                                                                         min(sd_difs_count$sinc1_1)))
    user_info = user_info %>% mutate(sinc2_1 = (sinc2_1 - min(sd_difs_count$sinc2_1))/(max(sd_difs_count$sinc2_1) -
                                                                                         min(sd_difs_count$sinc2_1)))
    
    user_info = user_info %>% mutate(intel1_1 = (intel1_1 - min(sd_difs_count$intel1_1))/(max(sd_difs_count$intel1_1) -
                                                                                            min(sd_difs_count$intel1_1)))
    user_info = user_info %>% mutate(intel2_1 = (intel2_1 - min(sd_difs_count$intel2_1))/(max(sd_difs_count$intel2_1) -
                                                                                            min(sd_difs_count$intel2_1)))
    
    user_info = user_info %>% mutate(fun1_1 = (fun1_1 - min(sd_difs_count$fun1_1))/(max(sd_difs_count$fun1_1) - 
                                                                                      min(sd_difs_count$fun1_1)))
    user_info = user_info %>% mutate(fun2_1 = (fun2_1 - min(sd_difs_count$fun2_1))/(max(sd_difs_count$fun2_1) -
                                                                                      min(sd_difs_count$fun2_1)))
    
    user_info = user_info %>% mutate(amb1_1 = (amb1_1 - min(sd_difs_count$amb1_1))/(max(sd_difs_count$amb1_1) -
                                                                                      min(sd_difs_count$amb1_1)))
    user_info = user_info %>% mutate(amb2_1 = (amb2_1 - min(sd_difs_count$amb2_1))/(max(sd_difs_count$amb2_1) -
                                                                                      min(sd_difs_count$amb2_1)))
    
    user_info = user_info %>% mutate(shar1_1 = (shar1_1 - min(sd_difs_count$shar1_1))/(max(sd_difs_count$shar1_1) -
                                                                                         min(sd_difs_count$shar1_1)))
    user_info = user_info %>% mutate(shar2_1 = (shar2_1 - min(sd_difs_count$shar2_1))/(max(sd_difs_count$shar2_1) -
                                                                                         min(sd_difs_count$shar2_1)))

    sd_difs_count = sd_difs_count %>% distinct(iid, .keep_all = TRUE)
    sd_users = sd_difs_count
        
    sd_difs_count = sd_difs_count %>% mutate(attr1_1 = (attr1_1 - min(sd_difs_count$attr1_1))/(max(sd_difs_count$attr1_1) -
                                                                                                 min(sd_difs_count$attr1_1)))
    sd_difs_count = sd_difs_count %>% mutate(attr2_1 = (attr2_1 - min(sd_difs_count$attr2_1))/(max(sd_difs_count$attr2_1) -
                                                                                                 min(sd_difs_count$attr2_1)))
    
    sd_difs_count = sd_difs_count %>% mutate(sinc1_1 = (sinc1_1 - min(sd_difs_count$sinc1_1))/(max(sd_difs_count$sinc1_1) -
                                                                                                 min(sd_difs_count$sinc1_1)))
    sd_difs_count = sd_difs_count %>% mutate(sinc2_1 = (sinc2_1 - min(sd_difs_count$sinc2_1))/(max(sd_difs_count$sinc2_1) -
                                                                                                 min(sd_difs_count$sinc2_1)))
    
    sd_difs_count = sd_difs_count %>% mutate(intel1_1 = (intel1_1 -min(sd_difs_count$intel1_1))/(max(sd_difs_count$intel1_1) -
                                                                                                   min(sd_difs_count$intel1_1)))
    sd_difs_count = sd_difs_count %>% mutate(intel2_1 = (intel2_1 - min(sd_difs_count$intel2_1))/(max(sd_difs_count$intel2_1) - min(sd_difs_count$intel2_1)))
    
    sd_difs_count = sd_difs_count %>% mutate(fun1_1 = (fun1_1 - min(sd_difs_count$fun1_1))/(max(sd_difs_count$fun1_1) - min(sd_difs_count$fun1_1)))
    sd_difs_count = sd_difs_count %>% mutate(fun2_1 = (fun2_1 - min(sd_difs_count$fun2_1))/(max(sd_difs_count$fun2_1) -
                                                                                              min(sd_difs_count$fun2_1)))
    
    sd_difs_count = sd_difs_count %>% mutate(amb1_1 = (amb1_1 - min(sd_difs_count$amb1_1))/(max(sd_difs_count$amb1_1) -
                                                                                              min(sd_difs_count$amb1_1)))
    sd_difs_count = sd_difs_count %>% mutate(amb2_1 = (amb2_1 - min(sd_difs_count$amb2_1))/(max(sd_difs_count$amb2_1) -
                                                                                              min(sd_difs_count$amb2_1)))
    
    sd_difs_count = sd_difs_count %>% mutate(shar1_1 = (shar1_1 - min(sd_difs_count$shar1_1))/(max(sd_difs_count$shar1_1) -
                                                                                                 min(sd_difs_count$shar1_1)))
    sd_difs_count = sd_difs_count %>% mutate(shar2_1 = (shar2_1 - min(sd_difs_count$shar2_1))/(max(sd_difs_count$shar2_1) -
                                                                                                min(sd_difs_count$shar2_1)))
    sd_difs_count = sd_difs_count %>% mutate(
      age_dif = abs(sd_difs_count$age - user_info$age),
      imprace_dif = abs(sd_difs_count$imprace - user_info$imprace),
      imprelig_dif = abs(sd_difs_count$imprelig - user_info$imprelig),
      go_out_dif = abs(sd_difs_count$go_out - user_info$go_out),
      sports_dif = abs(sd_difs_count$sports - user_info$sports),
      tvsports_dif = abs(sd_difs_count$tvsports - user_info$tvsports),
      exercise_dif = abs(sd_difs_count$exercise - user_info$exercise),
      gaming_dif = abs(sd_difs_count$gaming - user_info$gaming),
      clubbing_dif = abs(sd_difs_count$clubbing - user_info$clubbing),
      reading_dif = abs(sd_difs_count$reading - user_info$reading),
      shopping_dif = abs(sd_difs_count$shopping - user_info$shopping),
      yoga_dif = abs(sd_difs_count$yoga - user_info$yoga),
      attr1_1_dif = abs(sd_difs_count$attr1_1 - user_info$attr1_1),
      sinc1_1_dif = abs(sd_difs_count$sinc1_1 - user_info$sinc1_1),
      intel1_1_dif = abs(sd_difs_count$intel1_1 - user_info$intel1_1),
      fun1_1_dif = abs(sd_difs_count$fun1_1 - user_info$fun1_1),
      amb1_1_dif = abs(sd_difs_count$amb1_1 - user_info$amb1_1),
      shar1_1_dif = abs(sd_difs_count$shar1_1 - user_info$shar1_1),
      attr2_1_dif = abs(sd_difs_count$attr2_1 - user_info$attr2_1),
      sinc2_1_dif = abs(sd_difs_count$sinc2_1 - user_info$sinc2_1),
      intel2_1_dif = abs(sd_difs_count$intel2_1 - user_info$intel2_1),
      fun2_1_dif = abs(sd_difs_count$fun2_1 - user_info$fun2_1),
      amb2_1_dif = abs(sd_difs_count$amb2_1 - user_info$amb2_1),
      shar2_1_dif = abs(sd_difs_count$shar2_1 - user_info$shar2_1))
    iid = sd_difs_count %>% select(iid)
    id = sd_difs_count %>% select(id)
    sd_difs_count = sd_difs_count %>% select(age_dif, imprace_dif, imprelig_dif, go_out_dif, sports_dif,
                                             tvsports_dif, exercise_dif, gaming_dif, clubbing_dif,
                                             reading_dif,shopping_dif, yoga_dif, attr1_1_dif, sinc1_1_dif, intel1_1_dif,
                                             fun1_1_dif, amb1_1_dif, shar1_1_dif, attr2_1_dif,
                                             sinc2_1_dif, intel2_1_dif, fun2_1_dif, amb2_1_dif, shar2_1_dif)
    
    pred_user_0 = predict(wf_rf, sd_difs_count, type = "prob")
    pred_user_0 = pred_user_0 %>% cbind(id) %>% cbind(iid)
    pred_user_0 = pred_user_0 %>% arrange(-pred_user_0$.pred_1)
    print(pred_user_0)
    
    df_with_iid = sd_users %>% filter(iid > -1) #bots
    df_with_id = sd_users %>% filter(id != -1) #users
    
    print(df_with_iid)
    print(length((df_with_iid$iid)))
    print("id:")
    print(length(unique(df_with_id$id)))
    print(length((df_with_id$id)))
    
    res_with_iid = merge(pred_user_0, df_with_iid, by="iid") %>% select(-id.x, -id.y) %>% mutate(id = -1)
    res_with_id = merge(pred_user_0, df_with_id, by="id") %>% select(-iid.x, -iid.y) %>% mutate(iid = -id)
    print(res_with_id)
    print(res_with_iid)
    
    res = rbind(res_with_iid, res_with_id) %>% arrange(-pred_user_0$.pred_1) %>% select(-iid, -.pred_1, -.pred_0)
    return(res)
}

onDatingAppStart <- function(input, output, user) {
  shinyjs::hide(id = "dateOpenRegistrationFormButton")
  shinyjs::hide(id = "dateAuthenticateButton")
  shinyjs::hide(id = "dateRegistrationForm")
  shinyjs::hide(id = "dateAuthWindow")
  
  predictions = getPredictions(user)
  
  
  output$datePredictedResult <- renderTable(predictions[1, ])
  
  output$dateLikeButton <- renderUI(
    actionButton("dateLikeButton", "❤️", style="float:right")
  )
  output$dateDislikeButton <- renderUI(
    actionButton("dateDislikeButton", "❌",  style="float:left")
  )
  
  return(predictions[-1, ])
}

server <- function(input, output, session) {
  #Connectivity test tab
  
  #hide until form is completed
  shinyjs::hide(id = "conRegisterForm")
  
  df = createEmptyInputDf()
  userSession = NULL
  rv <- reactiveVal(df)
  
  output$doneButton <- renderUI(
    actionButton("doneButton", label = ifelse(nrow(rv()) < 1, "Отправить свою анкету", "Отправить анкету партнера"))
  )
  
  observeEvent(input$doneButton, {
    # Логика обработки введенных пользователем данных
    newdf = rv()
    
    newdf[nrow(newdf) + 1,] = c(
      input$age, input$gender, input$sports, input$tv_sports, input$exercise, input$gaming, input$clubbing, input$reading, 
      input$shopping, input$yoga, input$museums, input$art, input$hiking, input$tv, input$theater, input$movies, input$go_out, input$concerts, input$music,
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
        paste("Ваш процент совместимости:", pred_prob, "%", sep="")
      )
      output$continueFormButton = renderUI(
        actionButton("continueFormButton", label = "Продолжить")
      )
    }
  })
  
  observeEvent(input$continueFormButton, {
    shinyjs::hide(id = "conResult")
    shinyjs::show(id = "conRegisterForm")
  })
  
  userSession = NULL
  
  observeEvent(input$conRegisterButton, {
    shinyjs::hide(id = "conRegisterForm")
    newdf = rv()
    
    saveUser(input$username, input$password)
    
    user = getUserByUsername(input$username)
    
    user_feature = cbind(id = user$id, newdf[1,])
    
    saveFeatures(user_feature)
    
    userSession <<- reactiveVal(user)
    
    df = createEmptyInputDf()
    
    shinyjs::show(id = "conResult")
    shinyjs::hide(id ="continueFormButton")
  })
  
  #Dating app tab
  predictions = NULL
  shinyjs::hide(id = "dateRegistrationForm")
  shinyjs::hide(id = "dateAuthWindow")
  
  if (is.null(userSession)) {
    
    observeEvent(input$dateOpenRegistrationFormButton, {
      shinyjs::show(id = "dateRegistrationForm")
      shinyjs::hide(id = "dateAuthWindow")
    })
    
    observeEvent(input$dateAuthenticateButton, {
      shinyjs::show(id = "dateAuthWindow")
      shinyjs::hide(id = "dateRegistrationForm")
    })
    
    observeEvent(input$dateRegister, {
      user = onDatingAppRegisterUser(input, output)
      if (!is.null(user)) {
        userSession <<- reactiveVal(user)
        predictions <<- reactiveVal(onDatingAppStart(input, output, userSession()))
        print(predictions)
      }
    })
    
    observeEvent(input$dateAuth, {
      user = onDatingAppAuth(input, output)
      if (!is.null(user)) {
        userSession <<- reactiveVal(user)
        predictions <<- reactiveVal(onDatingAppStart(input, output, userSession()))
        print(predictions)
      }
    })
    
  } else {
    shinyjs::hide(id = "dateOpenRegistrationFormButton")
    shinyjs::hide(id = "dateAuthenticateButton")
    shinyjs::hide(id = "dateRegistrationForm")
    shinyjs::hide(id = "dateAuthWindow")
    predictions <<- reactiveVal(onDatingAppStart(input, output, userSession()))
    print(predictions)
  }
  
  observeEvent(input$dateDislikeButton, {
    user = userSession()
    if (is.null(predictions()) | nrow(predictions()) == 0) {
      print("is null")
      predictions <<- reactiveVal(getPredictions(user))
      print("AFTER QUERY")
    }
    
    predDf = predictions()
    print(predDf)
    
    print("NEXT:")
    nextRow = predDf[1, ]
    print(nextRow)
    
    output$datePredictedResult = renderTable(nextRow)
    
    predDf = predDf[-1, ]
    predictions(predDf)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
