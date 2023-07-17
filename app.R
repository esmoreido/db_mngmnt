library(shiny)
library(shinydashboard)
library(RPostgreSQL)

# user table
user_base <- tibble::tibble(
  user = c("admin", "user"),
  password = c("1234", "4321"),
  permissions = c("admin", "standard"),
  name = c("Администратор", "Пользователь")
)
# хэдер с кнопкой выхода ----
header <- dashboardHeader(title = 'СУБД НГМС',
                          # кнопка выхода пользователя
                          tags$li(class = "dropdown", style = "padding-top: 15px; padding-right: 15px; 
                                  padding-bottom: 15px; color: #fff;", 
                                  div(class = "pull-right", 
                                      shinyauthr::logoutUI(id = "logout", 
                                                           label = "Log out", 
                                                           icon = NULL, 
                                                           class = "btn-danger",
                                                           style = "color: white;"))))

# меню справа ----
sidebar <- dashboardSidebar(
  collapsed = TRUE, 
  div(htmlOutput("welcome"), style = "padding: 20px"),
  sidebarMenu(
    menuItem("Просмотр таблиц", tabName = "view_table", icon = icon("search")),
    # menuItem("Создание таблиц", tabName = "create_table", icon = icon("plus-square")),
    menuItem("Обновление таблиц", tabName = "update_table", icon = icon("exchange-alt")),
    menuItem("Добавление записей", tabName = "insert_value", icon = icon("edit")),
    # menuItem("Удаление таблиц", tabName = "del_table", icon = icon("trash-alt")),
    menuItem("Информация", tabName = "about", icon = icon("info-circle"))
  )
)

# основной текст ----
body <- dashboardBody(
  
  # панель аутентификации
  shinyauthr::loginUI(id = "login", 
                      title = "Пожалуйста, войдите в систему", 
                      user_title = "Пользователь",
                      pass_title = "Пароль", 
                      login_title = "Вход",
                      error_message = "Неверный логин или пароль!",
                      additional_ui = NULL),
  
  # контент после логина
  tabItems(
    tabItem(tabName = "view_table", uiOutput("tab1UI")),
    # tabItem(tabName = "create_table", uiOutput("tab2UI")),
    tabItem(tabName = "update_table", uiOutput("tab3UI")),
    tabItem(tabName = "insert_value", uiOutput("tab4UI")),
    # tabItem(tabName = "del_table", uiOutput("tab5UI")),
    tabItem(tabName = "about", uiOutput("tab6UI"))
  )
)

# сборка интерфейса
ui <- dashboardPage(header, sidebar, body)

# connect DB ----
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "amur22_non_iwp",
                 host = "192.168.5.219", port = 5432,
                 user = "postgres", password = "qq")

# сервер ----
server <- function(input, output, session) { 
  # логин пользователя ----
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # кнопка выхода ----
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # разворачивание меню при входе пользователя ----
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })
  
  # приветствие пользователя
  user_info <- reactive({credentials()$info})
  
  output$welcome <- renderText({ 
    req(credentials()$user_auth)
    paste("Приветствуем, ","<font color=\"#f3b404\"><b>", {user_info()$permissions}, "</b></font>","!") 
  })
  
  # 1 просмотр таблиц ----
  # краткие описания таблиц
  table_intro <- list(catalog = "Каталог типов данных", 
                      data_forecast = "Прогнозные данные",
                      data_value = "Значения данных",
                      data_value_histor = "История загруженных данных",
                      data_value_source = "Источники данных",
                      data_source = "Каталог источников данных")
  
  output$tab1UI <- renderUI({
    req(credentials()$user_auth)
    box(width = '100%', status = "primary",
        sidebarLayout(
          sidebarPanel(width = 3, 
            box(width = 10, collapsible = TRUE,
                div(style = "height: 15px; background-color: white;"),
                title = "Информация о БД:",
                p("Это основная база данных ИВП по проекту НГМС. В ней хранятся 
                  данные наблюдений за гидрометеорологическими величинами, 
                  используемыми для расчетов по НГМС, а также результаты 
                  расчетов.")),
            selectInput(
              inputId = "sel_table_1",
              label = "Таблицы в базе данных",
              choices = dbListTables(con),
              selected = "catalog"
            ),
            textOutput(outputId = "tab_intro"),
            tags$head(tags$style("#tab_intro{font-size: 15px;font-style: italic;}"))
          ),
          mainPanel(
            div(style='overflow-y: scroll', 
            h4(strong("Просмотр записей таблицы")),
            dataTableOutput(outputId = "sel_table_view")
            )
          )
        )
    )
  })
  
  db_tb <- reactive({
    print(input$sel_table_1)
    qry <- paste0("SELECT * FROM data.", input$sel_table_1)
    print(qry)
    df <- dbGetQuery(con, qry)
    return(df)
    dbDisconnect(con)
  })
  
  output$sel_table_view <- renderDataTable(
    db_tb()
  )
  
  output$tab_intro <- renderText(
    if (input$sel_table_1 %in% c("catalog","data_forecast","data_value",
                                 "data_value_histor", "data_value_source",
                                 "data_source"))
    {table_intro[[input$sel_table_1]]}
    else {table_intro$other}
  )
  
  
  # 2 создание таблиц ----
  output$tab2UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Название новой таблицы"),
        numericInput(inputId = "ncols", label = "Количество столбцов", 1, min = 1),
        uiOutput(outputId = "cols"),
        actionButton(inputId = "create_table", label = "Создать", class = "btn-info", style = "")
    )
  })
  
  output$cols <- renderUI({
    req(input$ncols >= 1)
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Столбец", i), width = 6, solidHeader = TRUE, status = "primary",
        textInput(inputId = paste0("colName", i), label = "Название столбца"),
        selectInput(inputId = paste0("colType", i), label = "Тип данных", 
                    choices = c("NUMERIC", "VARCHAR(255)","BOOLEAN","DATE")
        )
      )
    }
    cols
  })
  
  # 3 обновление таблиц ----
  output$tab3UI <- renderUI({
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = "Note:", "")
      ),
      fluidRow(
        box(title = "Переименовать таблицу", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel(
              textInput(),
              actionButton())
        ),
        box(title = "Переименовать столбец", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        ),
        box(title = "Добавить столбец", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        )
      )
    )
  })
  
  output$tab6UI <- renderUI({
    box(width='100%',  status = "primary",
    h3("Национальная гидрологическая моделирующая система"),
    p("Национальная гидрологическая моделирующая система (НГМС) в составе 
      адаптивной системы гидрологического мониторинга (АСГМ) разрабатывается 
      как национальный методологический и технологический стандарт 
      информационного обеспечения федеральных и региональных органов 
      власти для поддержки принятия решений по эффективному и экологически 
      безопасному комплексному управлению водными ресурсами речных бассейнов."),
    renderImage({
      list(src='ngms_db_schema.jpg', align='center', width = 500, deleteFile=FALSE)
    }), 
    p(HTML("<em>Блок-схема локального сервера системы «Амур» 
      (AHC/AMC — автоматический гидрометеорологический комплекс, 
      WS — Windows-сервис; DAL — библиотека доступа к данным, QA/QC — 
      обеспечение/контроль качества)</em>"))
    )
    })
  
  session$onSessionEnded(function() {
    lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
    print('Disconnected')
  })
}

shinyApp(ui, server)