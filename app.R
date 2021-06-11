library(plotly)
#library(dygraphs)
library(prophet)
library(shinythemes)
library(shiny)
library(shinyjs)
library(shinymanager)
library(dplyr)

options(digits = 5)

# load the virtual env, commet it out while running locally
reticulate::virtualenv_create("python35_env", python = "python3")
reticulate::virtualenv_install("python35_env",
                               packages = c("bdshare"),
                               ignore_installed = T)
reticulate::use_virtualenv("python35_env", required = TRUE)

# read instrument names
inst_name <- readr::read_csv("Inst.csv")
inst_name <- inst_name$TRADING.CODE
t_default <- which(inst_name == "ACI")

# set the dates
enddate <- Sys.Date()
startdate <- enddate - 365 * 2

# authetication
credentials <- data.frame(
  user = c("HOT", "HOC"),
  # mandatory
  password = c("HOT", "HOC"),
  # mandatory
  #start = c("2019-04-15"), # optinal (all others)
  #expire = c(NA, "2019-12-31"),
  admin = c(TRUE, FALSE),
  comment = "Simple and secure authentification mechanism
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)

# Change language
set_labels(
  language = "en",
  "Please authenticate" = "Please Authenticate",
  "Username:" = "Username:",
  "Password:" = "Password:"
)


# the user interface
ui <- fluidPage (
  theme = shinytheme("readable"),
  headerPanel("DSE Forecast"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 2,
      selectInput(
        "instrument",
        label = "Select Instrument",
        choices =
          inst_name,
        selected = inst_name[t_default]
      ),
      selectInput(
        "crossvalidation",
        label = "Perform Cross Validationn",
        choices =
          c("No", "Yes"),
        selected = "No"
      ),
      submitButton(text = "Forecast")
    ),
    mainPanel (
      h1(""),
      align = "center",
      width = 10,
      # width of the mainpanel, total width of the window is 12, 10 is mainpanel, 2 is sidebar
      tabsetPanel(
        tabPanel("Forecasted Plot", plotlyOutput("output_forecast"), width = "100%"),
        # 100% means the plot would take the entire window
        #dygraphOutput if we want default Dygraph
        tabPanel("Cross Validation", tableOutput("output_eval"))
      )
    )
  )
)

ui <- secure_app(ui)

# the server side
server <- function(input, output, session) {
  # authentication part
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # function part
  forecast_func <-
    function(instrument, crossvalidation) {
      # get the data
      reticulate::source_python("dse.py")
      
      df <-
        dse_hist(startdate,
                 enddate,
                 instrument)
      
      # keep only the required columns
      df_mod <- df %>% select(DATE, CLOSEP)
      
      # rename the columns as per requirements
      names(df_mod) <- c("ds", "y")
      
      # fit the model
      model_fit <- prophet(df_mod, seasonality.mode = "additive")
      future_get <- make_future_dataframe(model_fit, periods = 60)
      forecast_get <-
        predict(model_fit, future_get) %>% select(ds, yhat, yhat_lower, yhat_upper)
      
      # get the performance metrics
      if (crossvalidation == "Yes") {
        df.cv <-
          cross_validation(
            model_fit,
            initial = 100,
            horizon = 30,
            units = 'days'
          )
        model_eval <- performance_metrics(df.cv)
        model_eval$horizon <- as.integer(model_eval$horizon)
      }
      
      else {
        model_eval <-
          data.frame(
            horizon = c(""),
            mse = c(""),
            rmse = c(""),
            mae = c(""),
            mape = c(""),
            mdape = c(""),
            smape = c(""),
            coverage = c("")
          )
      }
      
      # plot the forecasts
      # pplot <-
      #   dyplot.prophet(model_fit, forecast_get, main = instrument)
      df_dt <- df %>% select(DATE, CLOSEP, VALUE)
      names(df_dt)[1] <- "ds"
      df_dt <-
        df_dt %>% mutate(ds = as.Date(ds, format = "%Y-%m-%d"))
      df_plot <- df_dt %>% full_join(forecast_get, by = "ds")
      df_plot <-
        df_plot %>% mutate_at(c(4:6), plyr::round_any, .10)
      
      pplot <- plot_ly(df_plot,
                       x = ~ ds
                       #width = 1000, #height = 450) 
                       ) %>%
                       
                       add_trace(
                         y = ~ CLOSEP,
                         size = ~ VALUE,
                         fill = ~ '',
                         type = "scatter",
                         mode = "lines+markers",
                         name = "Actual Price",
                         marker = list(color = "blue", opacity = 0.5),
                         text = ~ paste(
                           "</br> Date:",
                           ds,
                           "</br> Closing Price:",
                           CLOSEP,
                           "</br> Total Value:",
                           VALUE,
                           "M"
                         ),
                         hoverinfo = "text"
                       ) %>% add_trace(
                         y = ~ yhat,
                         name = "Forecasted Price",
                         type = "scatter",
                         mode = "lines"
                       ) %>% add_trace(
                         y = ~ yhat_upper,
                         name = "Upper Band",
                         type = "scatter",
                         mode = "lines",
                         line = list(dash = "dot")
                       ) %>% add_trace(
                         y = ~ yhat_lower,
                         name = "Lower Band",
                         type = "scatter",
                         mode = "lines",
                         line = list(dash = "dot")
                       ) %>% layout(
                         title = paste0("<br>", instrument),
                         xaxis = list(title = "Date"),
                         yaxis = list(title = "Closing Price"),
                         margin = list(t = 120), 
                         legend = list(orientation = "h", x = 0, y = 1.4)
                       )
                       
                       # return the outcomes
                       return(list(output_forecast = pplot, output_eval = model_eval))
    }
  
  # generate the output
  output_get <-
    reactive({
      forecast_func(input$instrument, input$crossvalidation)
    })
  
  #renderDygraph if we want the default Dygraph
  output$output_forecast <- renderPlotly({
    output_get()$output_forecast
  })
  
  output$output_eval <- renderTable({
    output_get()$output_eval
  })
  
  # output$output_forecast <- renderPlotly({
  #   forecast_func(input$instrument, input$crossvalidation)$output_forecast
  # })
  #
  # output$output_eval <- renderTable({
  #   forecast_func(input$instrument, input$crossvalidation)$output_eval
  # })
  
  lapply(c("output_forecast", "output_eval"), function(x)
    outputOptions(output, x, suspendWhenHidden = F))
  
}

# launch the app
shinyApp(ui = ui, server = server)
