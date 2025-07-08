# Load libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(purrr)
library(bslib)
library(thematic)
library(qs)

# SET UP

# load in filtered simulation outputs
sim <- qread("Shiny_simulations_filtered.qs")

# set custom theme for app
custom_theme <- bs_theme(bg = "#1A242F", fg = "#FBF9F5", primary = "#D4DADC", secondary = "#F4F8F9",
                         base_fonts = font_google("Pacifico"), font_scale = 0.77)

# set common plot theme
common_theme <- theme(
  axis.title.y = element_text(size = 15, colour = "#FBF9F5", margin = margin(t = 0, r = 8, b = 0, l = 0)),
  axis.title.x = element_text(size = 17, colour = "#FBF9F5", margin = margin(t = 0, r = 8, b = 0, l = 0)),
  axis.text = element_text(size = 15, colour = "#FBF9F5"),
  axis.line.y = element_line(color = "#FBF9F5"),
  axis.line.x = element_line(color = "#FBF9F5"),
  panel.background = element_rect(fill = "#1A242F"),
  plot.background = element_rect(color = "#1A242F", fill = "#1A242F"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

# define function to get run ID
get_run_id <- function(input) {
  paste(input$n_days, input$group_size, input$p_terrain_visibility, 
        input$p_behavior_visibility, input$mean_events, input$behavior_duration, 
        input$focal_duration, 5, # set focal_break_time_min fixed at 5 minutes
        input$scan_obsTime_perID, input$scan_break_time, 
        sep = '_')}

# check distributuions of MSRE and CV across all simulations to decide on limits for plots
all_MSREs <- map_dbl(sim, ~ mean(.x$accuracy_frame$mean_squared_error))
plot(density(all_MSREs))
MSRE_lower_limit = 1
MSRE_upper_limit = 500

all_CVs <- map_dbl(sim, ~ mean(.x$precision_frame$CV))
plot(density(all_CVs))
CV_lower_limit = 1
CV_upper_limit = 250

# USER INTERFACE 

ui <- fluidPage(
  padding = 20,
  theme = custom_theme,
  
  hr(),
  
  fluidRow(
    column(12, 
           h3("SIMBO: Simulator of Methods for Behavioural Observation"),
           hr(style = "margin-bottom: 10px;"))),  # reduced bottom margin of the horizontal line
  
  fluidRow(
    column(12, 
           wellPanel(
             style = "margin-top: -10px;",  # pulls text closer to the line above
             p("Selecting an appropriate observation method is fundamental in any behavioral study. While ideally, researchers would record every behavior that animals engage in, this is often unfeasible. Several systematic observation methods have therefore been developed. Two of the most widely used protocols are focal continuous sampling (also known as 'focal sampling' or 'focal follows') and group time sampling (or 'group scans'). Each of these observation methods has its own strengths and limitations."),
             tags$ul(tags$li(tags$b("Focal Follows:"), " A focal animal is continuously observed for a set time, recording all its behaviors. Provides detailed data about the focal, but overlooks all behaviors that don't involve the focal."),
                     tags$li(tags$b("Group Scans:"), " The behaviour of all individuals in a group is recorded at set intervals. Captures a broader overview of behaviours across the group but overlooks behaviors between sampling points.")),
             p("These trade-offs affect how precisely and accurately the recorded behaviors reflect the complete pattern of all behaviours occurring."),
             p("The SIMBO app allows you to explore how various parameters related to the study system, the behavior of interest, and the observational setup influence the performance of both data collection methods. It is meant to help you assess which observational protocol might be best suited to the specific properties of your system and research question. The performance is estimated using a simulation-based approach, where a known complete pattern of behavior is recorded using the two observational protocols. This creates a ground truth of behavioral events that allows us to benchmark the performance of focal follows and group scans."),
             p("Performance is measured using:"),
             tags$ul(tags$li(tags$b("Accuracy"), " (left plot): How closely the recorded behavior matches the true behavior. Estimated as the standardized root mean squared error (RMSE) between the estimated and true values.",
                             tags$br(), tags$i("Low RMSE values indicate higher accuracy.")),
                     tags$li(tags$b("Precision"), " (middle plot): How consistent the recorded behavior is across repeated observations. Estimated as the mean coefficient of variation (CV) across simulation iterations.",
                             tags$br(), tags$i("Low CV values indicate higher precision.")),
                     tags$li(tags$b("Correlation to true value"), " (right plot): How strongly the recorded behavior is linearly related to the true behavior. Estimated as the Pearson correlation coefficient between between the estimated and true values.",
                             tags$br(), tags$i("High correlation values indicate higher correlation."))))),
  
  hr(),
  
  fluidRow(
    column(2,  
           wellPanel(
             h5("Study Population"),
             sliderTextInput(inputId = "group_size", label = "Number of individuals on which to record behaviour", 
                             choices = c(20, 50, 100), selected = 100),
             sliderTextInput(inputId = "p_terrain_visibility", label = "Proportion of individuals visible at any time", 
                             choices = c(0.2, 0.5, 0.8), selected = 0.8)),
           wellPanel(
             h5("Behaviour of interest"),
             sliderTextInput(inputId = "p_behavior_visibility", label = "Visibility of behaviour", 
                             choices = c(0.2, 0.5, 0.8), selected = 0.5), 
             sliderTextInput(inputId = "mean_events", label = "Average number of occurrences of the behaviour per individual per day", 
                             choices = c(1, 7, 20, 50), selected = 20),
             sliderTextInput(inputId = "behavior_duration", label = "Average duration of the behaviour (in seconds)", 
                             choices = c(3, 30, 120, 600), selected = 3))),
    
    column(2,  
           wellPanel(
             h5("Observational set-up"),
             sliderTextInput(inputId = "n_days", label = "Number of observation days", 
                             choices = c(30, 90, 180), selected = 90),
             sliderTextInput(inputId = "focal_duration", label = "Length of focal observation (in minutes)", 
                             choices = c(15, 60), selected = 60), 
             sliderTextInput(inputId = "scan_break_time", label = "Time between scans (in minutes)", 
                            choices = c(1, 15, 60), selected = 15),
             sliderTextInput(inputId = "scan_obsTime_perID", label = "Time spent observing each individual when performing a group scan (in seconds)", 
                             choices = c(1, 5), selected = 5))),
    
    column(8,  
           fluidRow(
             tags$style(HTML(".tab-content { margin-top: 20px; }")),  
             
             column(4, 
                    h4("ACCURACY"),
                    hr(), 
                    plotOutput("accuracy_plot")),
             
             column(4, 
                    h4("PRECISION"),
                    hr(), 
                    plotOutput("precision_plot")),
             
             column(4, 
                    h4("CORRELATION TO TRUE VALUE"),
                    hr(), 
                    plotOutput("correlation_plot"))),
           
           fluidRow(
             column(12, 
                    wellPanel(
                      p("For each parameter set, we conducted ten simulation runs. Within each run, we calculated the accuracy and precision of each individual's behavioral estimate by comparing their recorded behavior to their true behavior. The data points in the accuracy and precision plots represent the accuracy and precision of individual behavioral estimates for a random selection of individuals across simulation runs for the selected set of parameters. In contrast, the correlation to true values was calculated at the simulation level by measuring the correlation between recorded and true behaviors across all individuals within each run. Each data point in the correlation plots represents the overall correlation for a single simulation run.",
                        style = "margin-bottom: -5px"))))))))

# SERVER
server <- function(input, output, session) {
  
  # create reactive filtered data for selected parameters
  filtered_data <- reactive({
    
    run <- get_run_id(input)
    print(run)
    # # for debugging:
    # run <- "90_100_0.8_0.5_20_3_60_5_5_15"
    
    # remove runs where too many long events would occur
    if (input$behavior_duration >= 600 && input$mean_events > 7) {
      validate(need(FALSE, "Too many long events to fit within one observation day. Please reduce behavior duration or number of events."))
      return(NULL)}
    
    # check if run exists directly
    if (!(run %in% names(sim))) {
      validate(need(FALSE, "No data available for this parameter set."))
      return(NULL)}
    
    data <- sim[run] %>%
      transpose() %>% 
      map(bind_rows)
    
    data})
  
  # create reactive accuracy plot for selected parameters
  accuracy_plot <- reactive({
    data <- filtered_data()
    MSE_run <- data$accuracy_frame %>%
      select(mean_squared_error, observed_data) %>%
      filter(observed_data != "focal continuous sampling rate") # filter out rates
    
    ggplot(MSE_run, aes(observed_data, mean_squared_error)) +
      geom_violin(color = "#FBF9F550", fill = "#FBF9F505") +
      geom_jitter(size = 3, color = "#FBF9F550", width = 0.2) +
      xlab("") + ylab("MSE from true proportion") +
      scale_x_discrete(labels = c('focal follows', 'group scans')) +
      scale_y_continuous(
        trans = "log10",
        labels = scales::label_number(),
        limits = c(MSRE_lower_limit, MSRE_upper_limit),
        breaks = c(1, 10, 100, 500)) +
      annotate("text", x = 0.5, y = 1 * 1.05, 
               label = "← More accurate", angle = 90, hjust = 0, vjust = 0.3, size = 4.5, color = "white") +
      annotate("text", x = 0.5, y = MSRE_upper_limit * 0.95, 
               label = "Less accurate →", angle = 90, hjust = 1, vjust = 0.3, size = 4.5, color = "white") +
      common_theme})
  
  # create reactive precision plot for selected parameters
  precision_plot <- reactive({
    data <- filtered_data()
    CV_run <- data$precision_frame %>%
      select(CV, observed_data) %>%
      filter(observed_data != "focal continuous sampling rate") # filter out rates
    
    ggplot(CV_run, aes(observed_data, CV)) +
      geom_violin(color = "#FBF9F550", fill = "#FBF9F505") +
      geom_jitter(size = 3, color = "#FBF9F550", width = 0.2) +
      xlab("") + labs(y = "CV in estimated proportions") +
      scale_x_discrete(labels = c('focal follows', 'group scans')) +
      scale_y_continuous(
        trans = "log10",
        labels = scales::label_number(),
        limits = c(CV_lower_limit, CV_upper_limit),
        breaks = c(1, 10, 100, 250)) +
      annotate("text", x = 0.5, y = CV_lower_limit * 1.05, 
               label = "← More precise", angle = 90, hjust = 0, vjust = 0.3, size = 4.5, color = "white") +
      annotate("text", x = 0.5, y = CV_upper_limit * 0.95, 
               label = "Less precise →", angle = 90, hjust = 1, vjust = 0.3, size = 4.5, color = "white") +
      common_theme})
  
  # create reactive correlation plot for selected parameters
  
  correlation_plot <- reactive({
    data <- filtered_data()
    
    print(data)
    
    cor_run <- data$cor_frame %>%
      select(cor_true_focal, cor_true_scan) %>%
      pivot_longer(everything(), names_to = "method", values_to = "value") %>%
      mutate(method = recode(method,
                             "cor_true_focal" = "focal follows",
                             "cor_true_scan" = "group scans"))
    
    ggplot(cor_run, aes(x = method, y = value)) +
      geom_violin(color = "#FBF9F550", fill = "#FBF9F505") +
      geom_jitter(size = 3, color = "#FBF9F550", width = 0.2) +
      labs(x = NULL, y = "Correlation estimated to true proportions") +
      scale_y_continuous(limits = c(0, 1)) +
      common_theme})
  
  # render plots
  output$accuracy_plot <- renderPlot({
    accuracy_plot()})
  
  output$precision_plot <- renderPlot({
    precision_plot()})
  
  output$correlation_plot <- renderPlot({
    correlation_plot()})
  }

# run the application
shinyApp(ui = ui, server = server)
