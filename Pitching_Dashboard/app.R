# Shiny App: Visualize Pitching and Batting data

# ui.R
library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Baseball Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("pitcher_id", "Select Pitcher ID:", choices = NULL),
      selectizeInput("pitch_type", "Select Pitch Type:", choices = NULL),
      selectizeInput("batter_id", "Select Batter ID:", choices = NULL),
      conditionalPanel(
        condition = "input.main_tab == 'Pitching Data'",
        selectizeInput("color_by", "Color By:", choices = c("Outcome", "Pitch Number"), selected = "Outcome")
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tab",
                  tabPanel("Pitching Data",
                           fluidRow(
                             column(12, plotlyOutput("pitch_location_plot_pitching"))
                           ),
                           br(),
                           DTOutput("pitching_data_table")
                  ),
                  tabPanel("Batting Data",
                           tabPanel("Batting Data",
                                    fluidRow(
                                      column(6, plotlyOutput("pitch_location_plot_batting")),
                                      column(6, plotlyOutput("launch_angle_plot"))
                                    ),
                                    br(),
                                    DTOutput("batting_data_table")
                           ) )
                  ),
      width = 9
      )
    )
  )

server <- function(input, output, session) {
  
  # Load baseball dataset
  baseball_data <- read.csv("Baseball_Dataset.csv")
  
  # All unique initial options
  all_pitchers <- sort(unique(baseball_data$PitcherID))
  all_pitch_types <- sort(unique(baseball_data$PitchType))
  all_batters <- sort(unique(baseball_data$BatterID))
  
  # Initialize at startup
  updateSelectInput(session, "pitcher_id", choices = c("All", all_pitchers))
  updateSelectInput(session, "pitch_type", choices = c("All", all_pitch_types))
  updateSelectInput(session, "batter_id", choices = c("All", all_batters))
  
  # Dynamic updating of dropdowns based on any selection
  observe({
    data <- baseball_data
    
    # Apply current selections
    if (input$pitcher_id != "All") {
      data <- data[data$PitcherID == input$pitcher_id, ]
    }
    if (input$pitch_type != "All") {
      data <- data[data$PitchType == input$pitch_type, ]
    }
    if (input$batter_id != "All") {
      data <- data[data$BatterID == input$batter_id, ]
    }
    
    # --- Now find valid options after filtering ---
    valid_pitchers <- sort(unique(data$PitcherID))
    valid_pitch_types <- sort(unique(data$PitchType))
    valid_batters <- sort(unique(data$BatterID))
    
    # --- Save current selections ---
    current_pitcher <- isolate(input$pitcher_id)
    current_pitch_type <- isolate(input$pitch_type)
    current_batter <- isolate(input$batter_id)
    
    # --- Update PitcherID dropdown ---
    updateSelectInput(
      session, "pitcher_id",
      choices = c("All", valid_pitchers),
      selected = if (current_pitcher %in% valid_pitchers) current_pitcher else "All"
    )
    
    # --- Update PitchType dropdown ---
    updateSelectInput(
      session, "pitch_type",
      choices = c("All", valid_pitch_types),
      selected = if (current_pitch_type %in% valid_pitch_types) current_pitch_type else "All"
    )
    
    # --- Update BatterID dropdown ---
    updateSelectInput(
      session, "batter_id",
      choices = c("All", valid_batters),
      selected = if (current_batter %in% valid_batters) current_batter else "All"
    )
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- baseball_data
    
    if (input$pitcher_id != "All") {
      data <- data[data$PitcherID == input$pitcher_id, ]
    }
    if (input$pitch_type != "All") {
      data <- data[data$PitchType == input$pitch_type, ]
    }
    if (input$batter_id != "All") {
      data <- data[data$BatterID == input$batter_id, ]
    }
    
    data
    
    
  })
  
  
  outcome_levels <- c("Strike", "Foul", "In-play out", "Ball", "Single", "Double", "Triple", "Home Run")
  outcome_colors <- c(
    "Strike" = "green",
    "Foul" = "limegreen",
    "In-play out" = "limegreen",
    "Ball" = "yellow",
    "Single" = "orange",
    "Double" = "orangered",
    "Triple" = "tomato",
    "Home Run" = "red"
  )
  
  # Plot for Pitching Tab
  output$pitch_location_plot_pitching <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    if (input$color_by == "Outcome") {
      p <- plot_ly(
        data,
        x = ~X_Location_m,
        y = ~Y_Location_m,
        type = 'scatter',
        mode = 'markers',
        color = ~factor(Outcome, levels = outcome_levels),
        colors = outcome_colors,
        marker = list(size = 7),
        text = ~paste(
          "Outcome:", Outcome, "<br>",
          "Velocity:", round(Velocity_mph, 1), "mph", "<br>",
          "Spin Rate:", round(SpinRate_rpm), "rpm"
        ),
        hoverinfo = 'text'
      )
    } else if (input$color_by == "Pitch Number") {
      data$PitchPercent <- (1:nrow(data)) / nrow(data)
      
      p <- plot_ly(
        data,
        x = ~X_Location_m,
        y = ~Y_Location_m,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = 7,
          color = ~PitchPercent,
          colorscale = list(
            list(0, "#00CC00"),
            list(1, "#CC0000")
          ),
          showscale = TRUE,
          colorbar = list(
            title = "Pitch Progression",
            tickvals = c(0, 1),
            ticktext = c("First", "Last")
          )
        ),
        text = ~paste(
          "Pitch %:", PitchPercent, "<br>",
          "Velocity:", round(Velocity_mph, 1), "mph", "<br>",
          "Spin Rate:", round(SpinRate_rpm, 1), "rpm"
        ),
        hoverinfo = 'text'
      )
    }
    
    p %>% layout(
      xaxis = list(
        title = "Horizontal Location (m)",
        range = c(-.75, .75),
        zeroline = FALSE,
        showgrid = FALSE,
        scaleanchor = "y",
        scaleratio = 1
      ),
      yaxis = list(
        title = "Vertical Location (m)",
        range = c(0, 1.5),
        zeroline = FALSE,
        showgrid = FALSE
      ),
      shapes = list(
        list(
          type = "rect",
          x0 = -0.35, x1 = 0.35,
          y0 = 0.30, y1 = 1.20,
          line = list(color = "red"),
          fillcolor = "rgba(0,0,0,0)",
          layer = "below"
        )
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  })
  
  # Table for plotted pitching data
  output$pitching_data_table <- renderDT({
    data <- filtered_data()
    
    # Handle empty cases
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No matching pitching data"), options = list(dom = 't')))
    }
    
    # Select columns to show
    data_to_show <- data[, c(
      "PitcherID",
      "BatterID",
      "PitchType",
      "Velocity_mph",
      "SpinRate_rpm",
      "Outcome"
    )]
    
    # Round nicely
    data_to_show$Velocity_mph <- round(data_to_show$Velocity_mph, 1)
    data_to_show$SpinRate_rpm <- round(data_to_show$SpinRate_rpm, 0)
    
    DT::datatable(
      data_to_show,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Plot for Batting Tab
  output$pitch_location_plot_batting <- renderPlotly({
    
    data <- filtered_data()
    
    # Filter batting data to only include specific outcomes
    batting_outcomes <- c("In-play out", "Single", "Double", "Triple", "Home Run")
    data <- data[data$Outcome %in% batting_outcomes, ]
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(
          title = "No matching batting data for this selection",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    } else {
      plot_ly(
        data,
        x = ~X_Location_m,
        y = ~Y_Location_m,
        type = 'scatter',
        mode = 'markers',
        color = ~factor(Outcome, levels = batting_outcomes),
        colors = c(
          "In-play out" = "green",
          "Single" = "yellow",
          "Double" = "orangered",
          "Triple" = "tomato",
          "Home Run" = "red"
        ),
        marker = list(size = 7),
        text = ~paste(
          "Outcome:", Outcome, "<br>",
          "Velocity:", round(Velocity_mph, 1), "mph", "<br>",
          "Spin Rate:", round(SpinRate_rpm, 1), "rpm"
        ),
        hoverinfo = 'text'
      ) %>%
        layout(
          xaxis = list(
            title = "Horizontal Location (m)",
            range = c(-.75, .75),
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = "Vertical Location (m)",
            range = c(0, 1.5),
            zeroline = FALSE,
            showgrid = FALSE
          ),
          shapes = list(
            list(
              type = "rect",
              x0 = -0.35, x1 = 0.35,
              y0 = 0.30, y1 = 1.20,
              line = list(color = "red"),
              fillcolor = "rgba(0,0,0,0)",
              layer = "below"
            )
          ),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    }
    
  })
  
  output$launch_angle_plot <- renderPlotly({
    data <- filtered_data()
    
    batting_outcomes <- c("In-play out", "Single", "Double", "Triple", "Home Run")
    batting_colors <- c(
      "In-play out" = "green",
      "Single" = "yellow",
      "Double" = "orangered",
      "Triple" = "tomato",
      "Home Run" = "red"
    )
    data <- data[data$Outcome %in% batting_outcomes, ]
    
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(
          title = "No matching batting data for this selection",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    } else {
      # ✅ Calculate means
      summary_data <- data %>%
        group_by(Outcome) %>%
        summarise(
          mean_launch = mean(LaunchAngle_deg, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Convert to radians for plotting
      summary_data$angle_rad <- summary_data$mean_launch * pi / 180
      fixed_length <- 1.5  # adjust this for visual
      summary_data$x <- cos(summary_data$angle_rad) * fixed_length
      summary_data$y <- sin(summary_data$angle_rad) * fixed_length
      
      # Create plot
      p <- plot_ly()
      
      # Add arrow lines as shapes
      launch_shapes <- lapply(1:nrow(summary_data), function(i) {
        list(
          type = "line",
          x0 = 0,
          y0 = 0,
          x1 = summary_data$x[i],
          y1 = summary_data$y[i],
          line = list(
            color = batting_colors[summary_data$Outcome[i]],
            width = 3
          ),
          arrowhead = 2,
          arrowsize = 1
        )
      })
      
      # Add the launch angle labels as text markers
      p <- p %>%
        add_trace(
          type = "scatter",
          mode = "text",
          x = summary_data$x,
          y = summary_data$y,
          text = paste0(round(summary_data$mean_launch), "°"),
          textposition = "top center",
          showlegend = FALSE,
          textfont = list(size = 14, color = "black")
        )
      
      # Final layout
      p %>%
        layout(
          xaxis = list(
            title="Average Launch Angle (Degrees) per Outcome",
            zeroline = TRUE,
            showgrid = FALSE,
            range = c(0, 1.5),
            scaleanchor = "y",
            scaleratio = 1,
            showticklabels = FALSE
          ),
          yaxis = list(
            zeroline = TRUE,
            showgrid = FALSE,
            range = c(0, 1.5),
            showticklabels = FALSE
          ),
          shapes = launch_shapes,
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    }
  })
  
  output$batting_data_table <- renderDT({
    data <- filtered_data()
    
    # Filter batting outcomes
    batting_outcomes <- c("In-play out", "Single", "Double", "Triple", "Home Run")
    data <- data[data$Outcome %in% batting_outcomes, ]
    
    # Handle empty cases
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No matching batting data"), options = list(dom = 't')))
    }
    
    # Show datatable
    data_to_show <- data[, c(
      "PitcherID",
      "BatterID",
      "PitchType",
      "Velocity_mph",
      "SpinRate_rpm",
      "Outcome",
      "LaunchAngle_deg",
      "ExitVelocity_mph"
    )]
    
    # Round data for better visualisation
    data_to_show$Velocity_mph <- round(data_to_show$Velocity_mph, 1)
    data_to_show$ExitVelocity_mph <- round(data_to_show$ExitVelocity_mph, 1)
    data_to_show$LaunchAngle_deg <- round(data_to_show$LaunchAngle_deg, 1)
    data_to_show$SpinRate_rpm <- round(data_to_show$SpinRate_rpm, 0)
    
    DT::datatable(
      data_to_show,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)
