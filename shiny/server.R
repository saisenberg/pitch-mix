# server----
server <- function(input, output, session){
  
  # server - Pitcher tab----
  
  # Change input for number of pitches to cluster on
  max_n_pitches <- reactive(length(pitcherTopPitches(player_name = input$player_choice, n_pitches = 100, metrics = FALSE)))
  isolate(observe({updateSelectInput(session = session, inputId = 'n_pitches_choice', choices = seq(1:max_n_pitches()), selected = min(max_n_pitches(), 2))}))
  
  # Scatterplot box title text
  pitch_mix_string <- reactive(paste0('&nbsp;', paste0(input$pitch_mix_choice, collapse = ', '), '<br><h5>&nbsp;', input$player_choice_2, '</h5>'))
  output$scatterplot_box_title_2 <- renderUI({
    HTML(pitch_mix_string())
  })
  
  # List of neighbors text
  pitcher_neighbors <- reactive(nearestNeighborsFromPitcher(player_name = input$player_choice, n_pitches = as.numeric(input$n_pitches_choice), k = 5, type = 'NONE', scale = TRUE, nonmetric = TRUE, spin_ratio_importance = 2 ** as.numeric(input$spin_ratio_importance_choice)))
  pitcher_neighbors_formatted <- reactive(pitcher_neighbors() %>% mutate(neighbor_formatted=paste0(row_number(), '. ', neighbors_names)))
  neighbors_text_string <- reactive(paste0(pitcher_neighbors_formatted()$neighbor_formatted, ' - ', sprintf('%.2f', round(pitcher_neighbors_formatted()$neighbors_dists, 4)), collapse = '<br>'))
  
  # List of metrics text
  metrics_df <- reactive(pitcherTopPitches(player_name = input$player_choice, n_pitches = input$n_pitches_choice, metrics = TRUE))
  metrics_text_string <- reactive(paste0(metrics_df()$pitch_list, ' - ', metrics_df()$velocity_list, ' MPH / ', metrics_df()$spin_rate_list, ' RPM', collapse = '<br>'))
  
  # Data table of all possible neighbors
  all_possible_neighbors <- reactive(allNeighborsFromPitcher(player_name = input$player_choice, n_pitches = as.numeric(input$n_pitches_choice), type = 'NONE', scale = TRUE, nonmetric = TRUE, spin_ratio_importance = 2 ** as.numeric(input$spin_ratio_importance_choice)))
  
  # Separate datasets for selected pitcher and non-selected pitchers
  mds_full <- reactive(createMDSFromPitcher(player_name = input$player_choice, n_pitches = input$n_pitches_choice))
  mds_selection_yes <- reactive(filter(mds_full(), player_name == input$player_choice))
  mds_selection_neighbors <- reactive(filter(mds_full(), player_name %in% pitcher_neighbors()$neighbors_names))
  mds_selection_no <- reactive(filter(mds_full(), !(player_name %in% c(input$player_choice, pitcher_neighbors()$neighbors_names))))
  n_possible_neighbors <- reactive(nrow(mds_selection_neighbors()) + nrow(mds_selection_no()))
  
  # Disclaimer text
  output$disclaimer_text <- renderUI({
    HTML(paste0('<center><h6>', disclaimer_text, '</h6></center>'))
  })
  
  # Global geom aesthetics
  axis_text_size <- 8
  point_size = 3.5
  
  # OUTPUT - scatterplot
  output$scatterplot <- renderPlotly({
    req(input$player_choice)
    
    if(input$points_format_choice == 'Points'){
      p <- ggplot(NULL, aes(x=V1, y=V2, text=player_name)) +
        geom_point(data=mds_selection_no(),
                   shape=21,
                   size=point_size,
                   alpha=0.5,
                   color='blue3',
                   fill='skyblue2',
                   stroke=0.1) +
        geom_point(data=mds_selection_neighbors(),
                   shape=21,
                   size=point_size,
                   alpha=0.75,
                   color='orange2',
                   fill='orange') +
        geom_point(data=mds_selection_yes(),
                   shape=21,
                   size=point_size,
                   alpha=0.75,
                   color='red2',
                   fill='red')
    } else {
      p <- ggplot(NULL, aes(x=V1, y=V2, text=player_name)) +
        geom_text(data=mds_selection_no(), aes(label=player_name), color='steelblue3', size=3) +
        geom_text(data=mds_selection_neighbors(), aes(label=player_name), color='orange', size=3) +
        geom_text(data=mds_selection_yes(), aes(label=player_name), color='red3', size=3) +
        scale_x_continuous(expand = c(.15, .15))
    }
    
    p2 <- p +
      theme_minimal() +
      labs(title = NULL, x='Dimension 1*', y='Dimension 2*') +
      theme(#text = element_text(family='Source Sans Pro'),
            axis.title.x = element_text(size=axis_text_size, face='italic'),
            axis.title.y = element_text(size=axis_text_size, face='italic'),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    ggplotly(p2, tooltip = c('text')) %>%
      layout(xaxis = list(side = 'top'))
  })
  
  # OUTPUT - neighbors list
  output$neighbors_list <- renderUI({
    req(input$player_choice)
    HTML(paste0('<b>Nearest neighbors (of ', n_possible_neighbors(), ') :</b><br>', neighbors_text_string(), '<br>&nbsp;'))
  })
  
  # OUTPUT - metrics list
  output$metrics_list <- renderUI({
    req(input$player_choice)
    HTML(paste0('<b>Pitch metrics:</b><br>', metrics_text_string(), '<br>'))
  })
  
  # OUTPUT - data table
  output$neighbors_table <- renderDataTable({
    all_possible_neighbors()
  },
  rownames = FALSE,
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '5px', targets = 0),
                      list(width = '150px', targets = 1)),
    dom = c('tp'),
    pageLength = 10,
    processing = FALSE
  ))
  
  
  # server - Pitch mix tab----
  
  # Change input for possible pitchers to highlight
  possible_highlighted_pitchers <- reactive(nearestNeighborsFromPitchList(pitch_list = input$pitch_mix_choice, k = 1, type = 'NONE', spin_ratio_importance = 2 ** as.numeric(input$spin_ratio_importance_choice_2))$player_name)
  observe({updateSelectizeInput(session = session, inputId = 'player_choice_2', choices = as.character(possible_highlighted_pitchers()))})
  
  # Scatterplot box title text (2)
  pitches_chosen_string <- reactive(paste0(pitcherTopPitches(player_name = input$player_choice, n_pitches = input$n_pitches_choice, metrics = FALSE), collapse = ', '))
  output$scatterplot_box_title <- renderUI({
    req(input$player_choice)
    HTML(paste0(input$player_choice, br(), '<h5>', pitches_chosen_string(), '</h5>'))
  })
  
  # Change select text for possible pitchers to highlight
  n_possible_highlighted_pitchers <- reactive(length(as.character(possible_highlighted_pitchers())))
  output$n_possible_highlighted_pitchers <- reactive(paste0('Highlighted pitcher (of ', as.character(n_possible_highlighted_pitchers()), '):'))
  
  # Collect all possible nearest neighbors and prepare text for top 5
  all_possible_neighbors_mix <- reactive(nearestNeighborsWideToLong(nearestNeighborsFromPitchList(pitch_list = input$pitch_mix_choice, k=1e5, type = 'NONE', scale = TRUE, nonmetric = TRUE, spin_ratio_importance = 2 ** as.numeric(input$spin_ratio_importance_choice_2))))
  pitcher_neighbors_2 <- reactive(all_possible_neighbors_mix() %>% filter(player_name == input$player_choice_2, num <= 5) %>% mutate(neighbors_formatted = paste0(num, '. ', neighbors_names, ' - ', neighbors_dists)))
  five_neighbors_from_mix_text_string <- reactive(paste0(pitcher_neighbors_2()$neighbors_formatted, collapse = '<br>'))
  
  # List of metrics text (2)
  metrics_df_2 <- reactive(pitcherMetricsFromPitchList(player_name = input$player_choice_2, pitch_list = input$pitch_mix_choice))
  metrics_text_string_2 <- reactive(paste0(metrics_df_2()$pitch, ' - ', metrics_df_2()$velocity, ' MPH / ', metrics_df_2()$spin_rate, ' RPM', collapse = '<br>'))
  
  # Data table of all possible neighbors
  all_possible_neighbors_2 <- reactive(allNeighborsFromPitcherWithPitchList(player_name = input$player_choice_2, pitch_list = input$pitch_mix_choice, type = 'NONE', scale = TRUE, nonmetric = TRUE, spin_ratio_importance = 2 ** as.numeric(input$spin_ratio_importance_choice_2)))
  
  # Disclaimer text (2)
  output$disclaimer_text_2 <- renderUI({
    HTML(paste0('<center><h6>', disclaimer_text, '</h6></center>'))
  })
  
  # MDS dataset from pitch list
  mds_from_mix <- reactive(createMDSFromPitchList(pitch_list = input$pitch_mix_choice, scale = TRUE, nonmetric = TRUE))
  mds_from_mix_yes <- reactive(filter(mds_from_mix(), player_name == input$player_choice_2))
  mds_from_mix_neighbors <- reactive(filter(mds_from_mix(), player_name %in% pitcher_neighbors_2()$neighbors_names))
  mds_from_mix_no <- reactive(filter(mds_from_mix(), !(player_name %in% c(input$player_choice2, pitcher_neighbors_2()$neighbors_names))))
  
  # OUTPUT - scatterplot_2
  output$scatterplot_2 <- renderPlotly({
    
    validate(
      need(!is.null(nearestNeighbors(df = prepClustering(pitch_list = input$pitch_mix_choice))), 'Not enough pitchers with this pitch mix!')
    )
    
    if(input$points_format_choice_2 == 'Points'){
      p <- ggplot(NULL, aes(x=V1, y=V2, text=player_name)) + 
        geom_point(data=mds_from_mix_no(),
                   shape=21,
                   size=point_size,
                   alpha=0.5,
                   color='blue3',
                   fill='skyblue2',
                   stroke=0.1) +
        geom_point(data=mds_from_mix_neighbors(),
                   shape=21,
                   size=point_size,
                   alpha=0.75,
                   color='orange2',
                   fill='orange') +
        geom_point(data=mds_from_mix_yes(),
                   shape=21,
                   size=point_size,
                   alpha=0.75,
                   color='red2',
                   fill='red')
    } else {
      p <- ggplot(NULL, aes(x=V1, y=V2, text=player_name)) +
        geom_text(data=mds_from_mix_no(), aes(label=player_name), color='steelblue3', size=3) + 
        geom_text(data=mds_from_mix_neighbors(), aes(label=player_name), color='orange', size=3) +
        geom_text(data=mds_from_mix_yes(), aes(label=player_name), color='red3', size=3) +
        scale_x_continuous(expand = c(.15, .15))
    }
    
    p2 <- p +
      theme_minimal() + 
      labs(title = NULL, x='Dimension 1*', y='Dimension 2*') +
      theme(#text = element_text(family='Source Sans Pro'), 
            axis.title.x = element_text(size=axis_text_size, face='italic'), 
            axis.title.y = element_text(size=axis_text_size, face='italic'), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    ggplotly(p2, tooltip = c('text')) %>% 
      layout(xaxis = list(side = 'top'))
  })
  
  # OUTPUT - neighbors list 2
  output$neighbors_list_2 <- renderUI({
    req(input$player_choice_2)
    HTML(paste0('<b>Nearest neighbors:</b><br>', five_neighbors_from_mix_text_string()))
  })
  
  # OUTPUT - metrics list 2
  output$metrics_list_2 <- renderUI({
    req(input$player_choice_2)
    HTML(paste0('<br><b>Pitch metrics:</b><br>', metrics_text_string_2(), '<br>'))
  })
  
  # OUTPUT - data table 2
  output$neighbors_table_2 <- renderDataTable({
    req(input$player_choice_2)
    all_possible_neighbors_2()
  },
  rownames = FALSE,
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '5px', targets = 0),
                      list(width = '150px', targets = 1)),
    dom = c('tp'),
    pageLength = 10,
    processing = FALSE
  ))
  
  
}