# ui----
ui <- dashboardPage(
  
  dashboardHeader(title = 'Pitcher Similarity', 
                  titleWidth = 200
  ), # dashboardHeader
  
  dashboardSidebar(width = 200,
                   sidebarMenu(
                     menuItem('Select by pitcher', tabName = 'tab_select_by_pitcher'),
                     menuItem('Select by pitch mix', tabName = 'tab_select_by_pitch_mix')
                   ) # sidebarMenu
  ), # dashboardSidebar
  
  dashboardBody(
    
    tabItems(
      
      # ui - Pitcher tab----
      tabItem(tabName = 'tab_select_by_pitcher',
              
              box(width = 7,
                  height = '500px',
                  plotlyOutput(outputId = 'scatterplot', height = '410px'), 
                  title = htmlOutput('scatterplot_box_title'),
                  solidHeader = TRUE, 
                  status = 'primary'
              ), # box (scatterplot)
              
              box(width = 5, status = 'info',
                  column(12,
                         selectizeInput(inputId = 'player_choice', 
                                        label = 'Pitcher:',
                                        choices = sort(unique(pitches$player_name)), 
                                        multiple = FALSE, 
                                        selected = 'Noah Syndergaard', 
                                        options = list(placeholder='Select pitcher')),
                         selectInput(inputId = 'n_pitches_choice', 
                                     label = 'Number of pitches:', 
                                     choices = seq(1:7), 
                                     selected = 2),
                         sliderInput(inputId = 'spin_ratio_importance_choice',
                                     label = 'Emphasize velocity or spin rate?**',
                                     min = -1,
                                     max = 1,
                                     value = 0,
                                     step = 0.05,
                                     round = -2,
                                     ticks = FALSE),
                         radioButtons(inputId = 'points_format_choice', 
                                      label = 'Output type:', 
                                      choices = c('Points', 'Names'), 
                                      selected = 'Points'),
                         htmlOutput(outputId = 'neighbors_list'),
                         htmlOutput(outputId = 'metrics_list')
                  ) # column
              ), # box (inputs)
              
              fluidRow(
                div(dataTableOutput(outputId = 'neighbors_table'), style = 'font-size:80%')
              ), # fluidRow
              
              fluidRow(htmlOutput(outputId = 'disclaimer_text'))
              
      ), # tabItem (tab_select_by_pitcher)
      
      
      
      
      # ui - Pitch mix tab----
      tabItem(tabName = 'tab_select_by_pitch_mix',
              
              box(width = 7,
                  height = '500px',
                  plotlyOutput(outputId = 'scatterplot_2', height = '410px'), 
                  title = htmlOutput('scatterplot_box_title_2'),
                  solidHeader = TRUE, 
                  status = 'primary'
              ), # box (scatterplot_2)
              
              box(width = 5, status = 'info',
                  column(12,
                         pickerInput(inputId = 'pitch_mix_choice', 
                                     label = 'Select pitches:', 
                                     choices = sort(unique(pitches$pitch)),
                                     multiple = T, 
                                     selected = c('FF', 'CH', 'SL'),
                                     options = list(`none-selected-text`='No pitches selected')),
                         selectizeInput(inputId = 'player_choice_2', 
                                        label = textOutput(outputId = 'n_possible_highlighted_pitchers'),
                                        choices = sort(unique(pitches$player_name)), 
                                        multiple = FALSE,
                                        selected = 'Noah Syndergaard', 
                                        options = list(placeholder='Select pitcher',
                                                       onInitialize = I('function() { this.setValue(""); }'))),
                         sliderInput(inputId = 'spin_ratio_importance_choice_2',
                                     label = 'Emphasize velocity or spin rate?**',
                                     min = -1,
                                     max = 1,
                                     value = 0,
                                     step = 0.05,
                                     round = -2,
                                     ticks = FALSE),
                         radioButtons(inputId = 'points_format_choice_2', 
                                      label = 'Output type:', 
                                      choices = c('Points', 'Names'), 
                                      selected = 'Points'),
                         htmlOutput(outputId = 'neighbors_list_2'),
                         htmlOutput(outputId = 'metrics_list_2')
                  ) # column
              ), # box (inputs_2)
              
              fluidRow(
                div(dataTableOutput(outputId = 'neighbors_table_2'), style = 'font-size:80%')
              ), # fluidRow
              
              fluidRow(htmlOutput(outputId = 'disclaimer_text_2'))
              
      ) # tabItem (tab_select_by_pitch_mix)
      
    ) # tabItems
  ) # dashboardBody
) # dashboardPage