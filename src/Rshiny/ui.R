

ui <- dashboardPage(
  
  fullscreen = TRUE,
  ## Header ----  
  header = dashboardHeader(
    title = "RENTAL IN MASS",
    status = status,
    sidebarIcon = shiny::icon("list-alt"),
    skin = skin
  ),
  
  ## Footer ----
  footer = NULL,
  
  ## Sidebar ----
  controlbar = NULL,
  
  sidebar = dashboardSidebar(
    id = "sidebar",
    status = status,
    skin = skin,
    sidebar = skin_colour,
    elevation = 5, # Sidebar elevation. 4 by default (until 5).
    collapsed = FALSE,
    minified = TRUE,
    fixed = TRUE,
    expandOnHover = FALSE, # whether to expand the sidebar om hover. TRUE by default.
    sidebarMenu(
      
      id = "sidebarmenu",
     
      sidebarHeader(title = "Background information"),
      
      menuItem("Introduction",
               tabName = "intro",
               icon = icon("info"),
               selected = TRUE
      ),
      
      menuItem("Help",
               tabName = "app_help",
               icon = icon("question-circle")
      ),
      
      sidebarHeader( title = "Data"),
      
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("table"),
               selected = FALSE
      ),
      
      menuItem("Correlation",
               tabName = "correlation",
               icon = icon("image"),
               selected = FALSE
      ),
      
      menuItem("Funnel",
               tabName = "filtering",
               icon = icon("filter"),
               selected = FALSE
      )
      
      
    )
  ),
  
  
  
  ## Body ----
  body = dashboardBody(
    tags$head(
      includeCSS('css/style.css')
    ),
    tabItems(
      
      tabItem(
        tabName = "intro",
        fluidPage(
          status = status,
          box(width = 12,
              title = "Application summary",
              status = status,
              h5(
                "This app consolidates Rent prices in different towns and cities in Massachusetts, house prices and other relevant information. 
                The main objective of the app is to prioritize towns in MA where house price and rent relationship is the most favorable for a potential investor."
              ),
              h5("In summary, there are 14 Counties, with 39 cities and 312 towns in Massachusetts. Not all data resources contain information for all towns.")
          ),
          fluidRow(width = 12, 
                   timelineBlock(
                     width = 12,
                     reversed = TRUE,
                     timelineLabel("Data Sources", color = "olive"),
                     
                     
                     timelineItem(
                       title = "Rent information",
                       icon = icon("cog"),
                       color = "secondary",
                       HTML(
                         "Average Fair Market Rent Prices information was scraped from https://www.rentdata.org/states/massachusetts/ from 2006 to 2022. 
                         Massachusetts has the 3rd highest rent in the country out of 56 states and territories.  "
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Population",
                       icon = icon("sign-out-alt"),
                       color = "secondary",
                       HTML(
                         "Annual Estimates of the Resident Population: April 1, 2010 to July 1, 2019. 
                         U.S. Census Bureau Population Division. May 21, 2020. The data is available for 351 towns in MA. "
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Home prices",
                       icon = icon("sign-out-alt"),
                       color = "secondary",
                       HTML(
                         "Home prices in MA were scraped from Boston Magazine web portal: https://www.bostonmagazine.com/property/single-family-home-price-chart-2021/.
                         SOURCES: Boston neighborhood and town median home prices, sales volumes, and days on market provided by the Massachusetts Association of Realtors (marealtor.com) and MLS Property Information Network (mlspin.com)."
                         
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Percent of housing owner-occupied",
                       icon = icon("info"),
                       color = "secondary",
                       HTML(
                         "Data was compiled by the Metropolitan Area Planning Council from American Community Survey data from 05-09.
                         http://archive.boston.com/yourtown/specials/snapshot/snapshot_percent_owner_occupied_housing/#",
                         "The percent of owner-occupied homes is much smaller in urban areas than in the suburbs. 
                         Communities such as Somerville, Chelsea and Boston have ownership rates in the 30-percent range. "
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Average income per capita per town",
                       icon = icon("info"),
                       color = "secondary",
                      
                       HTML(
                         "Massachusetts is the second wealthiest state in the United States of America, with a median household income of $77,378 (as of 2019).
                         The data was retrived from https://en.wikipedia.org/wiki/List_of_Massachusetts_locations_by_per_capita_income.
                         "
                       ),
                       time = "raw data"
                     ),
                     
                     timelineLabel("Calculations", color = "navy"),
                     timelineItem(
                       title = "Mean rental price",
                       icon = icon("info"),
                       color = "secondary",
                       HTML(
                         "Mean rental price was calculated per town across studio, one-, two-, three- and four-bedroom houses if available."
                       ),
                       time = "calculation"
                     ),
                     timelineItem(
                       title = "Buyer index",
                       icon = icon("info"),
                       color = "secondary",
                       HTML(
                         "Buyer index is a ratio between a house price in the specific town and 12-month mean rental. Usually, buyer index between 11 and 17 shows a good balance between house price and income. "
                       ),
                       time = "calcution"
                     )
                  )
          )
        )
      ),
      
      ###########################
      #### --- Dashboard tab ----
      ###########################
      tabItem(
        tabName = "dashboard",
        fluidRow(
              box(title = "Choose your town/city", width = 6, height = 150,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    status = status,
                    # options: https://rdrr.io/cran/shinyWidgets/man/pickerOptions.html
                    shinyWidgets::pickerInput("townPicker", label = "Max three options", choices = towns, 
                                        selected = "Carlisle", 
                                        multiple = TRUE, 
                                        options = list(`selected-text-format`= "values", # count, static
                                                        title = "Choose your town of interest", 
                                                        `actions-box` = TRUE, 
                                                        "max-options" = 3, `multiple-separator` = " | ", 
                                                        "max-options-group" = 3, 
                                                        `live-search`=TRUE,
                                                        "max-options-text" = "No more!")),
                    br()
             #       shinyWidgets::actionBttn(inputId = "Go", label = "GET DATA", 
            #                               color = "primary", style = "bordered",
            #                               icon = icon("pen"))
                       
                 ),
              box(width = 6, height = 150,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      status = status,
                      br(),
                      shinyWidgets::prettyCheckboxGroup("housetypePicker", 
                                                    label = "Select number of bedrooms", 
                                                    inline = TRUE, 
                                                    status = status,
                                                    choices = housetypes, 
                                                    selected = housetypes) #  icon = icon("dev")
                      
                  
                      )
        ),
        fluidRow(
            shinydashboard::infoBoxOutput("income_per_household_reactive_ibox"),
            shinydashboard::infoBoxOutput("number_of_households_reactive_ibox"),
            shinydashboard::infoBoxOutput("per_capita_income_reactive_ibox")
          
        ),
      
        
          fluidRow(width = 12, 
                   box(title = "Monthly rental prices",
                       width = 6,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       status = status,
                       plotly::plotlyOutput("rentPlot")),
                   
                   box(title = "Single family home prices",
                       width = 6,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       status = status,
                       plotly::plotlyOutput("pricePlot"))
         
        ),
        
        fluidRow(width = 12, 
                 box(title = "Ratio between home price to rent price per year per town",
                     width = 6,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     status = status,
                     plotly::plotlyOutput("indexPlot")),
                 
                 box(title = "Town population",
                     width = 6,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     status = status,
                     plotly::plotlyOutput("popPlot"))
        ),
         
        
        fluidRow(width = 6, 
                 box(title = "Ratio between home price to rent price per year per town",
                     width = 12,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     status = status,
                     dataTableOutput('indexDT')
        )
      )
      ),
      
      ##################################
      #### ---CORRELATION tab ----
      ###################################
      tabItem(
        tabName = "correlation",
        fluidRow(title = "Look at the overall correlation between different attributes using smoothScatter. 
                 SmoothScatter is basically a scatter plot with a two-dimensional density estimation. 
                 It is useful especially in the case of a lot of observations and for outlier detection.", 
                 width = 12
        ),
        fluidRow(
          shinyWidgets::pickerInput("varX", label = "Choose X variable", choices = vars, selected = vars[1], multiple = FALSE ),
          shinyWidgets::pickerInput("varY", label = "Choose Y variable", choices = vars, selected = vars[2], multiple = FALSE )
        ),
        
        fluidRow(
          box( width = 12, solidHeader = TRUE, title = "Density plot", plotOutput('scatter1')),
          br(),
          box(width = 12, solidHeader = TRUE, plotOutput('ggscatter_plot'))
        )
      ),
      
      ###################################
      ### FUNNEL tab
      ###################################
      tabItem(
        tabName = "filtering",
        fluidRow(title = "This simple tool filters towns in MA based on the user's preset attributes.", 
                 width = 12,
          box(id = "attr_box",  title = "Check house-based attributes:", height = 360,
              width = 6,
              collapsible = TRUE,
              collapsed = FALSE,
              status = status,
              shiny::sliderInput(inputId = "filtering_price_range", label = "Select house price range:", min = min(data$price), max = max(data$price), step = 10000, value = c(300000, 500000)),
              
              shiny::sliderInput(inputId = "filtering_index_range", label = "Select buyer index range:", min = 5, max= max(unique(data$index)), step = 1, value = c(10, 20)),
              
              shinyWidgets::prettyCheckboxGroup("time_year_filter", 
                                                label = "Select year", 
                                                inline = TRUE, 
                                                status = status,
                                                choices = sort(unique(data$year)), 
                                                selected = 2021)
              
              ),
          
          box(id = "attr_box", title = "Check town based attributes:",
              width = 6,  height = 360,
              collapsible = TRUE,
              collapsed = FALSE,
              status = status,
              shiny::sliderInput(inputId = "filtering_popolation_range", label = "Select population range ( from 2019 ) exluding Boston:", min = 100, max = 200000, value = c(5000, 30000)),
              
              shinyWidgets::prettyCheckboxGroup("county_filter", 
                                                label = "Select county",
                                                inline = TRUE, 
                                                status = status,
                                                choices = counties, 
                                                selected = counties),
              
              shinyWidgets::actionBttn(inputId = "Go2", label = "GET LIST", 
                                       color = "primary", style = "bordered",
                                       icon = icon("pen"))
        )
        ),
        br(),
        fluidRow(
          dataTableOutput('filteredData')
        )
      ),
      
      #######################################
      #### Help tab ----    
      ######################################
      tabItem(
        tabName = "app_help",
        
        ##### Application help summary box ----
        box(
          title = "Application help",
          width = 12,
          collapsible = FALSE,
          status = status,
          "
          This app was created by Irina Kalatskaya using the R programming language and shiny R Web framework.
          Please contact the application developers if you have any questions
          about the application or encounter any errors. Please also include
          screenshots of the error(s) you wish to report."
        ),
        
        ##### Contact information boxes ----
        fluidRow(
          bs4Card(
            status = status,
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            boxProfile(
              image = NULL,
              title = "Irina Kalatskaya, PhD",
              subtitle = HTML(
                "Data Scientist, Bioinformatician and Computational Scientist <br/>
                https://www.linkedin.com/in/irina-kalatskaya/<br/>
                <br/>
                 ikalats at gmail.com"),
              bordered = TRUE
            )
          )
        ) # end of fluidRow
        
      )
    )
  )
  
)
