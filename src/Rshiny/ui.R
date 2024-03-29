

ui <- dashboardPage(
  
  fullscreen = TRUE,
  ## Header ----  
  header = dashboardHeader(
    title = "RENTAL IN MASS",
    status = status,
    sidebarIcon = shiny::icon("rectangle-list", verify_fa = FALSE),
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
               icon = icon("info", verify_fa = FALSE),
               selected = TRUE
      ),
      
      menuItem("Help",
               tabName = "app_help",
               icon = icon("question-circle", verify_fa = FALSE)
      ),
      
      sidebarHeader( title = " Data "),
      
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("columns", verify_fa = FALSE), # table
               selected = FALSE
      ),
      
     menuItem("County data",
               tabName = "county",
               icon = icon("chart-line", verify_fa = FALSE),
               selected = FALSE
      ),
      
      menuItem("Correlation",
               tabName = "correlation",
               icon = icon("image", verify_fa = FALSE),
               selected = FALSE
      ),
      
      menuItem("Funnel",
               tabName = "filtering",
               icon = icon("filter", verify_fa = FALSE),
               selected = FALSE
      )

#      menuItem("Linear Regresssion",
#               tabName = "lm",
#               icon = icon("drafting-compass", verify_fa = FALSE),
#               selected = FALSE
#      )
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
          fluidRow(
            box(width = 4,
                title = "Application summary",
                status = status, height = 450,
                h5(
                  "This app consolidates Rent prices in different towns and cities in Massachusetts, house prices and other relevant information. 
                  The main objective of the app is to prioritize towns in MA where house price and rent relationship is the most favorable for a potential investor."
                ),
                h5("In summary, there are 14 Counties, with 39 cities and 312 towns in Massachusetts. Not all data resources contain information for all towns.")
            ),
            box(width = 8,
                title = "Massachusetts map", 
                status = status, height = 450,
                leafletOutput("map")
            )
          ),
          fluidRow(width = 12, 
                   timelineBlock(
                     width = 12,
                     reversed = TRUE,
                     timelineLabel("  Data Sources  ", color = "primary"),
                     timelineItem(
                       title = "Rent information",
                       icon = icon("cog", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Average Fair Market Rent Prices information was scraped from https://www.rentdata.org/states/massachusetts/ from 2006 to 2022. 
                         Massachusetts has the 3rd highest rent in the country out of 56 states and territories.  "
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Population",
                       icon = icon("sign-out-alt", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Annual Estimates of the Resident Population: April 1, 2010 to July 1, 2019. 
                         U.S. Census Bureau Population Division. May 21, 2020. The data is available for 351 towns in MA. "
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Home prices",
                       icon = icon("sign-out-alt", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Home prices in MA were scraped from Boston Magazine web portal: https://www.bostonmagazine.com/property/single-family-home-price-chart-2021/.
                         SOURCES: Boston neighborhood and town median home prices, sales volumes, and days on market provided by the Massachusetts Association of Realtors (marealtor.com) and MLS Property Information Network (mlspin.com)."
                         
                       ),
                       time = "raw data"
                     ),
                     timelineItem(
                       title = "Percent of housing owner-occupied",
                       icon = icon("info", verify_fa = FALSE),
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
                       icon = icon("info", verify_fa = FALSE),
                       color = "secondary",
                      
                       HTML(
                         "Massachusetts is the second wealthiest state in the United States of America, with a median household income of $77,378 (as of 2019).
                         The data was retrived from https://en.wikipedia.org/wiki/List_of_Massachusetts_locations_by_per_capita_income.
                         "
                       ),
                       time = "raw data"
                     ),
                     
                     timelineLabel("  Tab functions  ", color = "olive"),
                     timelineItem(
                       title = "Dashboard",
                       icon = icon("columns", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Dashboard tab provides a summary of all above described information in one page per town."
                       ),
                       time = "app structure"
                     ),
                     timelineItem(
                       title = "County data",
                       icon = icon("chart-line", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Here you will find data arranged by MA counties. Data is presented in the form of interactive boxplots (built using plotly) for each data type descripbed above per each county."
                       ),
                       time = "app structure"
                     ),
                     timelineItem(
                       title = "Funnel",
                       color = "secondary",
                       HTML(
                         "It is the most interesting tab so far. The app filters all towns in MA based on user's provided specifications, like the size of the town, average house price and index buyer. There is an opportunity to focus your search in the specific counties."
                       ),
                       time = "app structure"
                     ),
                     
                     # calculation tabs
                     timelineLabel(" Added calculations  ", color = "navy"),
                     timelineItem(
                       title = "Annual average rental price",
                       icon = icon("info", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Annual average rental prices were calculated as a mean of studio, one-, two-, three- and four-bedroom houses multipled by 12 months in each town per each year if data was available."
                       ),
                       time = "calculation"
                     ),
                     timelineItem(
                       title = "Buyer index",
                       icon = icon("shopping-cart", verify_fa = FALSE),
                       color = "secondary",
                       HTML(
                         "Buyer index is a ratio between a house price in the specific town and 12-month mean rental. Usually, buyer index between 11 and 17 shows a good balance between house price and income. "
                       ),
                       time = "calculation"
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
              box(title = "Choose your town/city", 
                    width = 6, height = 150,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    status = status,
                    # options: https://rdrr.io/cran/shinyWidgets/man/pickerOptions.html
                    shinyWidgets::pickerInput("townPicker", label = "List of all towns and cities in MA will be populated below", choices = towns, 
                                        selected = "Carlisle", 
                                        multiple = FALSE, 
                                        options = list(`selected-text-format`= "values", # count, static
                                                        title = "Choose one town of interest", 
                                                        `actions-box` = TRUE, 
                                                        "max-options" = 3, 
                                                        `multiple-separator` = " | ", 
                                                        "max-options-group" = 3, 
                                                        `live-search`=TRUE,
                                                        "max-options-text" = "No more!")),
                    br()
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
            shinydashboard::infoBoxOutput("income_per_household_reactive_ibox", width = 3),
            shinydashboard::infoBoxOutput("number_of_households_reactive_ibox", width = 3),
            shinydashboard::infoBoxOutput("per_capita_income_reactive_ibox", width = 3),
            shinydashboard::infoBoxOutput("percent_of_homeowners_reactive_ibox", width = 3)
          
        ),
      
        
          fluidRow(width = 12, 
                   box(title = "Monthly rental prices",
                       width = 6,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       status = status,
                       plotly::plotlyOutput("rentPlot") %>% withSpinner(type = 8, color = spinner.colour)),
                   
                   box(title = "Single family home prices",
                       width = 6,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       status = status,
                       plotly::plotlyOutput("pricePlot") %>% withSpinner(type = 8, color = spinner.colour))
        ),
        
        fluidRow(width = 12, 
                 box(title = "Ratio between home price to rent price per year per town",
                     width = 6,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     status = status,
                     plotly::plotlyOutput("indexPlot") %>% withSpinner(type = 8, color = spinner.colour)),
                 
                 box(title = "Town population",
                     width = 6,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     status = status,
                     plotly::plotlyOutput("popPlot") %>% withSpinner(type = 8, color = spinner.colour))
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
      ###########################################
      #### --- Linear Regression ----
      ###########################################
    
      tabItem(tabName = "lm",
              fluidRow(title = "", width = 12),
              # https://stackoverflow.com/questions/66688910/shinydahsboardplus-how-to-add-box-without-a-title
              fluidRow(
                box(width = 4, id = 'foo', height = 150, title = NULL, headerBorder = FALSE,
                    shinyWidgets::pickerInput("lmY", label = "Choose Y variable", choices = vars, selected = vars[1], multiple = FALSE)
                    ),
                box(width = 4, id = 'foo',  height = 150, title = NULL, headerBorder = FALSE,
                    shinyWidgets::pickerInput("lmX", label = "Choose independept X variables for your linear model:", choices = vars, selected = vars[2], multiple = TRUE )
                    ),
                box(width = 4, id = 'foo', height = 150, title = NULL, headerBorder = FALSE,
                  shinyWidgets::pickerInput("selected_county", label = "Choose counties for modeling", choices = counties, selected = counties, multiple = TRUE)
                ),
                tags$head(tags$style('#foo .box-header{ display: none}'))  # target the box header of foo
              ),
              fluidPage(box = 4, plotOutput("residual_plot")),
              fluidPage(box = 4, plotOutput("residual_plot2"))
              
      ),
      
      
      #########################################
      #### ---COUNTY tab ----
      #########################################
      tabItem(tabName = "county",
              
              fluidRow(
                box(width = 10, title = "Choose your favorite counties and click 'plus' sign on rectangles below.", height = 180, solidHeader = TRUE,
                    selectizeInput("selected_county_v2", label = "Choose counties for visualization and comparison",
                                   choices = NULL, multiple = TRUE),
                    br(),
                    prettyCheckbox(" Should Boston be included? ", status = "primary", width = '100px', inputId = "isBostonIncluded", value = FALSE, inline = FALSE)
                ),
                br()
              ),
              br(),
              
              fluidRow(
                box( width = 4, solidHeader = TRUE, plotlyOutput('county_population'), collapsed = F, collapsible = TRUE, title = "POPULALATION"),
                br(),
                box( width = 4, solidHeader = TRUE, plotlyOutput('county_number_of_households'), collapsed = F, collapsible = TRUE, title = "HOUSEHOLDS"),
                br(),
                box( width = 4, solidHeader = TRUE, plotlyOutput('county_percent_of_homeowners'), collapsed = F, collapsible = TRUE, title = "HOMEOWNERS"),
                br()
              ),
              fluidRow(
                box(width = 6, solidHeader = TRUE, plotlyOutput('county_single_family_home_price'), collapsed = TRUE, collapsible = TRUE, title = "HOUSE COST"),
                br(),
                box(width = 6, solidHeader = TRUE, plotlyOutput('county_median_family_income'), collapsed = TRUE, collapsible = TRUE, title = "INCOME"),
                br()
              ),
              fluidRow(
                box(width = 6, solidHeader = TRUE, plotlyOutput('county_Br4_rent'), collapsed = TRUE, collapsible = TRUE, title = "RENT 4-BR"),
                br(),
                box(width = 6, solidHeader = TRUE, plotlyOutput('county_Br3_rent'), collapsed = TRUE, collapsible = TRUE, title = "RENT 3-BR"),
                br()
              ),
              fluidRow(
                box(width = 4, solidHeader = TRUE, plotlyOutput('county_average_teacher_salary'), collapsed = TRUE, collapsible = TRUE, title = "TEACHERS"),
                br(),
                box(width = 4, solidHeader = TRUE, plotlyOutput('county_number_of_school_fte'), collapsed = TRUE, collapsible = TRUE, title = "SCHOOL FTE"),
                br(),
                box(width = 4, solidHeader = TRUE, plotlyOutput('county_total_school_budget'), collapsed = TRUE, collapsible = TRUE, title = "SCHOOL BUDGET"),
                br()
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
          shinyWidgets::pickerInput("varX", label = "Choose X variable", 
                                    choices = vars, selected = vars[1], multiple = FALSE),
          shinyWidgets::pickerInput("varY", label = "Choose Y variable", 
                                    choices = vars, selected = vars[2], multiple = FALSE )
        ),
        
        fluidRow(
          box( width = 12, solidHeader = TRUE, title = "Density plot", plotOutput('scatter1')),
          br(),
          box(width = 12, solidHeader = TRUE, plotOutput('ggscatter_plot'))
        )
      ),
      
      ######################################################
      ### FUNNEL tab
      ######################################################
      tabItem(
        tabName = "filtering",
        fluidRow(title = "This simple tool filters towns in MA based on the user's preset attributes.", 
                 width = 12,
          box(id = "attr_box",  title = "Check house-based attributes:", height = 360,
              width = 6,
              collapsible = TRUE,
              collapsed = FALSE,
              status = status,
              shiny::sliderInput(inputId = "filtering_price_range", 
                                 label = "Select house price range:", 
                                 min = min_house_price, max = max_house_price, 
                                 step = 10000, value = c(300000, 500000)),
              
              shiny::sliderInput(inputId = "filtering_index_range", 
                                  label = "Select buyer index range:", 
                                  min = 5, max = 50, step = 1, 
                                  value = c(10, 20)),
              
              shinyWidgets::awesomeRadio("time_year_filter", 
                                  label = "Select year", 
                                  inline = TRUE, 
                                  status = status,
                                  choices = years,
                                  checkbox = TRUE,
                                  selected = 2021)
              ),
          
          box(id = "attr_box", title = "Check town based attributes:",
              width = 6,  height = 360,
              collapsible = TRUE,
              collapsed = FALSE,
              status = status,
              shiny::sliderInput(inputId = "filtering_popolation_range", 
                                 label = "Select population range based on 2019 (Boston is excluded):", 
                                 min = min_population, max = max_population, value = c(5000, 30000)),
              
              shinyWidgets::prettyCheckboxGroup("county_filter", 
                                                label = "Select county",
                                                inline = TRUE, 
                                                status = status,
                                                choices = counties, 
                                                selected = counties),
              
              br(),
              shinyWidgets::actionBttn(inputId = "Go2", label = "GET LIST", 
                                       color = "primary", style = "bordered",
                                       icon = icon("pen", verify_fa = FALSE))
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
        fluidRow(
          bs4Card(
            status = status,
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            boxProfile(
              # image = "https://www.dropbox.com/s/0cfjt6e8053dbkp/Screenshot%202023-02-27%20at%208.10.20%20PM.png",
              # image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
              title = "Developer: Irina Kalatskaya, PhD",
              
              subtitle = HTML(
                "Data Scientist, Bioinformatician and Computational Scientist <br/>",
              ),
              boxProfileItem(
                title = "LinkedIn profile",
                description = "https://www.linkedin.com/in/irina-kalatskaya/"
              ),
              boxProfileItem(
                title = "E-mail",
                description = "ikalats at gmail.com"
              ),
              bordered = TRUE
            )
          )
        ), # end of fluidRow
        
        box(
          title = "Application help",
          width = 12,
          collapsible = FALSE,
          status = status,
          " This app was created using the R programming language and Shiny R Web framework.
          Please contact the application developers if you have any questions
          about the application or encounter any errors. Please also include
          screenshots of the error(s) you wish to report."
        ),
        
        box(
          title = "Data download",
          width = 12,
          collapsible = FALSE,
          status = status,
          "A copy of the consoldated and clean data could be downloaded from ", tags$a(href = "https://www.kaggle.com/datasets/ikalats/massachusetts-house-pricing", "my Kaggle account.")
        ),
        
        box(
          title = "Source code",
          width = 12,
          collapsible = FALSE,
          status = status,
          "Source code for the app, code for data retrival and cleanup could be found ",
          tags$a(href="https://github.com/ikalatskaya/massachusetts_rental_property.git", " in my GitHub account.")
        )
        
        
      )
    )
  )
  
)
