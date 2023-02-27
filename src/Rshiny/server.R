
source("global.R")
source("functions.R")

server <- function(input, output, session) {
  
  ##############################################################
  ##### Intro tab
  ##############################################################
  output$map <- renderLeaflet({
    # m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 8)
    # m %>% addTiles()
    states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
    # boston is a center
    # m <- leaflet(states) %>% setView(lng = -71.0589, lat = 42.3601, zoom = 8)
    m <- leaflet(states) %>% setView(-72.015193, 42.414006,  zoom = 8)
    # %>%
    # setView(-96, 37.8, 4) %>%
    # addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
    #  accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    m %>% addPolygons(opacity = 0.3, fillColor = mycolors[1], fill = TRUE, weight = 4, color = mycolors[5]) %>% addTiles()
  })
  
  ###############################################################
  ##### Dashboard tab
  ###############################################################
  
  observe({
    updatePickerInput(session, inputId = "townPicker", choices = towns, selected = "Carlisle")
  })
  
  observe({
    updateCheckboxGroupInput(session, inputId = "housetypePicker", choices = housetypes, selected = housetypes, inline = TRUE)
  })
  
  data_per_town <- reactive({
    DATA %>% filter(town %in% !!input$townPicker)
  })
  
  # Dashboard: monthly rent
  selected_rent <- reactive({
    data_per_town() %>% filter(grepl("Br", cat)) %>% dplyr::filter(cat %in% !!input$housetypePicker)
  })
  
  # Dashboard: single house price
  selected_price <- reactive( {
    data_per_town() %>% filter(cat == "single_family_home_price")
  })
  
  income_per_household_reactive <- reactive({
    data_per_town() %>% filter(cat == "median_household_income")
  })
  
  ## Dashboard: Town population:
  selected_pop <- reactive( {
    data_per_town() %>% filter(cat == "population")
  })
  
  #####  Dashboard: Ratio between average house and average annual rent
  selected_index <- reactive( {
    selected_rent() %>% group_by(town, year) %>% dplyr::summarise(median = median(value), .groups = "drop") %>% 
      dplyr::inner_join(selected_price(), by=c('year', 'town')) %>% dplyr::mutate(index = value/(12*median))
  })
  
  output$income_per_household_reactive_ibox <- shinydashboard::renderInfoBox({
    income = data_per_town() %>% filter(cat == "median_household_income")
    shinydashboard::infoBox(
      "The median household income is ",
      value =  income$value,
      icon = shiny::icon("money-bill"),
      color = "navy", href = NULL, fill = FALSE
    )
  })
  
  output$percent_of_homeowners_reactive_ibox <- shinydashboard::renderInfoBox({
    home_owner = data_per_town() %>% filter(cat == "percent_of_homeowners")
    shinydashboard::infoBox(
      "The percent of homeowners in 2011 was ",
      value = home_owner$value,
      icon = shiny::icon("house"),
      color = "navy", href = NULL, fill = FALSE
    )
  })
  
  output$number_of_households_reactive_ibox <- shinydashboard::renderInfoBox({
    income = data_per_town() %>% filter(cat == "number_of_households")
    shinydashboard::infoBox(
      "Total Number of Households is ",
      value = income$value,
      # subtitle = " in this town.",
      icon = shiny::icon("house"),
      color = "orange", href = NULL, 
      fill = FALSE
    )
  })
  
  output$per_capita_income_reactive_ibox <- shinydashboard::renderInfoBox({
    income = data_per_town() %>% filter(cat == "per_capita_income")
    shinydashboard::infoBox(
      "Per capita income in 2019 is ",
      value = income$value,
      # value = paste(income$value, 12000, sep="\t"),
      icon = shiny::icon("diagram-project"),
      color = "green", href = NULL, fill = FALSE
    )
  })
  
  # Dashboard: price plot
  output$pricePlot <- renderPlotly({
    D = selected_price()
    if(nrow(D)==0){
      return(plotEmptyPlotly("No data for this town is available.", ""))
    }
    D %>% plot_ly(x = ~year, y=~value, color = ~town, colors = mycolors[1], type="scatter", mode="markers+lines", symbols = c('x'), marker = list(size = 10),
                                 text = ~paste(" town", town, "\nHouse price", value), hoverinfo=c("text"), 
                                 opacity=0.7, marker = list(size = 9)) %>% layout(title = paste0("Average property cost in ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "House price, $"))
  })
  
  # Dashboard: population plot
  output$popPlot <- renderPlotly({
    D = selected_pop()
    if(nrow(D)==0){
      return(plotEmptyPlotly("No data for this town is available.", ""))
    }
    D %>% plot_ly(x = ~year, y=~value, color = ~town, type="scatter", mode="markers+lines", marker = list(size = 10), colors = mycolors[2]) %>% layout(title = paste0("Population of ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "Number of residents"))
  })

  ##### Dashboard: Rent plot
  output$rentPlot <- renderPlotly({
    D = selected_rent()
    if(nrow(D)==0){
      return(plotEmptyPlotly("No data for this town is available.", ""))
    }
    D %>% plot_ly(x = ~year, y = ~value, symbol = ~town, color = ~cat, colors = mycolors[3:6], type="scatter",  
                                mode="markers+lines", symbols = c('circle','x','o'), 
                                text = ~paste(" Town", town, "\nType", cat, "\nPrice", value), hoverinfo=c("text"), 
                                opacity=0.7, marker = list(size = 9)) %>% layout(title = paste0("Rentals in ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "Price per month, $"))
  })
  
  ### Dashboard: Index Buyer plot  
  output$indexPlot <- renderPlotly({
    D = selected_index()
    if(nrow(D)==0){
      return(plotEmptyPlotly("No data for this town is available.", ""))
    }
    D %>% plot_ly(x = ~year, y= ~index, color = ~town, colors = mycolors[7], type="scatter", marker = list(size = 12), mode="markers+lines") %>% layout(title = paste0("Buyer index in ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "Index"))
  })
  

  ##### Dashboard: Merge all data together for DT 
  output$indexDT <- renderDataTable({
    rent_wide = selected_rent() %>% tidyr::pivot_wider(id_cols = c("year", "town"), names_from = "cat", values_from = "value")
    data_per_town = selected_index() %>% full_join(rent_wide, by=c("town", "year")) %>% dplyr::select(-median, -cat) %>% dplyr::rename("price" = "value") %>% full_join(selected_pop(), by=c("year", "town")) %>% dplyr::select(-cat) %>% dplyr::rename("population" = "value")
    data_per_town$year = as.integer(data_per_town$year)
    data_per_town %>% arrange(-year)
  })
  
  ###############################################################
  ##### DATA per county
  ###############################################################
  
  countyStats <- reactive({
    temp1 = DATA %>% filter(cat == "population" & year == 2019) %>% group_by(county, year) %>% summarise(population = sum(value), .groups="drop")
    temp2 = data.frame(county_stats)
    temp1 %>% full_join(temp2, by="county") %>% dplyr::select(-year, -X)
  })
  
  
  updateSelectizeInput(session, inputId = "selected_county_v2",
                       server = TRUE,
                       choices = county_stats,
                       selected = c("Middlesex", "Essex" ),
                       options = list(
                         valueField = 'county',
                         labelField = 'county',
                         searchField = c('county', 'county_seat'),
                         render = selectize_render_query_function,
                         placeholder = glue("{nrow(county_stats)} choices")
                       )
  )
  
  # County: data parsing
  data_per_county <- reactive({
    D = DATA_FULL  %>% tidyr::pivot_wider(id_cols = c("town", "year", "county"), names_from = "cat", values_from = "value") %>% dplyr::filter(county %in% !!input$selected_county_v2)
    if(!input$isBostonIncluded) {
      D = D %>% filter(town != "Boston")
    }
    D
  })
  
  # row1
  output$county_population <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "population", y="county", "county", pTitle=paste('Population by county in ', y, sep="\t"))
  })
  
  output$county_number_of_households <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "number_of_households", y="county", "county", pTitle = paste('Number of households by county in ', y, sep="\t"))
  })
  
  output$county_percent_of_homeowners <- renderPlotly({
    y = "2011"
    plotBox(data_per_county() %>% filter(year == y), x = "percent_of_homeowners", y="county", "county", pTitle = paste('Percent of homeowners by county in ', y, sep="\t"))
  })
  #row2
  output$county_single_family_home_price <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "single_family_home_price", y="county", "county", pTitle = paste('Single family home price by county in ', y, sep="\t"))
  })
  
  output$county_median_family_income <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "median_family_income", y="county", "county", pTitle = paste('Median family income price by county in ', y, sep="\t"))
  })
  
  #row3
  output$county_Br4_rent <- renderPlotly({
    y = "2021"
    plotBox(data_per_county() %>% filter(year == y), x = "Br4_rent", y="county", "county", pTitle = paste('Br4_rent by county in ', y, sep="\t"))
  })
  
  output$county_Br3_rent <- renderPlotly({
    y = "2021"
    plotBox(data_per_county() %>% filter(year == y), x = "Br3_rent", y="county", "county", pTitle = paste('Br3_rent by county in ', y, sep="\t"))
  })
  
  # Row4
  output$county_total_school_budget <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "total_school_budget", y="county", "county", pTitle = paste('Total school budget ', y, sep="\t"))
  })
  
  output$county_average_teacher_salary <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "average_teacher_salary", y="county", "county", pTitle = paste('Average teacher salary ', y, sep="\t"))
  })
  
  output$county_number_of_school_fte <- renderPlotly({
    y = "2019"
    plotBox(data_per_county() %>% filter(year == y), x = "number_of_school_fte", y="county", "county", pTitle = paste('Number of school FTE ', y, sep="\t"))
  })
  
  ###############################################################
  ##### Funnel tab
  ###############################################################
  
  observe({
    updateCheckboxGroupInput(session, inputId = "county_filter", choices = counties, selected = counties, inline = TRUE)
  })
  
  observe({
    updateSliderInput(session, inputId = "filtering_index_range", min = 10)
  })
  
  observe({
    updateSliderInput(session, inputId = "filtering_popolation_range", min = min_population)
  })
  
  observe({
    updateSliderInput(session, inputId = "filtering_price_range", min = 300000)
  })
  
  observe({
    updateAwesomeRadio(session, inputId = "time_year_filter", choices = years, selected = 2021, inline = TRUE)
  })

  ## # https://stackoverflow.com/questions/33401788/dplyr-using-mutate-like-rowmeans
  data_selected <- eventReactive(input$Go2, {
    # from initial df parse towns in the specific counties
    selected = DATA  %>% filter(county %in% !!input$county_filter)
    # calculate mean rental prices per town per year from slim_format
    mean_rent_df = selected %>% filter(cat %in% c("Br0_rent", "Br1_rent", "Br2_rent", "Br3_rent", "Br4_rent")) %>% na.omit() %>% dplyr::group_by(town, year) %>% dplyr::summarise(mean_annual_rent = 12*mean(value), .groups = "drop")
    # convert slim into wide format by town, year and county and join mean_rent_df
    selected_wide = selected %>% tidyr::pivot_wider(id_cols = c("town", "year", "county"), names_from = "cat", values_from = "value") %>% dplyr::left_join(mean_rent_df, by=c("town", "year"))
    # parse by house price range
    selected_wide = selected_wide %>% filter(single_family_home_price >= !!input$filtering_price_range[1] & single_family_home_price <= !!input$filtering_price_range[2])
    
    # from initial collection select towns with specific population and pull them into vector
    towns_with_selected_population = selected_wide %>% filter(year == 2019) %>% filter( population >= !!input$filtering_popolation_range[1] & population <= !!input$filtering_popolation_range[2]) %>% pull('town')
    # selected towns only with specified population
    selected_wide = selected_wide %>% filter(town %in% towns_with_selected_population)  
    # select specific year of data collection
    selected_wide = selected_wide %>% dplyr::filter(year %in% !!input$time_year_filter)
    selected_wide$mean_annual_rent = as.double(selected_wide$mean_annual_rent)
    # selected_wide = selected_wide %>% filter_at(vars(Br0_rent), all_vars(!is.na(.)))  %>% dplyr::mutate(mean_annual_rent = 12*mean(c(Br0_rent, Br1_rent,  Br2_rent, Br3_rent, Br4_rent)))
    
    # calculate index and parse by index
    selected_wide = selected_wide %>% dplyr::mutate(index = single_family_home_price/mean_annual_rent) %>% filter(index >= !!input$filtering_index_range[1] & index <= !!input$filtering_index_range[2]) 
    selected_wide %>% dplyr::select(town, year, county, index, single_family_home_price, mean_annual_rent, Br0_rent, Br1_rent,  Br2_rent, Br3_rent, Br4_rent, everything())
    
  })
  
  output$filteredData <- renderDataTable({
    data_selected() %>% arrange(index)  }, extensions = c('Responsive'), 
    options = list(responsive = TRUE), rownames = FALSE)
  
  
  ###############################################################
  ##### correlation tab
  ###############################################################
  
  output$scatter1 <- renderPlot({
    DATA = DATA %>% dplyr::filter(town != "Boston")
    x = DATA %>% dplyr::filter(cat == !!input$varX)
    y = DATA %>% dplyr::filter(cat == !!input$varY)
    temp = x %>% inner_join(y, by=c("town", "year"))
    smoothScatter(temp$value.x, temp$value.y, xlab = input$varX, ylab = input$varY)
  })
  
  ggscatter <- reactive({
    validate( 
      need(input$varX != "", "Please choose variable for X axes."),
      need(input$varY != "", "Please choose variable for Y axes.")
    )
    DATA = DATA %>% dplyr::filter(town != "Boston")
    x = DATA %>% dplyr::filter(cat == !!input$varX)
    y = DATA %>% dplyr::filter(cat == !!input$varY)
    temp = x %>% inner_join(y, by=c("town", "year"))
    
    
#    if(input$colorByTreatmentButton == "Color") {
      p = ggpubr::ggscatter(temp, x = 'value.x', y = 'value.y', conf.int = TRUE, combine = FALSE,   add.params = list(color = "blue", fill = "lightgray")) 
      p = p + geom_smooth(formula = y ~ x, method = "lm", color="#4d9219") + stat_cor(method = 'spearman') + theme_bw() + theme(legend.position="bottom", legend.title = element_blank())
#    }
#    else {
#      p = ggscatter(reactiveDataCibersort(), x = input$cibertype, y= input$flowtype,  show.legend = FALSE, conf.int = TRUE, combine = FALSE, xlab = tolower(input$cibertype), add.params = list(color = "blue", fill = "lightgray")) 
#      p = p + geom_smooth(formula = y ~ x, method = "lm", color="#4d9219") + stat_cor(method = method()) + color() + theme_bw()
#    }
    p
  })
  
  # Render scatterplot for cibesort
  output$ggscatter_plot <- renderPlot(res = 80, {
    ggscatter()
  })
  

}