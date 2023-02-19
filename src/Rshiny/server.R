
source("global.R")


server <- function(input, output, session) {
  
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
  
  selected_rent <- reactive({
    # monthly rent
    data_per_town() %>% filter(grepl("Br", cat)) %>% dplyr::filter(cat %in% !!input$housetypePicker)
  })
  
  selected_price <- reactive( {
    data_per_town() %>% filter(cat == "single_family_house_price")
  })
  
  income_per_household_reactive <- reactive({
    data_per_town() %>% filter(cat == "median_household_income")
  })
  
  selected_pop <- reactive( {
    ##### Town population:
    data_per_town() %>% filter(cat == "population")
  })
  
  selected_index <- reactive( {
    ##### Ratio between average house and average annual rent
    selected_rent() %>% group_by(town, year) %>% dplyr::summarise(median = median(value)) %>% 
      dplyr::inner_join(selected_price(), by=c('year', 'town')) %>% dplyr::mutate(index = value/(12*median))
  })
  
  output$income_per_household_reactive_ibox <- shinydashboard::renderInfoBox({
    income = data_per_town() %>% filter(cat == "median_household_income")
    shinydashboard::infoBox(
      "The median household income is ",
      # value = 12000,
      value = income$value,
      icon = shiny::icon("money-bill"),
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
      # value = 12000,
      value = income$value,
      icon = shiny::icon("diagram-project"),
      color = "green", href = NULL, fill = FALSE
    )
  })
  
  output$pricePlot <- renderPlotly({
    selected_price() %>% plot_ly(x = ~year, y=~value, color = ~town, type="scatter", mode="markers+lines", symbols = c('circle','x','o'), 
                                 text = ~paste(" town", town, "\nHouse price", value), hoverinfo=c("text"), 
                                 opacity=0.7, marker = list(size = 9)) %>% layout(title = paste0("Property cost In ", as.character(input$townPicker)),
                                                                                  xaxis = list(title = "Time, years"), yaxis = list(title = "House price, $"))
    
  })
  

  
  output$popPlot <- renderPlotly({
    selected_pop() %>% plot_ly(x = ~year, y=~value, color = ~town, type="scatter", mode="markers+lines") %>% layout(title = paste0("Population of ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "Number of residents"))
  })
  
  ##### Rent plot
  output$rentPlot <- renderPlotly({
    selected_rent() %>% plot_ly(x = ~year, y=~value, symbol = ~town, color = ~cat, type="scatter",  
                                mode="markers+lines", symbols = c('circle','x','o'), 
                                text = ~paste(" town", town, "\nType", cat, "\nPrice", value), hoverinfo=c("text"), 
                                opacity=0.7, marker = list(size = 9)) %>% layout(title = paste0("Rentals in ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "Price per month, $"))
  })
  
  output$indexPlot <- renderPlotly({
    selected_index() %>% plot_ly(x = ~year, y= ~index, color = ~town, type="scatter", mode="markers+lines") %>% layout(title = paste0("Buyer index in ", as.character(input$townPicker)), xaxis = list(title = "Time, years"), yaxis = list(title = "Index"))
  })
  
  ################################
  ##### merge all data together for DT 
  output$indexDT <- renderDataTable({
    rent_wide = selected_rent() %>% tidyr::pivot_wider(id_cols = c("year", "town"), names_from = "cat", values_from = "value")
    data_per_town = selected_index() %>% full_join(rent_wide, by=c("town", "year")) %>% dplyr::select(-median, -cat) %>% dplyr::rename("price" = "value") %>% full_join(selected_pop(), by=c("year", "town")) %>% dplyr::select(-cat) %>% dplyr::rename("population" = "value")
    data_per_town$year = as.integer(data_per_town$year)
    data_per_town %>% arrange(-year)
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

  
  data_selected <- eventReactive(input$Go2, {

    selected = DATA  %>% filter(county %in% !!input$county_filter)
    selected_wide = selected %>% tidyr::pivot_wider(id_cols = c("town", "year", "county"), names_from = "cat", values_from = "value")
    
    selected_wide = selected_wide %>% filter(single_family_house_price >= !!input$filtering_price_range[1] & single_family_house_price <= !!input$filtering_price_range[2])
    towns_with_selected_population = selected_wide %>% filter(year == 2019) %>% filter( population >= !!input$filtering_popolation_range[1] & population <= !!input$filtering_popolation_range[2]) %>% pull('town')
    selected_wide = selected_wide %>% dplyr::filter(year %in% !!input$time_year_filter) %>% filter(town %in% towns_with_selected_population) 
    selected_wide = selected_wide %>% filter_at(vars(Br0_rent), all_vars(!is.na(.))) %>% mutate(mean_annual_rent = 12*mean(c(Br0_rent, Br1_rent,  Br2_rent, Br3_rent, Br4_rent )))
    selected_wide = selected_wide %>% mutate(index = single_family_house_price/mean_annual_rent) %>% filter(index >= !!input$filtering_index_range[1] & index <= !!input$filtering_index_range[2]) 
    selected_wide %>% dplyr::select(town, year, county, single_family_house_price,mean_annual_rent, index, Br0_rent, Br1_rent,  Br2_rent, Br3_rent, Br4_rent, everything())
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
  
  ggscatter <- reactive( {
    
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