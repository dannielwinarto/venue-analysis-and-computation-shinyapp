## FS live Project

# rm(list = ls())
# setwd("~/Documents/Project2")

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(rsconnect)
library(plotly)


# rsconnect::setAccountInfo(name='dannielwinarto', token='9AFCF9650ABEB7B9E218CED9C69ADACC', secret='Xol/bJxKacqL0ocFe2R5YDG2tBcYC8Ct+6OtbUq/')
# rsconnect::deployApp('~/Documents/Project2/publish_test')
# library(devtools)  
# install_github("ropensci/plotly")

data = fread("fslive_project2.csv",
             colClasses = c("integer", "factor", "factor", "factor",
                            "integer", "factor", "character", "character", "integer"))
data = data %>% filter(tickets >= 0) # delete the observation of refunded ticket

data = as.data.table(data)

data$event_date = ymd(data$event_date)
data$order_date = ymd(data$order_date)

setkeyv(data, c("venue_name", "order_date" ) )

data = data[complete.cases(data),] # removing obs with NA

data.new = data %>% # adding variable which is the combination of venue name, city, and state
  mutate(venue_name_city_state = paste(venue_name, venue_city, venue_state, sep = "->"))

# cleaning the typo and input inconsistency
name.change = data.frame(old_name =c("Club Red->Phoenix->CA",
                                     "Columbia City Theater->Seattle->CA",
                                     "Drafthouse Comedy Theater->Washington->DC",
                                     "Fine arts Theater->Boston->CA",
                                     "Granada->Dallas->TX",
                                     "Helium->Philadelphia->PA",
                                     "Helium Comedy Club->Philadelphia->CA",
                                     "Helium Comedy Club->Philaldelphia->PA",
                                     "Helium Comedy Club->Portland->CA",
                                     "iPlay->Freehold->CA",
                                     "Joke Joint Comedy Club->Minneapolis->MN",
                                     "Lincoln Hall->Chicago->CA",
                                     "The Opera House->Toronto->ON",
                                     "Quixotic World Theatre->Dallas->TX",
                                     "Belasco->LA->CA",
                                     "Ridleys Comedy Club->Detroit->CA",
                                     "State Theater->Washington->DC"),
                         new_name = c("Club Red->Phoenix->AZ",
                                      "Columbia City Theatre->Seattle->WA",
                                      "Drafthous Comedy Club->Washington DC->DC",
                                      "Fine Arts Theater->Boston->MA",
                                      "Granada Theater->Dallas->TX",
                                      "Helium Comedy Club->Philadelphia->PA",
                                      "Helium Comedy Club->Philadelphia->PA",
                                      "Helium Comedy Club->Philadelphia->PA",
                                      "Helium Comedy Club->Portland->OR",
                                      "iPlay America->Freehold->NJ",
                                      "Joke Joint->Minneapolis->MN",
                                      "Lincoln Hall->Chicago->IL",
                                      "Opera House->Toronto->Canada",
                                      "Quixotic World->Dallas->TX",
                                      "Belasco->Los Angeles->CA",
                                      "Ridleys Comedy Club->Detroit->MI",
                                      "State Theater->Washington DC->DC"))

name.change$old_name = as.character(name.change$old_name)
name.change$new_name = as.character(name.change$new_name)

for(i in  seq(1 :dim(name.change)[1])){
  data.new$venue_name_city_state = str_replace_all(data.new$venue_name_city_state,
                                                   name.change$old_name[i],
                                                   name.change$new_name[i] )
}

data.new = data.new %>%
  group_by(venue_name_city_state, event_name) %>%
  mutate(cumsum_tix = cumsum(tickets)) %>% # adding cumulative sum of ticketsales
  mutate(fill_percentage = round((100*cumsum_tix/venue_capacity),1)) # adding the filling % of the stadium

start_sale =  data.new %>%
  group_by(event_key) %>%
  summarise(firstDaySale = min(order_date)) # finding the  first day of sale

data.new = data.new %>% # joining with start_sale to obtain the day count (day of tix sold, count from the start of the sale)
  left_join(start_sale, by = "event_key") %>%
  mutate(dayCount = as.numeric(order_date - firstDaySale + 1))

data.new = data.new %>% # since some of the days there are no tix sold, we need to day_consecutive_order to make sales count day to be consecutive
  group_by(event_key) %>%
  mutate(day_consecutive_order = seq(from = 1, to = (n()))) %>%
  select(-one_of("venue_name","venue_city", "venue_state")) %>% # removing incorrect column values
  separate(col = venue_name_city_state, # separating into correct venuename, venue city, venue state
           into = c("venue_name","venue_city","venue_state"),
           sep = "->",
           remove = F) %>%
  mutate(venue_city_state =  paste( venue_city , venue_state , sep = "->")) # adding the combination of city and state

# glimpse(data.new)
# converting some of the columns into factor class
data.new[,c("venue_name_city_state","venue_name","venue_city","venue_state","venue_city_state")] =
  lapply(data.new[,c("venue_name_city_state","venue_name","venue_city","venue_state","venue_city_state")],as.factor)
# glimpse(data.new)

TotArtist_pervenue = data.new %>%
  group_by(venue_name_city_state) %>%
  summarise(TotalEvents = length(unique(event_name))) %>%
  arrange(desc(TotalEvents))
check_names = data.frame(venue_name_city_state = sort(TotArtist_pervenue$venue_name_city_state))

data.new = data.new %>% group_by(venue_capacity) %>% filter(cumsum_tix <= venue_capacity) # delete the overload observation
data.new = as.data.table(data.new)


function(input, output){

  output$TotArtistperVenue = renderDataTable({ # table of venue, number of events,and capacity 
    TotArtist_pervenue
  })
  
  groupSelectionChoice = reactive({
    switch(input$groupingType,
           "by venue" = sort(unique(data.new$venue_name_city_state)),
           "by city" = sort(unique(data.new$venue_city_state)),
           "by state" = sort(unique(data.new$venue_state)))
  })
  
  output$groupingUI = renderUI({
    if (is.null(input$groupingType)){return()}
    switch(input$groupingType,
           "by venue" = selectInput( inputId = "venue_xx",
                                     label = "Please select the venue you interested",
                                     choices = groupSelectionChoice()),
           "by city" = selectInput( inputId = "city_xx",
                                    label = "Please select the state you interested",
                                    choices = groupSelectionChoice()),
           "by state" = selectInput( inputId = "state_xx",
                                     label = "Please select the state you interested",
                                     choices = groupSelectionChoice())
    )
  })
  
  filtered.dataset = reactive({
    if (is.null(input$groupingType)){return()}
    if (input$groupingType == "by venue"){
      temp = data.new[venue_name_city_state  == input$venue_xx,]
      return(temp)
    }
    if (input$groupingType == "by city"){
      temp = data.new[venue_city_state  == input$city_xx,]
      return(temp)
    }
    if (input$groupingType == "by state"){
      temp = data.new[venue_state  == input$state_xx,]
      return(temp)
    }
  })
  
  output$TotalShows = renderText({
    paste0("There are " , length(unique(filtered.dataset()[,event_name])) , " shows in the group you selected, they are : ",
           paste(unique(filtered.dataset()[,event_name]), collapse = ", "))
  })
  
  output$filteredTable = renderDataTable({
    if(input$showFilteredTable == FALSE){return()}
    filtered.dataset()
  })
  
  output$plot_Cumulative = renderPlot({ # plot of everything, plus the aggregated line with equation on the top
    if(input$groupingType == "by venue"){ # for choice by venue, we only need to standardize  x axis  to 0 and 1, leave y alone because the venue capacity is always constant
      refit.points = data.frame()
      count = 1
      for (i in unique(as.character(filtered.dataset()[,event_name]))){ # to analyze each event name
        venue_event_name = filtered.dataset()[event_name == i, list(dayCount, cumsum_tix, venue_capacity)]
        # standardizing process
        x_axes = venue_event_name$dayCount/ max(venue_event_name$dayCount)
        y_axes = venue_event_name$cumsum_tix 
        standardize.df = data.frame(xs = x_axes, ys = y_axes)
        if(count == 1){ # for the first iteration, we create the plot
          plot(x = standardize.df$x_axes, y = standardize.df$y_axes, 
               xaxt="n",
               xlim = c(0,1), ylim = c(0, (200 + max(venue_event_name$venue_capacity))),
               pch = 18, title(paste("Group by Venue: ",input$venue_xx, sep = " ")),
               xlab = "Time to the event", ylab = "Total Ticket Sold")
          axis(1, seq(0, 1, by = 0.2), c("100%", "80%" , "60%", "40%", "20%", "0%" ))
          abline(max(venue_event_name$venue_capacity), 0 , lty = 2, lwd = 3, col ="blue")
          count = 0
        }
        points(x = standardize.df$xs, 
               y = standardize.df$ys, 
               xlim = c(0,1), 
               ylim = c(0, (200 + max(venue_event_name$venue_capacity))),  
               pch = 18)
        lm_model = lm(ys ~ log(xs) + poly(xs,3), data = standardize.df) # building a log-and-polynomial model
        # summary(lm_model)
        lines(predict(lm_model, # predict based on this model, we want to refit the existed points
                      data.frame( xs = seq(0.01,1,.01))), # re feed the points based on specific x axes coord
              x = seq(0.01,1,.01),  pch = 18)
        refit.points[c(1:100),i] = predict(lm_model, 
                                           data.frame( xs = seq(0.01,1,.01))) # store the fitted points into the refit.points
      }
      aggregated.points = data.frame(y_aggr = apply(refit.points, 1, mean),
                                     x_aggr = seq(0.01,1,.01))
      points(aggregated.points$y_aggr~ aggregated.points$x_aggr, pch = 20, col = "red", cex =1)
      aggregated_lm_model = lm(y_aggr~ log(x_aggr) + poly(x_aggr,3),
                               data = aggregated.points)
      aggregated.points$fitted_points = fitted(aggregated_lm_model)
      lines(aggregated.points$fitted_points ~ aggregated.points$x_aggr, col = "red", lwd = 3)
      summary_aggr = summary(aggregated_lm_model)
      coef1 = round(coef(summary_aggr)[1],2)
      coef2 = round(coef(summary_aggr)[2],2)
      coef3 = round(coef(summary_aggr)[3],2)
      coef4 = round(coef(summary_aggr)[4],2)
      coef5 = round(coef(summary_aggr)[5],2)
      venue_cap = max(filtered.dataset()[,venue_capacity])
      equation = bquote("tix_sales" == .(coef1) + .(coef2)*"log(day)" +
                          .(coef3)*day + .(coef4)*day^2 + .(coef5)*day^3*
                          "          " ~~ bold("Venue Capacity = ")*.(venue_cap))
      legend("topleft", legend = equation)
    } else{ # for choice by city and state, we need to standardize both x axis and y axis to 0 and 1, we need to standardize y axis because the venue capacity cahnged depending upon venue name
      refit.points = data.frame()
      count = 1
      if(input$groupingType == "by city"){
        plotTitle = paste("Group by City:", input$city_xx, sep = " ")
      }
      if(input$groupingType == "by state"){
        plotTitle = paste("Group by State:", input$state_xx, sep = " ")
      }
      
      for (i in unique(as.character(filtered.dataset()[,event_name]))){ # to analyze each event name
        venue_event_name = filtered.dataset()[event_name == i, list(dayCount, cumsum_tix, venue_capacity)]
        # standardizing process
        x_axes = venue_event_name$dayCount/ max(venue_event_name$dayCount)
        y_axes = venue_event_name$cumsum_tix/  max(venue_event_name$venue_capacity, venue_event_name$cumsum_tix) # used max here per event name bcs sometimes the venue can be occupied over capacity, 
        standardize.df = data.frame(xs = x_axes, ys = y_axes)
        if(count == 1){ # for the first iteration, we create the plot
          plot(x = standardize.df$x_axes, y = standardize.df$y_axes, 
               xaxt="n", yaxt = "n",
               xlim = c(0,1), ylim = c(0, 1.2),
               pch = 18, title(plotTitle),
               xlab = "Time to the event", ylab = "Percentage Tickets Sold")
          axis(1, seq(0, 1, by = 0.2), c("100%", "80%" , "60%", "40%", "20%", "0%" ))
          axis(2, seq(0, 1.2, by = 0.2), c("0%", "20%" , "40%", "60%", "80%", "100%","" ))
          count = 0
        }
        points(x = standardize.df$xs, y = standardize.df$ys, xlim = c(0,1), ylim = c(0, 1.2),  pch = 18)
        lm_model = lm(ys ~ log(xs) + poly(xs,3), data = standardize.df) # building a log-and-polynomial model
        # summary(lm_model)
        lines(predict(lm_model, # predict based on this model, we want to refit the existed points
                      data.frame( xs = seq(0.01,1,.01))), # re feed the points based on specific x axes coord
              x = seq(0.01,1,.01),  pch = 18)
        refit.points[c(1:100),i] = predict(lm_model, 
                                           data.frame( xs = seq(0.01,1,.01))) # store the fitted points into the refit.points
      }
      aggregated.points = data.frame(y_aggr = apply(refit.points, 1, mean),
                                     x_aggr = seq(0.01,1,.01))
      points(aggregated.points$y_aggr~ aggregated.points$x_aggr, pch = 20, col = "red", cex =1)
      aggregated_lm_model = lm(y_aggr~ log(x_aggr) + poly(x_aggr,3),
                               data = aggregated.points)
      aggregated.points$fitted_points = fitted(aggregated_lm_model)
      lines(aggregated.points$fitted_points ~ aggregated.points$x_aggr, col = "red", lwd = 3)
      summary_aggr = summary(aggregated_lm_model)
      coef1 = round(coef(summary_aggr)[1],2)
      coef2 = round(coef(summary_aggr)[2],2)
      coef3 = round(coef(summary_aggr)[3],2)
      coef4 = round(coef(summary_aggr)[4],2)
      coef5 = round(coef(summary_aggr)[5],2)
      equation = bquote("tix_sales" == .(coef1) + .(coef2)*"log(day)" +
                          .(coef3)*day + .(coef4)*day^2 + .(coef5)*day^3)
      legend("topleft", legend = equation)
    }
  })
  
  # output$ggplot_raw = renderPlot({ # raw plot of venue before normalized, but shifted
  #   ggplot(data=filtered.dataset(), aes(x=dayCount, y=cumsum_tix, group=event_name, colour=event_name)) +
  #     geom_line()
  # })

  output$plotly_plot = renderPlotly({ # same as rawplot, but using plotly
    venue_filtered = data.new[venue_name_city_state  == input$venue_x,]
    plot_ly(filtered.dataset(), x = ~dayCount, y = ~cumsum_tix, mode = 'lines+markers', color = ~event_name )
  })

  curr_sales = reactive({ # reactive function for the raw input
    file1  =  input$file
    if (is.null(file1)){return()}
    read.csv(file = file1$datapath, header = T)
  })

  curr_sales_cumsum = reactive({ # calculating the cumulative sum from the raw input
    file1  =  input$file
    if (is.null(file1)){return()}
    temp = read.csv(file = file1$datapath, header = T)
    temp[,3] = cumsum(temp[,2])
    colnames(temp)[3] = "cumsum"
    return(temp)
  })

  output$UserInput = renderDataTable({ #showing the raw input
    if (is.null(curr_sales())){return()}
    if ( is.na(input$showUserTable) || is.null(input$showUserTable)){return()}
    if (input$showUserTable == "showRawUserInput" ){return(curr_sales())}
  })

  output$contents_cumsum = renderDataTable({ # showing the new cumsum table
    if (is.null(curr_sales_cumsum())){return()}
    if ( is.na(input$showUserTable) || is.null(input$showUserTable)){return()}
    if (input$showUserTable == "showUserInput_cumsum" ){return(curr_sales_cumsum())}
  })

  
  compiled_rescaled_table = eventReactive(input$go,{
    if (is.null(curr_sales())){return()}
    if (is.null(curr_sales_cumsum())){return()}
    if( input$groupingType == "by venue"){
      refit.points = data.frame()
      for (i in unique(as.character(filtered.dataset()[,event_name]))){ # to analyze each event name
        venue_event_name = filtered.dataset()[event_name == i, list(dayCount, cumsum_tix, venue_capacity)]
        x_axes = venue_event_name$dayCount/ max(venue_event_name$dayCount)
        y_axes = venue_event_name$cumsum_tix
        standardized.df = data.frame(xs = x_axes,  ys = y_axes)
        lm_model = lm(ys ~ log(xs) + poly(xs,3), data = standardized.df)
        refit.points[c(1:100),i] = predict(lm_model, data.frame( xs = seq(0.01,1,.01))) # store the fitted points into the refit.points
        }
      aggregated.points = data.frame(y_aggr = apply(refit.points, 1, mean),
                                     x_aggr = seq(0.01,1,.01))
      aggregated_lm_model = lm(y_aggr~ log(x_aggr) + poly(x_aggr,3), data = aggregated.points)
      rescaled.points.model = predict(aggregated_lm_model,   # model for aggregated points (same as the redline at the begining plot up up)
                                      data.frame(x_aggr = seq(from = 1/input$saleDuration, # divided by input$saleDuration because that we standardized when we built our model, so our model works best with the with standardized value
                                      to = input$saleDuration/input$saleDuration,
                                      by =  1/input$saleDuration)))
      new_point_lm_model_log_poly2 = lm(cumsum~log(day), data = curr_sales_cumsum())
      new_point_predict_log_poly2 = predict(new_point_lm_model_log_poly2,
                                            data.frame(day = seq(1,input$saleDuration, by = 1)))
      compiled.points.model = rbind(data.frame(x_axes = seq(from = 1, to = input$saleDuration, by = 1), # the fitted point of the model
                                               y_axes = round(rescaled.points.model,0),
                                               description = rep("model", input$saleDuration)),
                                    data.frame(x_axes = seq(from = 1, to = dim(curr_sales_cumsum())[1], by = 1),
                                               y_axes = curr_sales_cumsum()[,3],
                                               description = rep("current_performance", dim(curr_sales_cumsum())[1])),
                                    data.frame(x_axes = seq(from  = 1, to = input$saleDuration, by = 1),
                                               y_axes = round(new_point_predict_log_poly2,2),
                                               description = rep("fitted_line_log_function", input$saleDuration)))
      colnames(compiled.points.model) = c("dayCount", "ticket_sold", "desc")
      compiled.points.model[,3] = as.factor(compiled.points.model[,3])
      return(compiled.points.model)
    } else{
      refit.points = data.frame()
      for (i in unique(as.character(filtered.dataset()[,event_name]))){ # to analyze each event name
        venue_event_name = filtered.dataset()[event_name == i, list(dayCount, cumsum_tix, venue_capacity)]
        x_axes = venue_event_name$dayCount/ max(venue_event_name$dayCount)
        y_axes = venue_event_name$cumsum_tix / max(venue_event_name$venue_capacity, venue_event_name$cumsum_tix)
        standardized.df = data.frame(xs = x_axes,  ys = y_axes)
        lm_model = lm(ys ~ log(xs) + poly(xs,3), data = standardized.df)
        refit.points[c(1:100),i] = predict(lm_model, data.frame( xs = seq(0.01,1,.01))) # store the fitted points into the refit.points
      }
      aggregated.points = data.frame(y_aggr = apply(refit.points, 1, mean),
                                     x_aggr = seq(0.01,1,.01))
      aggregated_lm_model = lm(y_aggr~ log(x_aggr) + poly(x_aggr,3), data = aggregated.points)
      rescaled.points.model = predict(aggregated_lm_model,   # model for aggregated points (same as the redline at the begining plot up up)
                                      data.frame(x_aggr = seq(from = 1/input$saleDuration, # divided by input$saleDuration because that we standardized when we built our model, so our model works best with the with standardized value
                                                              to = input$saleDuration/input$saleDuration,
                                                              by =  1/input$saleDuration)))
      rescaled.points.model = input$venueCap * rescaled.points.model # we need to rescaled up based on user input of venue capacity
      new_point_lm_model_log_poly2 = lm(cumsum~log(day) , data = curr_sales_cumsum())
      new_point_predict_log_poly2 = predict(new_point_lm_model_log_poly2,
                                            data.frame(day = seq(1,input$saleDuration, by = 1)))
      compiled.points.model = rbind(data.frame(x_axes = seq(from = 1, to = input$saleDuration, by = 1), # the fitted point of the model
                                               y_axes = round(rescaled.points.model,0),
                                               description = rep("model", input$saleDuration)),
                                    data.frame(x_axes = seq(from = 1, to = dim(curr_sales_cumsum())[1], by = 1),
                                               y_axes = curr_sales_cumsum()[,3],
                                               description = rep("current_performance", dim(curr_sales_cumsum())[1])),
                                    data.frame(x_axes = seq(from  = 1, to = input$saleDuration, by = 1),
                                               y_axes = round(new_point_predict_log_poly2,2),
                                               description = rep("fitted_line_log_function", input$saleDuration)))
      colnames(compiled.points.model) = c("dayCount", "ticket_sold", "desc")
      compiled.points.model[,3] = as.factor(compiled.points.model[,3])
      return(compiled.points.model)
    }
  })
  
  output$comparisonPlot = renderPlotly({
    if (is.null(curr_sales_cumsum())){return()}
    plot_ly(compiled_rescaled_table(), x = ~dayCount, y = ~ticket_sold,  mode = 'lines+markers', color = ~desc, colors = c("red","black","green"))
  })
  
    output$CompiledData = renderDataTable({
    if (is.null(curr_sales())){return()}
    compiled_rescaled_table()
  })
}












# compiled_rescaled_table = reactive({ # this table consist of : the rescaled model points, the raw input cumsum, and the extrapolation of the raw input
#   if (is.null(curr_sales())){return()}
#   if (is.null(curr_sales_cumsum())){return()}
#   refit.points = data.frame()
#   venue_filtered = data.new[venue_name_city_state  == input$venue_x,]
#   for (i in unique(as.character(venue_filtered$event_name))){ # to analyze each event name
#     venue_event_name = venue_filtered[event_name == i, list(dayCount, cumsum_tix)]
#     x_axes = venue_event_name$dayCount/ max(venue_event_name$dayCount)
#     y_axes = venue_event_name$cumsum_tix
#     lm_model = lm(y_axes ~ log(x_axes) + poly(x_axes,3))
#     refit.points[c(1:100),i] = predict(lm_model, data.frame( x_axes = seq(0.01,1,.01))) # store the fitted points into the refit.points
#   }
#   aggregated.points = data.frame(y_aggr = apply(refit.points, 1, mean),
#                                  x_aggr = seq(0.01,1,.01))
#   
#   aggregated_lm_model = lm(y_aggr~ log(x_aggr) + poly(x_aggr,3), data = aggregated.points)
#   rescaled.points.model = round(predict(aggregated_lm_model,   # model
#                                         data.frame(x_aggr = seq(from = 1/100,
#                                                                 to = input$saleDuration/100,
#                                                                 by = .01))),0)
#   
#   new_point_lm_model_log_poly2 = lm(cumsum~log(day)+ poly(day,2) , data = curr_sales_cumsum())
#   new_point_predict_log_poly2 = round(predict(new_point_lm_model_log_poly2,
#                                               data.frame(day = seq(1,input$saleDuration, by = 1))),0)
#   
# 
#   compiled.points.model = rbind(data.frame(x_axes = seq(from = 1, to = input$saleDuration, by = 1),
#                                            y_axes = rescaled.points.model,
#                                            description = rep("model", input$saleDuration)),
#                                 data.frame(x_axes = seq(from = 1, to = dim(curr_sales_cumsum())[1], by = 1),
#                                            y_axes = curr_sales_cumsum()[,3],
#                                            description = rep(input$venue_x, dim(curr_sales_cumsum())[1])),
#                                 data.frame(x_axes = seq(from  = 1, to = input$saleDuration, by = 1),
#                                            y_axes = new_point_predict_log_poly2,
#                                            description = rep("fitted_line_log_poly2", input$saleDuration)))
#   colnames(compiled.points.model) = c("dayCount", "ticket_sold", "desc")
#   compiled.points.model[,3] = as.factor(compiled.points.model[,3])
#   return(compiled.points.model)
# })
# 
# 


# 
#   test_lm = reactive({
#     file1  =  input$file
#     if (is.null(file1)){return()}
#     temp = read.csv(file = file1$datapath, header = T)
#     temp[,3] = cumsum(temp[,2])
#     colnames(temp)[3] = "cumsum"
#     # input_lm = lm(cumsum~log(day) + poly(day,3), data = temp)
#     input_lm = lm(cumsum~ log(day) + poly(day,3), data = temp)
#     predict.new.point  = predict(input_lm, data.frame(day = seq(from = 1, to = input$saleDuration, by = 1)))
#     output_new_lm = data.frame(x_axes = seq(from = 1, to = input$saleDuration, by = 1),
#                               y_axes = predict.new.point,
#                               description = rep("prediction", input$saleDuration))
#     # output_new_lm = data.frame(curr_sales_cumsum()[1])
#     # input_lm = lm(cumsum~log(day) + poly(day,3), data = curr_sales_cumsum())
#     # fitted_lm = predict(input_lm, data.frame(day = c(1:50)))
#     # output_new_lm = data.frame(day = c(1:50),
#     #                            fitted_points = fitted_lm)
#     return(output_new_lm)
#   })
# 
#   output$test_lm_fitted = renderDataTable({
#     if (is.null(curr_sales())){return()}
#     if (is.null(curr_sales_cumsum())){return()}
#     test_lm()
#   })


#   
# 
# 
# norm.inc*rabd
# mean(rexp(10000, rate = 3.05))
# mean(rnorm(10000, mean = 2.95/100,sd= 2.95/100))
