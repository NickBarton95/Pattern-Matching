

PatternMatchingUI = function(id, data){
  
  ns <- NS(id)
  
  tagList(

    boxPlus(width = 12,
            title = "Pattern Matching", 
            status = "success", 
            solidHeader = F, 
            collapsible = T,
            closable = F, 
            
            fluidRow(
              column(4,
                     pickerInput(ns("var.sel"), selected = colnames(data)[-1][1], 
                                 choices = colnames(data)[-1], options = list(
                                   title = "Choose Target Variable:",
                                   style = "btn-success"
                                 ))
              ), 
              
              column(8, 
                     h5("Choose a period of interest for a selected variable, the algorithm will find similar patterns in historic plant data. 
                            Allowing exploration of these similar plant conidions to inform current decision making.")
              )
            ),
            
            fluidRow(
              dygraphOutput(ns("dygraph"))
            ),
            
            actionBttn(ns("run1"), "Search for Pattern Matches", size = "xs", color = "success"), 
            uiOutput(ns("AddPattern.in"), inline = T)
            
            
    ),
    column(6,
           boxPlus(
             id = ns("box1"), 
             width = 12,
             title = "Match Information", 
             status = "success", 
             solidHeader = F, 
             collapsible = T,
             closable = F, 
             collapsed = T,
             
             dataTableOutput(ns("table2")), 
             h5("If the returned matches are not as expected, try again with a more focused capture of the period of interest,  
               without including excess noise.")
             
           ),
           
           
           boxPlus(
             id = ns("box3"), 
             width = 12,
             title = "Forecast", 
             status = "success", 
             solidHeader = F, 
             collapsible = T,
             closable = F, 
             collapsed = T,
             plotlyOutput(ns("forecast")), 
             h5("Using the observed behaviour of the historic matched patterns, a forecast is made for the selected period of interest.")
           )
    ),
    
    boxPlus(
      id = ns("box2"), 
      width = 6,
      title = "Pattern Visualisation", 
      status = "success", 
      solidHeader = F, 
      collapsible = T,
      closable = F, 
      collapsed = T,
      
      column(12,
             htmlOutput(ns("length"))
      ),
      
      column(6,
             uiOutput(ns("var.select"))
             
      ),
      prettySwitch(ns("overlay"), label = "Show Selected Pattern", 
                   status = "success", fill = T, inline = T),
      textOutput(ns("match.show"), inline = T), 
      br(), 
      plotlyOutput(ns("plot.list2")), 
      h5("Explore the system behaviour around the period of interest along overlaid againts the pattern matches, use this to
             make the most of historic plant data.")
    )
    
    
  )
  
}
PatternMatching = function(input, output, session, data){
  
  outputs = reactiveValues()
  outputs[["dates"]] = c()
  
  rvs = reactiveValues()
  rvs$click.count = 0
  
  # Render timeseries plot for selected var and show selected and matched patterns if exist
  output[["dygraph"]] <- renderDygraph({
    
    obj = xts(x = data()[[input$var.sel]], order.by = data()[,1])
    
    plot = dygraph(obj) %>%
      dyRangeSelector() %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.5, hideOnMouseOut = FALSE) %>%
      dyAxis("y", label = input$var.sel) %>%
      dyAxis("x", label = "Date Time") %>%
      dyLegend(width = 600)
    
    
    if(length(outputs[["dates"]]) > 0){
      
      match.names = paste("Match", 1:length(outputs[["dates"]]))
      
      plot = plot %>% dyShading(from = outputs[["dates.target"]][1], to = outputs[["dates.target"]][2], color = "#CCEBD6")
      
      for(i in 1:length(outputs[["dates"]])){
        
        plot = plot %>% dyShading(from = outputs[["dates.start"]][i], to = outputs[["dates"]][i], color = "#FFE6E6") %>%
          dyEvent(outputs[["dates.start"]], match.names, labelLoc = "bottom", color = "red") %>%
          dyEvent(outputs[["dates"]], rep("", length(outputs[["dates"]])), labelLoc = "bottom", color = "red") %>%
          dyEvent(outputs[["dates.target"]][1:2], c("Target", ""), labelLoc = "bottom", color = "green")
      }
      
    }
    
    plot
  })
  
  # When pattern selected and ran carry out pattern search and save results to outputs
  observeEvent(input$run1, {
    
    chosen.dates = input[[paste0("dygraph", "_date_window")]]
    chosen.dates = as.POSIXct(chosen.dates)
    
    begin = which.min(abs(data()[,1]-chosen.dates[1])) #split points
    end = which.min(abs(data()[,1]-chosen.dates[2]))
    
    pattern = data()[[input$var.sel]][seq(begin, end)]
    
    if(length(pattern) > 2000){
      showNotification("Pattern exceeds length limit", action = "(2000 hrs)", type = "error")
    }else{
      
      outputs[["dates.target"]] = chosen.dates
      outputs[["pattern"]] = pattern
      
      pattern.bank = data()[[input$var.sel]][seq(1, begin-1)]
      outputs[["pattern.bank"]] = pattern.bank
      matches.df = Var.dist(sample = as.numeric(as.character(pattern)), global = as.numeric(as.character(pattern.bank)))
      
      outputs[["matches.df"]] = matches.df[[1]]
      outputs[["match.index"]] = matches.df[[2]]
      outputs[["distances.vec"]] = matches.df[[3]]
      outputs[["patterns"]] = matches.df$forecast
      outputs[["match.score"]] = matches.df$match.score
      
      output$AddPattern.in <- renderUI({
        ns <- session$ns
        actionBttn(ns("AddPattern"), "Find Next Best pattern match", size = "xs", color = "success")
      })
      
      outputs[["current.var"]] = isolate(input$var.sel)
      
      if(rvs$click.count == 0 ){
        
        ns = session$ns
        
        js$collapse(ns("box1"))
        js$collapse(ns("box2"))
        js$collapse(ns("box3"))
      }
      
      rvs$click.count =  rvs$click.count + 1
      
    }
    
    
  })
  
  # Calc and show pattern length, data() boundaries and target var
  output[["length"]] <- renderText({
    
    req(outputs[["matches.df"]])
    
    dates = outputs[["dates.target"]]
    
    duration = format(difftime(dates[2], dates[1]))
    
    string1 = paste("Date Range: ", dates[1], "to", dates[2])
    
    string2 = paste("Pattern Duration:", duration)
    
    string3 = paste("Target Variable:", isolate(input$var.sel))
    
    
    string = HTML(paste(string1, string2, string3, sep = "<br/>"))
    
  })
  
  # Match info table with sparkline and dates (add accuracy value?)
  output[["table2"]] <- renderDataTable({
    
    req(outputs[["matches.df"]])
    
    index = outputs[["match.index"]]
    
    index.start = index - length(outputs[["pattern"]])
    
    dates = as.character(data()[,1][index])
    
    dates.start = as.character(data()[,1][index.start])
    
    outputs[["dates.start"]] = dates.start
    outputs[["dates"]] = dates
    
    data = data.frame("Match" = seq(1, length(index)), "Start Date" = dates.start, "End Date" = dates, "Match Score" = round(outputs[["match.score"]]))
    
    drop.index = which(colnames(outputs[["matches.df"]]) %in% c("time", "original"))
    
    matches = outputs[["matches.df"]][, -drop.index]
    
    spk.chars = c()
    for(i in 1:ncol(matches)){
      
      spk.chars[i] = spk_chr(matches[,i], type = "line", 
                             lineColor = '#2a774d', fillColor = 'rgba(66, 245, 170, 0.2)', 
                             highlightSpotColor = 'rgb(66, 164, 245)',
                             lineWidth = 1, minSpotColor = '', maxSpotColor = '', spotColor = '' )
      
    }
    
    data$SparkLine = spk.chars
    
    colnames(data) = c("Match", "Start Date", "End Date", "Match Score", "Pattern")
    
    datatable(data, style = "bootstrap", class = "compact", escape = F,   
              caption = "Table: Returned matches for pattern of interest",
              selection = list(mode = "single", selected = 1),
              rownames = F, options = list(paging = FALSE, searching = F, dom='t', ordering=F,
                                           columnDefs = list(list(className = 'dt-center', targets = 0:4)), 
                                           fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')
              )) %>%
      spk_add_deps() 
    
  })
  
  proxy = dataTableProxy("table2")
  
  # Find the next best pattern from distances.vec and previous match.index and update results
  observeEvent(input$AddPattern,  {
    
    
    results = GetNextPattern(outputs[["matches.df"]], outputs[["distances.vec"]], outputs[["match.index"]], match.score = outputs[["match.score"]],
                             data = outputs[["pattern.bank"]], forecast = outputs[["patterns"]])
    
    outputs[["matches.df"]]  = results$matches.df
    outputs[["distances.vec"]] = results$distances.vec
    outputs[["match.index"]] = results$match.index
    outputs[["match.score"]] = results$match.score
    outputs[["patterns"]] = results$forecast
    
    
  })
  
  # Rank of shown match
  output$match.show <- renderText({
    req(input$table2_rows_selected )
    
    text = paste("Displaying Match:", input$table2_rows_selected )
  })
  
  # Choice of variable as options to user (column names of data)
  output$var.select <- renderUI({
    ns <- session$ns
    req(outputs[["dates.start"]])
    
    choices = colnames(data())[-1]
    
    choices = choices[-which(choices == isolate(input$var.sel))]
    
    pickerInput(ns("var.selector"), choices = choices, multiple = T, 
                options = list(
                  title = "View other variables:",
                  style = "btn-success"
                ))
    
  })
  
  # Plot row selected returned match with overlaid selected pattern (choice) and other related vars (choice)
  output[["plot.list2"]] <- renderPlotly({
    
    req(outputs[["dates.start"]], input$table2_rows_selected)
    
    pattern = input$table2_rows_selected #change to user input via match table
    
    vars  = c(outputs[["current.var"]], input$var.selector) #change to select input
    
    date1 = outputs[["dates.start"]][pattern]
    date2 = outputs[["dates"]][pattern]
    
    overlay = input$overlay
    plot.list = list()
    for(i in 1:length(vars)){
      
      if(i == 1){
        width = 4
      }else{
        width = 1
      }
      
      plot.list[[i]] = plot_ly(x = data()[,1], y = data()[[vars[i]]], type = "scatter", mode = "lines", name = vars[i], 
                               line = list(width = width, color = plot.colours[i])) %>% 
        layout(yaxis = list(nticks = 5))
      
      if(overlay == T){
        date1.index = which(substr(data()[,1], 1 ,19) == date1)
        date2.index = which(substr(data()[,1], 1 ,19) == date2)
        
        date.range = data()[date1.index:date2.index,1]
        
        chosen.dates = outputs[["dates.target"]]
        begin = which.min(abs(data()[,1]-chosen.dates[1])) #split points
        end = which.min(abs(data()[,1]-chosen.dates[2]))
        
        
        
        
        
        plot.list[[i]] = plot.list[[i]] %>% add_lines(x = date.range[-1], 
                                                      y = data()[[vars[i]]][begin:end], text = data()[,1][begin:end],
                                                      hoverinfo = "text+y", 
                                                      line = list(dash = "dot", width = 1), showlegend = F)
      }
      
      plot.list[[i]] = plot.list[[i]] %>%
        add_annotations(
          text = vars[i],
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = F,
          font = list(size = 15),
          inherit = F
        )
    }
    
    f.plot = plotly::subplot(plot.list, shareX = T, nrows = length(vars))
    f.plot = f.plot %>% layout(xaxis = list(range = c(date1, date2)), 
                               legend = list(orientation = "h"))
    
    f.plot    
  })
  
  # Plot selected pattern with forecast based on returned patterns 
  output[["forecast"]] <- renderPlotly({
    
    req(outputs[["patterns"]])
    
    new.patterns = matrix(ncol = ncol(outputs[["patterns"]]), nrow = nrow(outputs[["patterns"]]))
    
    for(i in 1:ncol(outputs[["patterns"]])){
      scaler = outputs[["patterns"]][1,i] - outputs[["matches.df"]]$original[length(outputs[["matches.df"]]$original)]
      temp.pattern = outputs[["patterns"]][,i] - scaler
      new.patterns[,i] = temp.pattern
    }
    
    w = nrow(new.patterns)
    pattern.length = nrow(outputs[["matches.df"]])
    
    plot = plot_ly(x = outputs[["matches.df"]]$time, y = outputs[["matches.df"]]$original,
                   type = "scatter", mode = "lines", name = "Selected Pattern", 
                   line = list(width = 3, color = plot.colours[1])) %>%  
      layout(legend = list(orientation = "h")) 
    for(i in 1:ncol(new.patterns)){
      plot = plot %>%
        add_lines(y = new.patterns[,i], x = (pattern.length +1):(pattern.length + w),
                  name = paste("forecast", i), inherit = F, 
                  line = list(width = 2, dash = "dot", color = plot.colours[i+1]))
    }
    
    plot
    
  })
  
  
}

