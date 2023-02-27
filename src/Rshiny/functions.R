nb.cols = 15
mycolors = colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


# JS function for rendering options in selectizeInput for queries
selectize_render_query_function <- I(
  '{
     option: function(item, escape) {
       return "<div>" +
         "<div style=\'font-weight: 500;border-bottom: 1px solid grey\'>" + escape(item.county) + "</div> <small>County seat: " + escape(item.county_seat) + " | Est.: " +
         escape(item.est) + " | population: " + escape(item.population) + "</small>" +
       "</div>"
     }
   }'
)


# plot the plotly box plot
# Arguments:
#   D: a data frame, observations as rows
#   x: a string, a column header indicate the x-axis
#   y: a string, a column header indicate the y-axis
#   color: a string, a column header indicate the color
#   pTitle: a string, the title of the plot
# Return: a plotly object for show
#
plotBox <- function(D,x,y,color,pTitle=""){
  if(nrow(D)==0){
    return(plotEmptyPlotly("No data was found.", pTitle))
  }
  
  tip <- apply(D,1,function(x) return(paste(paste(names(x),x,sep=": "), collapse="\n")))
  p <- plot_ly(D,x=as.formula(paste("~",x)),
               y=as.formula(paste("~",y)),
               color=as.formula(paste("~",color)),
                                colors = mycolors,
               type="box", hovertext=tip, hoverinfo="text",
               pointpos=0, boxpoints="all") %>% layout(showlegend = FALSE) 
  if(y != color) p <- p %>% layout(boxmode = "group")
  p <- p %>% layout(title = list(text = pTitle, y=1.0, yanchor="top"), margin = list(t = 30))
  return(p)
}


# provide an empty plotly with a message in the middle
# Arguments:
# msg: a string, message to show
# pTitle: a string, plot title
# Return: a plotly object for show
#
plotEmptyPlotly <- function(msg, pTitle=""){
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  return(plotly_empty(type = "scatter", mode = "markers")%>%
           layout(title=list(text=pTitle,y=0.95,yanchor="top"),
                  xaxis=ax,yaxis=ax,
                  annotations=list(text=msg,
                                   xref="paper",
                                   yref="paper",
                                   showarrow=F,
                                   font=list(size=28))))
}


