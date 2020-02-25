#Required Libraries: rstudioapi, plotly, shiny, base
shiny::runApp(
  list(
    ui = shiny::fluidPage(
      plotly::plotlyOutput("p")
      ),
    server = function(input, output, session) {
      output$p <- plotly::renderPlotly({
        plotly::plot_ly(x = 0, y = sin(0)) %>%
          plotly::add_lines()
        })
      
      # a reactive value to track the current x value
      x <- shiny::reactiveVal(0)
      
      # invalidate this R code every 100 milliseconds
      shiny::observeEvent(shiny::invalidateLater(100, session), 
                   {###### Put calls to LF alg here
                     y <- sin(x())
                     
                     # update line chart data
                     plotly::plotlyProxy("p", session) %>%
                       plotly::plotlyProxyInvoke(
                         "extendTraces", 
                         base::list(
                           y = base::list(base::list(y)),
                           x = base::list(base::list(x()))
                           ),
                         base::list(0)
                         )
                     # update current x value
                     x(x() + .1)
                     },
                   ignoreNULL = FALSE)
      }
    ),
  launch.browser = rstudioapi::viewer
  )


fileConn <- file(file.path(tempdir(), "LFOutput.txt"))
writeLines(c("Hello","World", "2"), fileConn)
close(fileConn)


sink(file.path(tempdir(), "LFOutput.txt"))
cat("hello??", sep = "\n")
cat("update", sep = "\n", append = T)
sink()
file.show(file.path(tempdir(), "LFOutput.txt"))


file(pattern = "LFOutput", tmpdir = tempdir(), fileext = ".txt")
tempdir(check = FALSE)
