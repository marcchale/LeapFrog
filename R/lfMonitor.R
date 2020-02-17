# monitoring function (this works in all consoles)
lfMonitor <- function(game, tourLengthBest)
{ 
  sumryStat <- c(game, tourLengthBest)
  sumryStat <- format(sumryStat, digits = 0)
  cat(paste("LF | Game = ", sumryStat[1], 
            "| Best =", sumryStat[2]))
  cat("\n")
  flush.console()
}



plot.lf <- function(x, y, ylim, cex.points = 0.7,
                    col = c("green3", "dodgerblue3", adjustcolor("green3", alpha.f = 0.1)),
                    pch = c(16, 1), lty = c(1,2), legend = TRUE,
                    grid = graphics:::grid, ...)
{
  object <- x  # Argh.  Really want to use 'object' anyway
  is.final <- !(any(is.na(object@summary[,1])))
  iters <- if(is.final) 1:object@iter else 1:object@maxiter
  summary <- object@summary
  if(missing(ylim)) 
  { ylim <- c(max(apply(summary[,c(2,4)], 2, 
                        function(x) min(range(x, na.rm = TRUE, finite = TRUE)))),
              max(range(summary[,1], na.rm = TRUE, finite = TRUE))) 
  }
  
  plot(iters, summary[,1], type = "n", ylim = ylim, 
       xlab = "Generation", ylab = "Fitness value", ...)
  if(is.final & is.function(grid)) 
  { grid(equilogs=FALSE) }
  points(iters, summary[,1], type = ifelse(is.final, "o", "p"),
         pch = pch[1], lty = lty[1], col = col[1], cex = cex.points)
  points(iters, summary[,2], type = ifelse(is.final, "o", "p"),
         pch = pch[2], lty = lty[2], col = col[2], cex = cex.points)
  if(is.final)
  { polygon(c(iters, rev(iters)), 
            c(summary[,4], rev(summary[,1])), 
            border = FALSE, col = col[3]) }
  else
  { title(paste("Iteration", object@iter), font.main = 1) }
  if(is.final & legend)
  { inc <- !is.na(col)
  legend("bottomright", 
         legend = c("Best", "Mean", "Median")[inc], 
         col = col[inc], pch = c(pch,NA)[inc], 
         lty = c(lty,1)[inc], lwd = c(1,1,10)[inc], 
         pt.cex = c(rep(cex.points,2), 2)[inc], 
         inset = 0.02) }
  
  out <- data.frame(iter = iters, summary)
  invisible(out)
}
