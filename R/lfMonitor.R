# monitoring function (this works in all consoles)
lfMonitor <- function(game, tourLengthBest)
{ 
  sumryStat <- c(game, tourLengthBest)
  sumryStat <- format(sumryStat, digits = 0)
  cat(paste("LF | Game = ", sumryStat[1], 
            "| Best =", sumryStat[2]))
  cat("\n")
  utils::flush.console()
}
