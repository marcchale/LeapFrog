p2 <- tibble::tibble(Description = character(),#
                     Rank = integer(),#
                     Status = character(),#
                     Value = character(),#
                     Inputs = character(),
                     Outputs = character(),
                     Use = character(),
                     Time = character(),
                     Required = character())
p2[1,] <- c("Tour Plot", 5, "Finished", 
            "Visual representation of results", "Tour, Coord Matrix", "plot", 
            "after LF execution, show best tour found on a map", "Yes", "Yes")

p2[2,] <- c("Summary Plot", 6, "In work", 
            "Visual representation of algorithm, helps with tuning", "LF execution", "plot", 
            "after LF execution, show LF progress", "Yes", "No")

p2[3,] <- c("Live Tour Plot", 9, "In work", 
            "Live results to see how solution converges", "NA", "plot", 
            "during LF execution, show best tour found on a map", "Maybe (tech hurdle)", "No")

p2[4,] <- c("Live Summary Plot", 8, "In work", 
            "Live algorithm execution plot, gives peace of mind during long executions", "NA", 
            "plot", "during LF execution, show LF progress", "Maybe (tech hurdle)", "No")

p2[5,] <- c("Live Statistics", 4, "Finished",
            "Provides a simple summary of progress", "LF execution", "console text", 
            "during LF execution, output progress", "Yes", "Yes")

p2[6,] <- c("Coordinate to Distance Matrix", 2, "Finished",
            "Converts a coordinate matrix to a distance matrix", "Coord Matrix", "Distance Matrix",
            "when only coord matrix is available", "Yes", "Yes")

p2[7,] <- c("Distance to Coordinate Matrix", 3, "Finished", 
            "Converts a distance matrix to a coordinate matrix", "Distance Matrix", "Coord Matrix", 
            "when only dist matrix is available, and tour plot is desired", "Yes", "Yes")

p2[8,] <- c("LF Algorithm", 1, "Finished", 
            "Executes the LeapFrog algorithm", "Distance Matrix, parameters", 
            "Tour", "find best tour in a TSP", "Yes", "Yes")

p2[9,] <- c("Sample Data", 7, "Finished", 
            "Provides known sample datasets", "NA", "Sample Data", 
            "practice with available data", "Yes", "Yes")
View(p2)



