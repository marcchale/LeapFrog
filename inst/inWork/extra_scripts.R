for(course in course.list$name){
test.coordMatrix <- LeapFrog::get_data(course)$coordinateMatrix
test.tour <- sample(1:dim(test.coordMatrix)[1])
LeapFrog::plot_tour(test.coordMatrix, test.tour)
}

for(name in course.list$name){
  i = i+1
  name = course.list$name[i]
  data.name <- paste0(name,"_coord")
  assign(data.name, course.coords[course.coords$Course == name, 3:4])
  name
  usethis::use_data(pma343_coord)
}


