myFunction <- function (moveInfo, readings, positions, edges, probs) 
{
  "
  Main function for the algorithm so solve WheresCroc
  "
  
  options = getOptions(positions[3], edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1 = readline("Move 1: ")
  if (mv1 == "q") {
    stop()
  }
  if (!mv1 %in% options && mv1 != 0) {
    warning("Invalid move. Search ('0') specified.")
    mv1 = 0
  }
  if (mv1 != 0) {
    options = getOptions(mv1, edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2 = readline("Move 2: ")
  if (mv2 == "q") {
    stop()
  }
  if (!mv1 %in% options && mv1 != 0) {
    warning("Invalid move. Search ('0') specified.")
    mv2 = 0
  }
  moveInfo$moves = c(mv1, mv2)
  return(moveInfo)
}


"
Helper functions
"
best_first <- function () 
{
  "
  Function to perform a best first search at the beginning of each game to calculate the best path between all
  of the different pools
  input: none
  output: Matrix with best path between all pools
  " 
  # Initialize the needed variables
  expanded = list(pos = 1, cost = 0, path = list())  # Expanded node
  frontier = list(list(pos = 2, cost = 1, path = list(1)))  # Initialize frontier with step to pool 2
  neighboors = list()  # To keep track of neighboors to a pool
  
  # Search for best path beginning with 1-40
  goal = 40
  while (expanded$pos != goal) {
    # Find the neighbooring pools
    for (i in 1:length(edges[,1])) {
      if (edges[i,1] == expanded) {
        neighboors <- append(neighboors, edges[i,1])
      } 
      else if (edges[i,2] == expanded) {
        neighboors <- append(neighboors, edges[i,2])
      }
    }
  }
}

"
Testing of helper functions
"
