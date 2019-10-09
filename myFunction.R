myFunction <- function (moveInfo, readings, positions, edges, probs) 
{
  "
  Main function for the algorithm so solve WheresCroc
  "
  paths = best_first(edges)
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
best_first <- function (edges) 
{
  "
  Function to perform a best first search at the beginning of each game to calculate the best path between all
  of the different pools
  input: matrix 'edges' of possible moves to make
  output: Matrix with best path between all pools
  " 
  # Initialize the needed variables
  
  results = matrix(data = 0, nrow = 40, ncol = 40)  # Result matrix
  
  # Search for best path beginning with 1-40
  #final_goal = edges[length(edges[,1]),1]
  final_goal = 0
  for (k in 1:length(results[,1])) {
    final_goal = 40
    for (l in 1:length(results[1,])) {
      expanded = list(pos = k, cost = 0, path = list())  # Expanded node
      frontier = list(list(pos = 1, cost = 1000, path = list(1)))  # Initialize frontier with step to pool 2
      neighboors = list()  # To keep track of neighboors to a pool
      if ((results[k, final_goal] == 0) && (k != final_goal)) {
        while (expanded$pos != final_goal) {
          # Find the neighbooring pools
          for (i in 1:length(edges[,1])) {
            if (edges[i,1] == expanded$pos) {
              neighboors <- append(neighboors, edges[i,2])
            } 
            else if (edges[i,2] == expanded$pos) {
              neighboors <- append(neighboors, edges[i,1])
            }
          }
          
          # Time to add the neighboors to the frontier
          for (i in 1:length(neighboors)) {
            pos = expanded$pos
            pos_check = neighboors[[i]]
            if (length(expanded$path) == 0) {
              expanded$path <- append(expanded$path, pos)
            }
            node_path <- append(expanded$path, pos_check)  # Path to the node being checked
            node = list(pos = pos_check, 
                        cost = expanded$cost + 1, 
                        path = node_path)  # Node to add to frontier
            
            # Check if pos_check is already in frontier and if so find where
            is_in = 0
            for (i in 1:length(frontier)) {
              if (frontier[[i]]$pos == pos_check) {
                is_in = i
              }
            }
            
            # If node is not in frontier, add it. If it is in frontier, keep the best one
            if (is_in == 0) {
              frontier <- append(frontier, list(node)) 
            }
            else if (frontier[[is_in]]$cost > node$cost) {
              frontier = frontier[-is_in]
              frontier <- append(frontier, list(node))
            }
          }
          
          "
      Find and expand the best node in the frontier
      "
          best_index = list()  # To keep track of the index of the best node/s
          best_cost = 1005  # To keep track of the best score found so far
          
          # Find the index/es with the best cost
          counter = 0
          for (i in 1:length(frontier)) {
            if (best_cost > frontier[[i]]$cost) {
              best_index <- append(best_index, i)
              if (counter > 0) {
                best_index = best_index[-counter]
              }
              else if (counter == 0) {
                counter = counter + 1
              }
              best_cost = frontier[[i]]$cost
            }
            else if (best_cost == frontier[[i]]$cost) {
              best_index <- append(best_index, i)
              counter = counter + 1
            }
          }
          
          # Break potential ties and determine the final best index of frontier
          best_index = best_index[[1]]  # No tie for best score so just grab the best one
          expanded = frontier[[best_index]]  # Reassign expanded to the new best node in frontier
          frontier = frontier[-best_index]  # Remove the new expanded node from the frontier
          
          "
          Add the currently expanded path to the matrix, if not already done
          "
          path = expanded$path
          for (i in 1:floor(length(path)/2)) {
            for (j in 1:length(path)) {
              if ((results[path[[i]],path[[j]]]) == 0 && (i != j) && (path[[i]] != path[[j]])) {
                results[path[[i]],path[[j]]] = path[[i + 1]] 
                results[path[[j]],path[[i]]] = path[[j - 1]]
              }
            }
          }
        }
      }
      final_goal = final_goal - 1
    }
  }
  return(results)
  
  # Goal is found. Handle matrix
  
}

"
Testing of helper functions
"
# Test the best_first function
