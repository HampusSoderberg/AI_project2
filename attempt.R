attempt <- function (moveInfo, readings, positions, edges, probs) 
{
  "
  Main function for the algorithm so solve WheresCroc
  "
  
  if (moveInfo$mem$status == 0) {
    paths = best_first(edges)  # Find best first path
    transitions = compute_t(edges)  # Compute transition matrix
    moveInfo$mem$paths = paths
    moveInfo$mem$transitions = transitions
  }
  else {
    paths = moveInfo$mem$paths
    transitions = moveInfo$mem$transitions
  }
  
  # Set up f(t-1)
  if ((moveInfo$mem$status == 0) || (moveInfo$mem$status == 1)) {
    # Set up initial state matrix
    prob = 1/(length(paths) - length(positions))
    f = matrix(data = prob, nrow = 1, ncol = 40)
    for (i in 1:2) {
      if (!is.na(positions[i])) {
        f[1,i] = 0
      }
    }
    moveInfo$mem$status = 2
  }
  else {
    f = moveInfo$mem$f
    for (i in 1:2) {
      if (!is.na(positions[i])) {
        f[1,i] = 0
      }
    }
  }
  
  # Calculate O(t)
  Ot = prob_function(readings, probs)
  
  for (i in 1:2) {
    if (((positions[i] < 0)) && (!is.na(positions[i]))) {
      f = matrix(data = 0, nrow = 1, ncol = 40)
      f[1,-positions[i]] = 1
    }
  }
  
  # Calculate F(t)
  Ft = f %*% transitions %*% Ot
  
  for (i in 1:2) {
    if (((positions[i] > 0)) && (!is.na(positions[i]))) {
      Ft[1, positions[i]] = 0
    }
  }
  
  moveInfo$mem$f = Ft
  
  # Move towards the most likely pool, if most likely pool is reached search
  moves = c(0,0)
  pos = positions[length(positions)]
  for (i in 1:2) {
    # Move towards the most likely pool, if most likely pool is reached search
    best_pool = which.max(Ft[1,])
    best_pool = best_pool[1]  # In case of tie
   
    if (pos == best_pool) {
      moves[i] = 0
      Ft[1, positions[i]] = 0  # Zero out if searched
    }
    else {
      moves[i] = paths[pos,best_pool]
      pos = paths[pos,best_pool]
    }
  }
  
  mv1 = moves[1]
  # Find the 10 most likely pools
  if (Ft[1,best_pool] != 1) {
    tops = Ft[1,]
    tops = sort(tops, decreasing = TRUE)
    tops = tops[-1]
    tops = tops[1:5]  # Take only top 10
    indices = list()
    for (j in 1:length(tops)) {
      indices = append(indices, which(Ft[1,] == tops[j]))
    }
    if (pos%in%indices) {
      moves[2] = 0
    }
  }
  mv2 = moves[2]
  moveInfo$moves = c(mv1, mv2)
  return(moveInfo)
}


"
Helper functions
"
best_first <- function (edges) 
{
  "
  Function to perform a best first search at the beginning of each game to calculate the best 
  path between all of the different positions (pools). This function is to be used once every
  game.
  
  Input: 
    Matrix 'edges' of possible moves to make
  
  Output: 
    Matrix with the best first move to take from one position to another for all 
    combinations of positions. I.e if you check the output matrix's row[i] and column[j]
    you will see the optimal first step to take when you're on position i and want to 
    get to position j.
  " 
  
  # Initialize needed variables
  results = matrix(data = 0, nrow = 40, ncol = 40)  # Result matrix
  final_goal = 0
  
  # Loop through rows
  for (k in 1:length(results[,1])) {
    final_goal = 40  # Reset goal variable
    
    # Loop through columns
    for (l in 1:length(results[1,])/2) {
      # Initialize and reset all the needed variables
      expanded = list(pos = k, cost = 0, path = list())  # Expanded node
      frontier = list(list(pos = 1, cost = 1000, path = list(1)))  # Frontier
      neighbors = list()  # Keep track of neighbors to a position
      
      # Avoid unnecessary best-first runs
      if ((results[k, final_goal] == 0) && (k != final_goal)) {
        
        # Run the actual best first algorithm
        while (expanded$pos != final_goal) {
          
          # Find the neighbooring positions
          for (i in 1:length(edges[,1])) {
            if (edges[i,1] == expanded$pos) {
              neighbors <- append(neighbors, edges[i,2])
            } 
            else if (edges[i,2] == expanded$pos) {
              neighbors <- append(neighbors, edges[i,1])
            }
          }
          
          # Add the neighbors to the frontier
          for (i in 1:length(neighbors)) {
            pos = expanded$pos
            pos_check = neighbors[[i]]
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
          
          # Find the index/es with the best cost
          best_index = list()  # To keep track of the index of the best node/s
          best_cost = 1005  # To keep track of the best score found so far
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
          best_index = best_index[[1]]  # No extra attribute to break ties, grab first one
          expanded = frontier[[best_index]]  # Reassign expanded to the new best node in frontier
          frontier = frontier[-best_index]  # Remove the new expanded node from the frontier
          
          
          # Add the currently expanded path to the matrix, if not already done
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
}

compute_t <- function(edges) {
  "
  Calculate the transition matrix t. Filled with 1/(number of neighbors + 1) for every
  position.
  Input:
    edges: The edges matrix
  Output: 
    40x40 matrix with probability of jumping from row[i] to column[j]
  "
  # Initialize variables
  results = matrix(data = 0, nrow = 40, ncol = 40)  # Result matrix
  
  # Loop through rows of result
  for (k in 1:length(results[,1])) {
    # Find neighbors
    neighbors = list()
    for (i in 1:length(edges[,1])) {
      if (edges[i,1] == k) {
        neighbors <- append(neighbors, edges[i,2])
      } 
      else if (edges[i,2] == k) {
        neighbors <- append(neighbors, edges[i,1])
      }
    }
    # Loop through columns
    for (l in 1:length(results[1,])) {
      is_in = which(neighbors == l)
      if ((length(is_in) > 0) || (k == l)) {
        results[k, l] = 1/(length(neighbors) + 1)
      }
    }
  }
  return(results)
}

prob_function <- function(readings, probs) {
  "
  Function to calculate the probabilities of the croc being in each position based on the 
  readings it gives.
  Input:
    readings: vector of length 3 contating the readings from croc (sal, pho, nit)
    probs: List of matrices of values and deviation for sal, pho, nit for each position
  Output: 
    The observed matrix, 40x40 with 0 in all but diagonal
  "
  # Perform all the dnorms
  sal_dnorm = dnorm(readings[1], probs$salinity[,1], probs$salinity[,2])
  pho_dnorm = dnorm(readings[2], probs$phosphate[,1], probs$phosphate[,2])
  nit_dnorm = dnorm(readings[3], probs$nitrogen[,1], probs$nitrogen[,2])
  
  # Combine them into final probability
  tot_dnorm = sal_dnorm * pho_dnorm * nit_dnorm
  tot_dnorm = tot_dnorm/sum(tot_dnorm)
  
  # Create diagonal matrix
  results = diag(x = tot_dnorm, length(tot_dnorm), length(tot_dnorm))
  return(results)
}
"
NOTES:
  i. Could make it more effective with finding all positions neighbors in one go and 
  not having to search through edges so many times
  
  ii. Add people being eaten
  
  iii. Make sure mem works as it should
  
  iv. 0 prob if we searched
  
  v. if move 1 is 0, move 2 should be second most likely
"