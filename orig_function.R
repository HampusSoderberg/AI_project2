orig_function <- function (moveInfo, readings, positions, edges, probs) 
{
  #options = getOptions(positions[3], edges)
  print("Move 1 options (plus 0 for search):")
  #print(options)
  #mv1 = readline("Move 1: ")
  #if (mv1 == "q") {
  #  stop()
  #}
  #if (!mv1 %in% options && mv1 != 0) {
  #  warning("Invalid move. Search ('0') specified.")
  #  mv1 = 0
  #}
  #if (mv1 != 0) {
  #  options = getOptions(mv1, edges)
  #}
  print("Move 2 options (plus 0 for search):")
  #print(options)
  #mv2 = readline("Move 2: ")
  #if (mv2 == "q") {
  #  stop()
  #}
  #if (!mv1 %in% options && mv1 != 0) {
  #  warning("Invalid move. Search ('0') specified.")
  #  mv2 = 0
  #}
  moveInfo$moves = c(mv1, mv2)
  return(moveInfo)
}