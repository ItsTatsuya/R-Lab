# Initialize an empty adjacency list
initialize_graph <- function(vertices) {
  graph <- vector("list", length = vertices)
  names(graph) <- as.character(1:vertices)
  for (i in 1:vertices) {
    graph[[i]] <- c()
  }
  return(graph)
}

# Function to add an edge between two vertices
add_edge <- function(graph, v1, v2) {
  graph[[as.character(v1)]] <- unique(c(graph[[as.character(v1)]], v2))
  graph[[as.character(v2)]] <- unique(c(graph[[as.character(v2)]], v1))
  return(graph)
}

# DFS function to traverse the graph
dfs <- function(graph, start_vertex) {
  visited <- rep(FALSE, length(graph))
  names(visited) <- names(graph)
  
  # Helper function to perform recursive DFS
  dfs_recursive <- function(v) {
    visited[[as.character(v)]] <<- TRUE
    cat(v, " ")  # Print or store the node as part of the traversal output
    
    # Recur for all the adjacent vertices
    for (neighbor in graph[[as.character(v)]]) {
      if (!visited[[as.character(neighbor)]]) {
        dfs_recursive(neighbor)
      }
    }
  }
  
  # Start DFS from the start_vertex
  dfs_recursive(start_vertex)
}

# Create an undirected graph with 5 vertices
graph <- initialize_graph(5)

# Add edges
graph <- add_edge(graph, 1, 2)
graph <- add_edge(graph, 1, 3)
graph <- add_edge(graph, 2, 4)
graph <- add_edge(graph, 3, 5)

# Perform DFS starting from vertex 1
cat("DFS Traversal starting from vertex 1:\n")
dfs(graph, 1)

