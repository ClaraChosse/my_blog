#Fonction pour renseigner les cooordonnées des points

#' milieu_segment
#' 
#' Cette fonction renvoie le milieu d'un segment AB grâce aux coordonnées de ces deux points
#'
#' @param x_a 
#' @param y_a 
#' @param x_b 
#' @param y_b 
#'
#' @return
#' @export
#'
milieu_segment <- function(x_a, y_a, x_b, y_b) {
  return(c((x_a + x_b)/2, (y_a+y_b)/2))
}

#' divide_triangle
#' 
#' Cette fonction renvoie une liste de triangles
#'
#' @param vect_coord Les coordonnees des 3 points du triangle
#'
#' @return
#' @export
#'
divide_triangle <- function(vect_coord = c(x_a, y_a, x_b, y_b, x_c, y_c)) {
  # Calculer les coordonnées des points moyens
  mid_ab <- milieu_segment(vect_coord[1], vect_coord[2], vect_coord[3], vect_coord[4])
  mid_ac <- milieu_segment(vect_coord[1], vect_coord[2], vect_coord[5], vect_coord[6])
  mid_bc <- milieu_segment(vect_coord[3], vect_coord[4], vect_coord[5], vect_coord[6])

  
  # Découper le triangle en 3 triangles
  triangle1 <- c(vect_coord[1],vect_coord[2], mid_ab[1], mid_ab[2], mid_ac[1], mid_ac[2])
  triangle2 <- c(vect_coord[3],vect_coord[4], mid_ab[1], mid_ab[2], mid_bc[1], mid_bc[2])
  triangle3 <- c(vect_coord[5],vect_coord[6], mid_ac[1], mid_ac[2], mid_bc[1], mid_bc[2])
  
  # Retirer le triangle intérieur
  return(list(triangle1, triangle2, triangle3))
  
}


#' divide_list_triangle
#' 
#' Cette fonction renvoie une nouvelle liste de triangles
#'
#' @param divide_triangle 
#'
#' @return Une nouvelle liste de triangles, avec une division des triangles de la première liste en sous triangles
#' @export
#'
#' @examples
divide_list_triangle <- function(liste_triangle) {
  new_list_triangle <- list()
  #Pour chaque triangle de la liste de triangles on divise le triangle en 3 sous triangles et retourne une liste contenant ces sous triangles. 
  for (i in liste_triangle) {
    new_list_triangle <- append(new_list_triangle, divide_triangle(i) )
  }
  return(new_list_triangle)
}




#' distance_cart
#'
#' @param x1 
#' @param y1 
#' @param x2 
#' @param y2 
#'
#' @return
#' @export
#'
#' @examples
distance_cart <- function(x1, y1, x2, y2) {
  return(sqrt((x2-x1) ^2 + (y2 - y1) ^2))
}



#' heron_liste
#'
#' @param liste_triangle 
#'
#' @return
#' @export
#'
#' @examples
heron_liste <- function(liste_triangle) {
  sum_aire <- 0
  
  for (i in seq_along(liste_triangle)) {
    A_B <- distance_cart(liste_triangle[[i]][1], 
                         liste_triangle[[i]][2], 
                         liste_triangle[[i]][3], 
                         liste_triangle[[i]][4] )
    
    B_C <- distance_cart(liste_triangle[[i]][3], 
                         liste_triangle[[i]][4], 
                         liste_triangle[[i]][5], 
                         liste_triangle[[i]][6] )
    
    C_A <- distance_cart(liste_triangle[[i]][5], 
                         liste_triangle[[i]][6], 
                         liste_triangle[[i]][1], 
                         liste_triangle[[i]][2] )
    
    sum_aire <- sum_aire + heron(A_B, B_C, C_A)
    return(sum_aire)
  }
}





#' plot_triangles
#'
#' @param liste_triangle 
#'
#' @return
#' @export
#'
#' @examples
plot_triangles <- function(liste_triangle) {
  df <- data.frame()
  
  #Construction d'un data frame avec les coordonnées des triangles
  for (i in seq_along(liste_triangle)) {
    df <- rbind(df, data.frame(
      x = c(liste_triangle[[i]][1], liste_triangle[[i]][3], liste_triangle[[i]][5]),
      y = c(liste_triangle[[i]][2], liste_triangle[[i]][4], liste_triangle[[i]][6]),
      triangle_id = i
    ))
  }
  
  #On trace les triangles
  plot <- ggplot(df, aes(x, y, fill = factor(triangle_id))) +
    geom_polygon(color = "black") +
    scale_fill_manual(values = rep("black", length(liste_triangle)), guide = "none") +
    theme_void()
  
  return(plot)
}















