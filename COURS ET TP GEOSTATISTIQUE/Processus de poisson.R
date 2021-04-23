
######################### Processus ponctuels de Poisson ############################

########## rho de l'énoncé ###############

rho <- function(x, y)  10^3 * x * y

lambda <- integrate(
  function(y){
    sapply(y, function(y){
      integrate(function(x) rho(x,y),0,1)$value
    })
  },0,1
)$value

nb_points <- rpois(1, lambda)

probas <- rho(S[,1], S[,2])/sum(rho(S[,1], S[,2]))
pts <- sample(1:n^2, nb_points, replace = TRUE, prob = probas)
coord <- S[pts,] - matrix(abs(rnorm(2 * nb_points, 0, 0.02)), ncol = 2)

p <- plot_ly(
  type = 'contour',
  x = x,
  y = y,
  z = matrix(rho(S[,1], S[,2]), nrow=n),
  autocontour = TRUE,
  width = 800, height = 600
) %>%
  add_trace(x = coord[,1], y = coord[,2], type = 'scatter', mode = 'markers', color = I('black'))
p


############# rho de l'exo précédent ##########


lambda <- sum(exp(gaussian_vec))

nb_points <- rpois(1, lambda)

probas <- exp(gaussian_vec)/sum(exp(gaussian_vec))
pts <- sample(1:n^2, nb_points, replace = TRUE, prob = probas)
coord <- S[pts,] + matrix(rnorm(2 * nb_points, 0, 0.02), ncol = 2)
coord[coord < 0] <- 0
coord[coord > 1] <- 1

p <- plot_ly(
  type = 'contour',
  x = x,
  y = y,
  z = matrix(gaussian_vec, nrow=n, byrow = TRUE),
  autocontour = TRUE,
  width = 800, height = 600
) %>%
  add_trace(x = coord[,1], y = coord[,2], type = 'scatter', mode = 'markers', color = I('black'))
p



########################## Les forets #####################"

pos_arbre <- c(0.5, 0.5)
plot(pos_arbre[1], pos_arbre[2], pch = 17, cex = 2, col = "darkgreen", xlim = c(0, 1), ylim = c(0, 1))
N1 <- rpois(1, 3)
distances <- apply(pos_arbre - S, 1, dist)
probas <- (1 - distances)/sum(1 - distances)
pts <- sample(1:n^2, N1, prob = probas)
coord <- S[pts, ]
points(coord, pch = 17, cex = 2, col = "darkgreen")
nb_arbre <- 1 + N1
coord_all <- rbind(pos_arbre, coord)

for( i in 1:4) {
  for (k in 1:nb_arbre) {
    pos_arbre <- coord_all[k, ]
    N1 <- rpois(1, 3)
    distances <- apply(pos_arbre - S, 1, dist)
    probas <- (1 - distances)/sum(1 - distances)
    pts <- sample(1:n^2, N1, prob = probas)
    coord <- S[pts, ] + matrix(rnorm(2 * N1, 0, 0.02), ncol = 2)
    points(coord, pch = 17, cex = 2, col = "darkgreen")
    nb_arbre <- nb_arbre + N1
    coord_all <- rbind(coord_all, coord)
  }
}
nb_arbre