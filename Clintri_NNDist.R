#Clintri_NNDist

install.packages("spatstat")
library("spatstat")

# NNDist : permet de calculer la distance des plus proches voisins pour chaque point
# k = nombre de voisins 
dist <- nndist(gamma_spread, k=5)
length(dist)
dist
max(dist)
min(dist)

gamma_spread[7,]$y
dist[7]
xN <- gamma_spread[1,]$x
xA <- gamma_spread[7,]$x
yN <- gamma_spread[1,]$y
yA <- gamma_spread[7,]$y

vec <- sqrt((xN-xA)^2+(yN-yA)^2)
vec<=dist[7]
dist[7]
gamma_spread[1,]
gamma_spread[gamma_spread$document == 'NCT00490633',]$x
nrow(gamma_spread)

dist <- nndist(gamma_spread, k=5)

for(i in 1:nrow(gamma_spread)){
  xN <- gamma_spread[i,]$x
  yN <- gamma_spread[i,]$y
  xA <- gamma_spread[7,]$x
  yA <- gamma_spread[7,]$y
  vec <- sqrt((xN-xA)^2+(yN-yA)^2)
  if(vec <= dist[7]){
    Neighbour$NCTid <- gamma_spread[i,]$document
  }
  return(Neighbour)
}

Neighbour

gamma_spread[7,]
View(gamma_spread)

xN <- gamma_spread[14,]$x
yN <- gamma_spread[14,]$y
xA <- gamma_spread[7,]$x
yA <- gamma_spread[7,]$y
vec <- sqrt((xN-xA)^2+(yN-yA)^2)
vec <= dist[7]
vec
dist[7]
