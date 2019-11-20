
#' Function to rotate a polygon by a given angle:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
st_rev_line = function(obj){
  ngeom = lapply(1:nrow(obj), function(i){
    coords = sf::st_cast(sf::st_geometry(obj[i,]), "POINT")
    ngeom = sf::st_cast(sf::st_combine(rev(coords)), "LINESTRING")
    return(ngeom)
  })
  ngeom = sf::st_sfc(do.call(c, ngeom))
  nobj = sf::st_set_geometry(obj, ngeom)
  return(nobj)
}


#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
st_average_line = function(ptsa, ptsb, a = 0.5){
  ptsc <- ptsa * a + ptsb * (1 - a)
  ngeom <- sf::st_cast(sf::st_combine(ptsc), "LINESTRING")
  return(ngeom)
} 

#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
to_borders <- function(pol, nv = 4) {
  dtol = 1e-10
  i = 1
  while (i < 100) {
    dtol = dtol * 2
    npol = sf::st_simplify(pol, dTolerance = dtol)
    if (nrow(sf::st_coordinates(npol)) <= (nv + 1)) break
    i = i + 1
  }
  rcrit = duplicated(rbind(sf::st_coordinates(npol), sf::st_coordinates(pol)))
  rcrit = which(rcrit[-c(1:(nv + 1))])
  
  pts <- sf::st_cast(sf::st_cast(sf::st_cast(sf::st_geometry(pol), "POLYGON"), "LINESTRING"), "POINT")
  segs <- lapply(1:nv, function(x) {
    sf::st_cast(sf::st_combine(pts[rcrit[x]:rcrit[x + 1]]), "LINESTRING")[[1]]
  })
  segs <- sf::st_as_sf(data.frame(group = 1:nv), geom = sf::st_as_sfc(segs))
  sf::st_crs(segs) <- sf::st_crs(pol)
  return(segs)
}


#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
create_points_hv <- function(l, nrows, ncols) {
  pts_v <- seq(0, nrows) / nrows
  pts_h <- seq(0, ncols) / ncols
  
  l0 <- expand_lines(l, 10)
  lref <- sf::st_sfc(sf::st_line_sample(l0, density = 1, sample = seq(0, 1, 0.01)))
  ptsa <- sf::st_cast(lref[1], "POINT")
  ptsb <- sf::st_cast(lref[3], "POINT")
  ptsc <- sf::st_cast(lref[2], "POINT")
  ptsd <- sf::st_cast(lref[4], "POINT")
  
  
  ll_v <- lapply(pts_v, function(i) {
    nl = st_average_line(ptsa, ptsb, a = i)
    return(nl)
  })
  ll_v <- do.call(c, ll_v)
  
  
  ll_h <- lapply(pts_h, function(i) {
    nl = st_average_line(ptsc, ptsd, a = i)
    return(nl)
  })
  
  ll_h <- do.call(c, ll_h)
  pts_hv = sf::st_intersection(ll_h, ll_v)
  return(pts_hv)
}

#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
create_trial = function(pols) {
  pols <- st_utm(pols)
  trial_pols <- list()
  for (block in pols$block) {
    pol <- pols[pols$block == block, ]
    nrows <- if ('nrows' %in% names(pol)) pol$nrows else 10
    ncols <- if ('ncols' %in% names(pol)) pol$ncols else 10
    invrc <- if ('invrc' %in% names(pol)) pol$invrc else FALSE
    invrn <- if ('invrn' %in% names(pol)) pol$invrn else FALSE
    invcn <- if ('invcn' %in% names(pol)) pol$invcn else FALSE
    invrcn <- if ('invrcn' %in% names(pol)) pol$invrcn  else FALSE
    zdesign <- if ('zdesign' %in% names(pol)) pol$zdesign  else FALSE
    
    
    l <- to_borders(pol)
    l$L <- sf::st_length(l)
    l <- if (invrc) l[c(2, 1, 4, 3), ] else l
    l[c(3,4),] = st_rev_line(l[c(3,4),])
    
    
    ll <- create_points_hv(l, nrows, ncols)
    
    ptsl = sf::st_coordinates(ll)[,1:2]
    dim(ptsl) = c(ncols + 1, nrows + 1,  2)
    
    pols_l = lapply(1:ncols, function(i){lapply(1:nrows, function(j){
      sf::st_polygon(list(rbind(ptsl[i,j,], ptsl[i+1,j,], ptsl[i+1,j+1,], ptsl[i,j+1,], ptsl[i,j,])))
    })})
    
    pols_lf <- do.call(c, pols_l)
    
    rid <- if (invrn) nrows:1 else 1:nrows
    cid <- if (invcn) ncols:1 else 1:ncols
    idf = data.frame(id = 1:length(pols_lf), rid = rep(rid, ncols), cid = rep(cid, each = nrows))
    
    if (zdesign) {
      if (invrcn) {
        zcrit <- idf$rid %% 2 == 0
        idf$cid[zcrit] <- 1 + (nrows - idf$cid[zcrit])
      }else{
        zcrit <- idf$cid %% 2 == 0
        idf$rid[zcrit] <- 1 + (nrows - idf$rid[zcrit])
      }
    }
    
    lpols = sf::st_as_sf(idf, geom = pols_lf)
    lpols$block <- block
    lpols$bid <- 1:nrow(lpols)
    trial_pols[[as.character(block)]] <- lpols
  }
  trial_pols <- do.call(rbind, trial_pols)
  return(trial_pols)
}
