
#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
to_borders <- function(pol, nv = 4) {
  dtol <- 1e-10
  i <- 1
  while (i < 100) {
    dtol <- dtol * 2
    npol <- sf::st_simplify(pol, dTolerance = dtol)
    if (nrow(sf::st_coordinates(npol)) <= (nv + 1)) break
    i <- i + 1
  }
  rcrit <- duplicated(rbind(sf::st_coordinates(npol), sf::st_coordinates(pol)))
  rcrit <- which(rcrit[-c(1:(nv + 1))])

  pts <- sf::st_cast(sf::st_cast(sf::st_cast(sf::st_geometry(pol), "POLYGON"), "LINESTRING"), "POINT")
  segs <- lapply(1:nv, function(x) {
    sf::st_cast(sf::st_combine(pts[rcrit[x]:rcrit[x + 1]]), "LINESTRING")[[1]]
  })
  segs <- sf::st_as_sf(data.frame(group = 1:nv), geom = sf::st_as_sfc(segs))
  sf::st_crs(segs) <- sf::st_crs(pol)
  return(segs)
}


#' Function to rotate a polygon by a given angle:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
st_rev_line <- function(obj) {
  ngeom <- lapply(1:nrow(obj), function(i) {
    coords <- sf::st_cast(sf::st_geometry(obj[i, ]), "POINT")
    ngeom <- sf::st_cast(sf::st_combine(rev(coords)), "LINESTRING")
    return(ngeom)
  })
  ngeom <- sf::st_sfc(do.call(c, ngeom))
  nobj <- sf::st_set_geometry(obj, ngeom)
  return(nobj)
}


#' Function to average to points:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
st_average_points <- function(ptsa, ptsb, a = 0.5) {
  ptsc <- ptsa * a + ptsb * (1 - a)
  ngeom <- sf::st_cast(sf::st_combine(ptsc), "LINESTRING")
  return(ngeom)
}



#' Function to average to lines in batch:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
st_average_lines <- function(ptsa, ptsb, ii, label) {
  ll <- lapply(ii, function(i) {
    nl <- st_average_points(ptsa, ptsb, a = i)
    return(nl)
  })
  ll <- do.call(c, ll)
  idf <- data.frame(id = 1:length(ll), label = label)
  ll <- sf::st_as_sf(idf, geom = ll)
  return(ll)
}


#' Function to average to lines in batch:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
create_lines_hv <- function(l, nrows, nranges) {
  pts_v <- seq(0, nrows) / nrows
  pts_h <- seq(0, nranges) / nranges

  l0 <- expand_lines(l, 10)
  lref <- sf::st_sfc(sf::st_line_sample(l0, density = 1, sample = seq(0, 1, 0.01)))
  lref <- sf::st_set_agr(sf::st_set_geometry(l0, lref), "constant")
  ptss <- sf::st_cast(lref, "POINT")
  ptss <- split(sf::st_geometry(ptss), ptss$group)

  ll_v <- st_average_lines(ptss[[1]], ptss[[3]], pts_v, "v")
  ll_h <- st_average_lines(ptss[[2]], ptss[[4]], pts_h, "h")
  llf <- rbind(ll_v, ll_h)
  llf <- sf::st_set_crs(llf, sf::st_crs(l))
  llf <- sf::st_set_agr(llf, "constant")
  return(llf)
}


#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
create_points_hv <- function(ll) {
  ll_h <- ll[ll$label == "h", ]
  ll_v <- ll[ll$label == "v", ]
  pts_hv <- sf::st_intersection(ll_h, ll_v)
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
create_trial <- function(pols, geom_type = "pol") {
  pols <- st_utm(pols)
  trial_pols <- list()
  for (block in pols$block) {
    pol <- pols[pols$block == block, ]
    nrows <- if ("nrows" %in% names(pol)) pol$nrows else 10
    nranges <- if ("nranges" %in% names(pol)) pol$nranges else 10
    invrc <- if ("invrc" %in% names(pol)) pol$invrc else FALSE

    l <- to_borders(pol)
    l$L <- as.numeric(sf::st_length(l))
    # asp_ratio <- l$L[1] / l$L[2]
    # rc_ratio <- nrows / nranges
    # inv_crit <- which.min(abs(3.7 - rc_ratio * c(asp_ratio, 1 / asp_ratio))) == 2
    # l$group <- if (inv_crit) c(2, 1, 4, 3) else l$group
    l$group <- if (invrc) c(2, 1, 4, 3) else l$group
    l[c(3, 4), ] <- st_rev_line(l[c(3, 4), ])
    ll <- create_lines_hv(l, nrows, nranges)

    if (geom_type == "line") {
      ll$block <- block
      ll <- sf::st_intersection(ll, sf::st_buffer(sf::st_geometry(pol), 0.1))
      trial_pols[[as.character(block)]] <- ll
    } else {
      lpts <- create_points_hv(ll)

      ptsl <- sf::st_coordinates(lpts)[, 1:2]
      dim(ptsl) <- c(nranges + 1, nrows + 1, 2)

      pols_l <- lapply(1:nranges, function(i) {
        lapply(1:nrows, function(j) {
          sf::st_polygon(list(rbind(ptsl[i, j, ], ptsl[i + 1, j, ], ptsl[i + 1, j + 1, ], ptsl[i, j + 1, ], ptsl[i, j, ])))
        })
      })

      pols_lf <- do.call(c, pols_l)
      idf <- data.frame(
        id = 1:length(pols_lf),
        rid = rep(1:nrows, nranges),
        cid = rep(1:nranges, each = nrows)
      )

      lpols <- sf::st_as_sf(idf, geom = pols_lf)
      lpols$block <- block
      lpols$bid <- 1:nrow(lpols)
      trial_pols[[as.character(block)]] <- lpols
    }
  }
  trial_pols <- do.call(rbind, trial_pols)
  trial_pols <- sf::st_set_crs(trial_pols, st_crs(pols))
  trial_pols$id <- 1:nrow(trial_pols)
  return(trial_pols)
}


#' Function to average to lines:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
make_trial_ids <- function(pols) {
  polsl <- list()
  for (block in unique(pols$block)) {
    pol <- pols[pols$block == block, ]
    invrn <- if ('invrn' %in% names(pol)) max(pol$invrn) else FALSE
    invcn <- if ('invcn' %in% names(pol)) max(pol$invcn) else FALSE
    invrcn <- if ('invrcn' %in% names(pol)) max(pol$invrcn) else FALSE
    zdesign <- if ('zdesign' %in% names(pol)) max(pol$zdesign) else FALSE
    
    pol$rid <- pol$rid - (min(pol$rid) - 1)
    pol$cid <- pol$cid - (min(pol$cid) - 1)

    pol$rid <- if (invrn) { (max(pol$rid) + 1) - pol$rid } else{pol$rid}
    pol$cid <- if (invcn) { (max(pol$cid) + 1) - pol$cid } else{pol$cid}
    
    if (zdesign) {
      if (invrcn) {
        zcrit <- pol$rid %% 2 == 0
        pol$cid[zcrit] <- (max(pol$cid) + 1) - pol$cid[zcrit]
      }else{
        zcrit <- pol$cid %% 2 == 0
        pol$rid[zcrit] <- (max(pol$rid) + 1) - pol$rid[zcrit]
      }
    }
    
    if (invrcn) {
      pol <- dplyr::mutate(pol, bid = order(order(rid, cid)))
    } else {
      pol <- dplyr::mutate(pol, bid = order(order(cid, rid)))
    }
    
    polsl[[as.character(block)]] <- pol
  }
  trial_pols <- do.call(rbind, polsl)
  trial_pols <- dplyr::arrange(trial_pols, block, bid)
  trial_pols$idn <- 1:nrow(trial_pols)
  return(trial_pols)
}

