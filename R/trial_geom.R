usethis::use_package("sf")
usethis::use_package("dplyr")


#' Function to get the heading angle between two points:
#'
#' This take in any two point features of class 'sf' and returns
#'  heading angle between the two points:
#'
#' @param x point feature of class 'sf'
#' @param y point feature of class 'sf'
#' @keywords Angle, heading, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' points <- sf::read_sf("data/Points.gpkg")
#' x <- sf::st_coordinates(points[1, 4])
#' y <- sf::st_coordinates(points[2, 4])
#' }
#' calc_angle(x, y)
calc_angle <- function(x, y) {
  dst_diff <- as.numeric(x - y)
  return(atan2(dst_diff[1], dst_diff[2]) + pi)
}


#' Function to get the heading angle between all consecutive points:
#'
#' This function take a point data frame of class 'sf' and return the heading angle between the
#' consecutive points. In other words, the result is a list of the direction of travel for each point:
#'
#' @param df data frame of class 'sf'
#' @keywords Angle, Heading, Simple Features, sf
#' @export
#' @examples
#' library(sf)
#' points <- read_sf("data/Points.gpkg")
#' calc_direction(points)
#' \dontrun{
#'
#' }
calc_direction <- function(df) {
  coords <- sf::st_coordinates(df)
  result <- sapply(c(2:nrow(coords)), function(x) {
    calc_angle(coords[x - 1, ], coords[x, ])
  })
  return(c(result[1], result))
}


usethis::use_package("sf")

#' Tranform any projection to UTM for a sf object:
#'
#' This take in any sf geometries and returns
#'  a new sf geometry with the right UTM zone:
#' @param sf_obj  of class 'sf'
#' @keywords UTM, Projection, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#' st_utm(fields)
#' }
st_utm <- function(sf_obj) {
  # Function to get UTM Zone from mean longitude:
  long2UTM <- function(long) {
    (floor((long + 180) / 6) %% 60) + 1
  }

  # Check if the object class is 'sf':
  obj_c <- class(sf_obj)[1]
  if (obj_c == "sf") {
    # In case the object has no projectin assigned,
    #  assume it to geographic WGS84 :
    if (is.na(sf::st_crs(sf_obj))) {
      sf::st_crs(sf_obj) <- sf::st_crs(4326)
    }

    # Get the center longitude in degrees:
    bb <- sf::st_as_sfc(sf::st_bbox(sf_obj))
    bb <- sf::st_transform(bb, sf::st_crs(4326))

    # Get UTM Zone from mean longitude:
    utmzone <- long2UTM(mean(sf::st_bbox(bb)[c(1, 3)]))

    # Get the hemisphere based on the latitude:
    NS <- 100 * (6 + (mean(sf::st_bbox(bb)[c(2, 4)]) < 0))

    # Add all toghether to get the EPSG code:
    projutm <- sf::st_crs(32000 + NS + utmzone)

    # Reproject data:
    sf_obj <- sf::st_transform(sf_obj, projutm)
    return(sf_obj)
  } else {
    options(error = NULL)
    stop("Object class is not 'sf', please insert a sf object!")
  }
}

#' Function to get distances between consecutive points:
#'
#' This goes through every point in sf_obj and sums the distances between each consecutive point. If parameter max is FALSE sf_obj should
#' be a point feature. If max = TRUE, sf_obj should be a polygon and it will return the diagonal distance of the field.
#' @param sf_obj object of class 'sf'
#' @param max conditional variable - default = FALSE
#' @keywords Distance, Simple Features, sf
#' @export
#' @examples
#' #Default
#' points =  sf::read_sf('./data/Points.gpkg')
#' DIFMR::calc_distance(points)
#' #max = TRUE
#' field = DIFMR::fields[4,]
#' DIFMR::calc_distance(field, max = TRUE)
#'
calc_distance <- function(sf_obj, max = FALSE) {
  if (max == TRUE) {
    pts <- sf::st_cast(sf::st_as_sfc(sf::st_bbox(sf_obj)), "POINT")
    return(as.numeric(sf::st_distance(pts[1], pts[3])))
  } else {
    ab_dist <- function(a, b) {
      return(sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2))
    }
    coords <- st_coordinates(sf_obj)
    result <- as.numeric(sapply(c(2:nrow(coords)), function(x) {
      ab_dist(coords[x - 1, ], coords[x, ])
    }))
    return(c(result[1], result))
  }
}


#' Function to create a line based in a starting point and
#' an angle or two points:
#'
#' This take in any two sets of sf geometries and returns
#'  the intersects in the same way as the sp over:
#' @param p1 of class 'matrix' as point 1
#' @param p2 of class 'matrix' as point 2
#' @param angle of class 'numeric' to be calculated with p1
#' @param p4s of class 'crs'
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' }
make_line <- function(p1, p2 = NA, angle = 0, p4s = NA) {
  if (all(is.na(p2))) {
    p2 <- p1 - c(sin(angle), cos(angle))
  }
  l <- sf::st_linestring(rbind(p1, p2))
  return(sf::st_sfc(l, crs = p4s))
}

#' Function to expand both extremities of a line:
#'
#' @param line of class 'sfc' to be expanded
#' @param length of class 'numeric' to expand line by
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#' line <- ablines[1, ]
#' line_exp <- expand_line(line, 1000)
#' st_length(line)
#' st_length(line_exp)
#' }
expand_lines <- function(lines, dst = 10000) {
  linesf <- sapply(1:nrow(lines), simplify = FALSE, function(i) {
    line <- lines[i, ]
    cl <- sf::st_coordinates(line)[, 1:2]
    angle <- calc_angle(cl[2, ], cl[1, ])
    p0 <- cl[1, ] + dst * c(sin(angle), cos(angle))
    n <- nrow(cl)
    angle <- calc_angle(cl[n - 1, ], cl[n, ])
    pn <- cl[n, ] + dst * c(sin(angle), cos(angle))
    nc <- rbind(p0, cl, pn)
    nl <- sf::st_linestring(nc)
    nl <- sf::st_sfc(nl, crs = sf::st_crs(line))
    nl <- sf::st_simplify(nl, dTolerance = 0.1)
    line <- sf::st_set_geometry(line, nl)
    return(line)
  })
  linesf <- do.call(rbind, linesf)
  return(linesf)
}

#' Function to generate parallel straight lines:
#'
#' This takes in a line and draws parallel lines to it
#' across the field, separated by the distance given in width
#' @param line of class 'sfc' to draw lines parallel to
#' @param width of class 'numeric' to separate parallel lines by
#' @param field of class 'sf'
#' @param max_dist of class 'numeric'
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' }
parallel_lines <- function(line, width, field, offset_path = 0, max_dist = NA, clip = FALSE) {
  if (is.na(max_dist)) {
    max_dist <- calc_max_dist(field)
  }
  cl <- sf::st_coordinates(line)[, 1:2]
  angle <- calc_angle(cl[1, ], cl[nrow(cl), ]) + pi / 2
  n <- ceiling(max_dist / width)
  nl <- sapply(seq(-n, n), simplify = FALSE, function(w) {
    sf::st_geometry(line) + (offset_path + w) * width * c(sin(angle), cos(angle))
  })

  path_lines <- sf::st_sfc(do.call(rbind, nl), crs = sf::st_crs(line))

  # Remove the lines outside the field:
  fieldb <- sf::st_union(sf::st_buffer(field, width / 2))
  crit <- !is.na(as.numeric(sf::st_intersects(path_lines, fieldb)))
  path_lines <- path_lines[crit]
  df_id <- data.frame(id = 1:length(path_lines))
  path_lines <- sf::st_as_sf(df_id, geometry = path_lines)
  if (clip) {
    fieldb <- sf::st_buffer(field, width)
    sf::st_agr(path_lines) <- "constant"
    path_lines <- sf::st_intersection(path_lines, sf::st_geometry(fieldb))
    sf::st_agr(path_lines) <- "constant"
    path_lines <- sf::st_cast(sf::st_cast(path_lines, "MULTILINESTRING"), "LINESTRING")
  }
  return(path_lines)
}

#' Function to generate polygon coverage over a field:
#'
#' This take in any two sets of sf geometries and returns
#'  the intersects in the same way as the sp over:
#' @param path_lines of class 'sf' containing parallel lines drawn across field
#' @param width of class 'numeric' representing distance between lines
#' @param field of class 'sf'
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' }
get_coverage <- function(path_lines, width, field) {

  # Make the polygons path from the lines:
  path_pols <- sf::st_buffer(path_lines, width / 2)
  sf::st_agr(path_pols) <- "constant"
  path_pols <- sf::st_intersection(path_pols, field)
  sf::st_agr(path_pols) <- "constant"
  path_pols <- sf::st_cast(sf::st_cast(path_pols, "MULTIPOLYGON"), "POLYGON")
  return(path_pols)
}



#' Function to generate polygon from each point:
#' TODO: Split in simpler functions
#'
#' This take in any two sets of sf geometries and returns
#'  the intersects in the same way as the sp over:
#' @param pts of class 'sf' to intersect
#' @param w of class 'sf' to be intersected
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' }
creat_polygons <- function(pts, w) {
  n <- nrow(pts)
  coords <- sf::st_coordinates(pts)
  Distance <- calc_distance(pts)
  Width <- rep(w, n)
  angle <- -calc_direction(pts) + pi / 2
  critp1 <- c(TRUE, rep(FALSE, n - 1))
  critp2 <- c(critp1[2:n], TRUE)

  coordsx <- ((0.5 * Distance) %*% cbind(-1, 1, 1, -1))
  coordsy <- ((Width * 0.5) %*% cbind(-1, -1, 1, 1))

  coordsxn <- cos(angle) * coordsx - sin(angle) * coordsy
  coordsyn <- sin(angle) * coordsx + cos(angle) * coordsy

  coordsx <- as.numeric(coords[, 1] + coordsxn)
  coordsy <- as.numeric(coords[, 2] + coordsyn)
  n4 <- n * 4

  pf <- data.frame(
    POS = 1 + ((1:n4 - 1) %/% n),
    ID = ifelse(1:n4 %% n == 0, n, 1:n4 %% n),
    X = coordsx, Y = coordsy,
    Xa = coordsx, Ya = coordsy,
    Xm = coordsx, Ym = coordsy
  )

  ii <- cbind(c(1, 4), c(2, 3))
  for (i in 1:2) {
    for (j in 3:4) {
      crit1 <- which(pf$POS == ii[i, 1])
      crit2 <- which(pf$POS == ii[i, 2])
      critd <- crit1[c(2:n, n)]
      crita <- crit2[c(1, 1:(n - 1))]
      critf <- c(crit1[!critp1], crit2[!critp2])
      pf[crit1, j + 2] <- pf[crita, j]
      pf[crit1, j + 4] <- rowMeans(pf[crit1, c(j, j + 2)])
      pf[crit2, j + 4] <- pf[critd, j + 4]
      pf[critf, j] <- pf[critf, j + 4]
    }
  }

  pfo <- as.matrix(pf[order(pf$ID, pf$POS), ])
  pols <- sapply(1:n, simplify = FALSE, function(x) {
    sf::st_polygon(list(pfo[4 * (x - 1) + c(1:4, 1), 3:4]))
  })
  polss <- sf::st_sfc(pols, crs = sf::st_crs(pts))
  polss <- sf::st_set_geometry(pts, polss)
  return(polss)
}

#' Function to create the subpolygons in each pass:
#'
#' This takes in the path lines and path polygons
#' and divides the polygons into subpolygons
#'
#' @param path_lines of class 'sf' containing parallel lines
#' @param path_pols of class 'sf' containing polygons along the lines
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' }
get_subpols <- function(path_lines, path_pols, plot_width, subplot_length) {
  polsf <- sapply(path_lines$id, simplify = FALSE, function(i) {
    l <- path_lines[path_lines$id == i, ]
    pth <- sf::st_geometry(path_pols[path_pols$id == i, ])
    pts <- sf::st_line_sample(l, density = 1 / subplot_length)
    pts <- sf::st_cast(pts, "POINT")
    pts <- sf::st_as_sf(data.frame(id = 1:length(pts)), pts)
    pols <- creat_polygons(pts, plot_width)
    sf::st_agr(pols) <- "constant"
    pols <- sf::st_intersection(pols, sf::st_union(pth))
    if (nrow(pols)) {
      pols <- dplyr::mutate(pols, id = dplyr::row_number())
      pols$pid <- i
    }
    return(pols)
  })
  pols <- do.call(rbind, polsf)
  pols$Area <- as.numeric(sf::st_area(pols))
  pols <- pols[pols$Area > 0, ]
  ov <- st_over(sf::st_centroid(sf::st_geometry(pols)), path_pols)
  pols$Type <- path_pols$Type[ov]
  return(pols)
}

#' Function to create the experimental units based on the subpolygons:
#'
#' This take in any two sets of sf geometries and returns
#'  the intersects in the same way as the sp over:
#' @param spols of class 'sf'
#' @param plot_length of class 'numeric'
#' @param subplot_length of class 'numeric'
#' @param min_length of class 'numeric'
#' @keywords Simple Features, sf
#' @export
#' @examples
#' \dontrun{
#'
#' }
group_subpols <- function(spols, plot_length, subplot_length,
                          max_gap = Inf, min_length = 0,
                          fixed_length = FALSE, invert_side = FALSE) {
  pols_cnt <- sf::st_centroid(sf::st_geometry(spols))
  spols$Dist <- calc_distance(pols_cnt)
  spols$bid <- cumsum(spols$Dist > (max_gap + subplot_length))
  spols$bid <- spols$bid + 1e9 * spols$pid
  spols <- dplyr::group_by(spols, bid)

  plot_n <- plot_length / subplot_length
  spols <- dplyr::add_tally(spols)
  spols$Poln <- round(spols$n / plot_n)
  spols <- spols[spols$Poln > 0, ]

  min_n <- min_length / subplot_length
  spols <- spols[spols$n > min_n, ]

  spols <- dplyr::mutate(spols, sid = dplyr::row_number())
  if (fixed_length) {
    spols$rid <- ceiling(spols$sid / plot_n)
    cod <- paste(spols$pid, spols$bid, spols$rid, sep = "_")
    tbl <- table(cod)
    spols <- spols[cod %in% names(tbl[tbl == plot_n]), ]
  } else {
    spols$rid <- ceiling(spols$sid / (spols$n / spols$Poln))
  }
  cod <- list(paste(spols$pid, spols$bid, spols$rid, sep = "_"))
  spols <- sf::st_as_sf(spols)

  spolsag <- aggregate(spols, cod, dplyr::first)
  spolsag$prow <- as.numeric(as.factor(spolsag$pid))
  spolsag <- dplyr::group_by(spolsag, prow)
  fct <- ifelse(invert_side, -1, 1)
  spolsag <- dplyr::mutate(spolsag, pcol = as.numeric(as.factor(fct * id)))
  spolsag <- spolsag[order(spolsag$prow, spolsag$pcol), ]
  spolsag$PolID <- 1:nrow(spolsag)
  return(spolsag)
}


#' Function to make the field headland.
#'
#' This function creates a constant buffer around the field boundary.
#' FOr more complex types of headland, it is suggested to use graphical tools
#' such as QGIS
#' @param field the field boundary of class 'sf'
#' @param pols the trial design polygons of class 'sf'
#' @param width the size of the headland
#' @keywords Trial Design, Headland, Buffer, Border
#' @export
#' @examples
#' plot(make_headland(fields[4, ], 10, 30))
#' plot(make_headland(fields[4, ], 10, 30, pi))
make_headland <- function(field, width1, width2 = 0, angle = pi / 2, inv = FALSE) {
  sf::st_agr(field) <- "constant"
  if (width2 > 0) {
    field_b <- to_line(field)
    delta_angle <- function(x, y) atan2(sin(x - y), cos(x - y))
    head_crit <- abs(delta_angle(field_b$Heading %% pi, angle %% pi)) > pi / 4
    head_crit <- if (inv) !head_crit else head_crit
    field_h <- sf::st_buffer(field_b, head_crit * abs(width2))
    field_tmp <- sf::st_difference(field, sf::st_union(field_h))
  } else {
    field_tmp <- field
  }
  fieldu <- sf::st_buffer(field_tmp, -abs(width1))
  fieldu$Type <- "Trial"
  fieldb <- sf::st_difference(field, sf::st_geometry(fieldu))
  fieldb$Type <- "Headland"
  fieldd <- rbind(fieldu, fieldb)["Type"]
  sf::st_agr(fieldd) <- "constant"
  return(fieldd)
}

#' Function to rotate a polygon by a given angle:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @param a angle (radians)
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
#' @examples
#'
#' st_rotate(fields[1, ], pi / 2)
st_rotate <- function(obj, a) {
  m <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  obj <- sf::st_set_precision(obj, 1e3)
  cnt <- sf::st_centroid(sf::st_as_sfc(sf::st_bbox(obj)))
  cnt <- sf::st_set_precision(cnt, 1e3)
  objr <- (sf::st_geometry(obj) - cnt) * m
  objr <- sf::st_set_precision(objr, 1e3)
  objr <- sf::st_simplify(objr, 1e-3)
  objr <- objr + cnt
  sf::st_geometry(obj) <- objr
  sf::st_crs(obj) <- sf::st_crs(cnt)
  return(obj)
}



#' Function to rotate a polygon by a given angle:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @param a angle (radians)
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
#' @examples
#'
#' sfc_as_cols(fields[1, ])
sfc_as_cols <- function(x, xy_names = c("x", "y")) {
  stopifnot(inherits(x, "sf"))
  geometry <- sf::st_geometry(sf::st_centroid(sf::st_geometry(x)))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(xy_names) == ncol(ret))
  x <- x[, !names(x) %in% xy_names]
  ret <- setNames(ret, xy_names)
  dplyr::bind_cols(x, ret)
}


#' Function to rotate a polygon by a given angle:
#'
#' This take in any features of class 'sf' and returns
#'  the rotate version of the geometries:
#'
#' @param obj feature of class 'sf'
#' @param a angle (radians)
#' @keywords Angle, Rotation, Affine, Simple Features, sf
#' @export
#' @examples
#'
#' to_line(fields[1, ])
to_line <- function(pol) {
  pts <- sf::st_cast(sf::st_cast(sf::st_cast(sf::st_geometry(pol), "POLYGON"), "LINESTRING"), "POINT")
  midx <- which.min(rowSums(sf::st_coordinates(pts)))
  midx <- 1 + (1:length(pts) + midx - 2) %% (length(pts) - 1)
  pts <- pts[midx]
  segs <- sapply(2:length(pts), simplify = FALSE, function(x) {
    sf::st_cast(sf::st_combine(pts[c(-1:0) + x]), "LINESTRING")[[1]]
  })
  Heading <- calc_direction(pts)[-1]
  segs <- sf::st_as_sf(data.frame(Heading), geom = sf::st_as_sfc(segs))
  sf::st_crs(segs) <- sf::st_crs(pol)
  return(segs)
}


#' Function to create trials around points:
#'
#' @param pols
#' @keywords Plots, Trial Design
#' @export
creat_lines <- function(la, lb, np, center = FALSE, invp = TRUE) {
  pts <- seq(0, np) / np
  if (center) {
    pts <- seq(1, np) / np - 0.5 / np
  }
  ptsa <- sf::st_line_sample(la, density = 1, sample = pts)
  ptsa <- sf::st_cast(ptsa, "POINT")

  pts <- if (!is.null(invp) && invp) rev(pts) else pts
  ptsb <- sf::st_line_sample(lb, density = 1, sample = pts)
  ptsb <- sf::st_cast(ptsb, "POINT")
  sll <- lapply(1:length(ptsa), function(i) {
    sf::st_linestring(sf::st_coordinates(c(ptsa[i], ptsb[i])))
  })
  sll <- do.call(c, sll)
  sll <- sf::st_cast(sf::st_sfc(sll, crs = sf::st_crs(la)), "LINESTRING")
  slf <- sf::st_as_sf(data.frame(id = 1:length(sll)), geometry = sll)
  return(slf)
}

#' Function to create trials around points:
#'
#' @param pols
#' @keywords Plots, Trial Design
#' @export
make_trial <- function(pols) {
  pols <- st_utm(pols)
  trial_pols <- list()
  for (block in pols$block) {
    pol <- pols[pols$block == block, ]
    nrows <- if (is.null(pol$nrows)) 10 else pol$nrows
    ncols <- if (is.null(pol$ncols)) 10 else pol$ncols
    invrc <- if (is.null(pol$invrc)) FALSE else pol$invrc
    invrn <- if (is.null(pol$invrn)) FALSE else pol$invrn
    invcn <- if (is.null(pol$invcn)) FALSE else pol$invcn
    invrcn <- if (is.null(pol$invrcn)) FALSE else pol$invrcn
    invp <- if (is.null(pol$invp)) TRUE else pol$invp
    zdesign <- if (is.null(pol$zdesign)) FALSE else pol$zdesign

    l <- to_line(pol)
    l <- if (which.max(l$L) %% 2 == 1) l[c(2, 1, 4, 3), ] else l
    l <- if (invrc) l[c(2, 1, 4, 3), ] else l

    lra <- creat_lines(l[1, ], l[3, ], nrows, center = TRUE, invp = invp)
    lra$rid <- if (invrn) rev(lra$id) else lra$id
    lca <- creat_lines(l[2, ], l[4, ], ncols, center = TRUE, invp = invp)
    lca$cid <- if (invcn) rev(lca$id) else lca$id

    lpts <- sf::st_intersection(sf::st_set_agr(lra, "constant"), sf::st_set_agr(lca, "constant"))
    zcrit <- lpts$rid %% 2 == 0
    lpts$cid[zcrit] <- if (zdesign) 1 + (nrows - lpts$cid[zcrit]) else lpts$cid[zcrit]

    lr <- creat_lines(l[1, ], l[3, ], nrows, invp = invp)
    lc <- creat_lines(l[2, ], l[4, ], ncols, invp = invp)
    ll <- rbind(lr, lc)

    llb <- sf::st_union(sf::st_buffer(ll, 0.001))
    lpols <- sf::st_difference(sf::st_set_agr(pol, "constant"), llb)
    lpols <- sf::st_cast(sf::st_cast(sf::st_set_agr(lpols, "constant"), "MULTIPOLYGON"), "POLYGON")

    ov <- st_over(lpols, lpts)
    lpols$row <- lpts$rid[ov]
    lpols$col <- lpts$cid[ov]
    lpols <- lpols[order(lpols$col, lpols$row), ]

    lpols$block <- block
    lpols$bid <- 1:nrow(lpols)
    trial_pols[[as.character(block)]] <- lpols
  }
  trial_pols <- do.call(rbind, trial_pols)
  if (invrcn) {
    trial_pols <- dplyr::mutate(trial_pols, id = order(order(block, row, col)))
  } else {
    trial_pols <- dplyr::mutate(trial_pols, id = order(order(block, col, row)))
  }
  trial_pols <- trial_pols[order(trial_pols$id), ]
  return(trial_pols)
}
