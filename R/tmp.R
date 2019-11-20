# library(sf)
# library(PAR)
# 
# 
# st_average_line = function(ptsa, ptsb, a = 0.5){
#   ptsc <- ptsb * (1 - a) + ptsa * a
#   ngeom <- sf::st_cast(sf::st_combine(ptsc), "LINESTRING")
#   return(ngeom)
# } 
# 
# to_borders <- function(pol, nv = 4) {
#   dtol = 1e-10
#   i = 1
#   while (i < 100) {
#     dtol = dtol * 2
#     npol = sf::st_simplify(pol, dTolerance = dtol)
#     if (nrow(sf::st_coordinates(npol)) <= (nv + 1)) break
#     i = i + 1
#   }
#   rcrit = duplicated(rbind(sf::st_coordinates(npol), sf::st_coordinates(pol)))
#   rcrit = which(rcrit[-c(1:(nv + 1))])
#   
#   pts <- sf::st_cast(sf::st_cast(sf::st_cast(sf::st_geometry(pol), "POLYGON"), "LINESTRING"), "POINT")
#   segs <- lapply(1:nv, function(x) {
#     sf::st_cast(sf::st_combine(pts[rcrit[x]:rcrit[x + 1]]), "LINESTRING")[[1]]
#   })
#   segs <- sf::st_as_sf(data.frame(group = 1:nv), geom = sf::st_as_sfc(segs))
#   sf::st_crs(segs) <- sf::st_crs(pol)
#   return(segs)
# }
# 
# 
# creat_lines <- function(la, lb, lc, ld, nrows, ncols) {
#   pts_v <- seq(0, nrows) / nrows
#   ptsa <- sf::st_line_sample(la, density = 1, sample = pts_v)
#   ptsa <- sf::st_cast(ptsa, "POINT")
#   ptsb <- sf::st_line_sample(lb, density = 1, sample = pts_v)
#   ptsb <- sf::st_cast(ptsb, "POINT")
#   
#   pts_h <- seq(0, ncols) / ncols
#   ptsc <- sf::st_line_sample(lc, density = 1, sample = pts_h)
#   ptsc <- sf::st_cast(ptsc, "POINT")
#   ptsd <- sf::st_line_sample(ld, density = 1, sample = pts_h)
#   ptsd <- sf::st_cast(ptsd, "POINT")
#   
#   sll <- lapply(1:length(ptsa), function(i) {
#     nl = st_average_line(ptsc, ptsd, a = pts_v[i])
#     
#     cso = sf::st_coordinates(nl)[, 1:2]
#     cst = sf::st_coordinates(c(ptsa[i], ptsb[i]))
#     cpts = data.frame(cbind(cso[c(1,nrow(cso)),], cst))
#     cpts = rbind(cpts, colMeans(cpts))
#     aft = vec2dtransf::AffineTransformation(cpts)
#     vec2dtransf::calculateParameters(aft)
#     aft@parameters[is.na(aft@parameters)] = 0
#     nl = sf::st_as_sfc(vec2dtransf::applyTransformation(aft, sf::as_Spatial(nl)))
#     return(nl)
#   })
#   
#   
#   sll <- do.call(c, sll)
#   sll <- sf::st_cast(sf::st_sfc(sll, crs = sf::st_crs(la)), "LINESTRING")
#   slf <- sf::st_as_sf(data.frame(id = 1:length(sll)), geometry = sll)
#   return(slf)
# }
# 
# 
# pol = read_sf('D:/UIUC/GDM_2019/Champaign/spatial/boundary_blocks_test.gpkg')
# 
# nrows <- if ('nrows' %in% names(pol)) pol$nrows else 10
# ncols <- if ('ncols' %in% names(pol)) pol$ncols else 10
# invrc <- if ('invrc' %in% names(pol)) pol$invrc else FALSE
# invrn <- if ('invrn' %in% names(pol)) pol$invrn else FALSE
# invcn <- if ('invcn' %in% names(pol)) pol$invcn else FALSE
# invrcn <- if ('invrcn' %in% names(pol)) pol$invrcn  else FALSE
# invp <- if ('invp' %in% names(pol)) pol$invp  else TRUE
# zdesign <- if ('zdesign' %in% names(pol)) pol$zdesign  else FALSE
# 
# 
# l <- to_borders(pol)
# l$L <- sf::st_length(l)
# l <- if (invrc) l[c(2, 1, 4, 3), ] else l
# 
# la = l[1, ]
# lb = st_rev_line(l[3, ])
# lc = l[2, ]
# ld = st_rev_line(l[4, ])
# 
# ll <- creat_lines(la, lb, lc, ld, nrows, ncols)
# 
# ptsl = st_coordinates(ll)[,1:2]
# dim(ptsl) = c(ncols + 1, nrows + 1,  2)
# 
# pols_l = lapply(1:ncols, function(i){lapply(1:nrows, function(j){
#   st_polygon(list(rbind(ptsl[i,j,], ptsl[i+1,j,], ptsl[i+1,j+1,], ptsl[i,j+1,], ptsl[i,j,])))
# })})
# 
# pols_lf <- do.call(c, pols_l)
# idf = data.frame(id = 1:length(pols_lf))
# pols_ldf = st_as_sf(idf, geom = pols_lf)
# 
# write_sf(pols_ldf, 'D:/UIUC/GDM_2019/Champaign/spatial/Trial_pols_test.gpkg')
# 
# 
# # if (zdesign) {
# #   if (invrcn) {
# #     zcrit <- lpts$rid %% 2 == 0
# #     lpts$cid[zcrit] <- 1 + (nrows - lpts$cid[zcrit])
# #   }else{
# #     zcrit <- lpts$cid %% 2 == 0
# #     lpts$rid[zcrit] <- 1 + (nrows - lpts$rid[zcrit])
# #   }
# # }
# 
# 
# 
