library(tidyverse)
library(see)

equal_area <- function(dip){
  round(sin((90-dip)/2*pi/180)*90*2/sqrt(2),2)
}

inv_equal_area <- function(projdip){
  round(90-asin(sqrt(2)/2/90*projdip)*180/pi*2,2)
}

equal_area_trans <- function(){
  scales::trans_new(
    name="equal_area",
    transform=function(dip){equal_area(dip)},
    inverse=function(projdip){inv_equal_area(projdip)}
  )
  
}

RakeToTrend<-function(strike,dip,rake){ 

  #ofst<-ifelse(rake<90,180,0)
  ofst <- 0 # ???

  trend <- (strike + 180/pi*atan(
        tan( rake*pi/180 ) * cos(dip*pi/180)
      ) + ofst) %% 360

  plunge <- 180/pi*asin(
        sin(rake*pi/180)*sin(dip*pi/180)
  )

  return(tibble(trend,plunge)) 
}


#### Not needed if see is loaded...####
# coord_radar <- function(theta = "x", start = 0, direction = 1, ...) {
#   theta <- match.arg(theta, c("x", "y"))
#   r <- ifelse(theta == "x", "y", "x")
#   
#   ggplot2::ggproto(
#     "CordRadar",
#     CoordPolar,
#     theta = theta,
#     r = r,
#     start = start,
#     direction = sign(direction),
#     is_linear = function(coord) {
#       TRUE
#     },
#     ...
#   )
# }

#### Pole to planes: stat_stereo_pole ####
StatStereoPole <- ggproto("StatStereoPole", Stat,
                          compute_group = function(data, scales,convention="strike-rhr") {
                            
                            if(!convention %in% c("strike-rhr","dipdir") ){stop("convention must be strike-rhr or dipdir")}
                            
                            if(convention=="strike-rhr"){
                              
                              data$x <- (data$x+270) %% 360
                            }
                            
                            if(convention=="dipdir"){
                             
                              data$x <- (data$x+180) %% 360
                            }
                            
                            return(data)
                          },

                          required_aes = c("x", "y")
)

stat_stereo_pole <- function(mapping = NULL, data = NULL, geom = "point",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, convention="strike-rhr", ...) {
  layer(
    stat = StatStereoPole, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, convention=convention, ...)
  )
}



#### Planes: stat_stereo_plane ####
# CAREFUL here: stat_* are performed AFTER scaling
# The ideal solution would be to do the projection with a coord_ not a scale_ (coords are done AFTER stat)
# ... but writing coords seems to be difficult, and actively discouraged
# so we need to manually transform the (projected) dip to true one, and back again
StatStereoPlane <- ggproto("StatStereoPlane", Stat,
                          compute_group = function(data, scales,convention="strike-rhr") {
                            
                            ### Inner function - compute the coords of a plane from strike & dip
                            .drawplane<-function(x,y){ # y is transformed, strike (=x) is not
                              
                              strike <-x
                              dip <- inv_equal_area(y)
                                
                              plane <- tibble(rake=seq(-0,180,length.out=100))
                              plane %>% mutate(ofst=case_when(
                                rake > 90  ~180,
                                TRUE ~0
                              )) %>%
                                mutate(
                                  trend=(strike + 180/pi*atan(
                                    tan( rake*pi/180 ) * cos(dip*pi/180)
                                ) + ofst) %% 360
                                ) %>%
                                mutate(
                                  plunge= 180/pi*asin(
                                    sin(rake*pi/180)*sin(dip*pi/180)
                                  )
                                ) %>%
                                mutate(yp=equal_area(plunge)) %>%   # reproject
                                {.} -> plane
                              
                              
                              return(plane) 
                            }
                            
                            if(!convention %in% c("strike-rhr","dipdir") ){stop("convention must be strike-rhr or dipdir")}
                            
                            # Get dip direction
                            if(convention=="strike-rhr"){
                              data$strike <- data$x
                            }
                            
                            if(convention=="dipdir"){
                              data$strike <- (data$x + 270) %% 360 
                            }
                            
                            # Make one group per data
                            idx <- seq(1,nrow(data))
                            data$group <- paste(data$group,idx,sep="_")
                            
                            # Compute the plane for each data
                            data %>% rowwise() %>%
                              mutate(plane=list(.drawplane(strike,y) ) ) %>%
                              unnest(plane) %>%
                              mutate(
                                x=trend,
                                y=yp
                              ) %>%
                              {.} -> data

                            
                            return(data)
                          },
                          
                          required_aes = c("x", "y")
)

stat_stereo_plane <- function(mapping = NULL, data = NULL, geom = "path",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, convention="strike-rhr", ...) {
  layer(
    stat = StatStereoPlane, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, convention=convention, ...)
  )
}


#### Base graphic ####

base_stereo<-list(
    scale_x_continuous(limits=c(0,360),expand=c(0,0),breaks=c(0,90,180,270)),
    scale_y_continuous(limits=c(90,0),expand=c(0,0),breaks=0,trans="equal_area"),
    coord_radar(),
    theme_minimal(),
    theme(axis.text.y=element_blank(),
          axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color=c("black",NA))
    )
 )
