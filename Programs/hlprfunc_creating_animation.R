
library(purrr)
library(magick)

create_animation.f <- function(enviro_var){
  
  ev_pngs.v <- list.files(file.path("Results", enviro_var))
  
  ev_mpas.l <- lapply(1:length(ev_pngs.v), FUN = function(f){
    
    one_file.png <- map(file.path("Results", enviro_var, ev_pngs.v[f]), image_read)
    
    return(one_file.png)
    
  })
  
  images <- image_join(ev_mpas.l)
  animation <- image_animate(images, fps = 5)
  image_write(animation, file.path("Results", paste0(enviro_var, ".gif")))
              
}
  
  
  
  