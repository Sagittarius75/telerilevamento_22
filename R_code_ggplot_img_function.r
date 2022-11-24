##### let's write a Function to create ggplot images #####

ggplot_img <- function(temp_obj, temp_title)  {
      # 'function' Function (base package) creates a function in R. The Arguments 'temp_obj' 
      # and 'temp_title' are the ones we pass. We return 'ggp_obj'

    if (typeof(temp_obj) == "S4")  {
        ggp_obj <- ggplot() +
        geom_raster(temp_obj, mapping =aes(x=x, y=y, fill=layer)) +
        scale_fill_viridis(option = "inferno") +
        ggtitle(temp_title)
        # 'ggplot Function (ggplot2 package) creates a new ggplot. 
        # 'ggplot()' initializes a ggplot object.
        # geom_raster Function (ggplot2 package) permits to create the rectangle for plotting.
        # 'scale_fill_viridis' Function (viridis package) is used to choose a Viridis Color 
        # Scales for ggplot2.
        # 'ggtitle' Function (ggplot2 package) permits us to write a title to the plot
        return(ggp_obj)  # we return the object to the main code, associating the final object.
     }  else if (typeof(temp_obj) == "list")  {
           ggp_obj <- ggplot() +
           geom_raster(temp_obj$map, mapping =aes(x=x, y=y, fill=class)) +
           scale_fill_viridis(option = "inferno") +
           ggtitle(temp_title)
           return(ggp_obj)
     }
}
