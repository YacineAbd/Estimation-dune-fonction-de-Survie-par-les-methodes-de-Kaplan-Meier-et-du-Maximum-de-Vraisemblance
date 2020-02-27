#' Title Ajouter un theme aux graphes
#'
#' @param ggplot_graphe 
#'
#' @return
#' @export
#'
#' @examples
theme_ggplot <- function(){
    
    theme(
    plot.background = element_rect(fill = "#ffffff"),
      
    plot.title = element_text(colour = "#02012c", 
                              face = "bold.italic",
                              size = 17),
    
    axis.line = element_line(size = 1, 
                             colour = "#191f22"),
    
    #axis.ticks = element_line(size = 3),
    
    axis.title.x = element_text(face="bold", 
                                colour="#02012c", 
                                size = 15),
    
    axis.text.x  = element_text(#angle = -90, 
                                vjust = 0.5, 
                                size = 12,
                                colour = "#0709a9",
                                face="bold"), 
    
    axis.title.y = element_text(face="bold", 
                                colour="#02012c", 
                                size = 15),
    
    axis.text.y  = element_text(size = 12, 
                                colour = "#0709a9",
                                face="bold")
    )

}
