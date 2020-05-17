
#Librerias
	library(tidyverse); library(ggplot2); library(readr); library(gganimate); library(anytime);


#Carga y ordenamiento Data 
	Data <-read.csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv"))

	funcion_rbin <- function(x) {
		n <- x %>%
		ncol()

		datalist = list()

		for (i in 2:n) {
			
			columna <- x %>%
			select(i) %>%
			colnames()

	  		base <- x %>%
	  		dplyr::select(1,i) %>%
	  		dplyr::mutate(dia = x %>% 
								select(i) %>%
								colnames()) %>%
	  		dplyr::rename(n = all_of(columna)) %>%
	  		dplyr::mutate(dia = gsub("X","",dia),
	  					dia = as.Date(anytime(dia)),
	  					i_loop= i)
	  		datalist[[i]] <- base 
	  }

	 	data = do.call(rbind, datalist) %>%
	 			dplyr::select(Region,dia,n,i_loop) 
	}

	Base <-funcion_rbin(Data) %>%
			dplyr::filter(Region != "Total")


#GGplotmean(Base$dia)
	 a <- ggplot(Base, aes(x=Region, y=n)) + 
	  geom_bar(stat='identity', position = "identity", fill="#1AC000") +
	  geom_text(aes(label=n , y = n - +3),
	              position = position_dodge(width = 0.9)) +
	  xlab("Región") + ylab("Número de casos")+
	  coord_flip() +
	  		labs(label = "N de casos Positivo Covid-19 Chile por Región",
	  			subtitle = "fecha: {frame_time}",
	  			caption = "Fuente Datos: Min. Ciencias\n 
	  							Twitter: S_bravo") + 
	  ggtitle("N de casos Positivo Covid-19 Chile por Región") + 
	  theme_bw() +
	  	theme(
	  	plot.title = element_text(color = "#333333", size = 14, face = "bold", hjust = 0.5),
	  	plot.subtitle = element_text(color = "#1ABF00", size = 12, hjust = 0.5),
	  	plot.caption = element_text(color = "Gray", face = "italic", lineheight = 0.5),
	  	panel.background = element_blank()
	  	)  +
	  transition_time(dia)

	animate(a, nframes = Data %>%
                             ncol(), fps=4)

#Save
	   anim_save("Covid-19.gif", animation = last_animation()
