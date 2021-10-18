#load all libraries
library(methods)
library(base)
library(stats)
library(datasets)

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(broom)
library(gganimate)
library(transformr)
library(gifski)
library(shinybusy)

#load data nilai impor
myfile <- "https://raw.githubusercontent.com/Xeryus01/data/main/impor_fix.csv"
nilai <- read_csv(myfile)
nilai$time <- as.Date(nilai$time, "%m/%d/%Y")

#load data berat impor
myfile <- "https://raw.githubusercontent.com/Xeryus01/data/main/berat_fix.csv"
berat <- read_csv(myfile)
berat$time <- as.Date(berat$time, "%m/%d/%Y")

#load data asal negara impor
myfile <- "https://raw.githubusercontent.com/Xeryus01/data/main/impor_country.csv"
impor <- read_csv(myfile)
impor$time <- as.Date(impor$time, "%m/%d/%Y")
impor$betterTime <- format(as.Date(impor$time), '%B %Y')

#all server function
function(input, output){
  
  #render plot nilai impor
  output$nilai_plot <- renderPlot({
    label <- paste('Nilai Impor dari ', 
                   format(as.Date(input$nilaiFrom), "%d %B %Y"),
                   'sampai',
                   format(as.Date(input$nilaiTo), "%d %B %Y"))
    
    nilai %>%
      filter(time > input$nilaiFrom & time < input$nilaiTo) %>%
      ggplot(aes(x = time, y = value)) +
      geom_line(aes(y = value, color = "Nilai Impor"), size = 1.2) +
      geom_smooth(aes(color = "Trend Linear"), method = "lm", formula = y ~ x, size = 1, se = FALSE, linetype = "dashed") +
      geom_smooth(aes(color = "Trend Kuadratik"), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, linetype = "dashed") +
      labs(title = label,
           x = "Periode",
           y = "Nilai Impor (dalam juta USD)",
           color = "Keterangan") 
  })
  
  #render plot berat impor
  output$berat_plot <- renderPlot({
    label <- paste('Berat Impor dari ', 
                   format(as.Date(input$beratFrom), "%d %B %Y"),
                   'sampai',
                   format(as.Date(input$beratTo), "%d %B %Y"))
    
    berat %>%
      filter(time > input$beratFrom & time < input$beratTo) %>%
      ggplot(aes(x = time, y = value)) +
      geom_line(aes(y = value, color = "Nilai Impor"), size = 1.2) +
      geom_smooth(aes(color = "Trend Linear"), method = "lm", formula = y ~ x, size = 1, se = FALSE, linetype = "dashed") +
      geom_smooth(aes(color = "Trend Kuadratik"), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, linetype = "dashed") +
      labs(title = label,
           x = "Periode",
           y = "Berat Impor (dalam ribu Ton)",
           color = "Keterangan") 
  })
  
  #render grafik dinamis nilai impor (line chart race)
  observeEvent(input$nilai_line, {
    output$line_race <- renderImage({
      show_modal_spinner()
      
      outfile <- tempfile(fileext='.gif')
      
      datat <- nilai %>%
        filter(time > input$dateFrom & time < input$dateTo)
      
      lin_mod <- lm(value ~ time, datat)
      quad_mod <- lm(value ~ poly(time, 2), datat)
      
      aug_mod <- augment(lin_mod)
      new_mod <- augment(quad_mod)
      
      aug_mod$.quad <- new_mod$.fitted
      
      p <- aug_mod %>% 
        ggplot(aes(x=time, y = value)) + 
        geom_line(aes(x=time, y = value, color="Nilai Impor"), size=1) +
        geom_point(size = 3) + 
        geom_text(aes(label=paste("$", value,sep=" ")),color="darkblue",fontface="bold", vjust=-2, size=5)+
        geom_line(aes(y = .fitted, color="Trend Linear"), size = 0.8, linetype = "dashed") +
        geom_text(aes(y=.fitted, x = max(time)+.1, label = sprintf("$ %5.0f", .fitted)), hjust=0, vjust=0, size=5) +
        geom_segment(aes(y=.fitted, xend=max(time), yend = .fitted), linetype=2, colour='blue') +
        geom_line(aes(y = .quad, color="Trend Kuadratik"), size = 0.8, linetype = "dashed") +
        geom_text(aes(y = .quad, x = max(time)+.1, label = sprintf("$ %5.0f", .quad)), hjust=0, vjust=0, size=5) +
        geom_segment(aes(y = .quad, xend=max(time), yend = .quad), linetype=2, colour='blue') +
        view_follow(fixed_y = TRUE)+
        coord_cartesian(clip = 'off') + 
        labs(title = "Nilai Impor Provinsi Papua Barat : {format(as.Date(input$dateFrom), '%B %Y')} sampai {format(as.Date(input$dateTo), '%B %Y')}",
             subtitle = "Tanggal saat ini : {format(as.Date(frame_along), '%B %Y')}",
             x = "Periode",
             y = "Nilai Impor (dalam juta USD)",
             color = "Keterangan") +
        enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
        theme_bw() +
        theme(axis.line = element_line(colour = "black")) +
        transition_reveal(time)
      
      anim_save("outfile.gif", animate(p, fps=3, nframes = 100, height = 720, width = 1600))
      
      remove_modal_spinner()
      
      return(list(src = "outfile.gif",
                  contentType = 'image/gif',
                  height = 720, 
                  width = 1080,
                  alt = "This is alternate text"
      ))
    }, deleteFile = TRUE)
  })
  
  #render grafik dinamis berat impor (line chart race)
  observeEvent(input$berat_line, {
    output$line_race <- renderImage({
      show_modal_spinner()
      
      outfile <- tempfile(fileext='.gif')
      
      datat <- berat %>%
        filter(time > input$dateFrom & time < input$dateTo)
      
      lin_mod <- lm(value ~ time, datat)
      quad_mod <- lm(value ~ poly(time, 2), datat)
      
      aug_mod <- augment(lin_mod)
      new_mod <- augment(quad_mod)
      
      aug_mod$.quad <- new_mod$.fitted
      
      p <- aug_mod %>% 
        ggplot(aes(x=time, y = value)) + 
        geom_line(aes(x=time, y = value, color="Berat Impor"), size=1) +
        geom_point(size = 3) + 
        geom_text(aes(label=paste("$", value,sep=" ")),color="darkblue",fontface="bold", vjust=-2, size=5)+
        geom_line(aes(y = .fitted, color="Trend Linear"), size = 0.8, linetype = "dashed") +
        geom_text(aes(y=.fitted, x = max(time)+.1, label = sprintf("$ %5.0f", .fitted)), hjust=0, vjust=0, size=5) +
        geom_segment(aes(y=.fitted, xend=max(time), yend = .fitted), linetype=2, colour='blue') +
        geom_line(aes(y = .quad, color="Trend Kuadratik"), size = 0.8, linetype = "dashed") +
        geom_text(aes(y = .quad, x = max(time)+.1, label = sprintf("$ %5.0f", .quad)), hjust=0, vjust=0, size=5) +
        geom_segment(aes(y = .quad, xend=max(time), yend = .quad), linetype=2, colour='blue') +
        view_follow(fixed_y = TRUE)+
        coord_cartesian(clip = 'off') + 
        labs(title = "Berat Impor Provinsi Papua Barat : {format(as.Date(input$dateFrom), '%B %Y')} sampai {format(as.Date(input$dateTo), '%B %Y')}",
             subtitle = "Tanggal saat ini : {format(as.Date(frame_along), '%B %Y')}",
             x = "Periode",
             y = "Berat Impor (dalam ribu Ton)",
             color = "Keterangan") +
        enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
        theme_bw() +
        theme(axis.line = element_line(colour = "black")) +
        transition_reveal(time)
      
      anim_save("outfile.gif", animate(p, fps=3, nframes = 100, height = 720, width = 1600))
      
      remove_modal_spinner()
      
      return(list(src = "outfile.gif",
                  contentType = 'image/gif',
                  height = 720, 
                  width = 1080,
                  alt = "This is alternate text"
      ))
    }, deleteFile = TRUE)
  })
  
  #render grafik dinamis asal negara impor (bar chart race)
  observeEvent(input$race, {
    output$bar_race <- renderImage({
      show_modal_spinner()
      
      outfile <- tempfile(fileext='.gif')
      
      gdp_formatted <- impor %>%
        filter(time > input$bar_dateFrom & time < input$bar_dateTo) %>%
        group_by(betterTime) %>%
        mutate(rank = rank(-value),
               Value_rel = value/value[rank==1],
               Value_lbl = paste0(" ",value)
        ) %>%
        group_by(country_name) %>% 
        filter(rank <= 6) %>%
        ungroup()
      
      anim <- ggplot(gdp_formatted, aes(rank, group = country_name, 
                                        fill = as.factor(country_name), color = as.factor(country_name))) +
        geom_tile(aes(y = value/2,
                      height = value,
                      width = 0.9), alpha = 0.8, color = NA) +
        geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
        geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
        coord_flip(clip = "off", expand = FALSE) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_reverse() +
        guides(color = FALSE, fill = FALSE) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x = element_line( size=.1, color="grey" ),
              panel.grid.minor.x = element_line( size=.1, color="grey" ),
              plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
              plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
              plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
              plot.background=element_blank(),
              plot.margin = margin(2,2, 2, 4, "cm")) +
        transition_states(gdp_formatted$time, transition_length = 4, state_length = 1, wrap = FALSE) +
        view_follow(fixed_x = TRUE)  +
        labs(title = 'Jumlah Impor Berdasarkan Asal Negara : {format(as.Date(closest_state), "%B %Y")}',  
             subtitle  = "{format(as.Date(input$bar_dateFrom), '%B %Y')} sampai {format(as.Date(input$bar_dateTo), '%B %Y')}",
             caption  = "Data Source: Berita Resmi Statistik BPS Papua Barat") 
      
      anim_save("outfile.gif", animate(anim, 200, fps = 5,  width = 1200, height = 1000, 
                                       renderer = gifski_renderer(), end_pause = 15, start_pause =  15))
      
      remove_modal_spinner()
      
      return(list(src = "outfile.gif",
                  contentType = 'image/gif',
                  height = 720, 
                  width = 1080,
                  alt = "This is alternate text"
      ))
    }, deleteFile = TRUE)
  })
}
