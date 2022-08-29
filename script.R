# clean slate
rm(list = ls(all.names = TRUE))
# load packages
packages = c('dplyr','magrittr','gplots','MASS','lattice','arules','lubridate','ggplot2','gPdtest',
             'rgdal','tidytext','stringr','mapview','tm','caret','shiny','shinydashboard','shinyjs')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
############################
####### Directories ########
############################
working_directory<-"D:/MND/Procurement_Sample/"
data_directory<-"D:/MND/Procurement_Sample/Datasets"
#################################################################################################################
############################
####### Section 1 Q4 #######
############################
# section 1 Q4
# read data
setwd(data_directory)
q4_data<-read.csv("sample_data.csv",stringsAsFactors = FALSE)
setwd(working_directory)

data_char1<-q4_data%>%group_by(Student_Group)%>%summarize(total=sum(Count))
#data characteristic 2
data_char2<-q4_data%>%group_by(Student_Group,Job_Nature)%>%summarize(total=sum(Count))
data_char2<-left_join(data_char2,data_char1,by=c("Student_Group"="Student_Group"))%>%
  mutate(Proportion=total.x/total.y)%>%
  filter(Job_Nature%in%"Unrelated")

q4_data$Industry = factor(q4_data$Industry,levels=(unique(q4_data$Industry)))

#data characteristic 5
q4_data_X<-q4_data%>%filter(Student_Group=="X")
q4_data_Y<-q4_data%>%filter(Student_Group=="Y")

a=c()
for(i in (1:9)){
  a[i]=q4_data$Median_Salary[i]-q4_data$Median_Salary[i+9]
  a[i+9]=-a[i]
}
for (i in (1:length(a))){
  if(a[i]<0){
    a[i]<-0
  }
}
q4_data_difference<-q4_data
q4_data_difference$Difference<-a

#for donut charts
data_char1$fraction=data_char1$total / sum(data_char1$total)
data_char1$ymax = cumsum(data_char1$fraction)
data_char1$ymin = c(0, head(data_char1$ymax, n=-1))
data_char1$labelPosition <- (data_char1$ymax + data_char1$ymin) / 2
data_char1$label <- paste0(data_char1$Student_Group, "\n Total: ", data_char1$total,"\n Proportion: ",100*round(data_char1$fraction,2),"%")

#for stacked bar
data_char2<-data_char2%>%
  ungroup()%>%
  dplyr::select(Student_Group,Job_Nature,Proportion)%>%
  add_row(Student_Group="X",Job_Nature="Related",Proportion=1-.$Proportion[1])%>%
  add_row(Student_Group="Y",Job_Nature="Related",Proportion=1-.$Proportion[2])



graph3_size<-22
graph2_size<-17

### start of shinyapp
shinyApp(
  ui <- fluidPage(
    titlePanel("Visualization Dashboard"),
    
    sidebarLayout(
      sidebarPanel(width = 2,
        radioButtons("rb", "Choose one for Pyramid Plots:",
                     choiceNames = list(
                       "Distibution of Students across Industries",
                       "Median Salary across Industries",
                       "Difference of Median Salary across Industries"
                     ),
                     choiceValues = list(
                       "choice1", "choice2", "choice3"
                     ))
      ),
      mainPanel(
        fluidPage(
          fluidRow(
            column(width=12,
                   height=400,
                   box(title=h2("No. of Students"),
                       width=6,
                       height=400,
                       plotOutput("box1")
                   ),#end of box
                   box(title=h2("Job Nature of Students"),
                       width=6,
                       height=400,
                       plotOutput("box2")
                   ) #end of box
            ),#end of column
            column(width = 12,
                   height=1000,
                   box(title=h2("Pyramid Plots"),
                       width=12,
                       height=1000,
                       plotOutput("plot1")
              
              )#end of box
            )#end of column
          )#end of fluidrow
        )#end of fluidpage
      )#end of main panel
  )
  ),
  server <-shinyServer(function(input, output) {
    
    output$box1<-renderPlot({
      ggplot(data_char1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Student_Group)) +
        geom_rect() +
        coord_polar(theta="y") +
        xlim(c(2, 4))+ 
        theme_void()+
        theme(legend.position = "none")+
        geom_label(x=3.5, aes(y=labelPosition, label=label), size=7)
    })
    
    output$box2<- renderPlot({
      ggplot(data_char2,aes(x=Student_Group,y=Proportion,fill=Job_Nature))+
        geom_bar(stat = "identity")+
        theme(strip.background = element_rect(fill = NA),
              axis.title.x = element_text(size = graph3_size),
              axis.title.y = element_text(size = graph3_size),
              axis.text.x = element_text(size = graph3_size),
              axis.text.y = element_text(size = graph3_size),
              title = element_text(size = graph3_size),
              legend.text = element_text(size = graph3_size),
              legend.title = element_text(size = graph3_size))+
        scale_fill_manual(values=c("#ffb3b3","#ff4d4d"))+
        geom_text(aes(label=paste0(100*round(Proportion,3),"%")),size=6, position = position_stack(vjust=0.5))
    })
    plotchoice<-reactive({
      req(input$rb)
      if(input$rb=="choice1"){
        a<-ggplot(q4_data,aes(x=Industry,fill=Student_Group,label=Count,
                             y=ifelse(test=Student_Group=="X",
                                      yes=Count, no=-Count)))+
          geom_text(
            aes(vjust = ifelse(test=Student_Group=="X",
                               -1, 1)), size = 5
          ) +
          geom_bar(stat = "identity") +
          theme(strip.background = element_rect(fill = NA),
                axis.title.x = element_text(size = graph3_size),
                axis.title.y = element_text(size = graph3_size),
                axis.text.x = element_text(size =graph3_size),
                axis.text.y = element_text(size = graph3_size),
                title = element_text(size = graph3_size),
                legend.text = element_text(size = graph3_size),
                legend.title = element_text(size = graph3_size))+
          scale_y_continuous(labels = abs, limits = max(q4_data$Count) * c(-1,1.2)) +
          xlab("Industry")+
          ylab("Count")+
          ggtitle("Distibution of Students across Industries")+
          labs(fill = "Student Group")
      }
      else if(input$rb=="choice2"){
        a<-ggplot(q4_data,aes(x=Industry,fill=Student_Group,label=Median_Salary,
                           y=ifelse(test=Student_Group=="X",
                                    yes=Median_Salary, no=-Median_Salary)))+
          geom_text(
            aes(vjust = ifelse(test=Student_Group=="X",
                               -1,1)), size = 5
          ) +
          geom_bar(stat = "identity") +
          theme(strip.background = element_rect(fill = NA),
                axis.title.x = element_text(size = graph3_size),
                axis.title.y = element_text(size = graph3_size),
                axis.text.x = element_text(size =graph3_size),
                axis.text.y = element_text(size = graph3_size),
                strip.text = element_text(size = graph2_size),
                title = element_text(size = graph3_size),
                legend.text = element_text(size = graph3_size),
                legend.title = element_text(size = graph3_size))+
          scale_y_continuous(labels = abs, limits = max(q4_data$Median_Salary) * c(-1,1.2)) +
          facet_grid(.~Job_Nature, switch="x",space = "free_x", scales = "free_x")+
          xlab("Industry")+
          ylab("Median Salary")+
          ggtitle("Median Salary across Industries")+
          labs(fill = "Student Group")
      }
      else if(input$rb=="choice3"){
        a<-ggplot(q4_data_difference,aes(x=Industry,fill=Student_Group,label=Difference,
                                      y=ifelse(test=Student_Group=="X",
                                               yes=Difference, no=-Difference)))+
          geom_text(
            aes(vjust = ifelse(test=Student_Group=="X",
                               -1, 1)),data=q4_data_difference%>%filter(Student_Group=="X"), size = 5)+
          geom_bar(stat = "identity") +
          theme(strip.background = element_rect(fill = NA),
                axis.title.x = element_text(size = graph3_size),
                axis.title.y = element_text(size = graph3_size),
                axis.text.x = element_text(size = graph3_size),
                axis.text.y = element_text(size = graph3_size),
                strip.text = element_text(size = graph2_size),
                title = element_text(size = graph3_size),
                legend.text = element_text(size = graph3_size),
                legend.title = element_text(size = graph3_size))+
          scale_y_continuous(labels = abs, limits = max(q4_data_difference$Difference) * c(-1,1.2)) +
          facet_grid(.~Job_Nature, switch="x",space = "free_x", scales = "free_x")+
          xlab("Industry")+
          ylab("Difference of Median Salary")+
          ggtitle("Difference of Median Salary across Industries")+
          labs(fill = "Student Group")
      }
      return(a)
    })
    output$plot1<-renderPlot({
      plotchoice()
    })
  }
)
)
