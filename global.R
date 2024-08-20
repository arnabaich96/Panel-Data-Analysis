

get_cohort_plot <- function(temp2){

  coh <- temp2 |>
    select(`Strata Group`) |>
    unique() |>
    unlist()
  unt <- temp2 |>
    select(Unit) |>
    unique() |>
    unlist()

  sd <- crosstalk::SharedData$new(temp2)


  p1_raw <- ggplot(sd,aes(x=`Visit label`, y=Result, colour=`Screen ID`, group=`Screen ID`)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = min(temp2$LLN),linetype = "dashed",alpha=0.7, size=0.2) +
    geom_hline(yintercept = max(temp2$ULN),linetype = "dashed",alpha=0.7, size=0.2) +
    ylim(min(min(temp2$Result),min(temp2$LLN)),max(max(temp2$Result),max(temp2$ULN)))+
    annotate("text", x = 0.6,
             y = max(temp2$ULN),
             label = c("<b>ULN</b>") , color="black",
             size=2.5) +
    annotate("text", x = 0.6,
             y = min(temp2$LLN),
             label = c("<b>LLN</b>") , color="black",
             size=2.5) +
    ylab(paste0("Results  (",unt,")"))+
    ggtitle(coh)+
    theme(plot.title = element_text(size = 14,color="blue", face = "bold"))


p1_ULN <- sd |>
  ggplot(aes(x=`Visit label`, y=`Fold change from ULN`, colour=`Screen ID`, group=`Screen ID`))+
  geom_point() +
  geom_line() +
  ylim(min(temp2$`Fold change from ULN`),
       max(temp2$`Fold change from ULN`))


  p1_raw1 <- ggplotly(p1_raw, tooltip=c('group','x','y'))

  p1_ULN <- ggplotly(p1_ULN, tooltip=c('group','x','y'))

  p1 <- subplot(style(p1_raw1,showlegend=FALSE),style(p1_ULN,showlegend=TRUE),heights=0.95, nrows=1, shareX = FALSE, shareY = FALSE, titleY = TRUE ,titleX = TRUE , margin = 0.035 )


  p11 <- highlight(p1,on='plotly_selected',
                   off='plotly_relayout',
                   persistent = getOption("persistent", FALSE))



 lab_tbl <- DT::datatable(sd,
                          class = "display",
                          caption =paste0( "Lab data for ",coh,". Date : 22 Nov 2022"),
                          options = list(pageLength=5, scrollX = TRUE, dom = 'lftip'),
                          filter=c('top'),
                          rownames = FALSE)

  crosstalk::bscols(p11,lab_tbl,widths=12)

 }

#
moduleUI <- function(id, temp_df, add_tag ){

  ns <- NS(id)
  AA=tagList()
  # return a list of tags
  for (i in 1:length(colnames(temp_df) ) )
  {
    if (is.numeric(temp_df[[i]]))
    {
      AA[[i]]<-numericInput(ns(paste0(colnames(temp_df)[i],"_add",add_tag) ), paste0(colnames(temp_df)[i],":"),0)
    }
    # else if( is.Date(temp_df[[i]])){
    #   AA[[i]]<-dateInput(paste0(colnames(temp_df)[i],"_add",add_tag),paste0(colnames(temp_df)[i],":"), Sys.Date())
    # }
    else{
      AA[[i]]<-textInput(ns(paste0(colnames(temp_df)[i],"_add",add_tag)), paste0(colnames(temp_df)[i],":") )
    }
  }
  return(AA)
}

### Server part
module_server<- function(input, output, session,temp_df, add_tag) {
  dataframe=reactive({

    inputlist=list()
    for (j in 1:length(colnames(temp_df) ) ){
      inputlist[j]=input[[paste0(colnames(temp_df)[j],"_add",add_tag )]]
    }
    df_matrix=do.call(cbind.data.frame,inputlist)

    df_temp=data.frame(df_matrix)
    colnames(df_temp)=colnames(temp_df)
    df_temp
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}
