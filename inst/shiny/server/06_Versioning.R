output$session_info <- renderUI({
    info <- capture.output(sessionInfo())
    HTML(paste0(paste(info, collapse ='<br/>'), "<br/><br/><br/>"))
})