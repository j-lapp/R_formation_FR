library(reactable)

clean_table <- function (df, width){

reactable(data.frame(df), 
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = width,
            headerStyle = list(color= "white", background = "#58585A")
          ),  
          columns = list(
            description = colDef(minWidth = 200)
          ),  
          fullWidth = FALSE,
          bordered = TRUE,
          outlined= FALSE,
          highlight = TRUE)
}

