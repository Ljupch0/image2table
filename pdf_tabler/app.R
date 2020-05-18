
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tesseract)
library(jsonlite)
library(tibble)
library(magick)
library(DT)
library(stringi)
library(dplyr)

js <- '
    $(document).ready(function() {
       // define options to pass to bounding box constructor
        var options = {
          url: "",
          input_method: "select", 
          labels: [""],
          color_list:  [""], 
          onchange: function(entries) {
                Shiny.onInputChange("rectCoord", JSON.stringify(entries, null, "  "));
          }
        };

        // Initialize the bounding-box annotator.
        var annotator = new BBoxAnnotator(options);

        // Initialize the reset button.
        $("#reset_button").click(function(e) {
            annotator.clear_all();
        })

        // define function to reset the bbox
        // ...upon choosing new label category or new url
        function reset_bbox(options) {
          document.getElementById("bbox_annotator").setAttribute("style", "display:inline-block");
          $(".image_frame").remove();
          annotator = new BBoxAnnotator(options);
        }

        // update image url from shiny
        Shiny.addCustomMessageHandler("change-img-url", function(url) {
          options.url = url;
          options.width = null;
          options.height = null;
          reset_bbox(options);
        });

        // update colors and categories from shiny
        Shiny.addCustomMessageHandler("update-category-list", function(vals) {
          options.labels = Object.values(vals);
          options.color_list = Object.keys(vals);
          reset_bbox(options);
        });

        // redraw rectangles based on list of entries
        Shiny.addCustomMessageHandler("redraw-rects", function(vals) {
          var arr = JSON.parse(vals);
          arr.forEach(function(rect){
             annotator.add_entry(rect);
          });
          if (annotator.onchange) {
             annotator.onchange(annotator.entries);
          }
        }); 
    });
'






ui <- fluidPage(

    
    titlePanel("Image2Table",
               useShinydashboard()),
    tags$head(
        tags$style(HTML("
        /*
               .input-group-btn:first-child > .btn, .input-group-btn:first-child
               > .btn-group { padding: 40px 70px; font-size: 18px;}
               
               .input-group .form-control:last-child, .input-group-addon:last-child, .input-group-btn:first-child > 
               .btn-group:not(:first-child) > .btn, .input-group-btn:first-child > .btn:not(:first-child), 
               .input-group-btn:last-child > .btn, .input-group-btn:last-child > .btn-group > .btn,
               .input-group-btn:last-child > .dropdown-toggle { padding: 52.5px; background-color: white; font-size: 25px;}
               
    */           
               .box { font-size: 20px; }
               
               label.control-label {font-size: large;}
               
               #image_url {padding: 21px; border-radius: 5px; }
               
               body {background-color: #e6e6e6;}
               
               h2 {
    margin-left: 15px;
}
            "))
    ),
    tags$head(tags$script(HTML(js)),
              tags$head(
                  tags$script(src = "bbox_annotation.js")
                  )),
    
 fluidPage(
     #br(),
     #    fileInput("table_pic", "Upload a cropped pic of your table",
     #              buttonLabel = "Drag your image file here",
     #              multiple = FALSE,
     #              width = "100%"),
     fluidRow(
         box(
             width = 12,
             p("Image2Table unlocks the data stuck in a table image or PDF report.
                The outcome is an html table that can be copied and pasted into Excel, or immediately downloaded in CSV or Excel format.
               To get started, paste a URL link of an image containing your table."),
             textInput("image_url", "Paste Table Image URL", value = NULL, placeholder="https://i.ibb.co/QnMxcyk/test4.png"),
             
             
             
            # div(HTML(
            #     '<input id="reset_button" type="reset" />'
            # )),
         ),  
         box(title = NULL,
             width = 12,
             p("Select the number of columns you'd like to extract.
               Then draw and tag the column borders on your table image."),
             p("For best results use large images, make your column selections wide and don't select the column names."),
             radioGroupButtons(inputId = "column_number",
                               label = "Number of Columns",
                               selected = character(0),
                               choices=c(1,2, 3, 4, 5, 6, 7, 8, 9, 10),
                               justified = TRUE,
                               size = "lg"
             ),
             div(id = "bbox_annotator", style = "display:inline-block"),
             br(),
             br(),
             splitLayout(actionBttn("crop","Create Table", style = "material-flat", color = "primary", block = TRUE),
                         actionBttn("reset_button", "Reset Selections", style = "material-flat", color="danger", block = TRUE),
                         cellWidths = c("75%", "25%"))
             
             
             #verbatimTextOutput("crop_strings"),
             #verbatimTextOutput("image_crop"),
             #verbatimTextOutput("ocr_text"),
             ),
         box("Liberated Table",
             width = 12,
             DTOutput("final_table") 
         )
         
     )
 )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$image_url, {
            session$sendCustomMessage("change-img-url", input$image_url )
        })
    

    output$rectangles <- renderPrint({
            as_tibble(jsonlite::fromJSON(input$rectCoord))
    })
    
    
    
    
    observeEvent(input$column_number, {
        
        colfunc <- colorRampPalette(c("blue", "orange"))
        colors_vector <- c(colfunc(if (is.na(input$column_number)) 1 else input$column_number))
        
        ncol <- seq(from=1, to = if (is.na(input$column_number)) 1 else input$column_number, by=1)
        cols_vector <- paste0("column", ncol)
        
        vals <- as.list(setNames(cols_vector, colors_vector))
        
        
        # update category list
        session$sendCustomMessage("update-category-list", vals)
        # redraw rectangles
        session$sendCustomMessage("redraw-rects", input$rectCoord)
    })
    
   html_table <- eventReactive(input$crop, {
       crops <- as_tibble(jsonlite::fromJSON(input$rectCoord)) %>% arrange (label)
       
       ncol <- seq(from=1, to = if (is.na(input$column_number)) 1 else input$column_number, by=1)
       cols_vector <- paste0("column", ncol)
   
       
       crop_strings <- paste0(crops$width,"x", crops$height, "+", crops$left, "+", crops$top)
       
       crop_strings <- as.list(setNames( crop_strings, cols_vector))
       
       #output$crop_strings <- renderPrint({crop_strings})
       
       table_image <- image_read(input$image_url)
       
       images_cropped <- map(crop_strings, .f = image_crop, image = table_image)
       
       #output$image_crop <- renderPrint(images_cropped)
       
       
       ocr_text <- map(images_cropped, ocr, engine = tesseract('eng'))
       ocr_text <- str_split(ocr_text,  pattern="\\n")
       
       #output$ocr_text <- renderPrint(ocr_text)
       
       names(ocr_text) <- cols_vector
       
       ocr_text <- map(ocr_text, stri_remove_empty)
       
       as_tibble(do.call(cbind, ocr_text))
   })
   
   
   output$final_table <- renderDT({
       datatable(data=html_table(),
                 style = "bootstrap",
                 selection = "none",
                 extensions = c('Buttons'),
                 rownames = FALSE,
                 options = list(dom = 'Bt',
                                buttons = c('copy', 'csv', 'excel'),
                                scrollY = 500,
                                paging = FALSE
                                )
                 
                 )
       })
   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
