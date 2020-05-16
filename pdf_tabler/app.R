
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tesseract)
library(jsonlite)

js <- '
    $(document).ready(function() {
       // define options to pass to bounding box constructor
        var options = {
          url: "https://www.r-project.org/logo/Rlogo.svg",
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

    
    titlePanel("PDF Tabler",
               useShinydashboard()),
    tags$head(
        tags$style(HTML("
               .input-group-btn:first-child > .btn, .input-group-btn:first-child
               > .btn-group { padding: 40px 70px; font-size: 18px;}
               
               .input-group .form-control:last-child, .input-group-addon:last-child, .input-group-btn:first-child > 
               .btn-group:not(:first-child) > .btn, .input-group-btn:first-child > .btn:not(:first-child), 
               .input-group-btn:last-child > .btn, .input-group-btn:last-child > .btn-group > .btn,
               .input-group-btn:last-child > .dropdown-toggle { padding: 52.5px; background-color: white; font-size: 25px;}
               
               label.control-label {font-size: large;}
            "))
    ),
    tags$head(tags$script(HTML(js)),
              tags$head(
                  tags$script(src = "bbox_annotation.js")
              )),
    
 fluidPage(
     br(),
         fileInput("table_pic", "Upload a cropped pic of your table",
                   buttonLabel = "Drag your image file here",
                   multiple = FALSE,
                   width = "100%"),
     fluidRow(
         box("Table Image",
             imageOutput("image")),
         box("Table HTML",
             textOutput("location"),
             textInput("image_url", "Paste image URL"),
             selectInput("category_type", "Label Category", c("animals", "fruits")),
             div(HTML(
                 '<input id="reset_button" type="reset" />'
             )),
             HTML(
                 '<input id="annotation_data" name="annotation_data" type="hidden" />'
             ),
             hr(),
             h4("Entries"),
             verbatimTextOutput("rectangles")
         ),
         box(title = NULL,
             width = 12,
             div(id = "bbox_annotator", style = "display:inline-block"))
     )
 )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$image <- renderImage({
        req(input$table_pic)
        list( src = input$table_pic$datapath,
              width = "100%")
    })
    
    
    
    output$location <- renderText(input$table_pic$datapath)
    
    observeEvent(input$table_pic, {
        #session$sendCustomMessage("change-img-url", paste0("file://", gsub("\\\\", "/", input$table_pic$datapath)) )
        session$sendCustomMessage("change-img-url", input$image_url )
        
        })
    
    #works
    output$rectangles <- renderPrint({

            as.data.frame(jsonlite::fromJSON(input$rectCoord))
    })
    
    
    
    
    observeEvent(input$category_type, {
        vals <- switch(input$category_type, 
                       fruits = list("yellow" = "banana", 
                                     "orange" = "pineapple",
                                     "pink" = "grapefruit"),
                       animals = list("grey" = "raccoon",
                                      "brown" = "dog",
                                      "tan" = "cat")
        )
        # update category list
        session$sendCustomMessage("update-category-list", vals)
        # redraw rectangles
        session$sendCustomMessage("redraw-rects", input$rectCoord)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
