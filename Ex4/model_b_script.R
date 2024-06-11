library(httr)
library(jsonlite)

# Function to call OpenAI API for Model B
chat_gpt_interaction <- function(message, api_key) {
  url <- "https://api.openai.com/v1/chat/completions"
  
  body <- list(
    model = "gpt-4",  # Hypothetical model, adjust as needed
    messages = list(
      list(role = "user", content = message)
    ),
    max_tokens = 150,
    temperature = 0.7
  )
  
  response <- POST(
    url,
    add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  if (status_code(response) != 200) {
    stop("API request failed with status code ", status_code(response), ": ", content(response, as = "text", encoding = "UTF-8"))
  }
  
  content <- content(response, as = "text", encoding = "UTF-8")
  json_content <- fromJSON(content)
  
  response_text <- json_content$choices[[1]]$message$content
  total_tokens <- json_content$usage$total_tokens
  
  return(list(response_text = response_text, total_tokens = total_tokens))
}

# Path to the shared file
shared_file <- "conversation.txt"

# API Key setup
api_key <- Sys.getenv("OPENAI_API_KEY")

while (TRUE) {
  # Wait for Model A's response from the shared file
  Sys.sleep(5)
  
  # Read Model A's response
  try({
    message_a <- readLines(shared_file, warn = FALSE)
    message_a <- paste(message_a, collapse = "\n")
    
    # Call Model B with Model A's response
    cat("Model A says:\n", message_a, "\n\n")
    response_b <- chat_gpt_interaction(message_a, api_key)
    message_b <- response_b$response_text
    
    # Write Model B's response to the shared file
    writeLines(message_b, shared_file)
    
    cat("Model B says:\n", message_b, "\n\n")
    
    Sys.sleep(5)
  }, silent = TRUE)
}