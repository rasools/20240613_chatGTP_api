library(httr)
library(jsonlite)

# Function to call OpenAI API for Model A
chat_gpt_interaction <- function(message, api_key) {
  url <- "https://api.openai.com/v1/chat/completions"
  
  body <- list(
    model = "gpt-3.5-turbo",  # Adjust model as needed
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

# API Key and initial message setup
api_key <- Sys.getenv("OPENAI_API_KEY")
initial_message <- "Let's discuss the types of data available in the project folder."

# Start the conversation
cat("Model A says:\n", initial_message, "\n\n")
response_a <- chat_gpt_interaction(initial_message, api_key)
message_a <- response_a$response_text

# Write the initial response to the shared file
writeLines(message_a, shared_file)

while (TRUE) {
  # Wait for Model B's response from the shared file
  Sys.sleep(5)
  
  # Read Model B's response
  try({
    message_b <- readLines(shared_file, warn = FALSE)
    message_b <- paste(message_b, collapse = "\n")
    
    # Call Model A with Model B's response
    cat("Model B says:\n", message_b, "\n\n")
    response_a <- chat_gpt_interaction(message_b, api_key)
    message_a <- response_a$response_text
    
    # Write Model A's response to the shared file
    writeLines(message_a, shared_file)
    
    cat("Model A says:\n", message_a, "\n\n")
  }, silent = TRUE)
}