library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(shinyjs)

# Database connection function
get_db_connection <- function() {
  # Create database directory
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Connect to SQLite database
  dbConnect(RSQLite::SQLite(), "data/letters_diary.sqlite")
}

# Initialize database and create table
initialize_database <- function() {
  con <- get_db_connection()
  
  # Create letters table
  create_table_sql <- "
  CREATE TABLE IF NOT EXISTS letters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    recipient TEXT DEFAULT 'Myself',
    letter_content TEXT NOT NULL,
    mood TEXT NOT NULL,
    date_created DATE DEFAULT (date('now')),
    date_modified DATE DEFAULT (date('now'))
  );"
  
  dbExecute(con, create_table_sql)
  dbDisconnect(con)
}

# CRUD Operations
create_letter <- function(title, recipient, content, mood) {
  con <- get_db_connection()
  tryCatch({
    query <- "INSERT INTO letters (title, recipient, letter_content, mood) VALUES (?, ?, ?, ?)"
    result <- dbExecute(con, query, params = list(title, recipient, content, mood))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    stop(e)
  })
}

read_letters <- function() {
  con <- get_db_connection()
  tryCatch({
    letters <- dbGetQuery(con, "SELECT * FROM letters ORDER BY date_created DESC, id DESC")
    dbDisconnect(con)
    return(letters)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame())
  })
}

get_letter_by_id <- function(id) {
  con <- get_db_connection()
  tryCatch({
    query <- "SELECT * FROM letters WHERE id = ?"
    letter <- dbGetQuery(con, query, params = list(id))
    dbDisconnect(con)
    return(letter)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame())
  })
}

update_letter <- function(id, title, recipient, content, mood) {
  con <- get_db_connection()
  tryCatch({
    query <- "UPDATE letters SET title = ?, recipient = ?, letter_content = ?, mood = ?, date_modified = date('now') WHERE id = ?"
    result <- dbExecute(con, query, params = list(title, recipient, content, mood, id))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    stop(e)
  })
}

delete_letter <- function(id) {
  con <- get_db_connection()
  tryCatch({
    query <- "DELETE FROM letters WHERE id = ?"
    result <- dbExecute(con, query, params = list(id))
    dbDisconnect(con)
    return(result)
  }, error = function(e) {
    dbDisconnect(con)
    stop(e)
  })
}

get_recent_letters <- function(limit = 3) {
  con <- get_db_connection()
  tryCatch({
    query <- paste0("SELECT * FROM letters ORDER BY date_created DESC, id DESC LIMIT ", limit)
    letters <- dbGetQuery(con, query)
    dbDisconnect(con)
    return(letters)
  }, error = function(e) {
    dbDisconnect(con)
    return(data.frame())
  })
}

# UI
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;500;600;700&family=Source+Sans+Pro:wght@300;400;600&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    
    tags$style(HTML("
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      @keyframes slideInFromTop {
        from { opacity: 0; transform: translateY(-30px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      @keyframes scaleIn {
        from { opacity: 0; transform: scale(0.9); }
        to { opacity: 1; transform: scale(1); }
      }
      
      @keyframes shake {
        0%, 100% { transform: translateX(0); }
        25%, 75% { transform: translateX(-5px); }
        50% { transform: translateX(5px); }
      }
      
      body {
        background: linear-gradient(135deg, #C6BDA7 0%, #AAA18F 100%);
        min-height: 100vh;
        font-family: 'Source Sans Pro', sans-serif;
        animation: fadeIn 0.6s ease-out;
      }
      
      /* Login Screen Styles */
      .login-container {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100vh;
        background: linear-gradient(135deg, #C6BDA7 0%, #AAA18F 100%);
        display: flex;
        justify-content: center;
        align-items: center;
        z-index: 9999;
      }
      
      .login-box {
        background: rgba(255, 255, 255, 0.95);
        padding: 50px 60px;
        border-radius: 20px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.15);
        text-align: center;
        min-width: 400px;
        animation: scaleIn 0.5s ease-out;
        border: 1px solid #A5AA70;
      }
      
      .login-box.shake {
        animation: shake 0.5s ease-out;
      }
      
      .login-avatar {
        width: 120px;
        height: 120px;
        background: linear-gradient(135deg, #A5AA70 0%, #7A8450 100%);
        border-radius: 50%;
        margin: 0 auto 30px;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 5px 20px rgba(122, 132, 80, 0.3);
      }
      
      .login-avatar i {
        font-size: 60px;
        color: #FFFFFF;
      }
      
      .login-title {
        font-family: 'Playfair Display', serif;
        font-size: 2em;
        color: #7A8450;
        margin-bottom: 10px;
        font-weight: 600;
      }
      
      .login-subtitle {
        color: #AAA18F;
        margin-bottom: 30px;
        font-size: 1.1em;
      }
      
      .login-form {
        margin-bottom: 20px;
      }
      
      .login-input {
        width: 100%;
        padding: 15px 20px;
        border: 2px solid #C6BDA7;
        border-radius: 50px;
        font-size: 1em;
        font-family: 'Source Sans Pro', sans-serif;
        transition: all 0.3s;
        box-sizing: border-box;
        text-align: center;
        letter-spacing: 2px;
      }
      
      .login-input:focus {
        outline: none;
        border-color: #A5AA70;
        box-shadow: 0 0 0 3px rgba(165, 170, 112, 0.3);
      }
      
      .login-input.error {
        border-color: #7A8450;
        animation: shake 0.5s ease-out;
      }
      
      .login-btn {
        width: 100%;
        padding: 15px;
        background: linear-gradient(135deg, #A5AA70 0%, #7A8450 100%);
        color: white;
        border: none;
        border-radius: 50px;
        font-size: 1.1em;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s;
        font-family: 'Source Sans Pro', sans-serif;
        box-shadow: 0 4px 15px rgba(122, 132, 80, 0.3);
      }
      
      .login-btn:hover:not(:disabled) {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(122, 132, 80, 0.4);
      }
      
      .login-btn:disabled {
        opacity: 0.6;
        cursor: not-allowed;
      }
      
      .login-error {
        color: #7A8450;
        margin-top: 15px;
        font-size: 0.9em;
        font-weight: 600;
        min-height: 20px;
      }
      
      .login-attempts {
        color: #AAA18F;
        margin-top: 10px;
        font-size: 0.85em;
      }
      
      .login-locked {
        color: #7A8450;
        font-weight: 600;
        margin-top: 15px;
      }
      
      h1, h2, h3, h4, h5, h6,
      .navbar-brand,
      .recent-card-title,
      .home-greeting,
      .recent-title {
        font-family: 'Playfair Display', serif !important;
        color: #7A8450;
      }
      
      .navbar-default {
        background: rgba(255, 255, 255, 0.98) !important;
        border: none;
        box-shadow: 0 2px 10px rgba(122, 132, 80, 0.1);
        animation: slideInFromTop 0.5s ease-out;
      }
      
      .navbar-default .navbar-brand {
        color: #7A8450 !important;
        font-weight: 700;
        font-size: 1.5em;
      }
      
      .navbar-default .navbar-nav > li > a {
        color: #AAA18F !important;
        font-weight: 600;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .navbar-default .navbar-nav > .active > a {
        background-color: #7A8450 !important;
        color: white !important;
      }
      
      .main-container {
        background: #FFFFFF;
        border-radius: 15px;
        padding: 30px;
        margin: 30px auto;
        max-width: 1200px;
        box-shadow: 0 10px 40px rgba(122, 132, 80, 0.15);
        animation: scaleIn 0.6s ease-out 0.2s both;
        border: 1px solid #C6BDA7;
      }
      
      .home-greeting {
        text-align: center;
        color: #7A8450;
        font-size: 2.2em;
        margin: 40px 0 30px 0;
        font-weight: 600;
        animation: fadeIn 0.8s ease-out 0.4s both;
      }
      
      .write-letter-btn-container {
        text-align: center;
        margin: 40px 0;
        animation: fadeIn 0.8s ease-out 0.6s both;
      }
      
      .write-letter-btn {
        background-color: #A5AA70;
        color: white;
        border: none;
        padding: 15px 40px;
        font-size: 1.1em;
        font-weight: 600;
        border-radius: 50px;
        cursor: pointer;
        transition: all 0.3s;
        box-shadow: 0 4px 15px rgba(165, 170, 112, 0.3);
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .write-letter-btn:hover {
        background-color: #7A8450;
        box-shadow: 0 4px 15px rgba(122, 132, 80, 0.4);
      }
      
      .stats-container {
        display: flex;
        justify-content: space-around;
        margin: 40px 0;
        flex-wrap: wrap;
      }
      
      .stat-card {
        background: linear-gradient(135deg, #A5AA70 0%, #7A8450 100%);
        color: white;
        padding: 30px;
        border-radius: 15px;
        text-align: center;
        min-width: 200px;
        margin: 10px;
        box-shadow: 0 5px 15px rgba(122, 132, 80, 0.2);
      }
      
      .stat-number {
        font-size: 2.5em;
        font-weight: bold;
        margin: 10px 0;
        font-family: 'Playfair Display', serif;
      }
      
      .stat-label {
        font-size: 1em;
        opacity: 0.95;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .recent-section {
        margin-top: 50px;
        animation: fadeIn 1s ease-out 0.8s both;
      }
      
      .recent-title {
        color: #7A8450;
        font-size: 1.9em;
        margin-bottom: 20px;
        border-bottom: 2px solid #A5AA70;
        padding-bottom: 10px;
        font-weight: 600;
      }
      
      .recent-card {
        background: #F8F8F8;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 15px;
        border-left: 4px solid #9CAF88;
        transition: all 0.3s;
      }
      
      .recent-card:hover {
        transform: translateX(5px);
        box-shadow: 0 3px 10px rgba(156, 175, 136, 0.2);
        border-left-color: #7A8450;
      }
      
      .recent-card-title {
        color: #7A8450;
        font-size: 1.4em;
        font-weight: 600;
        margin-bottom: 5px;
      }
      
      .recent-card-to {
        color: #AAA18F;
        font-style: italic;
        margin-bottom: 10px;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .recent-card-preview {
        color: #666;
        line-height: 1.7;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .recent-card-date {
        color: #999;
        font-size: 0.9em;
        margin-top: 10px;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .form-section {
        background: #F8F8F8;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 30px;
        border: 1px solid #C6BDA7;
      }
      
      .btn-primary {
        background-color: #A5AA70;
        border: none;
        padding: 10px 30px;
        font-size: 16px;
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
      }
      
      .btn-primary:hover {
        background-color: #7A8450;
      }
      
      .btn-warning {
        background-color: #9CAF88;
        border: none;
        color: #FFFFFF;
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
      }
      
      .btn-warning:hover {
        background-color: #7A8450;
        color: white;
      }
      
      .btn-danger {
        background-color: #AAA18F;
        border: none;
        color: white;
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
      }
      
      .btn-danger:hover {
        background-color: #8A8270;
      }
      
      .btn-info {
        background-color: #C6BDA7;
        border: none;
        color: #7A8450;
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
      }
      
      .btn-info:hover {
        background-color: #AAA18F;
      }
      
      textarea {
        min-height: 150px;
        font-family: 'Source Sans Pro', sans-serif;
        line-height: 1.6;
        border: 2px solid #C6BDA7;
        border-radius: 8px;
        padding: 15px;
        resize: none !important;
      }
      
      textarea:focus {
        border-color: #A5AA70;
        box-shadow: 0 0 0 0.2rem rgba(165, 170, 112, 0.25);
      }
      
      input[type='text'],
      select,
      .form-control {
        font-family: 'Source Sans Pro', sans-serif;
        border: 2px solid #C6BDA7;
        border-radius: 8px;
      }
      
      input[type='text']:focus,
      select:focus,
      .form-control:focus {
        border-color: #A5AA70;
        box-shadow: 0 0 0 0.2rem rgba(165, 170, 112, 0.25);
      }
      
      label {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
        color: #7A8450;
      }
      
      .filters-section {
        background: #F8F8F8;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        border: 1px solid #9CAF88;
      }
      
      .mood-badge {
        display: inline-block;
        padding: 5px 12px;
        border-radius: 15px;
        font-size: 0.85em;
        font-weight: 600;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .mood-Hopeful { background-color: #9CAF88; color: white; }
      .mood-Happy { background-color: #A5AA70; color: white; }
      .mood-Grateful { background-color: #C6BDA7; color: #7A8450; border: 1px solid #A5AA70; }
      .mood-Melancholic { background-color: #AAA18F; color: white; }
      .mood-Sad { background-color: #8A8270; color: white; }
      .mood-Angry { background-color: #7A8450; color: white; }
      .mood-Regretful { background-color: #9CAF88; color: white; }
      .mood-Neutral { background-color: #E8E6E1; color: #7A8450; }
      .mood-Loving { background-color: #D4C5A7; color: #7A8450; }
      .mood-Anxious { background-color: #B8C6A7; color: white; }
      .mood-Excited { background-color: #A5AA70; color: white; }
      
      .modal-content {
        background-color: #FFFFFF;
        border: 1px solid #C6BDA7;
      }
      
      .modal-header {
        border-bottom-color: #A5AA70;
        background-color: #F8F8F8;
      }
      
      .modal-header h3,
      .modal-header h4,
      .modal-title {
        font-family: 'Playfair Display', serif;
        font-weight: 600;
        color: #7A8450;
      }
      
      .modal-footer {
        border-top-color: #A5AA70;
        background-color: #F8F8F8;
      }
      
      .modal-body p {
        font-family: 'Source Sans Pro', sans-serif;
        line-height: 1.7;
      }
      
      .dataTables_wrapper {
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      table.dataTable thead th {
        font-family: 'Playfair Display', serif;
        font-weight: 600;
        color: #7A8450;
      }
      
      table.dataTable tbody tr.selected,
      table.dataTable tbody tr.selected > * {
        background-color: #9CAF88 !important;
        color: white !important;
      }
      
      table.dataTable tbody tr:hover,
      table.dataTable tbody tr:hover > * {
        background-color: #F0F0E8 !important;
      }
      
      .mood-selector-container {
        margin-bottom: 20px;
      }
      
      .mood-selector-label {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
        color: #7A8450;
        margin-bottom: 10px;
        display: block;
      }
      
      .mood-selector-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 10px;
        margin-bottom: 20px;
      }
      
      @media (max-width: 768px) {
        .mood-selector-grid {
          grid-template-columns: repeat(3, 1fr);
        }
      }
      
      @media (max-width: 480px) {
        .mood-selector-grid {
          grid-template-columns: repeat(2, 1fr);
        }
      }
      
      .mood-option {
        background: white;
        border: 2px solid #C6BDA7;
        border-radius: 10px;
        padding: 15px 10px;
        text-align: center;
        cursor: pointer;
        transition: all 0.3s;
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .mood-option:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(122, 132, 80, 0.1);
      }
      
      .mood-option.selected {
        border-width: 3px;
        box-shadow: 0 4px 12px rgba(122, 132, 80, 0.15);
        transform: translateY(-2px);
      }
      
      .mood-option i {
        font-size: 2em;
        margin-bottom: 5px;
        display: block;
      }
      
      .mood-option .mood-label {
        font-size: 0.9em;
        font-weight: 600;
        margin-top: 5px;
      }
      
      .mood-option[data-mood='Hopeful'] { border-color: #9CAF88; }
      .mood-option[data-mood='Hopeful'].selected { background-color: #9CAF88; color: white; }
      .mood-option[data-mood='Hopeful'] i { color: #9CAF88; }
      .mood-option[data-mood='Hopeful'].selected i { color: white; }
      
      .mood-option[data-mood='Happy'] { border-color: #A5AA70; }
      .mood-option[data-mood='Happy'].selected { background-color: #A5AA70; color: white; }
      .mood-option[data-mood='Happy'] i { color: #A5AA70; }
      .mood-option[data-mood='Happy'].selected i { color: white; }
      
      .mood-option[data-mood='Grateful'] { border-color: #C6BDA7; }
      .mood-option[data-mood='Grateful'].selected { background-color: #C6BDA7; color: #7A8450; border-color: #A5AA70; }
      .mood-option[data-mood='Grateful'] i { color: #A5AA70; }
      .mood-option[data-mood='Grateful'].selected i { color: #7A8450; }
      
      .mood-option[data-mood='Melancholic'] { border-color: #AAA18F; }
      .mood-option[data-mood='Melancholic'].selected { background-color: #AAA18F; color: white; }
      .mood-option[data-mood='Melancholic'] i { color: #AAA18F; }
      .mood-option[data-mood='Melancholic'].selected i { color: white; }
      
      .mood-option[data-mood='Sad'] { border-color: #8A8270; }
      .mood-option[data-mood='Sad'].selected { background-color: #8A8270; color: white; }
      .mood-option[data-mood='Sad'] i { color: #8A8270; }
      .mood-option[data-mood='Sad'].selected i { color: white; }
      
      .mood-option[data-mood='Angry'] { border-color: #7A8450; }
      .mood-option[data-mood='Angry'].selected { background-color: #7A8450; color: white; }
      .mood-option[data-mood='Angry'] i { color: #7A8450; }
      .mood-option[data-mood='Angry'].selected i { color: white; }
      
      .mood-option[data-mood='Regretful'] { border-color: #9CAF88; }
      .mood-option[data-mood='Regretful'].selected { background-color: #9CAF88; color: white; }
      .mood-option[data-mood='Regretful'] i { color: #9CAF88; }
      .mood-option[data-mood='Regretful'].selected i { color: white; }
      
      .mood-option[data-mood='Neutral'] { border-color: #E8E6E1; }
      .mood-option[data-mood='Neutral'].selected { background-color: #E8E6E1; color: #7A8450; }
      .mood-option[data-mood='Neutral'] i { color: #AAA18F; }
      .mood-option[data-mood='Neutral'].selected i { color: #7A8450; }
      
      .mood-option[data-mood='Loving'] { border-color: #D4C5A7; }
      .mood-option[data-mood='Loving'].selected { background-color: #D4C5A7; color: #7A8450; }
      .mood-option[data-mood='Loving'] i { color: #D4C5A7; }
      .mood-option[data-mood='Loving'].selected i { color: #7A8450; }
      
      .mood-option[data-mood='Anxious'] { border-color: #B8C6A7; }
      .mood-option[data-mood='Anxious'].selected { background-color: #B8C6A7; color: white; }
      .mood-option[data-mood='Anxious'] i { color: #B8C6A7; }
      .mood-option[data-mood='Anxious'].selected i { color: white; }
      
      .mood-option[data-mood='Excited'] { border-color: #A5AA70; }
      .mood-option[data-mood='Excited'].selected { background-color: #A5AA70; color: white; }
      .mood-option[data-mood='Excited'] i { color: #A5AA70; }
      .mood-option[data-mood='Excited'].selected i { color: white; }
      
      .modal-dialog textarea,
      #modal_content,
      #edit_content {
        width: 100% !important;
        min-height: 500px !important;
        height: 500px !important;
        font-family: 'Source Sans Pro', sans-serif;
        line-height: 1.8;
        font-size: 15px;
        border: 2px solid #C6BDA7;
        border-radius: 8px;
        padding: 15px;
        resize: none !important;
      }
      
      .modal-dialog textarea:focus,
      #modal_content:focus,
      #edit_content:focus {
        border-color: #A5AA70;
        box-shadow: 0 0 0 0.2rem rgba(165, 170, 112, 0.25);
      }
      
      .modal-body {
        padding: 20px 30px;
      }
      
      .modal-body .form-group {
        margin-bottom: 0;
      }
      
      .modal-dialog.modal-lg {
        max-width: 900px;
      }
      
      /* Music Player Styles */
      .music-toggle-btn {
        position: fixed;
        bottom: 30px;
        right: 30px;
        width: 60px;
        height: 60px;
        background: linear-gradient(135deg, #A5AA70 0%, #7A8450 100%);
        border-radius: 50%;
        border: none;
        color: white;
        font-size: 24px;
        cursor: pointer;
        box-shadow: 0 4px 20px rgba(122, 132, 80, 0.3);
        transition: all 0.3s;
        z-index: 1000;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .music-toggle-btn:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 25px rgba(122, 132, 80, 0.4);
      }
      
      .music-player {
        position: fixed;
        bottom: 30px;
        right: 30px;
        width: 320px;
        background: rgba(255, 255, 255, 0.98);
        border-radius: 20px;
        box-shadow: 0 10px 40px rgba(122, 132, 80, 0.2);
        padding: 20px;
        z-index: 999;
        animation: slideInFromBottom 0.4s ease-out;
        backdrop-filter: blur(10px);
        border: 1px solid #C6BDA7;
      }
      
      .music-player.minimized {
        width: 320px;
        padding: 15px 20px;
      }
      
      .music-player.expanded {
        width: 380px;
        max-height: 600px;
      }
      
      @keyframes slideInFromBottom {
        from { opacity: 0; transform: translateY(50px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .player-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #C6BDA7;
      }
      
      .player-title {
        font-family: 'Playfair Display', serif;
        font-size: 1.2em;
        color: #7A8450;
        font-weight: 600;
      }
      
      .player-controls-header {
        display: flex;
        gap: 10px;
      }
      
      .player-btn {
        background: none;
        border: none;
        color: #7A8450;
        cursor: pointer;
        font-size: 18px;
        transition: all 0.2s;
        padding: 5px;
      }
      
      .player-btn:hover {
        color: #A5AA70;
        transform: scale(1.1);
      }
      
      .track-info {
        text-align: center;
        margin-bottom: 15px;
      }
      
      .track-name {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
        color: #7A8450;
        font-size: 1em;
        margin-bottom: 3px;
      }
      
      .track-artist {
        font-family: 'Source Sans Pro', sans-serif;
        color: #AAA18F;
        font-size: 0.85em;
      }
      
      .progress-container {
        margin-bottom: 15px;
        position: relative;
      }
      
      .progress-bar {
        width: 100%;
        height: 6px;
        background: #C6BDA7;
        border-radius: 3px;
        cursor: pointer;
        position: relative;
        overflow: hidden;
      }
      
      .progress-fill {
        height: 100%;
        background: linear-gradient(90deg, #A5AA70 0%, #7A8450 100%);
        border-radius: 3px;
        width: 0%;
        transition: width 0.1s linear;
      }
      
      .time-display {
        display: flex;
        justify-content: space-between;
        align-items: center;
        font-family: 'Source Sans Pro', sans-serif;
        font-size: 0.8em;
        color: #AAA18F;
        margin-top: 8px;
        width: 100%;
      }
      
      .time-display span {
        font-variant-numeric: tabular-nums;
        font-family: monospace;
        letter-spacing: 1px;
        min-width: 45px;
      }
      
      #current_time {
        text-align: left;
      }
      
      #total_time {
        text-align: right;
      }
      
      .player-controls {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 15px;
        margin-bottom: 15px;
      }
      
      .control-btn {
        background: white;
        border: 2px solid #C6BDA7;
        width: 45px;
        height: 45px;
        border-radius: 50%;
        color: #7A8450;
        font-size: 18px;
        cursor: pointer;
        transition: all 0.3s;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .control-btn:hover {
        border-color: #A5AA70;
        background: #9CAF88;
        color: white;
        transform: scale(1.05);
      }
      
      .control-btn.play-pause {
        width: 55px;
        height: 55px;
        font-size: 22px;
        background: linear-gradient(135deg, #A5AA70 0%, #7A8450 100%);
        color: white;
        border: none;
      }
      
      .control-btn.play-pause:hover {
        transform: scale(1.1);
        box-shadow: 0 4px 15px rgba(165, 170, 112, 0.4);
      }
      
      .volume-container {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 15px;
      }
      
      .volume-icon {
        color: #7A8450;
        font-size: 16px;
        min-width: 20px;
      }
      
      .volume-slider {
        flex: 1;
        height: 6px;
        background: #C6BDA7;
        border-radius: 3px;
        outline: none;
        -webkit-appearance: none;
      }
      
      .volume-slider::-webkit-slider-thumb {
        -webkit-appearance: none;
        width: 16px;
        height: 16px;
        background: #A5AA70;
        border-radius: 50%;
        cursor: pointer;
        transition: all 0.2s;
      }
      
      .volume-slider::-webkit-slider-thumb:hover {
        background: #7A8450;
        transform: scale(1.2);
      }
      
      .playlist-container {
        max-height: 250px;
        overflow-y: auto;
        border-top: 2px solid #C6BDA7;
        padding-top: 10px;
        margin-top: 10px;
      }
      
      .playlist-item {
        padding: 10px;
        background: #F8F8F8;
        border-radius: 8px;
        margin-bottom: 8px;
        cursor: pointer;
        transition: all 0.2s;
        border: 2px solid transparent;
      }
      
      .playlist-item:hover {
        background: #F0F0E8;
        border-color: #9CAF88;
      }
      
      .playlist-item.active {
        background: #9CAF88;
        border-color: #7A8450;
      }
      
      .playlist-item-name {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
        color: #7A8450;
        font-size: 0.9em;
        margin-bottom: 2px;
      }
      
      .playlist-item.active .playlist-item-name {
        color: white;
      }
      
      .playlist-item-artist {
        font-family: 'Source Sans Pro', sans-serif;
        color: #AAA18F;
        font-size: 0.8em;
      }
      
      .playlist-item.active .playlist-item-artist {
        color: rgba(255, 255, 255, 0.9);
      }
      
      .playlist-container::-webkit-scrollbar {
        width: 6px;
      }
      
      .playlist-container::-webkit-scrollbar-track {
        background: #C6BDA7;
        border-radius: 3px;
      }
      
      .playlist-container::-webkit-scrollbar-thumb {
        background: #A5AA70;
        border-radius: 3px;
      }
      
      .playlist-container::-webkit-scrollbar-thumb:hover {
        background: #7A8450;
      }
    ")),
    
    tags$script(HTML("
      // Music player functionality
      $(document).ready(function() {
        // Create audio element
        var audio = new Audio();
        var isPlaying = false;
        var currentTrackIndex = 0;
        var volume = 0.7;
        var isPlayerVisible = true;
        var isPlayerExpanded = false;
        
        // MP3 FILES PLAYLIST
        var playlist = [
          {
            name: 'Ale',
            artist: 'The Bloomfields',
            file: 'audio/ale.mp3',
            duration: 0
          },
          {
            name: 'Binibini',
            artist: 'Brownman Revival',
            file: 'audio/binibini.mp3',
            duration: 0
          },
          {
            name: 'Everybody Wants To Rule The World',
            artist: 'Tears for Fears',
            file: 'audio/everybody-wants-to-rule-the-world.mp3',
            duration: 0
          },
          {
            name: 'Isang Linggong Pag-ibig',
            artist: 'Imelda Papin',
            file: 'audio/isang-linggong-pag-ibig.mp3',
            duration: 0
          },
          {
            name: 'Kabisado',
            artist: 'IV of Spades',
            file: 'audio/kabisado.mp3',
            duration: 0
          },
          {
            name: 'Opalite',
            artist: 'Taylor Swift',
            file: 'audio/opalite.mp3',
            duration: 0
          },
          {
            name: 'Pag-ibig Ay Kanibalismo II',
            artist: 'fitterkarma',
            file: 'audio/pag-ibig-ay-kanibalismo-ii.mp3',
            duration: 0
          },
          {
            name: 'True',
            artist: 'Spandau Ballet',
            file: 'audio/true.mp3',
            duration: 0
          },
          {
            name: 'Vienna',
            artist: 'Billy Joel',
            file: 'audio/vienna.mp3',
            duration: 0
          },
          {
            name: 'Wala Na Bang Pag-ibig',
            artist: 'JAYA',
            file: 'audio/wala-na-bang-pag-ibig.mp3',
            duration: 0
          },
          {
            name: 'Walt of Four Left Feet',
            artist: 'Shirebound',
            file: 'audio/waltz-of-four-left-feet.mp3',
            duration: 0
          },
          {
            name: 'Way Back Into Love',
            artist: 'Hugh Grant, Drew Barrymore',
            file: 'audio/way-back-into-love.mp3',
            duration: 0
          },
          {
            name: 'Tensionado',
            artist: 'Soapdish',
            file: 'audio/tensionado.mp3',
            duration: 0
          }
        ];
        
        // Initialize player
        function initPlayer() {
          audio.volume = volume;
          loadTrack(currentTrackIndex);
          updatePlaylistUI();
          updateTrackInfo();
          
          // Audio event listeners
          audio.addEventListener('timeupdate', updateProgress);
          audio.addEventListener('ended', nextTrack);
          audio.addEventListener('loadedmetadata', updateTotalTime);
          audio.addEventListener('error', handleAudioError);
          
          // Show player by default
          showPlayer();
        }
        
        // Handle audio errors
        function handleAudioError(e) {
          console.log('Audio error:', e);
          $('#current_track_name').text('Error loading track');
          $('#current_track_artist').text('Check console for details');
        }
        
        // Toggle play/pause
        function togglePlay() {
          if (isPlaying) {
            pauseTrack();
          } else {
            playTrack();
          }
        }
        
        // Play track
        function playTrack() {
          audio.play().then(function() {
            isPlaying = true;
            $('#play_pause_icon').removeClass('fa-play').addClass('fa-pause');
          }).catch(function(error) {
            console.log('Play error:', error);
            alert('Cannot play audio. Please check if MP3 files are in www/audio folder.');
          });
        }
        
        // Pause track
        function pauseTrack() {
          audio.pause();
          isPlaying = false;
          $('#play_pause_icon').removeClass('fa-pause').addClass('fa-play');
        }
        
        // Load track
        function loadTrack(index) {
          currentTrackIndex = index;
          var track = playlist[index];
          audio.src = track.file;
          updateTrackInfo();
          updateActivePlaylistItem();
        }
        
        // Next track
        function nextTrack() {
          var nextIndex = (currentTrackIndex + 1) % playlist.length;
          loadTrack(nextIndex);
          if (isPlaying) {
            audio.play();
          }
        }
        
        // Previous track
        function prevTrack() {
          var prevIndex = (currentTrackIndex - 1 + playlist.length) % playlist.length;
          loadTrack(prevIndex);
          if (isPlaying) {
            audio.play();
          }
        }
        
        // Update track info
        function updateTrackInfo() {
          var track = playlist[currentTrackIndex];
          $('#current_track_name').text(track.name);
          $('#current_track_artist').text(track.artist);
        }
        
        // Format time to always show 2 digits for seconds
        function formatTime(seconds) {
          if (isNaN(seconds) || !isFinite(seconds)) {
            return '0:00';
          }
          var mins = Math.floor(seconds / 60);
          var secs = Math.floor(seconds % 60);
          return mins + ':' + (secs < 10 ? '0' : '') + secs;
        }
        
        // Update progress bar
        function updateProgress() {
          if (audio.duration && !isNaN(audio.duration)) {
            var percent = (audio.currentTime / audio.duration) * 100;
            $('#progress_fill').css('width', percent + '%');
            
            // Update current time with formatted time
            $('#current_time').text(formatTime(audio.currentTime));
          }
        }
        
        // Update total time
        function updateTotalTime() {
          if (audio.duration && !isNaN(audio.duration)) {
            // Update total time with formatted time
            $('#total_time').text(formatTime(audio.duration));
          }
        }
        
        // Seek in track
        function seekTrack(percent) {
          if (audio.duration && !isNaN(audio.duration)) {
            audio.currentTime = percent * audio.duration;
          }
        }
        
        // Update playlist UI
        function updatePlaylistUI() {
          var playlistHTML = '';
          playlist.forEach(function(track, index) {
            playlistHTML += '<div class=\"playlist-item' + (index === currentTrackIndex ? ' active' : '') + '\" onclick=\"Shiny.setInputValue(\\'play_track\\', ' + index + ', {priority: \\'event\\'});\">' +
                            '<div class=\"playlist-item-name\">' + track.name + '</div>' +
                            '<div class=\"playlist-item-artist\">' + track.artist + '</div>' +
                            '</div>';
          });
          $('#playlist_container').html(playlistHTML);
        }
        
        // Update active playlist item
        function updateActivePlaylistItem() {
          $('.playlist-item').removeClass('active');
          $('.playlist-item').eq(currentTrackIndex).addClass('active');
        }
        
        // Toggle player visibility
        function togglePlayerVisibility() {
          isPlayerVisible = !isPlayerVisible;
          if (isPlayerVisible) {
            showPlayer();
          } else {
            hidePlayer();
          }
        }
        
        // Show player
        function showPlayer() {
          $('#music_player').show();
          $('#music_toggle_btn').hide();
        }
        
        // Hide player
        function hidePlayer() {
          $('#music_player').hide();
          $('#music_toggle_btn').show();
        }
        
        // Toggle player expand
        function togglePlayerExpand() {
          isPlayerExpanded = !isPlayerExpanded;
          if (isPlayerExpanded) {
            expandPlayer();
          } else {
            minimizePlayer();
          }
        }
        
        // Expand player
        function expandPlayer() {
          $('#music_player').removeClass('minimized').addClass('expanded');
          $('#playlist_container').show();
          $('#expand_icon').removeClass('fa-expand-alt').addClass('fa-compress-alt');
        }
        
        // Minimize player
        function minimizePlayer() {
          $('#music_player').removeClass('expanded').addClass('minimized');
          $('#playlist_container').hide();
          $('#expand_icon').removeClass('fa-compress-alt').addClass('fa-expand-alt');
        }
        
        // Set volume
        function setVolume(value) {
          volume = value;
          audio.volume = volume;
        }
        
        // Initialize player when document is ready
        initPlayer();
        
        // Expose functions to Shiny
        Shiny.addCustomMessageHandler('music_player_toggle_play', function(message) {
          togglePlay();
        });
        
        Shiny.addCustomMessageHandler('music_player_play_track', function(index) {
          loadTrack(index);
          if (isPlaying) {
            audio.play();
          }
        });
        
        Shiny.addCustomMessageHandler('music_player_next_track', function(message) {
          nextTrack();
        });
        
        Shiny.addCustomMessageHandler('music_player_prev_track', function(message) {
          prevTrack();
        });
        
        Shiny.addCustomMessageHandler('music_player_seek', function(percent) {
          seekTrack(percent);
        });
        
        Shiny.addCustomMessageHandler('music_player_set_volume', function(value) {
          setVolume(value);
        });
        
        Shiny.addCustomMessageHandler('music_player_toggle_visibility', function(message) {
          togglePlayerVisibility();
        });
        
        Shiny.addCustomMessageHandler('music_player_toggle_expand', function(message) {
          togglePlayerExpand();
        });
      });
    "))
  ),
  
  # Login Screen
  div(id = "login_screen", class = "login-container",
      div(id = "login_box", class = "login-box",
          div(class = "login-avatar",
              tags$i(class = "fas fa-user")
          ),
          div(class = "login-title", "Welcome Back"),
          div(class = "login-subtitle", "Enter your password to continue"),
          div(class = "login-form",
              passwordInput("login_password", NULL, 
                            placeholder = "Password",
                            width = "100%")
          ),
          actionButton("login_btn", "Sign In", class = "login-btn"),
          div(id = "login_error", class = "login-error"),
          div(id = "login_attempts", class = "login-attempts")
      )
  ),
  
  # Main App (hidden initially)
  hidden(
    div(id = "main_app",
        navbarPage(
          title = "Letters Never Sent",
          theme = NULL,
          id = "navbar",
          
          # Home Tab
          tabPanel("Home",
                   div(class = "main-container",
                       h2(class = "home-greeting", "Hey there! Tell me your thoughts today!"),
                       
                       div(class = "write-letter-btn-container",
                           actionButton("write_new_letter", "Write A New Letter", 
                                        class = "write-letter-btn", icon = icon("pen"))
                       ),
                       
                       div(class = "recent-section",
                           h3(class = "recent-title", "Recent Letters"),
                           uiOutput("recent_letters_ui")
                       )
                   )
          ),
          
          # All Letters Tab
          tabPanel("All Letters",
                   div(class = "main-container",
                       h2(style = "color: #7A8450; margin-bottom: 20px;", "All Letters"),
                       
                       div(class = "filters-section",
                           fluidRow(
                             column(6,
                                    textInput("search_title", "Search by Title:", 
                                              placeholder = "Type to search...")
                             ),
                             column(6,
                                    selectInput("filter_mood", "Filter by Mood:",
                                                choices = c("All", "Hopeful", "Happy", "Grateful", 
                                                            "Melancholic", "Sad", "Angry", "Regretful", 
                                                            "Neutral", "Loving", "Anxious", "Excited"),
                                                selected = "All")
                             )
                           )
                       ),
                       
                       DTOutput("letters_table")
                   )
          )
        ),
        
        # Music toggle button (always visible when player is hidden)
        tags$div(
          id = "music_toggle_btn",
          class = "music-toggle-btn",
          style = "display: none;",
          onclick = "Shiny.setInputValue('toggle_player_visibility', Math.random(), {priority: 'event'});",
          tags$i(class = "fas fa-music")
        ),
        
        # Music player container
        tags$div(
          id = "music_player",
          class = "music-player minimized",
          
          # Player header
          tags$div(class = "player-header",
                   tags$div(class = "player-title", "Malakasang Playlist"),
                   tags$div(class = "player-controls-header",
                            tags$button(class = "player-btn", onclick = "Shiny.setInputValue('toggle_player_expand', Math.random(), {priority: 'event'});",
                                        tags$i(id = "expand_icon", class = "fas fa-expand-alt")),
                            tags$button(class = "player-btn", onclick = "Shiny.setInputValue('toggle_player_visibility', Math.random(), {priority: 'event'});",
                                        tags$i(class = "fas fa-times"))
                   )
          ),
          
          # Track info
          tags$div(class = "track-info",
                   tags$div(id = "current_track_name", class = "track-name", "Ale"),
                   tags$div(id = "current_track_artist", class = "track-artist", "Your Collection")
          ),
          
          # Progress bar
          tags$div(class = "progress-container",
                   tags$div(class = "progress-bar", onclick = "Shiny.setInputValue('seek_position', event.offsetX/event.target.offsetWidth, {priority: 'event'});",
                            tags$div(id = "progress_fill", class = "progress-fill", style = "width: 0%;")
                   ),
                   tags$div(class = "time-display",
                            tags$span(id = "current_time", "0:00"),
                            tags$span(id = "total_time", "0:00")
                   )
          ),
          
          # Player controls
          tags$div(class = "player-controls",
                   tags$button(class = "control-btn prev-btn", 
                               onclick = "Shiny.setInputValue('prev_track', Math.random(), {priority: 'event'});",
                               tags$i(class = "fas fa-step-backward")),
                   tags$button(id = "play_pause_btn", class = "control-btn play-pause",
                               onclick = "Shiny.setInputValue('toggle_play', Math.random(), {priority: 'event'});",
                               tags$i(id = "play_pause_icon", class = "fas fa-play")),
                   tags$button(class = "control-btn next-btn",
                               onclick = "Shiny.setInputValue('next_track', Math.random(), {priority: 'event'});",
                               tags$i(class = "fas fa-step-forward"))
          ),
          
          # Volume control
          tags$div(class = "volume-container",
                   tags$i(class = "fas fa-volume-up volume-icon"),
                   tags$input(id = "volume_slider", type = "range", min = "0", max = "100", value = "70",
                              class = "volume-slider",
                              oninput = "Shiny.setInputValue('volume_change', this.value/100, {priority: 'event'});")
          ),
          
          # Playlist (hidden in minimized mode)
          tags$div(id = "playlist_container", class = "playlist-container", style = "display: none;")
        )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize database
  initialize_database()
  
  # Login variables
  login_attempts <- reactiveVal(0)
  max_attempts <- 3
  is_locked <- reactiveVal(FALSE)
  correct_password <- "euan08"
  
  # Handle login
  observeEvent(input$login_btn, {
    if (is_locked()) {
      return()
    }
    
    if (is.null(input$login_password) || input$login_password == "") {
      runjs("
        $('#login_box').addClass('shake');
        setTimeout(function() { $('#login_box').removeClass('shake'); }, 500);
      ")
      shinyjs::html("login_error", "Please enter a password")
      return()
    }
    
    if (input$login_password == correct_password) {
      # Successful login
      shinyjs::hide("login_screen")
      shinyjs::show("main_app")
      login_attempts(0)
    } else {
      # Failed login
      current_attempts <- login_attempts() + 1
      login_attempts(current_attempts)
      
      runjs("
        $('#login_box').addClass('shake');
        $('#login_password').addClass('error');
        setTimeout(function() { 
          $('#login_box').removeClass('shake'); 
          $('#login_password').removeClass('error');
        }, 500);
      ")
      
      if (current_attempts >= max_attempts) {
        is_locked(TRUE)
        shinyjs::disable("login_btn")
        shinyjs::disable("login_password")
        shinyjs::html("login_error", "Account locked. Too many failed attempts.")
        shinyjs::html("login_attempts", "")
      } else {
        remaining <- max_attempts - current_attempts
        shinyjs::html("login_error", "Incorrect password. Please try again.")
        shinyjs::html("login_attempts", 
                      paste0("Attempts remaining: ", remaining, " of ", max_attempts))
      }
      
      updateTextInput(session, "login_password", value = "")
    }
  })
  
  # Allow Enter key to submit
  observeEvent(input$login_password, {
    runjs("
      $('#login_password').on('keypress', function(e) {
        if (e.which == 13) {
          $('#login_btn').click();
        }
      });
    ")
  })
  
  # Music player server logic
  observeEvent(input$toggle_play, {
    session$sendCustomMessage("music_player_toggle_play", list())
  })
  
  observeEvent(input$play_track, {
    session$sendCustomMessage("music_player_play_track", input$play_track)
  })
  
  observeEvent(input$next_track, {
    session$sendCustomMessage("music_player_next_track", list())
  })
  
  observeEvent(input$prev_track, {
    session$sendCustomMessage("music_player_prev_track", list())
  })
  
  observeEvent(input$seek_position, {
    session$sendCustomMessage("music_player_seek", input$seek_position)
  })
  
  observeEvent(input$volume_change, {
    session$sendCustomMessage("music_player_set_volume", input$volume_change)
  })
  
  observeEvent(input$toggle_player_visibility, {
    session$sendCustomMessage("music_player_toggle_visibility", list())
  })
  
  observeEvent(input$toggle_player_expand, {
    session$sendCustomMessage("music_player_toggle_expand", list())
  })
  
  # Reactive value to store current letter being edited
  editing_id <- reactiveVal(NULL)
  
  # Reactive value to trigger refresh
  refresh_trigger <- reactiveVal(0)
  
  # Reactive value for selected mood
  selected_mood <- reactiveVal(NULL)
  selected_mood_edit <- reactiveVal(NULL)
  
  # Recent letters
  output$recent_letters_ui <- renderUI({
    refresh_trigger()
    recent <- get_recent_letters(3)
    
    if (nrow(recent) == 0) {
      return(tags$p("No letters yet. Start writing your first letter!", 
                    style = "text-align: center; color: #AAA18F; font-style: italic;"))
    }
    
    lapply(1:nrow(recent), function(i) {
      letter <- recent[i, ]
      preview <- substr(letter$letter_content, 1, 150)
      if (nchar(letter$letter_content) > 150) preview <- paste0(preview, "...")
      
      div(class = "recent-card",
          div(class = "recent-card-title", letter$title),
          div(class = "recent-card-to", paste0("To: ", letter$recipient)),
          div(class = "recent-card-preview", preview),
          div(class = "recent-card-date", 
              paste0("Written on ", letter$date_created))
      )
    })
  })
  
  # Write new letter button
  observeEvent(input$write_new_letter, {
    selected_mood(NULL)
    
    showModal(modalDialog(
      title = "Write A New Letter",
      size = "l",
      
      div(
        # Mood selector - UPDATED TO MATCH ALL FILTER MOODS
        div(class = "mood-selector-container",
            tags$label(class = "mood-selector-label", "How are you feeling right now?"),
            div(class = "mood-selector-grid",
                tags$div(class = "mood-option", `data-mood` = "Hopeful", onclick = "Shiny.setInputValue('modal_mood_click', 'Hopeful', {priority: 'event'});",
                         tags$i(class = "fas fa-seedling"),
                         div(class = "mood-label", "Hopeful")
                ),
                tags$div(class = "mood-option", `data-mood` = "Happy", onclick = "Shiny.setInputValue('modal_mood_click', 'Happy', {priority: 'event'});",
                         tags$i(class = "fas fa-smile"),
                         div(class = "mood-label", "Happy")
                ),
                tags$div(class = "mood-option", `data-mood` = "Grateful", onclick = "Shiny.setInputValue('modal_mood_click', 'Grateful', {priority: 'event'});",
                         tags$i(class = "fas fa-hands-praying"),
                         div(class = "mood-label", "Grateful")
                ),
                tags$div(class = "mood-option", `data-mood` = "Melancholic", onclick = "Shiny.setInputValue('modal_mood_click', 'Melancholic', {priority: 'event'});",
                         tags$i(class = "fas fa-cloud-moon"),
                         div(class = "mood-label", "Melancholic")
                ),
                tags$div(class = "mood-option", `data-mood` = "Sad", onclick = "Shiny.setInputValue('modal_mood_click', 'Sad', {priority: 'event'});",
                         tags$i(class = "fas fa-sad-tear"),
                         div(class = "mood-label", "Sad")
                ),
                tags$div(class = "mood-option", `data-mood` = "Angry", onclick = "Shiny.setInputValue('modal_mood_click', 'Angry', {priority: 'event'});",
                         tags$i(class = "fas fa-fire"),
                         div(class = "mood-label", "Angry")
                ),
                tags$div(class = "mood-option", `data-mood` = "Regretful", onclick = "Shiny.setInputValue('modal_mood_click', 'Regretful', {priority: 'event'});",
                         tags$i(class = "fas fa-undo"),
                         div(class = "mood-label", "Regretful")
                ),
                tags$div(class = "mood-option", `data-mood` = "Neutral", onclick = "Shiny.setInputValue('modal_mood_click', 'Neutral', {priority: 'event'});",
                         tags$i(class = "fas fa-meh"),
                         div(class = "mood-label", "Neutral")
                ),
                tags$div(class = "mood-option", `data-mood` = "Loving", onclick = "Shiny.setInputValue('modal_mood_click', 'Loving', {priority: 'event'});",
                         tags$i(class = "fas fa-heart"),
                         div(class = "mood-label", "Loving")
                ),
                tags$div(class = "mood-option", `data-mood` = "Anxious", onclick = "Shiny.setInputValue('modal_mood_click', 'Anxious', {priority: 'event'});",
                         tags$i(class = "fas fa-cloud"),
                         div(class = "mood-label", "Anxious")
                ),
                tags$div(class = "mood-option", `data-mood` = "Excited", onclick = "Shiny.setInputValue('modal_mood_click', 'Excited', {priority: 'event'});",
                         tags$i(class = "fas fa-bolt"),
                         div(class = "mood-label", "Excited")
                )
            )
        ),
        
        # Title and recipient
        fluidRow(
          column(6,
                 textInput("modal_title", "Title:", placeholder = "Give your letter a title")
          ),
          column(6,
                 textInput("modal_recipient", "To (optional):", placeholder = "Who is this letter for?")
          )
        ),
        
        # Letter content
        tags$label("Your Letter", style = "font-family: 'Source Sans Pro', sans-serif; font-weight: 600; color: #7A8450; margin-bottom: 10px; display: block;"),
        textAreaInput("modal_content", NULL,
                      placeholder = "Write what you need to say...",
                      rows = 20,
                      width = "100%")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_modal_btn", "Save Letter", class = "btn-primary")
      )
    ))
  })
  
  # Handle mood selection
  observeEvent(input$modal_mood_click, {
    selected_mood(input$modal_mood_click)
    
    runjs("
      $('.mood-option').removeClass('selected');
      $('.mood-option[data-mood=\"' + Shiny.shinyapp.$inputValues.modal_mood_click + '\"]').addClass('selected');
    ")
  })
  
  observeEvent(input$edit_mood_click, {
    selected_mood_edit(input$edit_mood_click)
    
    runjs("
      $('.mood-option').removeClass('selected');
      $('.mood-option[data-mood=\"' + Shiny.shinyapp.$inputValues.edit_mood_click + '\"]').addClass('selected');
    ")
  })
  
  # Save letter from modal
  observeEvent(input$save_modal_btn, {
    if (is.null(input$modal_title) || input$modal_title == "") {
      showNotification("Please enter a title", type = "error")
      return()
    }
    if (is.null(selected_mood())) {
      showNotification("Please select a mood", type = "error")
      return()
    }
    if (is.null(input$modal_content) || input$modal_content == "") {
      showNotification("Please write your letter content", type = "error")
      return()
    }
    
    recipient <- if (is.null(input$modal_recipient) || input$modal_recipient == "") {
      "Myself"
    } else {
      input$modal_recipient
    }
    
    tryCatch({
      create_letter(input$modal_title, recipient, 
                    input$modal_content, selected_mood())
      
      showNotification("Letter saved successfully!", type = "message")
      removeModal()
      selected_mood(NULL)
      refresh_trigger(refresh_trigger() + 1)
      
    }, error = function(e) {
      showNotification(paste("Error saving letter:", e$message), type = "error")
    })
  })
  
  # Load and filter letters for table
  filtered_letters <- reactive({
    refresh_trigger()
    letters <- read_letters()
    
    if (!is.null(input$search_title) && input$search_title != "") {
      letters <- letters %>%
        filter(grepl(input$search_title, title, ignore.case = TRUE))
    }
    
    if (!is.null(input$filter_mood) && input$filter_mood != "All") {
      letters <- letters %>%
        filter(mood == input$filter_mood)
    }
    
    if (nrow(letters) > 0) {
      letters <- letters %>%
        mutate(
          letter_num = paste0("#", id),
          Actions = sprintf(
            '<button class="btn btn-sm btn-info view-btn" data-id="%d"><i class="fa fa-eye"></i> View</button>
             <button class="btn btn-sm btn-warning edit-btn" data-id="%d"><i class="fa fa-edit"></i> Edit</button>
             <button class="btn btn-sm btn-danger delete-btn" data-id="%d"><i class="fa fa-trash"></i> Delete</button>',
            id, id, id
          )
        ) %>%
        select(letter_num, title, recipient, mood, date_modified, Actions)
    } else {
      letters <- data.frame(
        letter_num = character(),
        title = character(),
        recipient = character(),
        mood = character(),
        date_modified = character(),
        Actions = character(),
        stringsAsFactors = FALSE
      )
    }
    
    return(letters)
  })
  
  # Render table
  output$letters_table <- renderDT({
    datatable(
      filtered_letters(),
      escape = FALSE,
      selection = 'none',
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        searching = FALSE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        drawCallback = JS("
          function(settings) {
            $('.view-btn').off('click').on('click', function() {
              var id = $(this).data('id');
              Shiny.setInputValue('view_letter_id', id, {priority: 'event'});
            });
            
            $('.edit-btn').off('click').on('click', function() {
              var id = $(this).data('id');
              Shiny.setInputValue('edit_letter_id', id, {priority: 'event'});
            });
            
            $('.delete-btn').off('click').on('click', function() {
              var id = $(this).data('id');
              Shiny.setInputValue('delete_letter_id', id, {priority: 'event'});
            });
          }
        ")
      ),
      colnames = c("Letter #", "Title", "To", "Mood", "Last Updated", "Actions"),
      rownames = FALSE
    )
  })
  
  # Handle View button
  observeEvent(input$view_letter_id, {
    letter <- get_letter_by_id(input$view_letter_id)
    
    if (nrow(letter) > 0) {
      showModal(modalDialog(
        title = tags$div(
          tags$h3(letter$title[1], style = "font-family: 'Playfair Display', serif; font-weight: 600;"),
          tags$p(style = "color: #AAA18F; font-style: italic; font-family: 'Source Sans Pro', sans-serif;", 
                 paste0("To: ", letter$recipient[1])),
          tags$span(class = paste0("mood-badge mood-", letter$mood[1]), letter$mood[1])
        ),
        tags$div(style = "background: #F8F8F8; padding: 20px; border-radius: 8px; 
                        font-family: 'Source Sans Pro', sans-serif; line-height: 1.8; white-space: pre-wrap;
                        border: 1px solid #9CAF88;",
                 letter$letter_content[1]
        ),
        tags$p(style = "text-align: right; color: #AAA18F; font-size: 0.9em; font-family: 'Source Sans Pro', sans-serif;",
               paste0("Last updated: ", letter$date_modified[1])
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    }
  })
  
  # Handle Edit button
  observeEvent(input$edit_letter_id, {
    letter <- get_letter_by_id(input$edit_letter_id)
    
    if (nrow(letter) > 0) {
      editing_id(input$edit_letter_id)
      selected_mood_edit(letter$mood[1])
      
      showModal(modalDialog(
        title = tags$div(style = "display: flex; justify-content: space-between; align-items: center;",
                         tags$span("Edit Letter"),
                         tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                                     tags$span(`aria-hidden` = "true", HTML("&times;"))
                         )
        ),
        size = "l",
        
        div(
          # Mood selector - UPDATED TO MATCH ALL FILTER MOODS
          div(class = "mood-selector-container",
              tags$label(class = "mood-selector-label", "How are you feeling right now?"),
              div(class = "mood-selector-grid",
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Hopeful") " selected" else ""), 
                           `data-mood` = "Hopeful", onclick = "Shiny.setInputValue('edit_mood_click', 'Hopeful', {priority: 'event'});",
                           tags$i(class = "fas fa-seedling"),
                           div(class = "mood-label", "Hopeful")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Happy") " selected" else ""), 
                           `data-mood` = "Happy", onclick = "Shiny.setInputValue('edit_mood_click', 'Happy', {priority: 'event'});",
                           tags$i(class = "fas fa-smile"),
                           div(class = "mood-label", "Happy")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Grateful") " selected" else ""), 
                           `data-mood` = "Grateful", onclick = "Shiny.setInputValue('edit_mood_click', 'Grateful', {priority: 'event'});",
                           tags$i(class = "fas fa-hands-praying"),
                           div(class = "mood-label", "Grateful")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Melancholic") " selected" else ""), 
                           `data-mood` = "Melancholic", onclick = "Shiny.setInputValue('edit_mood_click', 'Melancholic', {priority: 'event'});",
                           tags$i(class = "fas fa-cloud-moon"),
                           div(class = "mood-label", "Melancholic")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Sad") " selected" else ""), 
                           `data-mood` = "Sad", onclick = "Shiny.setInputValue('edit_mood_click', 'Sad', {priority: 'event'});",
                           tags$i(class = "fas fa-sad-tear"),
                           div(class = "mood-label", "Sad")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Angry") " selected" else ""), 
                           `data-mood` = "Angry", onclick = "Shiny.setInputValue('edit_mood_click', 'Angry', {priority: 'event'});",
                           tags$i(class = "fas fa-fire"),
                           div(class = "mood-label", "Angry")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Regretful") " selected" else ""), 
                           `data-mood` = "Regretful", onclick = "Shiny.setInputValue('edit_mood_click', 'Regretful', {priority: 'event'});",
                           tags$i(class = "fas fa-undo"),
                           div(class = "mood-label", "Regretful")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Neutral") " selected" else ""), 
                           `data-mood` = "Neutral", onclick = "Shiny.setInputValue('edit_mood_click', 'Neutral', {priority: 'event'});",
                           tags$i(class = "fas fa-meh"),
                           div(class = "mood-label", "Neutral")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Loving") " selected" else ""), 
                           `data-mood` = "Loving", onclick = "Shiny.setInputValue('edit_mood_click', 'Loving', {priority: 'event'});",
                           tags$i(class = "fas fa-heart"),
                           div(class = "mood-label", "Loving")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Anxious") " selected" else ""), 
                           `data-mood` = "Anxious", onclick = "Shiny.setInputValue('edit_mood_click', 'Anxious', {priority: 'event'});",
                           tags$i(class = "fas fa-cloud"),
                           div(class = "mood-label", "Anxious")
                  ),
                  tags$div(class = paste0("mood-option", if(letter$mood[1] == "Excited") " selected" else ""), 
                           `data-mood` = "Excited", onclick = "Shiny.setInputValue('edit_mood_click', 'Excited', {priority: 'event'});",
                           tags$i(class = "fas fa-bolt"),
                           div(class = "mood-label", "Excited")
                  )
              )
          ),
          
          # Title and recipient
          fluidRow(
            column(6,
                   textInput("edit_title", "Title:", value = letter$title[1])
            ),
            column(6,
                   textInput("edit_recipient", "To (optional):", value = letter$recipient[1])
            )
          ),
          
          # Letter content
          tags$label("Your Letter", style = "font-family: 'Source Sans Pro', sans-serif; font-weight: 600; color: #7A8450; margin-bottom: 10px; display: block;"),
          textAreaInput("edit_content", NULL,
                        value = letter$letter_content[1],
                        rows = 20,
                        width = "100%")
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("update_modal_btn", "Update Letter", class = "btn-warning")
        )
      ))
    }
  })
  
  # Handle Delete button
  observeEvent(input$delete_letter_id, {
    editing_id(input$delete_letter_id)
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete this letter? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  
  # Update letter
  observeEvent(input$update_modal_btn, {
    if (is.null(input$edit_title) || input$edit_title == "" ||
        is.null(input$edit_content) || input$edit_content == "" ||
        is.null(selected_mood_edit()) ||
        is.null(editing_id())) {
      showNotification("Please fill in all required fields", type = "error")
      return()
    }
    
    recipient <- if (is.null(input$edit_recipient) || input$edit_recipient == "") {
      "Myself"
    } else {
      input$edit_recipient
    }
    
    tryCatch({
      update_letter(editing_id(), input$edit_title, recipient, 
                    input$edit_content, selected_mood_edit())
      
      showNotification("Letter updated successfully!", type = "message")
      removeModal()
      editing_id(NULL)
      selected_mood_edit(NULL)
      refresh_trigger(refresh_trigger() + 1)
      
    }, error = function(e) {
      showNotification(paste("Error updating letter:", e$message), type = "error")
    })
  })
  
  # Confirm delete
  observeEvent(input$confirm_delete, {
    if (is.null(editing_id())) return()
    
    tryCatch({
      delete_letter(editing_id())
      
      showNotification("Letter deleted successfully!", type = "message")
      removeModal()
      editing_id(NULL)
      refresh_trigger(refresh_trigger() + 1)
      
    }, error = function(e) {
      showNotification(paste("Error deleting letter:", e$message), type = "error")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)