# Shiny Bingo

A polished, interactive web-based Bingo game built with [Shiny](https://shiny.posit.co/) in R.  
This project began as a console-based R script based off the original version of my R-coded Bingo game and was redesigned using Shiny.

---

## Features

- **Single or Two-Player Mode:** Play solo or head-to-head.
- **Dynamic Bingo Cards:** Cards are generated randomly each game.
- **Interactive UI:**  
  - Marked numbers shown with red "X".
  - Central "FREE" space styled in green and italic.
  - Responsive layout (side-by-side for two players, centered for one).
- **Progress Notifications:** See how close each player is to Bingo.
- **Game Status:**  
  - Last number called and winner announcement with clear visual emphasis.
  - Session high score tracking (fewest moves to Bingo).
- **Theme Support:** Light/dark mode toggle via `{bslib}`.
- **UI Polish:**  
  - Custom-styled buttons, radio inputs, and title panel.
  - Cards auto-centered for a professional look.

---

## Why This Project?

I built this Shiny Bingo app as a hands-on way to advance my skills in R web app development and interface design. The project demonstrates:

- Refactoring of classic R logic into a reactive Shiny application.
- Use of advanced UI/UX features and styling with `{bslib}` and custom CSS.
- Real-time feedback and notifications using Shiny’s reactive model.

---

## Usage

1. Clone or download this repo.
2. Open `app.R` in RStudio.
3. Click “Run App” to launch the interactive Bingo game in your browser.



