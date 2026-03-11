# jesperlindmarker.com

Personal website for Jesper Lindmarker — quantitative researcher, lecturer, and PhD candidate at Linköping University.

## About

Research on assortative mating and ethnic boundaries in Sweden, lecture materials from the MSc programme in Computational Social Science, and professional background.

## Structure

```
.
├── index.html              # Main single-page site
├── assets/
│   ├── css/style.css       # All styles
│   ├── js/main.js          # Interactions & animations
│   └── images/             # Photos and graphics
├── teaching/
│   └── sds-ii/             # SDS II lecture slides (Quarto/reveal.js)
│       ├── Lecture_*.html   # Lecture presentations
│       ├── Lab_*.html       # Lab sessions
│       └── data/            # Simulated datasets and R scripts
├── README.md
└── CHANGELOG.md
```

## Hosting

Designed for **GitHub Pages**. Static HTML/CSS/JS — no build step.

To point a custom domain (`www.jesperlindmarker.com`):

1. In GitHub repo settings, go to Pages and set the custom domain
2. The CNAME file is already included
3. Configure DNS to point to GitHub Pages ([docs](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site))

## Teaching materials

The `teaching/sds-ii/` directory contains reveal.js lecture slides generated with Quarto for the Statistics & Data Science II course. Each `.html` file has a companion `_files/` directory with JS/CSS dependencies. These are self-contained and render directly in the browser.

## Tech stack

- Plain HTML, CSS, JavaScript (no frameworks, no build tools)
- Google Fonts: Inter, Playfair Display, JetBrains Mono
- Lecture slides: Quarto + reveal.js
- CSS custom properties for theming
- IntersectionObserver for scroll animations
- Fully responsive

## License

Content and images are copyright Jesper Lindmarker. Code structure is free to use as inspiration.
