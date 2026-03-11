# Changelog

All notable changes to this project will be documented in this file.

## [2.0.0] - 2026-03-11

### Changed
- **Replaced Showcase section with Teaching section** — eight lecture slides from the SDS-II course now hosted directly on the site with linked reveal.js presentations
- **Repositioned profile** — hero and meta descriptions now reflect both research and teaching roles; subtitle mentions "lecturer" and the MSc in Computational Social Science
- **History section renamed to "Background"** — PhD entry expanded to include teaching responsibilities; section header simplified
- **Hero copy** broadened — signals quantitative methods, teaching, and business background without being explicit about consulting
- **Nav** updated: "Showcase" → "Teaching"

### Added
- `teaching/sds-ii/` directory with 8 lectures and 2 labs as self-contained reveal.js HTML presentations
- `teaching/sds-ii/data/` with simulated datasets and R data-generation scripts from the course
- Teaching cards grid with week-by-week curriculum (OLS → Multiple regression → Interactions → DAGs → Logistic → Discrete choice → Causal inference)

### Removed
- Showcase section and all associated CSS/JS (`.showcase-*` classes)

### Fixed
- Paper abstract toggle JS now uses `.closest('.paper-item')` instead of `previousElementSibling` (was broken when paper-links div was present between abstract and button)

## [1.1.0] - 2026-03-11

### Changed
- **Papers section** expanded from 2 to 5 papers with accurate titles, co-authors, and abstracts from dissertation project
  - Added published paper: "Cohabitation and Mortality Across the Life Course" (European Journal of Population, 2025)
  - Added preprint: "However Far Away?" with SocArXiv link
  - Added working papers: "2,500 Ethnic Boundaries", "Three Decades of Ethnic Assortative Mating", "Working Together, Living Together?"
  - Paper cards now show authors and direct links to publications
- **Research cards** rewritten to reflect actual dissertation framework (opportunity vs. preference, spatial contingencies, methods)
- **Copy refinement** throughout — less forced, more direct and professional
  - Replaced "The winding road so far" with "Before all this"
  - Toned down hero tagline; removed overly casual phrasing
  - Simplified contact section copy
- **Showcase section** — removed all "Coming soon" tags; cards now describe content areas without placeholder labels
- Increased image heights for full-width images (300px → 420px) and timeline images (260px → 380px)
- Removed decorative captions ("Norrköping, Sweden", "The human side of the data")

### Added
- Link to castle parties video (fb.watch)
- Link to cookbook digital copy (Google Drive)
- Correct Google Scholar profile link
- DOI links for published paper and preprint
- Paper author names and styled link buttons
- CSS for `.paper-authors` and `.paper-links` elements

### Fixed
- Google Scholar link now points to actual profile (user=12yuS3sAAAAJ)

## [1.0.0] - 2026-03-11

### Added
- Initial site launch
- **Hero section** with portrait, intro text, and animated background blobs
- **Research section** with three card layout
- **Publications section** (dark theme) with expandable abstracts
- **Professional History** timeline: PhD at IAS/LiU, The Castle Coworking, Cookbook Adventure, GreenCup
- **Showcase section** with cards for ABMs, visualizations, GitHub projects, and paper results
- **Contact section** with email, LinkedIn, and Google Scholar links
- Responsive design for mobile and tablet
- Scroll-triggered fade-in animations with staggered timing
- Sticky navigation with scroll state and mobile hamburger menu
- Active nav link highlighting on scroll
- Open Graph meta tags for social sharing
- SVG favicon
- README.md, CHANGELOG.md, CNAME, .nojekyll
- GitHub Pages deployment ready

### Design notes
- Inspired by the original Readymag site but rebuilt from scratch
- Color palette: warm off-white (#FAFAF8) with orange accent (#E85D3A) and blue accent (#2D5BFF)
- Typography: Playfair Display (headings), Inter (body), JetBrains Mono (labels/code)
