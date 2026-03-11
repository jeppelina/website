/* ============================================
   JESPER LINDMARKER — Main JavaScript
   ============================================ */

document.addEventListener('DOMContentLoaded', () => {

    // --- Nav scroll effect ---
    const nav = document.getElementById('nav');
    const handleScroll = () => {
        nav.classList.toggle('scrolled', window.scrollY > 40);
    };
    window.addEventListener('scroll', handleScroll, { passive: true });

    // --- Mobile nav toggle ---
    const toggle = document.getElementById('nav-toggle');
    const links = document.getElementById('nav-links');
    toggle.addEventListener('click', () => {
        links.classList.toggle('open');
        const isOpen = links.classList.contains('open');
        toggle.setAttribute('aria-expanded', isOpen);
    });
    // Close mobile nav on link click
    links.querySelectorAll('a').forEach(a => {
        a.addEventListener('click', () => links.classList.remove('open'));
    });

    // --- Paper abstract toggles ---
    document.querySelectorAll('.paper-toggle').forEach(btn => {
        btn.addEventListener('click', () => {
            const article = btn.closest('.paper-item');
            const abstract = article.querySelector('.paper-abstract');
            const isOpen = abstract.classList.toggle('open');
            btn.setAttribute('aria-expanded', isOpen);
            btn.textContent = isOpen ? 'Show less' : 'Read more';
        });
    });

    // --- Scroll-triggered fade-in animations ---
    const animateElements = () => {
        const targets = document.querySelectorAll(
            '.research-card, .paper-item, .timeline-item, .teaching-card, .papers-note, .contact-link'
        );
        targets.forEach(el => {
            if (!el.classList.contains('fade-in')) {
                el.classList.add('fade-in');
            }
        });

        const observer = new IntersectionObserver((entries) => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    entry.target.classList.add('visible');
                    observer.unobserve(entry.target);
                }
            });
        }, {
            threshold: 0.1,
            rootMargin: '0px 0px -40px 0px'
        });

        document.querySelectorAll('.fade-in').forEach(el => observer.observe(el));
    };

    animateElements();

    // --- Smooth scroll for anchor links ---
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', (e) => {
            const target = document.querySelector(anchor.getAttribute('href'));
            if (target) {
                e.preventDefault();
                const offset = 80;
                const top = target.getBoundingClientRect().top + window.scrollY - offset;
                window.scrollTo({ top, behavior: 'smooth' });
            }
        });
    });

    // --- Active nav link highlight ---
    const sections = document.querySelectorAll('section[id]');
    const navLinks = document.querySelectorAll('.nav-links a');

    const highlightNav = () => {
        let current = '';
        sections.forEach(section => {
            const top = section.offsetTop - 120;
            if (window.scrollY >= top) {
                current = section.getAttribute('id');
            }
        });
        navLinks.forEach(link => {
            link.style.color = '';
            if (link.getAttribute('href') === `#${current}`) {
                link.style.color = 'var(--color-text)';
            }
        });
    };
    window.addEventListener('scroll', highlightNav, { passive: true });

    // --- Staggered animation for cards ---
    const staggerElements = (selector, parentSelector) => {
        const parents = document.querySelectorAll(parentSelector);
        parents.forEach(parent => {
            const children = parent.querySelectorAll(selector);
            children.forEach((child, i) => {
                child.style.transitionDelay = `${i * 0.1}s`;
            });
        });
    };
    staggerElements('.research-card', '.research-grid');
    staggerElements('.teaching-card', '.teaching-grid');
    staggerElements('.timeline-item', '.timeline');

});
