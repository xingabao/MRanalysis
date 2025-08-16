/**
 * Template Name: Kelly
 * Template URL: https://bootstrapmade.com/kelly-free-bootstrap-cv-resume-html-template/
 * Updated: Mar 17 2024 with Bootstrap v5.3.3
 * Author: BootstrapMade.com
 * License: https://bootstrapmade.com/license/
 */

(function() {
	"use strict";

	/**
	 * Easy selector helper function
	 */
	const select = (el, all = false) => {
		el = el.trim()
		if (all) {
			return [...document.querySelectorAll(el)]
		} else {
			return document.querySelector(el)
		}
	}

	/**
	 * Easy event listener function
	 */
	const on = (type, el, listener, all = false) => {
		let selectEl = select(el, all)
		if (selectEl) {
			if (all) {
				selectEl.forEach(e => e.addEventListener(type, listener))
			} else {
				selectEl.addEventListener(type, listener)
			}
		}
	}

	/**
	 * Easy on scroll event listener 
	 */
	const onscroll = (el, listener) => {
		el.addEventListener('scroll', listener)
	}

	/**
	 * Scrolls to an element with header offset
	 */
	const scrollto = (el) => {
		let header = select('#header')
		let offset = header.offsetHeight

		let elementPos = select(el).offsetTop
		window.scrollTo({
			top: elementPos - offset,
			behavior: 'smooth'
		})
	}

	/**
	 * Back to top button
	 */
	let backtotop = select('.back-to-top')
	if (backtotop) {
		const toggleBacktotop = () => {
			if (window.scrollY > 100) {
				backtotop.classList.add('active')
			} else {
				backtotop.classList.remove('active')
			}
		}
		window.addEventListener('load', toggleBacktotop)
		onscroll(document, toggleBacktotop)
	}

 /**
   * Mobile nav toggle
   */
  const mobileNavToggleBtn = document.querySelector('.mobile-nav-toggle');

  function mobileNavToogle() {
    document.querySelector('body').classList.toggle('mobile-nav-active');
    mobileNavToggleBtn.classList.toggle('bi-list');
    mobileNavToggleBtn.classList.toggle('bi-x');
  }
  mobileNavToggleBtn.addEventListener('click', mobileNavToogle);

  /**
   * Hide mobile nav on same-page/hash links
   */
  document.querySelectorAll('#navmenu a').forEach(navmenu => {
    navmenu.addEventListener('click', () => {
      if (document.querySelector('.mobile-nav-active')) {
        mobileNavToogle();
      }
    });

  });

  /**
   * Toggle mobile nav dropdowns
   */
  document.querySelectorAll('.navmenu .toggle-dropdown').forEach(navmenu => {
    navmenu.addEventListener('click', function(e) {
      e.preventDefault();
      this.parentNode.classList.toggle('active');
      this.parentNode.nextElementSibling.classList.toggle('dropdown-active');
      e.stopImmediatePropagation();
    });
  });

	/**
	 * Preloader
	 */
	let preloader = select('#preloader');
	if (preloader) {
		window.addEventListener('load', () => {
			preloader.remove()
		});
	}

	/**
	 * Porfolio isotope and filter
	 */
	window.addEventListener('load', () => {
		let portfolioContainer = select('.portfolio-container');
		if (portfolioContainer) {
			let portfolioIsotope = new Isotope(portfolioContainer, {
				itemSelector: '.portfolio-item'
			});

			let portfolioFilters = select('#portfolio-flters li', true);

			on('click', '#portfolio-flters li', function(e) {
				e.preventDefault();
				portfolioFilters.forEach(function(el) {
					el.classList.remove('filter-active');
				});
				this.classList.add('filter-active');

				portfolioIsotope.arrange({
					filter: this.getAttribute('data-filter')
				});
				portfolioIsotope.on('arrangeComplete', function() {
					AOS.refresh()
				});
			}, true);
		}

	});

	/**
	 * Initiate portfolio lightbox 
	 */
	const portfolioLightbox = GLightbox({
		selector: '.portfolio-lightbox'
	});

	/**
	 * Initiate portfolio details lightbox 
	 */
	const portfolioDetailsLightbox = GLightbox({
		selector: '.portfolio-details-lightbox',
		width: '90%',
		height: '90vh'
	});

	/**
	 * Portfolio details slider
	 */
	new Swiper('.portfolio-details-slider', {
		speed: 400,
		loop: true,
		autoplay: {
			delay: 5000,
			disableOnInteraction: false
		},
		pagination: {
			el: '.swiper-pagination',
			type: 'bullets',
			clickable: true
		}
	});

	/**
	 * Skills animation
	 */
	let skilsContent = select('.skills-content');
	if (skilsContent) {
		new Waypoint({
			element: skilsContent,
			offset: '80%',
			handler: function(direction) {
				let progress = select('.progress .progress-bar', true);
				progress.forEach((el) => {
					el.style.width = el.getAttribute('aria-valuenow') + '%'
				});
			}
		})
	}

	/**
	 * Testimonials slider
	 */
	new Swiper('.testimonials-slider', {
		speed: 600,
		loop: true,
		autoplay: {
			delay: 5000,
			disableOnInteraction: false
		},
		slidesPerView: 'auto',
		pagination: {
			el: '.swiper-pagination',
			type: 'bullets',
			clickable: true
		}
	});

	/**
	 * Animation on scroll
	 */
	window.addEventListener('load', () => {
		AOS.init({
			duration: 1000,
			easing: "ease-in-out",
			once: true,
			mirror: false
		});
	});

	/**
	 * Initiate Pure Counter 
	 */
	new PureCounter();

})()

document.addEventListener('DOMContentLoaded', function() {
  const quicklinks = document.querySelectorAll('.portfolio-item');
  const hiddencontent= document.querySelector('.hidden-content');
  quicklinks.forEach((quicklink,index) => {
    
	const title = quicklink.querySelector('h4')//.textContent;
    const description = quicklink.querySelector('p').textContent;
    const image = quicklink.querySelector('a.portfolio-lightbox').getAttribute('href');
    const link = quicklink.querySelector('a[target="_blank"]').getAttribute('href');
    const easyintro = document.createElement('div');
	easyintro.className='easyintro'
	const img=document.createElement('img');
	img.setAttribute('src',image);
    const introText = document.createElement('div');
    introText.className = 'intro-text';
    
    // 创建链接
    const anchor = document.createElement('a');
    anchor.setAttribute('href', link);
            
            // 创建标题
    const h3 = document.createElement('h3');
    h3.textContent = title;
    anchor.appendChild(h3);
            
            // 创建描述
    const p = document.createElement('p');
    p.textContent = description;
    
            // 将链接和描述添加到 intro-text
    introText.appendChild(anchor);
    introText.appendChild(p);
    
    // 将 intro-text 添加到 easyintro 盒子
	easyintro.appendChild(img);
    easyintro.appendChild(introText);
    
    // 将新的 easyintro 盒子添加到容器中
    hiddencontent.appendChild(easyintro);
	
    });
});

document.addEventListener('DOMContentLoaded', function() {
	const formControl = document.querySelector('.form-control');
	const hiddenContent = document.querySelector('.hidden-content');
	const easyIntros = document.querySelectorAll('.easyintro');

	formControl.addEventListener('click', function() {
		hiddenContent.classList.toggle('show');
	});

	document.addEventListener('click', function(event) {
		if (!event.target.closest('.input-group')) {
			hiddenContent.classList.remove('show');
		}
	});
	formControl.addEventListener('input', function() {
		const searchTerm = this.value.toLowerCase();
		easyIntros.forEach(function(intro) {
			const title = intro.querySelector('h3').textContent.toLowerCase();
			const description = intro.querySelector('p').textContent.toLowerCase();
			if (title.includes(searchTerm) || description.includes(searchTerm)) {
				intro.style.display = 'flex';
			} else {
				intro.style.display = 'none';
			}
		});
	});
});

function handleButtonClick(event) {
	event.preventDefault();
}
