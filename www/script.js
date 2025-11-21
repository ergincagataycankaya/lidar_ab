/* ==============================================
   MODERN HIGH-END JAVASCRIPT FOR LIDAR AB
   ============================================== */

// Wait for DOM to be fully loaded
document.addEventListener('DOMContentLoaded', function() {
  console.log('Initializing modern UI...');
  
  // Initialize all components
  initThreeJS();
  initLenisScroll();
  initParallax();
  initAnimations();
  initInteractiveElements();
});

/* ==============================================
   THREE.JS 3D BACKGROUND SCENE
   ============================================== */
function initThreeJS() {
  // Check if Three.js is loaded
  if (typeof THREE === 'undefined') {
    console.warn('Three.js not loaded, skipping 3D scene');
    return;
  }

  const container = document.getElementById('canvas-container');
  if (!container) {
    console.warn('Canvas container not found');
    return;
  }

  // Scene setup
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(
    75,
    window.innerWidth / window.innerHeight,
    0.1,
    1000
  );
  camera.position.z = 30;

  const renderer = new THREE.WebGLRenderer({
    alpha: true,
    antialias: true
  });
  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  container.appendChild(renderer.domElement);

  // Create particle system (adaptive count for performance)
  const particlesGeometry = new THREE.BufferGeometry();
  const isMobile = window.innerWidth < 768;
  const particlesCount = isMobile ? 1000 : 2000; // Adaptive count: fewer particles on mobile
  const posArray = new Float32Array(particlesCount * 3);

  for (let i = 0; i < particlesCount * 3; i++) {
    posArray[i] = (Math.random() - 0.5) * 100;
  }

  particlesGeometry.setAttribute('position', new THREE.BufferAttribute(posArray, 3));

  // Particle material
  const particlesMaterial = new THREE.PointsMaterial({
    size: 0.15,
    color: 0x00f3ff,
    transparent: true,
    opacity: 0.8,
    blending: THREE.AdditiveBlending
  });

  const particlesMesh = new THREE.Points(particlesGeometry, particlesMaterial);
  scene.add(particlesMesh);

  // Add geometric shapes
  const geometry = new THREE.IcosahedronGeometry(10, 1);
  const material = new THREE.MeshBasicMaterial({
    color: 0x7000ff,
    wireframe: true,
    transparent: true,
    opacity: 0.3
  });
  const icosahedron = new THREE.Mesh(geometry, material);
  scene.add(icosahedron);

  // Add torus
  const torusGeometry = new THREE.TorusGeometry(15, 2, 16, 100);
  const torusMaterial = new THREE.MeshBasicMaterial({
    color: 0xff00ff,
    wireframe: true,
    transparent: true,
    opacity: 0.2
  });
  const torus = new THREE.Mesh(torusGeometry, torusMaterial);
  torus.rotation.x = Math.PI / 4;
  scene.add(torus);

  // Mouse movement interaction
  let mouseX = 0;
  let mouseY = 0;

  document.addEventListener('mousemove', (event) => {
    mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
  });

  // Animation loop
  function animate() {
    requestAnimationFrame(animate);

    // Rotate particles
    particlesMesh.rotation.y += 0.001;
    particlesMesh.rotation.x += 0.0005;

    // Rotate shapes
    icosahedron.rotation.x += 0.003;
    icosahedron.rotation.y += 0.005;
    
    torus.rotation.y += 0.002;
    torus.rotation.z += 0.001;

    // Mouse interaction
    camera.position.x += (mouseX * 5 - camera.position.x) * 0.05;
    camera.position.y += (mouseY * 5 - camera.position.y) * 0.05;
    camera.lookAt(scene.position);

    renderer.render(scene, camera);
  }

  animate();

  // Handle window resize
  window.addEventListener('resize', () => {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
  });

  console.log('Three.js scene initialized');
}

/* ==============================================
   LENIS SMOOTH SCROLLING
   ============================================== */
function initLenisScroll() {
  // Check if Lenis is loaded
  if (typeof Lenis === 'undefined') {
    console.warn('Lenis not loaded, using default scroll');
    return;
  }

  const lenis = new Lenis({
    duration: 1.2,
    easing: (t) => Math.min(1, 1.001 - Math.pow(2, -10 * t)),
    direction: 'vertical',
    gestureDirection: 'vertical',
    smooth: true,
    mouseMultiplier: 1,
    smoothTouch: false,
    touchMultiplier: 2,
    infinite: false,
  });

  function raf(time) {
    lenis.raf(time);
    requestAnimationFrame(raf);
  }

  requestAnimationFrame(raf);

  // Smooth scroll to sections
  document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function (e) {
      e.preventDefault();
      const target = document.querySelector(this.getAttribute('href'));
      if (target) {
        lenis.scrollTo(target, {
          offset: 0,
          duration: 2
        });
      }
    });
  });

  console.log('Lenis smooth scrolling initialized');
}

/* ==============================================
   PARALLAX SCROLLING EFFECTS
   ============================================== */
function initParallax() {
  const parallaxElements = document.querySelectorAll('.parallax-section');
  const glassPanels = document.querySelectorAll('.glass-panel'); // Cache DOM query
  
  if (parallaxElements.length === 0) {
    console.warn('No parallax elements found');
    return;
  }

  // Throttled scroll handler for better performance
  const handleScroll = throttle(() => {
    const scrolled = window.pageYOffset;

    parallaxElements.forEach((element, index) => {
      const speed = 0.5;
      const yPos = -(scrolled * speed);
      
      // Apply transform only if element is in viewport
      const rect = element.getBoundingClientRect();
      if (rect.top < window.innerHeight && rect.bottom > 0) {
        element.style.transform = `translateY(${yPos * 0.1}px)`;
      }
    });

    // Parallax for glass panels (using cached query)
    glassPanels.forEach((panel, index) => {
      const rect = panel.getBoundingClientRect();
      if (rect.top < window.innerHeight && rect.bottom > 0) {
        const scrollPercent = (window.innerHeight - rect.top) / window.innerHeight;
        const translateY = (scrollPercent - 0.5) * 20;
        panel.style.transform = `translateY(${translateY}px)`;
      }
    });
  }, 16); // ~60fps

  window.addEventListener('scroll', handleScroll, { passive: true });

  console.log('Parallax effects initialized');
}

/* ==============================================
   SCROLL-TRIGGERED ANIMATIONS
   ============================================== */
function initAnimations() {
  // Intersection Observer for fade-in animations
  const observerOptions = {
    threshold: 0.1,
    rootMargin: '0px 0px -100px 0px'
  };

  const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.style.opacity = '1';
        entry.target.style.transform = 'translateY(0)';
      }
    });
  }, observerOptions);

  // Observe all glass panels and sections
  const animatedElements = document.querySelectorAll('.glass-panel, .parallax-section');
  animatedElements.forEach(el => {
    el.style.opacity = '0';
    el.style.transform = 'translateY(50px)';
    el.style.transition = 'opacity 0.8s ease, transform 0.8s ease';
    observer.observe(el);
  });

  // Add scroll progress indicator (throttled for performance)
  const progressBar = document.querySelector('.scroll-progress'); // Cache DOM query
  if (progressBar) {
    const updateProgress = throttle(() => {
      const winScroll = document.body.scrollTop || document.documentElement.scrollTop;
      const height = document.documentElement.scrollHeight - document.documentElement.clientHeight;
      const scrolled = height > 0 ? (winScroll / height) * 100 : 0; // Prevent division by zero
      progressBar.style.width = scrolled + '%';
    }, 16); // ~60fps
    
    window.addEventListener('scroll', updateProgress, { passive: true });
  }

  console.log('Animations initialized');
}

/* ==============================================
   INTERACTIVE ELEMENTS
   ============================================== */
function initInteractiveElements() {
  // Add glow effect on hover for buttons
  const buttons = document.querySelectorAll('.btn, .hero-cta');
  buttons.forEach(button => {
    button.addEventListener('mouseenter', function() {
      this.style.transform = 'translateY(-3px) scale(1.05)';
    });
    
    button.addEventListener('mouseleave', function() {
      this.style.transform = 'translateY(0) scale(1)';
    });
  });

  // Add ripple effect on click
  buttons.forEach(button => {
    button.addEventListener('click', function(e) {
      const ripple = document.createElement('span');
      ripple.classList.add('ripple');
      this.appendChild(ripple);

      const x = e.clientX - this.offsetLeft;
      const y = e.clientY - this.offsetTop;

      ripple.style.left = x + 'px';
      ripple.style.top = y + 'px';

      setTimeout(() => {
        ripple.remove();
      }, 600);
    });
  });

  // Glass panel hover effects
  const glassPanels = document.querySelectorAll('.glass-panel');
  glassPanels.forEach(panel => {
    panel.addEventListener('mouseenter', function() {
      this.style.background = 'rgba(255, 255, 255, 0.08)';
    });
    
    panel.addEventListener('mouseleave', function() {
      this.style.background = 'rgba(255, 255, 255, 0.05)';
    });
  });

  // Smooth scroll for hero CTA
  const heroCta = document.querySelector('.hero-cta');
  if (heroCta) {
    heroCta.addEventListener('click', function(e) {
      e.preventDefault();
      const target = document.querySelector('#controls-section');
      if (target) {
        target.scrollIntoView({ behavior: 'smooth' });
      }
    });
  }

  console.log('Interactive elements initialized');
}

/* ==============================================
   UTILITY FUNCTIONS
   ============================================== */

// Debounce function for performance
function debounce(func, wait) {
  let timeout;
  return function executedFunction(...args) {
    const later = () => {
      clearTimeout(timeout);
      func(...args);
    };
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  };
}

// Throttle function for scroll events
function throttle(func, limit) {
  let inThrottle;
  return function() {
    const args = arguments;
    const context = this;
    if (!inThrottle) {
      func.apply(context, args);
      inThrottle = true;
      setTimeout(() => inThrottle = false, limit);
    }
  };
}

/* ==============================================
   SHINY INTEGRATION
   ============================================== */

// Hook into Shiny's message handlers (uses jQuery which is loaded by Shiny)
if (typeof Shiny !== 'undefined') {
  Shiny.addCustomMessageHandler('showNotification', function(message) {
    console.log('Shiny notification:', message);
  });

  // Use vanilla JS when jQuery might not be available yet
  document.addEventListener('DOMContentLoaded', function() {
    // jQuery event handlers only if jQuery is available (loaded by Shiny)
    if (typeof $ !== 'undefined') {
      // Listen for Shiny connection
      $(document).on('shiny:connected', function() {
        console.log('Shiny connected - UI ready');
      });

      $(document).on('shiny:disconnected', function() {
        console.log('Shiny disconnected');
      });

      // Add loading states for outputs
      $(document).on('shiny:busy', function() {
        document.body.classList.add('loading');
      });

      $(document).on('shiny:idle', function() {
        document.body.classList.remove('loading');
      });
    } else {
      console.warn('jQuery not available for Shiny event handlers');
    }
  });
}

console.log('Modern UI JavaScript loaded successfully');
