// ========================================
// Three.js Particle Cloud Background
// ========================================

document.addEventListener('DOMContentLoaded', function() {
  
  // Check if Three.js is loaded
  if (typeof THREE === 'undefined') {
    console.warn('Three.js library not loaded. Particle background will not be displayed.');
    return;
  }
  
  // Initialize Three.js scene
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
  const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
  
  // Setup renderer
  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.setClearColor(0x000000, 0);
  const container = document.getElementById('webgl-container');
  if (container) {
    container.appendChild(renderer.domElement);
  } else {
    console.warn('webgl-container element not found. Particle background will not be displayed.');
    return;
  }
  
  // Create particle cloud (simulating LiDAR data)
  const particleCount = 3000;
  const positions = new Float32Array(particleCount * 3);
  const colors = new Float32Array(particleCount * 3);
  
  for (let i = 0; i < particleCount; i++) {
    // Random positions in a sphere
    const radius = Math.random() * 50 + 10;
    const theta = Math.random() * Math.PI * 2;
    const phi = Math.random() * Math.PI;
    
    positions[i * 3] = radius * Math.sin(phi) * Math.cos(theta);
    positions[i * 3 + 1] = radius * Math.sin(phi) * Math.sin(theta);
    positions[i * 3 + 2] = radius * Math.cos(phi);
    
    // Color gradient (blue to cyan to white)
    const colorMix = Math.random();
    colors[i * 3] = 0.2 + colorMix * 0.8;     // R
    colors[i * 3 + 1] = 0.5 + colorMix * 0.5; // G
    colors[i * 3 + 2] = 0.9 + colorMix * 0.1; // B
  }
  
  const geometry = new THREE.BufferGeometry();
  geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
  geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
  
  const material = new THREE.PointsMaterial({
    size: 0.8,
    vertexColors: true,
    transparent: true,
    opacity: 0.6,
    blending: THREE.AdditiveBlending
  });
  
  const particles = new THREE.Points(geometry, material);
  scene.add(particles);
  
  // Position camera
  camera.position.z = 80;
  
  // Mouse movement for parallax effect
  let mouseX = 0;
  let mouseY = 0;
  let targetX = 0;
  let targetY = 0;
  
  document.addEventListener('mousemove', function(event) {
    mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
  });
  
  // Animation loop
  function animate() {
    requestAnimationFrame(animate);
    
    // Rotate particle cloud slowly
    particles.rotation.x += 0.0005;
    particles.rotation.y += 0.001;
    
    // Parallax effect with mouse movement
    targetX = mouseX * 0.3;
    targetY = mouseY * 0.3;
    
    camera.position.x += (targetX - camera.position.x) * 0.05;
    camera.position.y += (targetY - camera.position.y) * 0.05;
    camera.lookAt(scene.position);
    
    renderer.render(scene, camera);
  }
  
  animate();
  
  // Handle window resize
  window.addEventListener('resize', function() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
  });
  
  // ========================================
  // Lenis Smooth Scrolling
  // ========================================
  
  // Check if Lenis is loaded
  if (typeof Lenis !== 'undefined') {
    const lenis = new Lenis({
      duration: 1.2,
      easing: (t) => Math.min(1, 1.001 - Math.pow(2, -10 * t)),
      direction: 'vertical',
      gestureDirection: 'vertical',
      smooth: true,
      smoothTouch: false,
      touchMultiplier: 2
    });
    
    function raf(time) {
      lenis.raf(time);
      requestAnimationFrame(raf);
    }
    
    requestAnimationFrame(raf);
    
    // Smooth scroll on anchor links
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
      anchor.addEventListener('click', function (e) {
        e.preventDefault();
        const target = document.querySelector(this.getAttribute('href'));
        if (target) {
          lenis.scrollTo(target);
        }
      });
    });
  } else {
    console.warn('Lenis library not loaded. Smooth scrolling will not be available.');
  }
  
});
