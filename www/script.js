// Wait for DOM to be ready
document.addEventListener('DOMContentLoaded', function() {
  
  // ===== THREE.JS SETUP =====
  
  const container = document.getElementById('canvas-container');
  
  // Create scene
  const scene = new THREE.Scene();
  
  // Create camera
  const camera = new THREE.PerspectiveCamera(
    75,
    window.innerWidth / window.innerHeight,
    0.1,
    1000
  );
  camera.position.z = 30;
  
  // Create renderer
  const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.setPixelRatio(window.devicePixelRatio);
  container.appendChild(renderer.domElement);
  
  // ===== PARTICLE SYSTEM =====
  
  const particleCount = 2000;
  const particles = new THREE.BufferGeometry();
  const positions = new Float32Array(particleCount * 3);
  const colors = new Float32Array(particleCount * 3);
  
  // Generate random particle positions and colors
  for (let i = 0; i < particleCount * 3; i += 3) {
    // Position: random sphere distribution
    const radius = 50 + Math.random() * 50;
    const theta = Math.random() * Math.PI * 2;
    const phi = Math.acos(Math.random() * 2 - 1);
    
    positions[i] = radius * Math.sin(phi) * Math.cos(theta);
    positions[i + 1] = radius * Math.sin(phi) * Math.sin(theta);
    positions[i + 2] = radius * Math.cos(phi);
    
    // Colors: blue to cyan gradient
    colors[i] = 0.3 + Math.random() * 0.3;     // R
    colors[i + 1] = 0.5 + Math.random() * 0.5; // G
    colors[i + 2] = 0.8 + Math.random() * 0.2; // B
  }
  
  particles.setAttribute('position', new THREE.BufferAttribute(positions, 3));
  particles.setAttribute('color', new THREE.BufferAttribute(colors, 3));
  
  // Create material
  const particleMaterial = new THREE.PointsMaterial({
    size: 0.5,
    vertexColors: true,
    transparent: true,
    opacity: 0.8,
    blending: THREE.AdditiveBlending
  });
  
  // Create particle system
  const particleSystem = new THREE.Points(particles, particleMaterial);
  scene.add(particleSystem);
  
  // ===== MOUSE PARALLAX =====
  
  let mouseX = 0;
  let mouseY = 0;
  let targetX = 0;
  let targetY = 0;
  
  document.addEventListener('mousemove', function(event) {
    mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
  });
  
  // ===== ANIMATION LOOP =====
  
  function animate() {
    requestAnimationFrame(animate);
    
    // Rotate particle system slowly
    particleSystem.rotation.y += 0.0005;
    particleSystem.rotation.x += 0.0002;
    
    // Smooth parallax effect
    targetX = mouseX * 0.5;
    targetY = mouseY * 0.5;
    
    particleSystem.rotation.y += (targetX - particleSystem.rotation.y) * 0.05;
    particleSystem.rotation.x += (targetY - particleSystem.rotation.x) * 0.05;
    
    // Render scene
    renderer.render(scene, camera);
  }
  
  animate();
  
  // ===== LENIS SMOOTH SCROLLING =====
  
  if (typeof Lenis !== 'undefined') {
    const lenis = new Lenis({
      duration: 1.2,
      easing: (t) => Math.min(1, 1.001 - Math.pow(2, -10 * t)),
      direction: 'vertical',
      smooth: true,
      smoothTouch: false
    });
    
    function raf(time) {
      lenis.raf(time);
      requestAnimationFrame(raf);
    }
    
    requestAnimationFrame(raf);
  }
  
  // ===== WINDOW RESIZE HANDLER =====
  
  window.addEventListener('resize', function() {
    // Update camera aspect ratio
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    
    // Update renderer size
    renderer.setSize(window.innerWidth, window.innerHeight);
    renderer.setPixelRatio(window.devicePixelRatio);
  });
  
});
