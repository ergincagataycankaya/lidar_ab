// Wait for DOM to be fully loaded
document.addEventListener('DOMContentLoaded', function() {
  
  // ===== LENIS SMOOTH SCROLLING INITIALIZATION =====
  let lenis;
  if (typeof Lenis !== 'undefined') {
    lenis = new Lenis({
      duration: 1.2,
      easing: (t) => Math.min(1, 1.001 - Math.pow(2, -10 * t)),
      smooth: true,
      smoothTouch: false,
    });
  }

  // ===== THREE.JS 3D BACKGROUND INITIALIZATION =====
  if (typeof THREE !== 'undefined') {
    
    // Get canvas container
    const container = document.getElementById('canvas-container');
    if (!container) {
      console.warn('Canvas container not found, skipping 3D background initialization');
    } else {
      // Initialize 3D background only if container exists
      initThreeJsBackground(container, lenis);
    }
  }
});

// Function to initialize Three.js background
function initThreeJsBackground(container, lenis) {
  if (typeof THREE === 'undefined') return;

    // Scene setup
    const scene = new THREE.Scene();
    scene.fog = new THREE.FogExp2(0x667eea, 0.0008);

    // Camera setup
    const camera = new THREE.PerspectiveCamera(
      75,
      window.innerWidth / window.innerHeight,
      0.1,
      1000
    );
    camera.position.z = 50;

    // Renderer setup
    const renderer = new THREE.WebGLRenderer({ 
      antialias: true, 
      alpha: true 
    });
    renderer.setSize(window.innerWidth, window.innerHeight);
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    renderer.setClearColor(0x667eea, 1);
    container.appendChild(renderer.domElement);

    // ===== PARTICLE SYSTEM - LiDAR-style Point Cloud =====
    // Adjust particle count based on device performance
    const isMobile = window.innerWidth < 768;
    const particleCount = isMobile ? 5000 : 10000;
    
    const positions = new Float32Array(particleCount * 3);
    const colors = new Float32Array(particleCount * 3);

    // Create particles in a terrain-like pattern
    for (let i = 0; i < particleCount; i++) {
      const i3 = i * 3;
      
      // Create a wave/terrain pattern
      const x = (Math.random() - 0.5) * 200;
      const z = (Math.random() - 0.5) * 200;
      const y = Math.sin(x * 0.1) * Math.cos(z * 0.1) * 10 + (Math.random() - 0.5) * 5;
      
      positions[i3] = x;
      positions[i3 + 1] = y;
      positions[i3 + 2] = z;

      // Color gradient based on height (like LiDAR elevation data)
      const colorValue = (y + 15) / 30; // Normalize to 0-1
      colors[i3] = 0.4 + colorValue * 0.3;     // R
      colors[i3 + 1] = 0.5 + colorValue * 0.4; // G
      colors[i3 + 2] = 0.9;                     // B
    }

    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
    geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));

    // Particle material
    const material = new THREE.PointsMaterial({
      size: 0.8,
      vertexColors: true,
      transparent: true,
      opacity: 0.8,
      blending: THREE.AdditiveBlending,
      depthWrite: false
    });

    const particles = new THREE.Points(geometry, material);
    scene.add(particles);

    // Add ambient light
    const ambientLight = new THREE.AmbientLight(0xffffff, 0.5);
    scene.add(ambientLight);

    // ===== MOUSE INTERACTION - Parallax Effect =====
    let mouseX = 0;
    let mouseY = 0;
    let targetX = 0;
    let targetY = 0;

    document.addEventListener('mousemove', (event) => {
      mouseX = (event.clientX / window.innerWidth) * 2 - 1;
      mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
    });

    // ===== COMBINED ANIMATION LOOP =====
    let time = 0;
    const positionsArray = particles.geometry.attributes.position.array;
    
    function animate(currentTime) {
      requestAnimationFrame(animate);
      
      // Update Lenis smooth scrolling
      if (lenis) {
        lenis.raf(currentTime);
      }
      
      time += 0.001;

      // Smooth mouse following with easing
      targetX += (mouseX - targetX) * 0.05;
      targetY += (mouseY - targetY) * 0.05;

      // Rotate particle system slowly
      particles.rotation.y = time * 0.3;
      particles.rotation.x = time * 0.2;

      // Add parallax effect based on mouse position
      particles.rotation.y += targetX * 0.3;
      particles.rotation.x += targetY * 0.3;

      // Animate individual particles with wave motion
      for (let i = 0; i < particleCount; i++) {
        const i3 = i * 3;
        const x = positionsArray[i3];
        const z = positionsArray[i3 + 2];
        
        // Create animated wave effect
        positionsArray[i3 + 1] = Math.sin(x * 0.1 + time * 2) * Math.cos(z * 0.1 + time * 2) * 10 + 
                           Math.sin(time * 3 + i * 0.01) * 2;
      }
      particles.geometry.attributes.position.needsUpdate = true;

      renderer.render(scene, camera);
    }

    animate();

    // ===== WINDOW RESIZE HANDLER =====
    function onWindowResize() {
      camera.aspect = window.innerWidth / window.innerHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(window.innerWidth, window.innerHeight);
    }

    window.addEventListener('resize', onWindowResize, false);

  // Clean up on page navigation (Shiny page change)
  window.addEventListener('pagehide', () => {
    if (geometry) geometry.dispose();
    if (material) material.dispose();
    if (renderer) renderer.dispose();
  });
}
