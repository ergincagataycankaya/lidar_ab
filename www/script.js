// ===== Wait for DOM to be fully loaded =====
document.addEventListener('DOMContentLoaded', function() {
  
  // ===== Initialize Lenis Smooth Scrolling =====
  if (typeof Lenis !== 'undefined') {
    try {
      const lenis = new Lenis({
        duration: 1.2,
        easing: (t) => Math.min(1, 1.001 - Math.pow(2, -10 * t)),
        orientation: 'vertical',
        gestureOrientation: 'vertical',
        smoothWheel: true,
        wheelMultiplier: 1,
        smoothTouch: false,
        touchMultiplier: 2,
        infinite: false,
      });

      function raf(time) {
        lenis.raf(time);
        requestAnimationFrame(raf);
      }

      requestAnimationFrame(raf);
    } catch (error) {
      console.warn('Lenis smooth scrolling failed to initialize, using default scrolling:', error);
    }
  }

  // ===== Initialize Three.js Scene =====
  if (typeof THREE !== 'undefined') {
    const container = document.getElementById('canvas-container');
    
    if (container) {
      // Scene setup
      const scene = new THREE.Scene();
      const camera = new THREE.PerspectiveCamera(
        75,
        window.innerWidth / window.innerHeight,
        0.1,
        1000
      );
      camera.position.z = 50;

      const renderer = new THREE.WebGLRenderer({ 
        alpha: true, 
        antialias: true 
      });
      renderer.setSize(window.innerWidth, window.innerHeight);
      renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
      container.appendChild(renderer.domElement);

      // Create particle system (LiDAR point cloud effect)
      // Adjust particle count based on device performance
      // Use media query for better device detection than user agent
      const isMobile = window.matchMedia('(max-width: 768px)').matches || 
                       (window.matchMedia('(pointer: coarse)').matches && window.matchMedia('(hover: none)').matches);
      const particleCount = isMobile ? 4000 : 8000;
      const positions = new Float32Array(particleCount * 3);
      const colors = new Float32Array(particleCount * 3);
      const sizes = new Float32Array(particleCount);

      // Generate particles in a wave/terrain pattern
      for (let i = 0; i < particleCount; i++) {
        const i3 = i * 3;
        
        // Create a wave/terrain pattern
        const x = (Math.random() - 0.5) * 100;
        const z = (Math.random() - 0.5) * 100;
        const y = Math.sin(x * 0.1) * Math.cos(z * 0.1) * 5 + (Math.random() - 0.5) * 3;
        
        positions[i3] = x;
        positions[i3 + 1] = y;
        positions[i3 + 2] = z;
        
        // Color gradient based on height (blue to green)
        const colorMix = (y + 10) / 20;
        colors[i3] = 0.1 + colorMix * 0.2;      // R
        colors[i3 + 1] = 0.4 + colorMix * 0.4;  // G
        colors[i3 + 2] = 0.8 - colorMix * 0.3;  // B
        
        sizes[i] = Math.random() * 2 + 0.5;
      }

      const geometry = new THREE.BufferGeometry();
      geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
      geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
      geometry.setAttribute('size', new THREE.BufferAttribute(sizes, 1));

      // Custom shader material with GPU-based animation for better performance
      const material = new THREE.ShaderMaterial({
        uniforms: {
          time: { value: 0.0 },
          pointTexture: { value: null }
        },
        vertexShader: `
          uniform float time;
          attribute float size;
          attribute vec3 color;
          varying vec3 vColor;
          
          void main() {
            vColor = color;
            
            // Animate particle positions in the vertex shader (GPU)
            vec3 pos = position;
            float wave = sin(pos.x * 0.1 + time) * cos(pos.z * 0.1 + time) * 5.0;
            float offset = sin(time + float(gl_VertexID)) * 0.5;
            pos.y = wave + offset;
            
            vec4 mvPosition = modelViewMatrix * vec4(pos, 1.0);
            gl_PointSize = size * (300.0 / -mvPosition.z);
            gl_Position = projectionMatrix * mvPosition;
          }
        `,
        fragmentShader: `
          varying vec3 vColor;
          
          void main() {
            float r = distance(gl_PointCoord, vec2(0.5, 0.5));
            if (r > 0.5) discard;
            
            float alpha = 1.0 - smoothstep(0.0, 0.5, r);
            gl_FragColor = vec4(vColor, alpha * 0.8);
          }
        `,
        transparent: true,
        vertexColors: true,
        blending: THREE.AdditiveBlending,
        depthWrite: false
      });

      const particles = new THREE.Points(geometry, material);
      scene.add(particles);

      // Mouse interaction variables
      let mouseX = 0;
      let mouseY = 0;
      let targetRotationX = 0;
      let targetRotationY = 0;

      // Mouse move event for parallax effect
      document.addEventListener('mousemove', (event) => {
        mouseX = (event.clientX / window.innerWidth) * 2 - 1;
        mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
      });

      // Animation loop (optimized - animation now runs on GPU via vertex shader)
      const TIME_SCALE = 0.0001; // Controls animation speed
      let lastTime = 0;
      
      function animate(currentTime) {
        requestAnimationFrame(animate);

        // Use performance timing for better accuracy
        const deltaTime = currentTime - lastTime;
        lastTime = currentTime;

        // Update time uniform for GPU-based animation
        // Use modulo to prevent precision issues with large time values
        material.uniforms.time.value = (currentTime * TIME_SCALE) % (Math.PI * 2);

        // Smooth rotation based on mouse position
        targetRotationY = mouseX * 0.3;
        targetRotationX = mouseY * 0.2;
        
        particles.rotation.y += (targetRotationY - particles.rotation.y) * 0.05;
        particles.rotation.x += (targetRotationX - particles.rotation.x) * 0.05;
        
        // Continuous slow rotation
        particles.rotation.y += 0.001;

        renderer.render(scene, camera);
      }

      animate(0);

      // Handle window resize with debouncing
      let resizeTimeout;
      window.addEventListener('resize', () => {
        clearTimeout(resizeTimeout);
        resizeTimeout = setTimeout(() => {
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize(window.innerWidth, window.innerHeight);
          renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
        }, 150); // 150ms debounce
      });
    }
  }
});
