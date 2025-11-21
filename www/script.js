// ===== Wait for DOM to be fully loaded =====
document.addEventListener('DOMContentLoaded', function() {
  
  // ===== Initialize Lenis Smooth Scrolling =====
  if (typeof Lenis !== 'undefined') {
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
      const particleCount = 8000;
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

      // Custom shader material for better particle rendering
      const material = new THREE.ShaderMaterial({
        uniforms: {
          pointTexture: { value: null }
        },
        vertexShader: `
          attribute float size;
          attribute vec3 color;
          varying vec3 vColor;
          
          void main() {
            vColor = color;
            vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);
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

      // Animation loop
      function animate() {
        requestAnimationFrame(animate);

        // Smooth rotation based on mouse position
        targetRotationY = mouseX * 0.3;
        targetRotationX = mouseY * 0.2;
        
        particles.rotation.y += (targetRotationY - particles.rotation.y) * 0.05;
        particles.rotation.x += (targetRotationX - particles.rotation.x) * 0.05;
        
        // Continuous slow rotation
        particles.rotation.y += 0.001;
        
        // Animate particle positions (subtle wave effect)
        const positions = particles.geometry.attributes.position.array;
        const time = Date.now() * 0.0001;
        
        for (let i = 0; i < particleCount; i++) {
          const i3 = i * 3;
          const x = positions[i3];
          const z = positions[i3 + 2];
          positions[i3 + 1] = Math.sin(x * 0.1 + time) * Math.cos(z * 0.1 + time) * 5 + Math.sin(time + i) * 0.5;
        }
        
        particles.geometry.attributes.position.needsUpdate = true;

        renderer.render(scene, camera);
      }

      animate();

      // Handle window resize
      window.addEventListener('resize', () => {
        camera.aspect = window.innerWidth / window.innerHeight;
        camera.updateProjectionMatrix();
        renderer.setSize(window.innerWidth, window.innerHeight);
        renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
      });
    }
  }
});
