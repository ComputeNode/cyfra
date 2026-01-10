import type { ReactNode } from 'react';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

const styles = {
  // Hero Section
  heroSection: {
    position: 'relative' as const,
    minHeight: '90vh',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    overflow: 'hidden',
    background: 'linear-gradient(180deg, #0a0a0f 0%, #0f1018 50%, #0a0a0f 100%)',
  },
  heroBackground: {
    position: 'absolute' as const,
    inset: 0,
    background: `
      radial-gradient(ellipse 80% 50% at 50% -20%, rgba(0, 212, 170, 0.15), transparent),
      radial-gradient(ellipse 60% 40% at 80% 60%, rgba(123, 97, 255, 0.1), transparent),
      radial-gradient(ellipse 50% 30% at 20% 80%, rgba(0, 168, 204, 0.1), transparent)
    `,
  },
  gridPattern: {
    position: 'absolute' as const,
    inset: 0,
    backgroundImage: `
      linear-gradient(rgba(255, 255, 255, 0.02) 1px, transparent 1px),
      linear-gradient(90deg, rgba(255, 255, 255, 0.02) 1px, transparent 1px)
    `,
    backgroundSize: '60px 60px',
    maskImage: 'radial-gradient(ellipse 100% 100% at 50% 0%, black, transparent 70%)',
  },
  heroContent: {
    position: 'relative' as const,
    zIndex: 10,
    textAlign: 'center' as const,
    padding: '2rem',
    maxWidth: '900px',
  },
  heroTitle: {
    fontSize: 'clamp(3.5rem, 10vw, 7rem)',
    fontWeight: 800,
    letterSpacing: '-0.04em',
    marginBottom: '0.5rem',
    background: 'linear-gradient(135deg, #fff 0%, #00d4aa 50%, #7b61ff 100%)',
    backgroundClip: 'text',
    WebkitBackgroundClip: 'text',
    WebkitTextFillColor: 'transparent',
    backgroundSize: '200% 200%',
    animation: 'gradient-shift 8s ease infinite',
  },
  heroTagline: {
    fontSize: 'clamp(1.25rem, 3vw, 1.75rem)',
    color: 'rgba(255, 255, 255, 0.7)',
    fontWeight: 400,
    marginBottom: '2.5rem',
    letterSpacing: '-0.01em',
  },
  heroButtons: {
    display: 'flex',
    gap: '1rem',
    justifyContent: 'center',
    flexWrap: 'wrap' as const,
  },
  primaryButton: {
    background: 'linear-gradient(135deg, #00d4aa 0%, #00a8cc 100%)',
    color: '#000',
    padding: '1rem 2.5rem',
    borderRadius: '12px',
    fontSize: '1.1rem',
    fontWeight: 600,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.5rem',
    transition: 'all 0.3s ease',
    boxShadow: '0 4px 20px rgba(0, 212, 170, 0.3)',
    border: 'none',
  },
  secondaryButton: {
    background: 'rgba(255, 255, 255, 0.05)',
    color: '#fff',
    padding: '1rem 2.5rem',
    borderRadius: '12px',
    fontSize: '1.1rem',
    fontWeight: 500,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.5rem',
    transition: 'all 0.3s ease',
    border: '1px solid rgba(255, 255, 255, 0.1)',
  },

  // Section styles
  section: {
    padding: '6rem 2rem',
    background: '#0a0a0f',
  },
  sectionAlt: {
    padding: '6rem 2rem',
    background: 'linear-gradient(180deg, #0f1018 0%, #0a0a0f 100%)',
  },
  container: {
    maxWidth: '1200px',
    margin: '0 auto',
  },
  sectionTitle: {
    fontSize: 'clamp(2rem, 5vw, 3rem)',
    fontWeight: 700,
    textAlign: 'center' as const,
    marginBottom: '1rem',
    color: '#fff',
    letterSpacing: '-0.03em',
  },
  sectionSubtitle: {
    fontSize: '1.25rem',
    color: 'rgba(255, 255, 255, 0.6)',
    textAlign: 'center' as const,
    marginBottom: '4rem',
    maxWidth: '700px',
    margin: '0 auto 4rem',
  },

  // Feature cards
  featuresGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
    gap: '1.5rem',
    marginTop: '3rem',
  },
  featureCard: {
    background: 'rgba(255, 255, 255, 0.02)',
    border: '1px solid rgba(255, 255, 255, 0.06)',
    borderRadius: '16px',
    padding: '2rem',
    transition: 'all 0.3s ease',
  },
  featureIcon: {
    fontSize: '2.5rem',
    marginBottom: '1rem',
  },
  featureTitle: {
    fontSize: '1.25rem',
    fontWeight: 600,
    color: '#fff',
    marginBottom: '0.75rem',
  },
  featureDesc: {
    color: 'rgba(255, 255, 255, 0.6)',
    lineHeight: 1.7,
    fontSize: '0.95rem',
  },

  // Showcase gallery
  showcaseGrid: {
    display: 'grid',
    gap: '3rem',
  },
  showcaseItem: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
    gap: '3rem',
    alignItems: 'center',
    padding: '2rem',
    background: 'rgba(255, 255, 255, 0.02)',
    border: '1px solid rgba(255, 255, 255, 0.06)',
    borderRadius: '24px',
  },
  showcaseItemReverse: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
    gap: '3rem',
    alignItems: 'center',
    padding: '2rem',
    background: 'rgba(255, 255, 255, 0.02)',
    border: '1px solid rgba(255, 255, 255, 0.06)',
    borderRadius: '24px',
  },
  showcaseMedia: {
    borderRadius: '16px',
    overflow: 'hidden',
    boxShadow: '0 20px 60px rgba(0, 0, 0, 0.4)',
    border: '1px solid rgba(255, 255, 255, 0.1)',
  },
  showcaseGif: {
    width: '100%',
    height: 'auto',
    display: 'block',
  },
  showcaseContent: {
    padding: '1rem',
  },
  showcaseLabel: {
    display: 'inline-block',
    background: 'linear-gradient(135deg, rgba(0, 212, 170, 0.2), rgba(123, 97, 255, 0.2))',
    color: '#00d4aa',
    padding: '0.5rem 1rem',
    borderRadius: '8px',
    fontSize: '0.85rem',
    fontWeight: 600,
    marginBottom: '1rem',
    border: '1px solid rgba(0, 212, 170, 0.3)',
  },
  showcaseTitle: {
    fontSize: '1.75rem',
    fontWeight: 700,
    color: '#fff',
    marginBottom: '1rem',
    letterSpacing: '-0.02em',
  },
  showcaseDesc: {
    color: 'rgba(255, 255, 255, 0.7)',
    lineHeight: 1.8,
    fontSize: '1.05rem',
    marginBottom: '1.5rem',
  },
  showcaseButtons: {
    display: 'flex',
    gap: '0.75rem',
    flexWrap: 'wrap' as const,
  },
  showcaseButton: {
    background: 'rgba(255, 255, 255, 0.05)',
    color: '#fff',
    padding: '0.75rem 1.25rem',
    borderRadius: '10px',
    fontSize: '0.9rem',
    fontWeight: 500,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.5rem',
    transition: 'all 0.3s ease',
    border: '1px solid rgba(255, 255, 255, 0.1)',
  },
  showcaseButtonPrimary: {
    background: 'linear-gradient(135deg, #00d4aa 0%, #00a8cc 100%)',
    color: '#000',
    padding: '0.75rem 1.25rem',
    borderRadius: '10px',
    fontSize: '0.9rem',
    fontWeight: 600,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.5rem',
    transition: 'all 0.3s ease',
    border: 'none',
    boxShadow: '0 4px 15px rgba(0, 212, 170, 0.25)',
  },

  // Platform badges
  platformSection: {
    display: 'flex',
    justifyContent: 'center',
    gap: '1rem',
    flexWrap: 'wrap' as const,
    marginTop: '4rem',
  },
  platformBadge: {
    background: 'rgba(255, 255, 255, 0.03)',
    border: '1px solid rgba(255, 255, 255, 0.08)',
    borderRadius: '12px',
    padding: '1rem 1.5rem',
    display: 'flex',
    alignItems: 'center',
    gap: '0.75rem',
    color: 'rgba(255, 255, 255, 0.8)',
    fontSize: '0.95rem',
    fontWeight: 500,
  },

  // CTA Section
  ctaSection: {
    padding: '8rem 2rem',
    background: `
      radial-gradient(ellipse 100% 100% at 50% 100%, rgba(0, 212, 170, 0.08), transparent),
      #0a0a0f
    `,
    textAlign: 'center' as const,
  },
  ctaTitle: {
    fontSize: 'clamp(2rem, 5vw, 3.5rem)',
    fontWeight: 700,
    color: '#fff',
    marginBottom: '1.5rem',
    letterSpacing: '-0.03em',
  },
  ctaDesc: {
    fontSize: '1.25rem',
    color: 'rgba(255, 255, 255, 0.6)',
    marginBottom: '2.5rem',
    maxWidth: '600px',
    margin: '0 auto 2.5rem',
  },
};

function HeroSection() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <section style={styles.heroSection}>
      <div style={styles.heroBackground} />
      <div style={styles.gridPattern} />
      <div style={styles.heroContent}>
        <Heading as="h1" style={styles.heroTitle}>
          {siteConfig.title}
        </Heading>
        <p style={styles.heroTagline}>
          Write GPU compute pipelines in pure Scala 3.<br />
          Run anywhere.
        </p>
        <div style={styles.heroButtons}>
          <Link to="/docs/getting-started" style={styles.primaryButton}>
            Get Started →
          </Link>
          <Link to="https://github.com/computenode/cyfra" style={styles.secondaryButton}>
            <svg width="20" height="20" viewBox="0 0 24 24" fill="currentColor">
              <path d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"/>
            </svg>
            GitHub
          </Link>
        </div>
      </div>
    </section>
  );
}

function FeaturesSection() {
  const features = [
    {
      icon: '⚡',
      title: 'Pure Scala 3 DSL',
      description: 'Write GPU code using familiar Scala syntax. Case classes become GPU structs, functions compile to GPU programs.',
    },
    {
      icon: '🎯',
      title: 'Vulkan Runtime',
      description: 'Execute on any Vulkan-compatible GPU. Works with NVIDIA, AMD, Intel, and Apple (via MoltenVK) graphics hardware.',
    },
    {
      icon: '🌍',
      title: 'Cross-Platform',
      description: 'Runs on Linux, Windows, and macOS. No platform-specific code required.',
    },
    {
      icon: '🧩',
      title: 'Powerful Pipeline DSL',
      description: 'Build expressive, type-safe GPU pipelines with safety and minimal boilerplate.',
    },
    {
      icon: '🔧',
      title: 'Type-Safe Coding',
      description: 'Leverage Scala\'s type system for safer GPU programming. Catch errors at compile time, not runtime.',
    },
    {
      icon: '🚀',
      title: 'High Performance',
      description: 'Direct compilation to SPIR-V means your Scala code runs at native GPU speeds with zero overhead.',
    },
  ];

  return (
    <section style={styles.section}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>Why Cyfra?</h2>
        <p style={styles.sectionSubtitle}>
          A powerful toolkit for GPU programming that brings the elegance of Scala to compute shaders
        </p>
        <div style={styles.featuresGrid}>
          {features.map((feature, idx) => (
            <div key={idx} style={styles.featureCard}>
              <div style={styles.featureIcon}>{feature.icon}</div>
              <h3 style={styles.featureTitle}>{feature.title}</h3>
              <p style={styles.featureDesc}>{feature.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function ShowcaseSection() {
  return (
    <section style={styles.sectionAlt}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>See It In Action</h2>
        <p style={styles.sectionSubtitle}>
          Real examples of what you can create with Cyfra
        </p>
        
        <div style={styles.showcaseGrid}>
          {/* GPU Programming - Ray Tracing */}
          <div style={styles.showcaseItem}>
            <div style={styles.showcaseMedia}>
              <img 
                src="https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d"
                alt="Ray traced animation showing reflective spheres"
                style={styles.showcaseGif}
              />
            </div>
            <div style={styles.showcaseContent}>
              <span style={styles.showcaseLabel}>GPU Programming</span>
              <h3 style={styles.showcaseTitle}>GPU Ray Traced Scenes</h3>
              <p style={styles.showcaseDesc}>
                Create stunning ray-traced animations with realistic reflections, shadows, and lighting. 
                The Foton library provides a clean API for defining scenes, materials, and camera paths—all 
                executed at GPU speed.
              </p>
              <div style={styles.showcaseButtons}>
                <Link 
                  to="https://github.com/ComputeNode/cyfra/blob/main/cyfra-examples/src/main/scala/io/computenode/samples/cyfra/foton/AnimatedRaytrace.scala" 
                  style={styles.showcaseButtonPrimary}
                >
                  <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
                    <path d="M9.4 16.6L4.8 12l4.6-4.6L8 6l-6 6 6 6 1.4-1.4zm5.2 0l4.6-4.6-4.6-4.6L16 6l6 6-6 6-1.4-1.4z"/>
                  </svg>
                  View Code
                </Link>
                <Link 
                  to="https://www.youtube.com/watch?v=Prkp2VUVrYo" 
                  style={styles.showcaseButton}
                >
                  <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
                    <path d="M10 16.5l6-4.5-6-4.5v9zM12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8z"/>
                  </svg>
                  Watch Talk
                </Link>
              </div>
            </div>
          </div>

          {/* Complex GPU Pipelines - Navier-Stokes */}
          <div style={styles.showcaseItemReverse}>
            <div style={styles.showcaseContent}>
              <span style={styles.showcaseLabel}>Complex GPU Pipelines</span>
              <h3 style={styles.showcaseTitle}>Navier-Stokes Fluid Simulation</h3>
              <p style={styles.showcaseDesc}>
                Build sophisticated multi-stage GPU pipelines with Cyfra's expressive DSL. 
                This real-time fluid dynamics simulation solves the Navier-Stokes equations 
                entirely on the GPU, demonstrating complex data flow and iterative compute patterns.
              </p>
              <div style={styles.showcaseButtons}>
                <Link 
                  to="https://github.com/ComputeNode/cyfra/tree/fluids/cyfra-fluids" 
                  style={styles.showcaseButtonPrimary}
                >
                  <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
                    <path d="M9.4 16.6L4.8 12l4.6-4.6L8 6l-6 6 6 6 1.4-1.4zm5.2 0l4.6-4.6-4.6-4.6L16 6l6 6-6 6-1.4-1.4z"/>
                  </svg>
                  View Code
                </Link>
              </div>
            </div>
            <div style={styles.showcaseMedia}>
              <img 
                src="/img/full_fluid_8s.gif"
                alt="Navier-Stokes fluid simulation"
                style={styles.showcaseGif}
              />
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}

function PlatformSection() {
  const platforms = [
    { icon: '🐧', name: 'Linux' },
    { icon: '🪟', name: 'Windows' },
    { icon: '🍎', name: 'macOS' },
    { icon: '🎮', name: 'NVIDIA' },
    { icon: '🔴', name: 'AMD' },
    { icon: '🔵', name: 'Intel' },
  ];

  return (
    <section style={styles.section}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>Runs Everywhere</h2>
        <p style={styles.sectionSubtitle}>
          Works on any platform with Vulkan support—from gaming PCs to integrated graphics
        </p>
        <div style={styles.platformSection}>
          {platforms.map((platform, idx) => (
            <div key={idx} style={styles.platformBadge}>
              <span>{platform.icon}</span>
              <span>{platform.name}</span>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function CTASection() {
  return (
    <section style={styles.ctaSection}>
      <div style={styles.container}>
        <h2 style={styles.ctaTitle}>Ready to Write GPU Code in Scala?</h2>
        <p style={styles.ctaDesc}>
          Get started with Cyfra today and unlock the power of GPU programming with the elegance of Scala.
        </p>
        <Link to="/docs/getting-started" style={styles.primaryButton}>
          Read the Docs →
        </Link>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
  return (
    <Layout
      title="Home"
      description="Cyfra - GPU programming in Scala. Write compute shaders in pure Scala 3, compile to SPIR-V, and run on Vulkan.">
      <main>
        <HeroSection />
        <FeaturesSection />
        <ShowcaseSection />
        <PlatformSection />
        <CTASection />
      </main>
    </Layout>
  );
}
