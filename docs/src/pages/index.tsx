import type { ReactNode } from 'react';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

// Syntax highlighting colors
const syn = {
  keyword: '#f97316',
  type: '#fbbf24',
  string: '#a3e635',
  number: '#60a5fa',
  comment: '#6b7280',
  func: '#c4b5fd',
  plain: '#e7e5e4',
};

const styles = {
  heroSection: {
    position: 'relative' as const,
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    padding: '6rem 2rem 5rem',
    overflow: 'hidden',
    background: '#0c0a09',
  },
  heroBackground: {
    position: 'absolute' as const,
    inset: 0,
    background: `
      radial-gradient(ellipse 60% 40% at 50% 0%, rgba(234, 88, 12, 0.12), transparent),
      radial-gradient(ellipse 40% 30% at 80% 70%, rgba(220, 38, 38, 0.08), transparent)
    `,
  },
  gridPattern: {
    position: 'absolute' as const,
    inset: 0,
    backgroundImage: `
      linear-gradient(rgba(255, 255, 255, 0.015) 1px, transparent 1px),
      linear-gradient(90deg, rgba(255, 255, 255, 0.015) 1px, transparent 1px)
    `,
    backgroundSize: '80px 80px',
    maskImage: 'radial-gradient(ellipse 80% 60% at 50% 0%, black, transparent 70%)',
  },
  heroContent: {
    position: 'relative' as const,
    zIndex: 10,
    textAlign: 'center' as const,
    padding: '2rem',
    maxWidth: '800px',
  },
  heroTitle: {
    fontSize: 'clamp(3rem, 8vw, 5.5rem)',
    fontWeight: 700,
    letterSpacing: '-0.03em',
    marginBottom: '0.75rem',
    color: '#fafaf9',
  },
  heroTagline: {
    fontSize: 'clamp(1.1rem, 2.5vw, 1.4rem)',
    color: '#a8a29e',
    fontWeight: 400,
    marginBottom: '2rem',
    lineHeight: 1.6,
  },
  betaBadge: {
    display: 'inline-block',
    background: 'rgba(234, 88, 12, 0.15)',
    color: '#fb923c',
    padding: '0.5rem 1rem',
    borderRadius: '6px',
    fontSize: '0.85rem',
    fontWeight: 500,
    marginBottom: '2rem',
    border: '1px solid rgba(234, 88, 12, 0.3)',
  },
  heroButtons: {
    display: 'flex',
    gap: '1rem',
    justifyContent: 'center',
    flexWrap: 'wrap' as const,
  },
  primaryButton: {
    background: '#ea580c',
    color: '#fff',
    padding: '0.875rem 2rem',
    borderRadius: '8px',
    fontSize: '1rem',
    fontWeight: 600,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.5rem',
    transition: 'background 0.2s ease',
    border: 'none',
  },
  secondaryButton: {
    background: 'transparent',
    color: '#fafaf9',
    padding: '0.875rem 2rem',
    borderRadius: '8px',
    fontSize: '1rem',
    fontWeight: 500,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.5rem',
    transition: 'all 0.2s ease',
    border: '1px solid #44403c',
  },

  section: {
    padding: '5rem 2rem',
    background: '#0c0a09',
  },
  sectionAlt: {
    padding: '5rem 2rem',
    background: '#0a0908',
  },
  container: {
    maxWidth: '1100px',
    margin: '0 auto',
  },
  sectionTitle: {
    fontSize: 'clamp(1.75rem, 4vw, 2.5rem)',
    fontWeight: 600,
    textAlign: 'center' as const,
    marginBottom: '0.75rem',
    color: '#fafaf9',
    letterSpacing: '-0.02em',
  },
  sectionSubtitle: {
    fontSize: '1.1rem',
    color: '#a8a29e',
    textAlign: 'center' as const,
    marginBottom: '3rem',
    maxWidth: '600px',
    margin: '0 auto 3rem',
  },

  featuresGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))',
    gap: '1.25rem',
    marginTop: '2.5rem',
  },
  featureCard: {
    background: '#1c1917',
    border: '1px solid #292524',
    borderRadius: '12px',
    padding: '1.5rem',
  },
  featureTitle: {
    fontSize: '1.1rem',
    fontWeight: 600,
    color: '#fafaf9',
    marginBottom: '0.5rem',
  },
  featureDesc: {
    color: '#a8a29e',
    lineHeight: 1.6,
    fontSize: '0.9rem',
  },

  // Code snippets section - 2 columns, 3 rows
  snippetsGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(2, 1fr)',
    gap: '1.25rem',
    marginTop: '2.5rem',
  },
  snippetCard: {
    background: '#1c1917',
    border: '1px solid #292524',
    borderRadius: '12px',
    overflow: 'hidden',
    display: 'flex',
    flexDirection: 'column' as const,
  },
  snippetHeader: {
    padding: '0.75rem 1rem',
    borderBottom: '1px solid #292524',
    display: 'flex',
    alignItems: 'center',
    gap: '0.5rem',
  },
  snippetDot: {
    width: '8px',
    height: '8px',
    borderRadius: '50%',
    background: '#ea580c',
  },
  snippetTitle: {
    fontSize: '0.85rem',
    fontWeight: 500,
    color: '#fafaf9',
  },
  snippetCode: {
    padding: '1.25rem',
    margin: 0,
    background: '#0f0e0d',
    fontSize: '0.8rem',
    lineHeight: 1.75,
    fontFamily: '"JetBrains Mono", "Fira Code", "Consolas", monospace',
    overflowX: 'auto' as const,
    flex: 1,
  },
  snippetDesc: {
    padding: '0.875rem 1rem',
    borderTop: '1px solid #292524',
    color: '#a8a29e',
    fontSize: '0.85rem',
    lineHeight: 1.5,
  },

  // Showcase with fixed aspect ratio
  showcaseGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(2, 1fr)',
    gap: '1.5rem',
  },
  showcaseItem: {
    background: '#1c1917',
    border: '1px solid #292524',
    borderRadius: '16px',
    overflow: 'hidden',
    display: 'flex',
    flexDirection: 'column' as const,
  },
  showcaseMedia: {
    width: '100%',
    height: '280px',
    overflow: 'hidden',
    borderBottom: '1px solid #292524',
  },
  showcaseGif: {
    width: '100%',
    height: '100%',
    objectFit: 'cover' as const,
    display: 'block',
  },
  showcaseContent: {
    padding: '1.25rem',
    flex: 1,
    display: 'flex',
    flexDirection: 'column' as const,
  },
  showcaseLabel: {
    display: 'inline-block',
    background: 'rgba(234, 88, 12, 0.15)',
    color: '#fb923c',
    padding: '0.35rem 0.6rem',
    borderRadius: '5px',
    fontSize: '0.75rem',
    fontWeight: 500,
    marginBottom: '0.6rem',
    border: '1px solid rgba(234, 88, 12, 0.25)',
    alignSelf: 'flex-start',
  },
  showcaseTitle: {
    fontSize: '1.2rem',
    fontWeight: 600,
    color: '#fafaf9',
    marginBottom: '0.5rem',
    letterSpacing: '-0.01em',
  },
  showcaseDesc: {
    color: '#a8a29e',
    lineHeight: 1.6,
    fontSize: '0.9rem',
    marginBottom: '1rem',
    flex: 1,
  },
  showcaseButtons: {
    display: 'flex',
    gap: '0.6rem',
    flexWrap: 'wrap' as const,
  },
  showcaseButton: {
    background: 'transparent',
    color: '#fafaf9',
    padding: '0.5rem 0.875rem',
    borderRadius: '6px',
    fontSize: '0.8rem',
    fontWeight: 500,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.4rem',
    transition: 'all 0.2s ease',
    border: '1px solid #44403c',
  },
  showcaseButtonPrimary: {
    background: '#ea580c',
    color: '#fff',
    padding: '0.5rem 0.875rem',
    borderRadius: '6px',
    fontSize: '0.8rem',
    fontWeight: 600,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.4rem',
    transition: 'background 0.2s ease',
    border: 'none',
  },
  showcaseButtonDisabled: {
    background: 'transparent',
    color: '#57534e',
    padding: '0.5rem 0.875rem',
    borderRadius: '6px',
    fontSize: '0.8rem',
    fontWeight: 500,
    textDecoration: 'none',
    display: 'inline-flex',
    alignItems: 'center',
    gap: '0.4rem',
    border: '1px solid #292524',
    cursor: 'default',
  },
  showcaseLabelWip: {
    display: 'inline-block',
    background: 'rgba(113, 113, 122, 0.15)',
    color: '#a1a1aa',
    padding: '0.35rem 0.6rem',
    borderRadius: '5px',
    fontSize: '0.75rem',
    fontWeight: 500,
    marginBottom: '0.6rem',
    border: '1px solid rgba(113, 113, 122, 0.25)',
    alignSelf: 'flex-start',
  },

  platformSection: {
    display: 'flex',
    justifyContent: 'center',
    gap: '0.75rem',
    flexWrap: 'wrap' as const,
    marginTop: '3rem',
  },
  platformBadge: {
    background: '#1c1917',
    border: '1px solid #292524',
    borderRadius: '8px',
    padding: '0.75rem 1.25rem',
    display: 'flex',
    alignItems: 'center',
    gap: '0.5rem',
    color: '#d6d3d1',
    fontSize: '0.9rem',
    fontWeight: 500,
  },

  ctaSection: {
    padding: '6rem 2rem',
    background: '#0c0a09',
    textAlign: 'center' as const,
    borderTop: '1px solid #1c1917',
  },
  ctaTitle: {
    fontSize: 'clamp(1.5rem, 4vw, 2.25rem)',
    fontWeight: 600,
    color: '#fafaf9',
    marginBottom: '1rem',
    letterSpacing: '-0.02em',
  },
  ctaDesc: {
    fontSize: '1.05rem',
    color: '#a8a29e',
    marginBottom: '2rem',
    maxWidth: '500px',
    margin: '0 auto 2rem',
  },
};

// Helper components for syntax highlighting
const K = ({ children }: { children: ReactNode }) => <span style={{ color: syn.keyword }}>{children}</span>;
const T = ({ children }: { children: ReactNode }) => <span style={{ color: syn.type }}>{children}</span>;
const S = ({ children }: { children: ReactNode }) => <span style={{ color: syn.string }}>{children}</span>;
const N = ({ children }: { children: ReactNode }) => <span style={{ color: syn.number }}>{children}</span>;
const C = ({ children }: { children: ReactNode }) => <span style={{ color: syn.comment }}>{children}</span>;
const F = ({ children }: { children: ReactNode }) => <span style={{ color: syn.func }}>{children}</span>;
const P = ({ children }: { children: ReactNode }) => <span style={{ color: syn.plain }}>{children}</span>;

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
          GPU compute pipelines in pure Scala 3.<br />
          Run anywhere.
        </p>
        <div style={styles.betaBadge}>
          ⚠️ Beta release - not production ready. For experimentation and feedback.
        </div>
        <div style={styles.heroButtons}>
          <Link to="/docs/getting-started" style={styles.primaryButton}>
            Get Started
          </Link>
          <Link to="https://github.com/computenode/cyfra" style={styles.secondaryButton}>
            <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor">
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
      title: 'Pure Scala 3 DSL',
      description: 'Write GPU code using familiar Scala syntax. Case classes become GPU structs, functions compile to shaders.',
    },
    {
      title: 'Vulkan Backend',
      description: 'Direct execution on Vulkan-compatible GPUs. NVIDIA, AMD, Intel, and Apple via MoltenVK.',
    },
    {
      title: 'Composable Pipelines',
      description: 'Chain GPU programs together. Intermediate data stays on the GPU. No round-trips to CPU.',
    },
    {
      title: 'Type-Safe',
      description: 'Scala\'s type system catches GPU programming errors at compile time.',
    },
    {
      title: 'Cross-Platform',
      description: 'Linux, Windows, macOS. Write once, run on any Vulkan-capable hardware.',
    },
    {
      title: 'Zero Overhead',
      description: 'Direct compilation to SPIR-V. Your Scala code runs at native GPU speeds.',
    },
  ];

  return (
    <section style={styles.section}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>Why Cyfra?</h2>
        <p style={styles.sectionSubtitle}>
          GPU programming with the expressiveness of Scala
        </p>
        <div style={styles.featuresGrid}>
          {features.map((feature, idx) => (
            <div key={idx} style={styles.featureCard}>
              <h3 style={styles.featureTitle}>{feature.title}</h3>
              <p style={styles.featureDesc}>{feature.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function CodeSnippetsSection() {
  return (
    <section style={styles.sectionAlt}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>Code That Runs on GPU</h2>
        <p style={styles.sectionSubtitle}>
          Write Scala. Execute on thousands of GPU cores.
        </p>
        <div className="snippets-grid" style={styles.snippetsGrid}>
          {/* Row 1: Vector Operations | Sequence Comprehensions */}
          <div style={styles.snippetCard}>
            <div style={styles.snippetHeader}>
              <div style={styles.snippetDot} />
              <span style={styles.snippetTitle}>Vector Operations</span>
            </div>
            <pre style={styles.snippetCode}>
<K>val</K> <P>normalize</P> = <T>GFunction</T>[<T>Vec4</T>[<T>Float32</T>], <T>Vec4</T>[<T>Float32</T>]]:{'\n'}
{'  '}<P>v</P> =&gt; <P>v</P> / <F>length</F>(<P>v</P>){'\n'}
{'\n'}
<K>val</K> <P>dot</P> = <T>GFunction</T>[<T>Vec4</T>[<T>Float32</T>], <T>Float32</T>]:{'\n'}
{'  '}<P>v</P> =&gt; <P>v</P> <F>dot</F> (<N>1f</N>, <N>0f</N>, <N>0f</N>, <N>0f</N>)
            </pre>
            <div style={styles.snippetDesc}>
              Built-in vector types map to GPU hardware.
            </div>
          </div>

          <div style={styles.snippetCard}>
            <div style={styles.snippetHeader}>
              <div style={styles.snippetDot} />
              <span style={styles.snippetTitle}>Sequence Comprehensions</span>
            </div>
            <pre style={styles.snippetCode}>
<K>val</K> <P>iterations</P> = <T>GSeq</T>{'\n'}
{'  '}.<F>gen</F>(<F>vec2</F>(<N>0f</N>, <N>0f</N>), <P>z</P> =&gt;{'\n'}
{'    '}<F>vec2</F>(<P>z</P>.<P>x</P>*<P>z</P>.<P>x</P> - <P>z</P>.<P>y</P>*<P>z</P>.<P>y</P> + <P>cx</P>, <N>2f</N>*<P>z</P>.<P>x</P>*<P>z</P>.<P>y</P> + <P>cy</P>)){'\n'}
{'  '}.<F>limit</F>(<N>256</N>){'\n'}
{'  '}.<F>takeWhile</F>(<P>z</P> =&gt; <P>z</P>.<P>x</P>*<P>z</P>.<P>x</P> + <P>z</P>.<P>y</P>*<P>z</P>.<P>y</P> &lt; <N>4f</N>){'\n'}
{'  '}.<F>count</F>
            </pre>
            <div style={styles.snippetDesc}>
              Lazy sequences with map, filter, fold.
            </div>
          </div>

          {/* Row 2: Custom GPU Structs | Monadic GPU Effects */}
          <div style={styles.snippetCard}>
            <div style={styles.snippetHeader}>
              <div style={styles.snippetDot} />
              <span style={styles.snippetTitle}>Custom GPU Structs</span>
            </div>
            <pre style={styles.snippetCode}>
<K>case class</K> <T>Physics</T>(<P>gravity</P>: <T>Float32</T>, <P>dt</P>: <T>Float32</T>){'\n'}
{'  '}<K>extends</K> <T>GStruct</T>[<T>Physics</T>]{'\n'}
{'\n'}
<K>val</K> <P>step</P> = <T>GFunction</T>.<F>forEachIndex</F>[<T>Physics</T>, <T>Vec4</T>, <T>Vec4</T>]:{'\n'}
{'  '}(<P>cfg</P>, <P>i</P>, <P>buf</P>) =&gt;{'\n'}
{'    '}<K>val</K> <P>p</P> = <P>buf</P>.<F>read</F>(<P>i</P>){'\n'}
{'    '}(<P>p</P>.<P>x</P>, <P>p</P>.<P>y</P> + <P>cfg</P>.<P>gravity</P> * <P>cfg</P>.<P>dt</P>, <P>p</P>.<P>z</P>, <P>p</P>.<P>w</P>)
            </pre>
            <div style={styles.snippetDesc}>
              Case classes become GPU structs.
            </div>
          </div>

          <div style={styles.snippetCard}>
            <div style={styles.snippetHeader}>
              <div style={styles.snippetDot} />
              <span style={styles.snippetTitle}>Monadic GPU Effects</span>
            </div>
            <pre style={styles.snippetCode}>
<K>for</K>{'\n'}
{'  '}<P>value</P> = <P>layout</P>.<P>input</P>.<F>read</F>(<P>idx</P>){'\n'}
{'  '}<P>_</P> &lt;- <P>layout</P>.<P>bufferA</P>.<F>write</F>(<P>idx</P>, <P>value</P> + <N>1f</N>){'\n'}
{'  '}<P>_</P> &lt;- <P>layout</P>.<P>bufferB</P>.<F>write</F>(<P>idx</P>, <P>value</P> * <N>2f</N>){'\n'}
<K>yield</K> <P>value</P>
            </pre>
            <div style={styles.snippetDesc}>
              Composable GIO monad for effectful computation.
            </div>
          </div>

          {/* Row 3: GPU Pipelines | GPU Memory Management */}
          <div style={styles.snippetCard}>
            <div style={styles.snippetHeader}>
              <div style={styles.snippetDot} />
              <span style={styles.snippetTitle}>GPU Pipelines</span>
            </div>
            <pre style={styles.snippetCode}>
<K>val</K> <P>pipeline</P> = <T>GExecution</T>[<T>Int</T>, <T>Layout</T>](){'\n'}
{'  '}.<F>addProgram</F>(<P>multiply</P>):{'\n'}
{'    '}<P>size</P> =&gt; <P>size</P>{'\n'}
{'    '}<P>l</P> =&gt; <T>MulLayout</T>(<P>l</P>.<P>input</P>, <P>l</P>.<P>temp</P>, <P>l</P>.<P>mulParams</P>){'\n'}
{'  '}.<F>addProgram</F>(<P>add</P>):{'\n'}
{'    '}<P>size</P> =&gt; <P>size</P>{'\n'}
{'    '}<P>l</P> =&gt; <T>AddLayout</T>(<P>l</P>.<P>temp</P>, <P>l</P>.<P>output</P>, <P>l</P>.<P>addParams</P>)
            </pre>
            <div style={styles.snippetDesc}>
              Chain programs. Data stays on GPU.
            </div>
          </div>

          <div style={styles.snippetCard}>
            <div style={styles.snippetHeader}>
              <div style={styles.snippetDot} />
              <span style={styles.snippetTitle}>GPU Memory Management</span>
            </div>
            <pre style={styles.snippetCode}>
<T>GBufferRegion</T>{'\n'}
{'  '}.<F>allocate</F>[<T>MyLayout</T>]{'\n'}
{'  '}.<F>map</F>: <P>layout</P> =&gt;{'\n'}
{'    '}<P>pipeline</P>.<F>execute</F>(<P>params</P>, <P>layout</P>){'\n'}
{'  '}.<F>runUnsafe</F>({'\n'}
{'    '}<P>init</P> = <T>MyLayout</T>(<T>GBuffer</T>(<P>data</P>), ...),{'\n'}
{'    '}<P>onDone</P> = <P>l</P> =&gt; <P>l</P>.<P>output</P>.<F>readArray</F>(<P>results</P>))
            </pre>
            <div style={styles.snippetDesc}>
              Allocate, execute, and read back GPU buffers.
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}

function ShowcaseSection() {
  return (
    <section style={styles.section}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>See It In Action</h2>
        <p style={styles.sectionSubtitle}>
          Real projects built with Cyfra
        </p>
        
        <div className="showcase-grid" style={styles.showcaseGrid}>
          <div style={styles.showcaseItem}>
            <div style={styles.showcaseMedia}>
              <img 
                src="https://github.com/user-attachments/assets/3eac9f7f-72df-4a5d-b768-9117d651c78d"
                alt="Ray traced animation showing reflective spheres"
                style={styles.showcaseGif}
              />
            </div>
            <div style={styles.showcaseContent}>
              <span style={styles.showcaseLabel}>GPU programming</span>
              <h3 style={styles.showcaseTitle}>GPU Ray Traced Scenes</h3>
              <p style={styles.showcaseDesc}>
                Ray-traced animations with reflections and shadows. The Foton library provides 
                a clean API for scenes, materials, and camera paths.
              </p>
              <div style={styles.showcaseButtons}>
                <Link 
                  to="https://github.com/ComputeNode/cyfra/blob/8ef5d789db33ace6e6bdf2d0d3063fc28d78fd41/cyfra-examples/src/main/scala/io/computenode/cyfra/samples/foton/AnimatedRaytrace.scala" 
                  style={styles.showcaseButtonPrimary}
                >
                  View Code
                </Link>
                <Link 
                  to="https://www.youtube.com/watch?v=Prkp2VUVrYo" 
                  style={styles.showcaseButton}
                >
                  Watch Talk
                </Link>
              </div>
            </div>
          </div>

          <div style={styles.showcaseItem}>
            <div style={styles.showcaseMedia}>
              <img 
                src="/img/full_fluid_8s.gif"
                alt="Navier-Stokes fluid simulation"
                style={styles.showcaseGif}
              />
            </div>
            <div style={styles.showcaseContent}>
              <span style={styles.showcaseLabel}>Complex GPU pipelines</span>
              <h3 style={styles.showcaseTitle}>Navier-Stokes Solver</h3>
              <p style={styles.showcaseDesc}>
                Real-time fluid dynamics on the GPU. Multi-stage pipeline solving 
                Navier-Stokes equations with pressure projection.
              </p>
              <div style={styles.showcaseButtons}>
                <Link 
                  to="https://github.com/ComputeNode/cyfra/tree/8ef5d789db33ace6e6bdf2d0d3063fc28d78fd41/cyfra-fluids" 
                  style={styles.showcaseButtonPrimary}
                >
                  View Code
                </Link>
              </div>
            </div>
          </div>

          <div style={styles.showcaseItem}>
            <div style={styles.showcaseMedia}>
              <img 
                src="/img/clustering.gif"
                alt="GPU customer segmentation animation"
                style={styles.showcaseGif}
              />
            </div>
            <div style={styles.showcaseContent}>
              <span style={styles.showcaseLabel}>fs2 + GPU streaming</span>
              <h3 style={styles.showcaseTitle}>Real-time Customer Segmentation</h3>
              <p style={styles.showcaseDesc}>
                GPU-accelerated Fuzzy C-Means clustering integrated with fs2 streams. 
                Process 100k+ transactions/sec with live segment assignment.
              </p>
              <div style={styles.showcaseButtons}>
                <Link 
                  to="https://github.com/ComputeNode/cyfra/tree/main/cyfra-analytics" 
                  style={styles.showcaseButtonPrimary}
                >
                  View Code
                </Link>
              </div>
            </div>
          </div>

          <div style={styles.showcaseItem}>
            <div style={{...styles.showcaseMedia, background: 'linear-gradient(135deg, #1c1917 0%, #292524 100%)', display: 'flex', alignItems: 'center', justifyContent: 'center'}}>
              <div style={{textAlign: 'center', color: '#57534e', fontSize: '1rem', fontWeight: 500}}>
                Coming Soon
              </div>
            </div>
            <div style={styles.showcaseContent}>
              <span style={styles.showcaseLabel}>Machine learning</span>
              <h3 style={styles.showcaseTitle}>Machine Learning Library</h3>
              <p style={styles.showcaseDesc}>
                Neural network primitives on GPU. Matrix operations, activations, 
                and automatic differentiation in pure Scala.
              </p>
              <div style={styles.showcaseButtons}>
                <span style={styles.showcaseButtonDisabled}>
                  View Code
                </span>
              </div>
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
    <section style={styles.sectionAlt}>
      <div style={styles.container}>
        <h2 style={styles.sectionTitle}>Runs Everywhere</h2>
        <p style={styles.sectionSubtitle}>
          Any platform with Vulkan support
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
        <h2 style={styles.ctaTitle}>Try Cyfra Today</h2>
        <p style={styles.ctaDesc}>
          Clone the repo, run the examples, and see what GPU programming in Scala feels like.
        </p>
        <Link to="/docs/getting-started" style={styles.primaryButton}>
          Read the Docs
        </Link>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
  return (
    <Layout
      title="Home"
      description="Cyfra - GPU programming in Scala.">
      <style>{`
        @media (max-width: 800px) {
          .snippets-grid { grid-template-columns: 1fr !important; }
          .showcase-grid { grid-template-columns: 1fr !important; }
        }
      `}</style>
      <main>
        <HeroSection />
        <FeaturesSection />
        <ShowcaseSection />
        <CodeSnippetsSection />
        <PlatformSection />
        <CTASection />
      </main>
    </Layout>
  );
}
