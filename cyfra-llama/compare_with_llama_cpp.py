#!/usr/bin/env python3
"""
Compare GPU logits against llama.cpp reference.
Run this after running LayerByLayerDebugTest to see the actual llama.cpp output.
"""

from llama_cpp import Llama
import numpy as np

MODEL_PATH = "cyfra-llama/tinyllama-1.1b-chat-v1.0.Q4_K_M.gguf"

def main():
    print("Loading model via llama.cpp...")
    llm = Llama(
        model_path=MODEL_PATH,
        n_ctx=512,
        n_batch=512,
        verbose=False,
        logits_all=True,  # Get logits for all positions
    )
    
    # Test token: 15043 = "Hello"
    # We want to get the logits for predicting what comes after "Hello"
    tokens = [1, 15043]  # BOS + "Hello"
    
    print(f"\nTokens: {tokens}")
    print("Running llama.cpp forward pass...")
    
    # Run forward pass
    llm.reset()
    llm.eval(tokens)
    
    # Get logits for last position (predicting what comes after "Hello")
    logits = np.array(llm.scores[len(tokens) - 1])
    
    print(f"\n=== llama.cpp Reference Logits for token 15043 (Hello) ===")
    print(f"Logits shape: {logits.shape}")
    print(f"min={logits.min():.4f}, max={logits.max():.4f}, mean={logits.mean():.4f}, std={logits.std():.4f}")
    
    # Top-10 tokens
    top_indices = np.argsort(logits)[::-1][:10]
    print("\nTop-10 predicted tokens:")
    for idx in top_indices:
        token_str = llm.detokenize([idx]).decode('utf-8', errors='replace')
        print(f"  Token {idx:5d} ({token_str:>10s}): logit={logits[idx]:10.4f}")
    
    # Argmax
    predicted = np.argmax(logits)
    predicted_str = llm.detokenize([predicted]).decode('utf-8', errors='replace')
    print(f"\nPredicted next token: {predicted} ({predicted_str})")
    
    # Also test T=2 case: "Hello,"
    print("\n" + "="*60)
    print("Testing T=2: [BOS, Hello, ,]")
    tokens2 = [1, 15043, 29892]  # BOS + "Hello" + ","
    
    llm.reset()
    llm.eval(tokens2)
    
    logits2 = np.array(llm.scores[len(tokens2) - 1])
    print(f"\n=== llama.cpp Reference Logits for 'Hello,' (predicting 3rd token) ===")
    print(f"min={logits2.min():.4f}, max={logits2.max():.4f}, mean={logits2.mean():.4f}, std={logits2.std():.4f}")
    
    top_indices2 = np.argsort(logits2)[::-1][:10]
    print("\nTop-10 predicted tokens:")
    for idx in top_indices2:
        token_str = llm.detokenize([idx]).decode('utf-8', errors='replace')
        print(f"  Token {idx:5d} ({token_str:>10s}): logit={logits2[idx]:10.4f}")

if __name__ == "__main__":
    main()
