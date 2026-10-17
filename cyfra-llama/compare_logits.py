#!/usr/bin/env python3
"""Compare logits from llama-cpp-python with our implementation."""

import numpy as np
from llama_cpp import Llama

def main():
    model_path = "cyfra-llama/tinyllama-1.1b-chat-v1.0.Q4_K_M.gguf"
    
    print(f"Loading model from {model_path}...")
    llm = Llama(
        model_path=model_path,
        n_ctx=32,
        n_batch=32,
        verbose=True,
        logits_all=True,  # Get logits for all tokens
    )
    
    # Test tokens: BOS (1) + "Hello" token
    prompt = "Hello"
    print(f"\nPrompt: '{prompt}'")
    
    # Tokenize
    tokens = llm.tokenize(prompt.encode(), add_bos=True)
    print(f"Tokens: {tokens}")
    
    # Evaluate and get logits
    llm.reset()
    llm.eval(tokens)
    
    # Get logits for the last token
    logits = llm.scores[len(tokens) - 1]
    logits_array = np.array(logits, dtype=np.float32)
    
    print(f"\nLogits shape: {logits_array.shape}")
    print(f"Logits stats: min={logits_array.min():.4f}, max={logits_array.max():.4f}, mean={logits_array.mean():.4f}, std={logits_array.std():.4f}")
    print(f"Logits sum: {logits_array.sum():.4f}")
    
    # Get top 5 predictions
    top_indices = np.argsort(logits_array)[-5:][::-1]
    print("\nTop 5 predictions:")
    for idx in top_indices:
        token_str = llm.detokenize([idx]).decode('utf-8', errors='replace')
        print(f"  {idx}: '{token_str}' (score={logits_array[idx]:.2f})")
    
    # Print first and last few logits for comparison
    print(f"\nFirst 10 logits: {logits_array[:10]}")
    print(f"Last 10 logits: {logits_array[-10:]}")
    
    # Also test with just "Hello" (no BOS)
    print("\n" + "="*60)
    print("Testing single token (15043 = 'Hello')...")
    
    llm.reset()
    llm.eval([15043])  # Just the "Hello" token
    
    logits2 = llm.scores[0]
    logits2_array = np.array(logits2, dtype=np.float32)
    
    print(f"Logits stats: min={logits2_array.min():.4f}, max={logits2_array.max():.4f}, mean={logits2_array.mean():.4f}, std={logits2_array.std():.4f}")
    
    # Get top 5
    top_indices2 = np.argsort(logits2_array)[-5:][::-1]
    print("\nTop 5 predictions:")
    for idx in top_indices2:
        token_str = llm.detokenize([idx]).decode('utf-8', errors='replace')
        print(f"  {idx}: '{token_str}' (score={logits2_array[idx]:.2f})")

if __name__ == "__main__":
    main()
