"""Compare llama.cpp predictions for incremental generation."""
from llama_cpp import Llama
import numpy as np

# Load model
print("Loading model...")
llm = Llama(
    model_path="cyfra-llama/tinyllama-1.1b-chat-v1.0.Q4_K_M.gguf",
    n_ctx=64,
    verbose=False,
    logits_all=True,  # Enable all logits
)

def get_top_predictions(llm, tokens, pos):
    """Get top predictions for a specific position."""
    # Reset and evaluate
    llm.reset()
    llm.eval(tokens)
    
    # Get logits for the requested position
    logits = np.array(llm._scores[pos])
    
    top_indices = np.argsort(logits)[::-1][:10]
    print(f"Top-10 predictions for position {pos}:")
    for idx in top_indices:
        try:
            token_str = llm.detokenize([int(idx)]).decode('utf-8', errors='replace')
            token_str = token_str.encode('ascii', errors='replace').decode('ascii')
        except:
            token_str = f"[{idx}]"
        print(f"  Token {idx:5d} ({token_str:>10s}): logit={logits[idx]:10.4f}")
    
    print(f"  Stats: min={logits.min():.4f}, max={logits.max():.4f}, mean={logits.mean():.4f}")
    return logits

# Test 1: [BOS, Hello] -> predict next
tokens_1 = [1, 15043]  # BOS + Hello
print(f"\n=== Sequence 1: {tokens_1} (BOS + Hello) ===")
logits_1 = get_top_predictions(llm, tokens_1, 1)

# Test 2: [BOS, Hello, ,] -> predict next  
tokens_2 = [1, 15043, 29892]  # BOS + Hello + ,
print(f"\n=== Sequence 2: {tokens_2} (BOS + Hello + ,) ===")
logits_2 = get_top_predictions(llm, tokens_2, 2)

# Also compare logits for position 1 in both sequences (should be the same!)
print(f"\n=== Position 1 logits in sequence 2 (should match sequence 1) ===")
llm.reset()
llm.eval(tokens_2)
logits_2_pos1 = np.array(llm._scores[1])
print(f"  max diff between seq1 pos1 and seq2 pos1: {np.abs(logits_1 - logits_2_pos1).max():.6f}")
