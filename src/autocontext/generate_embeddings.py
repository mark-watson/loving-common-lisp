# generate_embeddings.py
#
# This script reads text lines from stdin, generates embeddings using
# sentence-transformers, and prints the result as a JSON array to stdout.
# This allows the Common Lisp program to easily get embeddings without
# needing a complex foreign function interface.
#
# Usage (from Lisp):
# echo "some text" | python3 generate_embeddings.py

import sys
import json
from sentence_transformers import SentenceTransformer

def main():
    # Use a pre-trained model, which will be downloaded on first run.
    model_name = 'all-MiniLM-L6-v2'
    try:
        model = SentenceTransformer(model_name)
    except Exception as e:
        print(f"Error loading SentenceTransformer model: {e}", file=sys.stderr)
        sys.exit(1)

    # Read all lines from standard input.
    lines = [line.strip() for line in sys.stdin if line.strip()]

    if not lines:
        print("[]") # Output empty JSON array if no input
        return

    # Generate embeddings and normalize them for cosine similarity.
    embeddings = model.encode(
        lines,
        normalize_embeddings=True,
        show_progress_bar=False # Keep stdout clean for the Lisp process
    )

    # Convert the numpy array to a list of lists for JSON serialization.
    embeddings_list = embeddings.tolist()

    # Print the JSON output to stdout.
    json.dump(embeddings_list, sys.stdout)

if __name__ == '__main__':
    main()
