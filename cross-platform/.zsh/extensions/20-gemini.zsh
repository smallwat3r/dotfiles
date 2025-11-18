# Gemini CLI

# Run the Gemini CLI with the API key loaded from `pass` for each invocation.
# This keeps the key out of your global environment and ensures the CLI
# always uses the current value stored in `pass`.
gemini() {
  GEMINI_API_KEY="$(pass gemini/key)" "$HOME/.npm-global/bin/gemini" "$@"
}
