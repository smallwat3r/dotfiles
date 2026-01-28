# YubiKey management
#
# Helpers for OATH codes (with fzf selection), GPG card status,
# FIDO2 operations, and SSH key generation with resident keys.

has ykman || return

# Show YubiKey info
yk() { ykman info 2>/dev/null || echo "No YubiKey detected" }
# List OATH accounts
yk-oath() { ykman oath accounts list 2>/dev/null }

# Get OATH code for an account (with fuzzy search)
yk-code() {
    local account
    if [[ -n "$1" ]]; then
        account="$1"
    elif has fzf; then
        account=$(ykman oath accounts list 2>/dev/null | fzf --prompt="Account: ")
    else
        echo "Usage: yk-code <account>" && return 1
    fi
    [[ -n "$account" ]] && ykman oath accounts code "$account" 2>/dev/null
}

# Show GPG card status
yk-gpg() { gpg --card-status 2>/dev/null }

# Reset FIDO2 credentials (use with caution)
yk-fido-reset() {
    echo "This will reset all FIDO2 credentials on the YubiKey."
    read -q "?Are you sure? [y/N] " && echo && ykman fido reset
}

# Generate FIDO2 SSH key (resident on YubiKey, requires touch)
yk-ssh-keygen() {
    local keyfile="${HOME}/.ssh/id_yubikey"
    if [[ -f "$keyfile" ]]; then
        echo "Key already exists: $keyfile"
        read -q "?Overwrite? [y/N] " || return 1
        echo
    fi
    echo "Touch your YubiKey when it blinks..."
    ssh-keygen -t ed25519-sk -O resident -O verify-required -f "$keyfile" -C "yubikey"
}
