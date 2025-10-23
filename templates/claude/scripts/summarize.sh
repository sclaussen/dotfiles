#!/usr/bin/env bash

# summarize.sh - Generate and log session summaries to ~/.claude.txt
# Usage: summarize.sh [custom_description] [--prompt | --summary "summary_text"]

CLAUDE_LOG="$HOME/.claude.txt"
TIMESTAMP=$(date '+%m/%d/%Y %H:%M')

# Function to efficiently prepend to large files
prepend_to_log() {
    local new_content="$1"
    local temp_file=$(mktemp)

    # Write new content first
    echo "$new_content" > "$temp_file"
    echo "" >> "$temp_file"  # Add blank line separator

    # Append existing content if file exists and is readable
    if [[ -f "$CLAUDE_LOG" && -r "$CLAUDE_LOG" ]]; then
        # For very large files, only keep the most recent 10000 lines
        if [[ $(wc -l < "$CLAUDE_LOG" 2>/dev/null || echo 0) -gt 10000 ]]; then
            echo "# Previous entries truncated (file was >10k lines)" >> "$temp_file"
            tail -n 9900 "$CLAUDE_LOG" >> "$temp_file"
        else
            cat "$CLAUDE_LOG" >> "$temp_file"
        fi
    fi

    # Atomically replace the log file
    mv "$temp_file" "$CLAUDE_LOG"
    chmod 644 "$CLAUDE_LOG"
}

# Function to generate summary using claude -p
generate_summary() {
    local custom_desc="$1"

    local prompt="Please analyze this Claude Code session and provide a concise summary including:
- Key tasks accomplished
- Files created/modified
- Important commands executed
- Technical decisions made
- Problems solved

Keep it structured and concise. Focus on actionable items and outcomes."

    if [[ -n "$custom_desc" ]]; then
        prompt="$prompt

Context: $custom_desc"
    fi

    # Use claude -p to generate summary
    if command -v claude >/dev/null 2>&1; then
        claude -p "$prompt" 2>/dev/null || echo "Unable to generate automatic summary. Please provide manual summary."
    else
        echo "Claude CLI not available. Please provide manual summary."
    fi
}

# Parse arguments
custom_description=""
summary_text=""
use_prompt=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --prompt)
            use_prompt=true
            shift
            ;;
        --summary)
            summary_text="$2"
            shift 2
            ;;
        *)
            if [[ -z "$custom_description" ]]; then
                custom_description="$1"
            else
                custom_description="$custom_description $1"
            fi
            shift
            ;;
    esac
done

# Generate the log entry
if [[ -n "$summary_text" ]]; then
    # Use provided summary
    summary="$summary_text"
elif [[ "$use_prompt" == true ]]; then
    # Use claude -p to generate summary
    summary=$(generate_summary "$custom_description")
else
    # Default: try to generate summary, fallback to description only
    if command -v claude >/dev/null 2>&1; then
        summary=$(generate_summary "$custom_description")
    else
        summary="Session completed"
        if [[ -n "$custom_description" ]]; then
            summary="$summary: $custom_description"
        fi
    fi
fi

# Format the log entry
log_entry="## $TIMESTAMP - Session Summary"
if [[ -n "$summary" ]]; then
    log_entry="$log_entry"$'\n'"$summary"
fi
if [[ -n "$custom_description" ]]; then
    log_entry="$log_entry"$'\n'$'\n'"Context: $custom_description"
fi

# Add to log
prepend_to_log "$log_entry"

echo "Session summary logged to ~/.claude.txt"
if [[ -n "$custom_description" ]]; then
    echo "Description: $custom_description"
fi