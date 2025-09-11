#!/usr/bin/env python3
"""
Claude Code notification settings updater.
Updates notify.yaml configuration based on user input.
This script is called by the /notify command to change notification settings.
"""

import sys
from pathlib import Path

def get_current_setting():
    """Get the current notification setting from notify.yaml."""
    config_path = Path(".claude/scripts/notify.yaml")
    
    # Default values
    sound_setting = '1'
    custom_text = 'done'
    
    if config_path.exists():
        try:
            with open(config_path, 'r') as f:
                for line in f:
                    line = line.strip()
                    if ':' in line and not line.startswith('#'):
                        key, value = line.split(':', 1)
                        key = key.strip()
                        value = value.strip()
                        if key == 'sound_setting':
                            sound_setting = value
                        elif key == 'custom_text':
                            custom_text = value
        except:
            pass
    
    # Get sound name
    sound_names = {
        '0': 'Silent',
        '1': 'Ping',
        '2': 'Glass',
        '3': 'Tink',
        '4': 'Sosumi',
        '5': 'Pop',
        '6': 'Blow',
        '7': 'Hero',
        '8': 'Basso',
        '9': 'Bottle',
        '10': 'Frog',
        '11': 'Funk',
        '12': 'Morse',
        'say_ready': 'Text-to-speech "ready"',
        'say_done': 'Text-to-speech "done"',
        'custom': f'Custom text "{custom_text}"'
    }
    
    current_name = sound_names.get(sound_setting, sound_setting)
    return f"Current setting: {current_name}"

def show_usage():
    """Display usage information for the notify command."""
    current = get_current_setting()
    usage = f"""{current}

Usage: /notify [option]

Options:
  0          - Silent (no sound)
  1          - Ping (default)
  2          - Glass
  3          - Tink
  4          - Sosumi
  5          - Pop
  6          - Blow
  7          - Hero
  8          - Basso
  9          - Bottle
  10         - Frog
  11         - Funk
  12         - Morse
  say_ready  - Text-to-speech "ready"
  say_done   - Text-to-speech "done"
  "text"     - Custom text-to-speech (in quotes)

Examples:
  /notify 7              # Set to Hero sound
  /notify say_done       # Use text-to-speech "done"
  /notify "task complete" # Custom text message"""
    return usage

def update_config(sound_setting, custom_text=None):
    """Update the notify.yaml configuration file."""
    config_path = Path(".claude/scripts/notify.yaml")
    
    # Ensure the directory exists
    config_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Prepare the configuration content
    config_content = f"sound_setting: {sound_setting}\n"
    if custom_text:
        config_content += f"custom_text: {custom_text}\n"
    else:
        config_content += "custom_text: done\n"
    
    # Write the configuration
    with open(config_path, 'w') as f:
        f.write(config_content)
    
    # Play the sound immediately to confirm the change
    import subprocess
    import sys
    if sys.platform.startswith('darwin'):
        try:
            if sound_setting == '0':
                pass  # Silent
            elif sound_setting == '1':
                subprocess.run(['afplay', '/System/Library/Sounds/Ping.aiff'], 
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '2':
                subprocess.run(['afplay', '/System/Library/Sounds/Glass.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '3':
                subprocess.run(['afplay', '/System/Library/Sounds/Tink.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '4':
                subprocess.run(['afplay', '/System/Library/Sounds/Sosumi.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '5':
                subprocess.run(['afplay', '/System/Library/Sounds/Pop.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '6':
                subprocess.run(['afplay', '/System/Library/Sounds/Blow.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '7':
                subprocess.run(['afplay', '/System/Library/Sounds/Hero.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '8':
                subprocess.run(['afplay', '/System/Library/Sounds/Basso.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '9':
                subprocess.run(['afplay', '/System/Library/Sounds/Bottle.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '10':
                subprocess.run(['afplay', '/System/Library/Sounds/Frog.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '11':
                subprocess.run(['afplay', '/System/Library/Sounds/Funk.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == '12':
                subprocess.run(['afplay', '/System/Library/Sounds/Morse.aiff'],
                             check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == 'say_ready':
                subprocess.Popen(['say', 'ready'],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == 'say_done':
                subprocess.Popen(['say', 'done'],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            elif sound_setting == 'custom' and custom_text:
                subprocess.Popen(['say', custom_text],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except:
            pass  # Silently fail
    
    # Return confirmation message
    sound_names = {
        '0': 'Silent',
        '1': 'Ping',
        '2': 'Glass',
        '3': 'Tink',
        '4': 'Sosumi',
        '5': 'Pop',
        '6': 'Blow',
        '7': 'Hero',
        '8': 'Basso',
        '9': 'Bottle',
        '10': 'Frog',
        '11': 'Funk',
        '12': 'Morse',
        'say_ready': 'Text-to-speech "ready"',
        'say_done': 'Text-to-speech "done"',
        'custom': f'Custom text "{custom_text}"'
    }
    
    sound_name = sound_names.get(sound_setting, sound_setting)
    return f"✓ Notification sound updated to: {sound_name}"

def main():
    """Main entry point for the notification updater."""
    # Get arguments
    args = sys.argv[1:] if len(sys.argv) > 1 else []
    
    # No arguments - show usage
    if not args:
        print(show_usage())
        return
    
    # Join all arguments to handle quoted text
    arg = ' '.join(args)
    
    # Check if it's a quoted custom text
    if arg.startswith('"') and arg.endswith('"'):
        custom_text = arg[1:-1]  # Remove quotes
        result = update_config('custom', custom_text)
        print(result)
    # Check if it's a predefined option
    elif arg in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', 'say_ready', 'say_done']:
        result = update_config(arg)
        print(result)
    else:
        # Treat anything else as custom text (unquoted)
        # This handles cases like: /notify you are the man
        custom_text = arg
        result = update_config('custom', custom_text)
        print(result)

if __name__ == '__main__':
    main()