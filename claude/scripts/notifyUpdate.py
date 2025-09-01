#!/usr/bin/env python3
"""
Claude Code notification configuration script.
Handles setting notification preferences via the /notify command.
"""

import sys
from pathlib import Path

def load_config():
    """Load notification configuration from YAML file (simple parser)."""
    config_path = Path(".claude/scripts/notify.yaml")

    # Default configuration
    config = {
        'sound_setting': '1',
        'custom_text': 'done'
    }

    if not config_path.exists():
        return config

    try:
        with open(config_path, 'r') as f:
            for line in f:
                line = line.strip()
                if ':' in line and not line.startswith('#'):
                    key, value = line.split(':', 1)
                    key = key.strip()
                    value = value.strip()
                    # Remove quotes if present
                    if value.startswith('"') and value.endswith('"'):
                        value = value[1:-1]
                    config[key] = value
    except Exception as e:
        # Silently use defaults on error
        pass

    return config

def save_config(config):
    """Save notification configuration to YAML file."""
    config_path = Path(".claude/scripts/notify.yaml")
    
    try:
        with open(config_path, 'w') as f:
            for key, value in config.items():
                f.write(f"{key}: {value}\n")
    except Exception as e:
        print(f"Error saving config: {e}")

def configure_notification(arg):
    """Configure notification based on command line argument."""
    if not arg:
        print_usage()
        return
    
    config = load_config()
    new_setting = ""
    
    if arg in ['0', 'false', 'silent']:
        new_setting = '0'
        print("🔇 Notification sounds disabled")
    elif arg in ['1', 'ping']:
        new_setting = '1'
        print("🔔 Using Ping sound (default)")
    elif arg in ['2', 'glass']:
        new_setting = '2'
        print("🔔 Using Glass sound")
    elif arg in ['3', 'tink']:
        new_setting = '3'
        print("🔔 Using Tink sound")
    elif arg in ['4', 'sosumi']:
        new_setting = '4'
        print("🔔 Using Sosumi sound")
    elif arg in ['5', 'pop']:
        new_setting = '5'
        print("🔔 Using Pop sound")
    elif arg in ['6', 'blow']:
        new_setting = '6'
        print("🔔 Using Blow sound")
    elif arg in ['7', 'hero']:
        new_setting = '7'
        print("🔔 Using Hero sound")
    elif arg in ['8', 'basso']:
        new_setting = '8'
        print("🔔 Using Basso sound")
    elif arg in ['9', 'bottle']:
        new_setting = '9'
        print("🔔 Using Bottle sound")
    elif arg in ['10', 'frog']:
        new_setting = '10'
        print("🔔 Using Frog sound")
    elif arg in ['11', 'funk']:
        new_setting = '11'
        print("🔔 Using Funk sound")
    elif arg in ['12', 'morse']:
        new_setting = '12'
        print("🔔 Using Morse sound")
    elif arg == 'say ready':
        new_setting = 'say_ready'
        print("🔔 Using text-to-speech 'ready'")
    elif arg == 'say done':
        new_setting = 'say_done'
        print("🔔 Using text-to-speech 'done'")
    else:
        # Custom text-to-speech
        new_setting = 'custom'
        config['custom_text'] = arg
        print(f"🔔 Using custom text-to-speech: '{arg}'")
    
    config['sound_setting'] = new_setting
    save_config(config)

def print_usage():
    """Print usage information."""
    config = load_config()
    current = config.get('sound_setting', '1')
    custom_text = config.get('custom_text', 'done')
    
    print("Usage: python notifyUpdate.py [option]")
    print("")
    print("System Sound Options:")
    print("  0, false, silent - No sound")
    print("  1, ping          - Ping - System ding (default)")
    print("  2, glass         - Glass - Crystal clear sound")
    print("  3, tink          - Tink - Light chime")
    print("  4, sosumi        - Sosumi - Classic bell")
    print("  5, pop           - Pop - Quick pop sound")
    print("  6, blow          - Blow - Whoosh sound")
    print("  7, hero          - Hero - Triumphant sound")
    print("  8, basso         - Basso - Deep bass tone")
    print("  9, bottle        - Bottle - Bottle pop")
    print("  10, frog         - Frog - Ribbit sound")
    print("  11, funk         - Funk - Funky bass")
    print("  12, morse        - Morse - Telegraph beep")
    print("")
    print("Text-to-Speech Options:")
    print("  'say ready'      - Text-to-speech 'ready'")
    print("  'say done'       - Text-to-speech 'done'")
    print("  'any text'       - Custom text-to-speech")
    print("")
    
    # Show current setting with more detail
    if current == '0':
        print("Current setting: 0 (silent - no sound)")
    elif current == '1':
        print("Current setting: 1 (ping - system ding)")
    elif current == '2':
        print("Current setting: 2 (glass - crystal clear sound)")
    elif current == '3':
        print("Current setting: 3 (tink - light chime)")
    elif current == '4':
        print("Current setting: 4 (sosumi - classic bell)")
    elif current == '5':
        print("Current setting: 5 (pop - quick pop sound)")
    elif current == '6':
        print("Current setting: 6 (blow - whoosh sound)")
    elif current == '7':
        print("Current setting: 7 (hero - triumphant sound)")
    elif current == '8':
        print("Current setting: 8 (basso - deep bass tone)")
    elif current == '9':
        print("Current setting: 9 (bottle - bottle pop)")
    elif current == '10':
        print("Current setting: 10 (frog - ribbit sound)")
    elif current == '11':
        print("Current setting: 11 (funk - funky bass)")
    elif current == '12':
        print("Current setting: 12 (morse - telegraph beep)")
    elif current == 'say_ready':
        print("Current setting: say_ready (text-to-speech 'ready')")
    elif current == 'say_done':
        print("Current setting: say_done (text-to-speech 'done')")
    elif current == 'custom':
        print(f"Current setting: custom (text-to-speech '{custom_text}')")
    else:
        print(f"Current setting: {current}")

if __name__ == '__main__':
    if len(sys.argv) > 1:
        # Configuration mode
        arg = ' '.join(sys.argv[1:])
        if arg in ['--help', '-h', 'help']:
            print_usage()
        else:
            configure_notification(arg)
    else:
        # No arguments - show usage
        print_usage()