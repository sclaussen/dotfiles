#!/usr/bin/env python3
"""
Browser forwarding server for devcontainer CLI access.
Run this on your host machine to enable browser opening from devcontainers.
"""

import socket
import subprocess
import sys
import threading
import platform

PORT = 9559  # Random port for browser forwarding

def open_browser(url):
    """Open browser on the host system."""
    system = platform.system()
    try:
        if system == "Darwin":  # macOS
            subprocess.run(["open", url])
        elif system == "Linux":
            subprocess.run(["xdg-open", url])
        elif system == "Windows":
            subprocess.run(["start", url], shell=True)
        print(f"Opened browser: {url}")
    except Exception as e:
        print(f"Failed to open browser: {e}")

def handle_client(client_socket):
    """Handle incoming browser open requests."""
    try:
        data = client_socket.recv(4096).decode('utf-8').strip()
        if data.startswith("OPEN:"):
            url = data[5:]
            open_browser(url)
            client_socket.send(b"OK")
        else:
            client_socket.send(b"ERROR")
    except Exception as e:
        print(f"Error handling client: {e}")
    finally:
        client_socket.close()

def start_server():
    """Start the browser forwarding server."""
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    
    try:
        server.bind(('0.0.0.0', PORT))
        server.listen(5)
        print(f"Browser forwarding server listening on port {PORT}")
        print("Keep this running while using devcontainers...")
        
        while True:
            client, addr = server.accept()
            thread = threading.Thread(target=handle_client, args=(client,))
            thread.daemon = True
            thread.start()
    except KeyboardInterrupt:
        print("\nShutting down server...")
    except Exception as e:
        print(f"Server error: {e}")
    finally:
        server.close()

if __name__ == "__main__":
    start_server()