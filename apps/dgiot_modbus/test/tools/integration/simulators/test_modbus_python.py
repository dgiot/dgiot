#!/usr/bin/env python3
import socket
import time
import struct

def test_modbus_rtu():
    print("=== Modbus RTU Python Test ===")
    print(f"Test time: {time.strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # Configuration
    host = "127.0.0.1"
    port = 20000
    test_device = "wrj_dm-zqy"
    
    print("1. Check if port is listening...")
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(2)
        result = sock.connect_ex((host, port))
        if result == 0:
            print(f"✅ Port {port} is listening")
            sock.close()
        else:
            print(f"❌ Port {port} is not listening (error: {result})")
    except Exception as e:
        print(f"❌ Error checking port: {e}")
    
    print()
    print("2. Send registration packet...")
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect((host, port))
        sock.sendall(test_device.encode())
        print(f"✅ Sent registration: {test_device}")
        
        # Try to receive response
        try:
            response = sock.recv(1024)
            if response:
                print(f"   Received response: {response.hex()}")
            else:
                print("   No response received")
        except socket.timeout:
            print("   No response (timeout)")
        
        sock.close()
    except Exception as e:
        print(f"❌ Error sending registration: {e}")
    
    print()
    print("3. Send Modbus data...")
    try:
        # Build Modbus RTU frame: slave_id=1, function_code=3, data=4 bytes
        slave_id = 0x01
        function_code = 0x03
        data = b'\x00\x00\x00\x00'
        frame = struct.pack('BB', slave_id, function_code) + data
        
        # Add CRC (simplified - not real CRC)
        frame += b'\xC4\x0B'
        
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect((host, port))
        sock.sendall(frame)
        print(f"✅ Sent Modbus data: {frame.hex()}")
        
        sock.close()
    except Exception as e:
        print(f"❌ Error sending Modbus data: {e}")
    
    print()
    print("=== Test completed ===")

if __name__ == "__main__":
    test_modbus_rtu()
