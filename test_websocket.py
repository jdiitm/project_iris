
import asyncio
import websockets
import struct

# Colors
GREEN = "\033[92m"
RED = "\033[91m"
RESET = "\033[0m"

async def test_websocket():
    uri = "ws://localhost:8086"
    print(f"Connecting to {uri}...")
    
    try:
        async with websockets.connect(uri) as websocket:
            print(f"{GREEN}[PASS]{RESET} Connected to WebSocket.")
            
            # 1. Login
            user = "web_user"
            print(f"Logging in as {user}...")
            payload = b'\x01' + user.encode('utf-8')
            await websocket.send(payload)
            
            # 2. Recv ACK
            ack = await websocket.recv()
            print(f"Received ACK: {ack}")
            
            # Since my logic sends <<3, "LOGIN_OK">>, we check that
            if b"LOGIN_OK" in ack:
                 print(f"{GREEN}[PASS]{RESET} Login Successful.")
            else:
                 print(f"{RED}[FAIL]{RESET} Login Failed. Got: {ack}")
                 return

            # 3. Send Message to self (Loopback via Router)
            target = "web_user"
            msg = b"Hello from Browser!"
            print(f"Sending message to {target}...")
            
            # Format: 0x02 | TargetLen(2) | Target | MsgLen(2) | Msg
            packet = b'\x02' + struct.pack('>H', len(target)) + target.encode() + struct.pack('>H', len(msg)) + msg
            await websocket.send(packet)
            
            # 4. Receive Message
            # Note: My WS implementation wraps received msg in Binary Frame.
            # `websockets` handles frame decoding automatically.
            # So I should receive raw binary packet send by `iris_ws_lite`.
            # `iris_ws_lite` wraps `deliver_msg` in binary frame.
            # `deliver_msg` is the raw message content (Msg).
            # Wait, `iris_edge_conn` sends raw Msg?
            # Let's check `iris_ws_lite:connected`.
            # `connected(info, {deliver_msg, Msg}, ...)` -> `gen_tcp:send(..., encode_frame(binary, Msg))`.
            # So yes, I receive the raw Msg string "Hello from Browser!".
            
            resp = await websocket.recv()
            print(f"Received: {resp}")
            
            if resp == msg:
                print(f"{GREEN}[PASS]{RESET} Message Loopback Successful.")
            else:
                print(f"{RED}[FAIL]{RESET} content mismatch. Expected {msg}, got {resp}")

    except Exception as e:
        print(f"{RED}[FAIL]{RESET} Error: {e}")

if __name__ == "__main__":
    asyncio.get_event_loop().run_until_complete(test_websocket())
