#!/usr/bin/env python3
"""Детальный разбор данных Shell (OpCode=9) из proxy graphic"""

import struct

# Shell 1: Rectangle (text background) - 164 bytes payload
shell1_hex = (
    "05000000"  # VertexCount = 5
    "c4588d1a1dcca140a4c5ba1232e09740" "0000000000000000"  # Vertex 0: (2278.0568, 1528.0489, 0)
    "62ac468dce08b740a4c5ba1232e09740" "0000000000000000"  # Vertex 1: (5896.8068, 1528.0489, 0)
    "62ac468dce08b740a4c5ba1232589d40" "0000000000000000"  # Vertex 2: (5896.8068, 1878.0489, 0)
    "c4588d1a1dcca140a4c5ba1232589d40" "0000000000000000"  # Vertex 3: (2278.0568, 1878.0489, 0)
    "c4588d1a1dcca140a4c5ba1232e09740" "0000000000000000"  # Vertex 4: (2278.0568, 1528.0489, 0) = Vertex 0
    "06000000"  # FaceEntryCount = 6
    "05000000"  # Edge count = 5 (first face)
    "00000000" "01000000" "02000000" "03000000" "04000000"  # Indices: 0,1,2,3,4
    # Then rest = 0 bytes for edge/face traits
    "00000000" "00000000" "00000000"  # Edge traits flags = 0 (no attributes)
)

# Shell 2: Arrow triangle - 124 bytes payload
# Full hex from command 23 payload
shell2_payload = "03000000909aba19935f9740e4638abe20829040000000000000000006a5c0233af49a404046987af2a493400000000000000000fcea0aa1a8c79b40281fe8b5c64e9240000000000000000004000000030000000000000001000000020000000100000000010001000190000300000000"

data1 = bytes.fromhex(shell1_hex)
data2 = bytes.fromhex(shell2_payload)

def parse_shell(data, label):
    print(f"\n=== {label} ({len(data)} байт) ===")
    offset = 0

    vc = struct.unpack_from('<i', data, offset)[0]
    offset += 4
    print(f"VertexCount: {vc}")

    vertices = []
    for i in range(vc):
        x, y, z = struct.unpack_from('<3d', data, offset)
        offset += 24
        vertices.append((x, y, z))
        print(f"  Vertex {i}: ({x:.4f}, {y:.4f}, {z:.4f})")

    fc = struct.unpack_from('<i', data, offset)[0]
    offset += 4
    print(f"FaceEntryCount: {fc}")

    # Разбираем face list
    entries_read = 0
    face_num = 0
    while entries_read < fc and offset < len(data):
        edge_count = struct.unpack_from('<i', data, offset)[0]
        offset += 4
        entries_read += 1

        if edge_count <= 0:
            print(f"  Face {face_num}: edge_count={edge_count} (skip/marker)")
            face_num += 1
            continue

        indices = []
        for j in range(edge_count):
            if offset + 4 <= len(data):
                idx = struct.unpack_from('<I', data, offset)[0]
                indices.append(idx)
                offset += 4
                entries_read += 1

        print(f"  Face {face_num}: edge_count={edge_count}, indices={indices}")
        face_num += 1

    # Remaining bytes are traits
    remaining = len(data) - offset
    if remaining > 0:
        print(f"Traits data ({remaining} байт): {data[offset:].hex()}")
        # Edge traits flags
        if remaining >= 4:
            edge_flags = struct.unpack_from('<I', data, offset)[0]
            print(f"  Edge flags: 0x{edge_flags:08X}")
            offset += 4
        # Face traits flags
        if offset + 4 <= len(data):
            face_flags = struct.unpack_from('<I', data, offset)[0]
            print(f"  Face flags: 0x{face_flags:08X}")
            offset += 4
        # Vertex traits flags
        if offset + 4 <= len(data):
            vertex_flags = struct.unpack_from('<I', data, offset)[0]
            print(f"  Vertex flags: 0x{vertex_flags:08X}")
            offset += 4
    else:
        print("No traits data")

parse_shell(data1, "Shell 1: Rectangle")
parse_shell(data2, "Shell 2: Arrow Triangle")
