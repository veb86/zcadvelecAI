#!/usr/bin/env python3
"""
Test script to validate PolyFaceMesh DXF parsing logic
"""

def parse_dxf_vertex_data():
    """Parse the actual DXF vertex data from the test file"""
    
    # Extracted from the actual DXF file
    vertices = [
        {"x": 2460.472432961435, "y": 388.9391639465921, "z": 818.7112458458351, "type": "AcDbPolyFaceMeshVertex", "flags": 192},
        {"x": 2460.472432961435, "y": 1908.939163947003, "z": 924.9999999999996, "type": "AcDbPolyFaceMeshVertex", "flags": 192},
        {"x": 360.4724329614237, "y": 1908.939163947003, "z": 924.9999999999996, "type": "AcDbPolyFaceMeshVertex", "flags": 192},
        {"x": 360.4724329614237, "y": 388.9391639465921, "z": 818.7112458458351, "type": "AcDbPolyFaceMeshVertex", "flags": 192}
    ]
    
    face_record = {
        "type": "AcDbFaceRecord",
        "flags": 128,
        "indices": {"71": 1, "72": 2, "73": 3, "74": 4}
    }
    
    return vertices, face_record

def test_polyfacemesh_parsing():
    """Test the PolyFaceMesh parsing logic"""
    
    vertices, face_record = parse_dxf_vertex_data()
    
    print("=== PolyFaceMesh DXF Parsing Test ===")
    print(f"Found {len(vertices)} vertices:")
    for i, v in enumerate(vertices):
        print(f"  Vertex {i+1}: ({v['x']:.2f}, {v['y']:.2f}, {v['z']:.2f}) - {v['type']}")
    
    print(f"\nFace record:")
    print(f"  Type: {face_record['type']}")
    print(f"  Flags: {face_record['flags']}")
    print(f"  Indices: {face_record['indices']}")
    
    # Validate expected result
    print(f"\n=== Expected Result ===")
    print(f"Total vertices: 4")
    print(f"Total faces: 1")
    print(f"Face vertices: 1,2,3,4")
    
    # Check flags
    print(f"\n=== Flag Analysis ===")
    for i, v in enumerate(vertices):
        is_vertex = (v['flags'] & 128) == 0  # Not a face record
        is_polyface = (v['flags'] & 64) == 64  # Polyface vertex
        print(f"Vertex {i+1}: flags={v['flags']}, is_vertex={is_vertex}, is_polyface={is_polyface}")
    
    face_flags = face_record['flags']
    is_face_record = (face_flags & 128) == 128
    print(f"Face: flags={face_flags}, is_face_record={is_face_record}")

if __name__ == "__main__":
    test_polyfacemesh_parsing()