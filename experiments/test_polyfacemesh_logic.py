#!/usr/bin/env python3
"""
Тест логики парсинга polyfacemesh из DXF файла
Проверяет, правильно ли мы извлекаем грани и вершины
"""

import re

def parse_polyfacemesh_dxf(filename):
    """Парсит polyfacemesh из DXF файла"""
    
    vertices = []
    faces = []
    current_entity = None
    polyline_flags = None
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    i = 0
    while i < len(lines):
        if i + 1 >= len(lines):
            break
            
        group_code = lines[i].strip()
        value = lines[i+1].strip()
        
        if group_code == '0':
            # Новая сущность
            current_entity = value
            print(f"\nEntity: {current_entity}")
            
            if value == 'POLYLINE':
                vertices = []
                faces = []
                polyline_flags = None
                current_vertex_type = None
                current_vertex_coords = [0.0, 0.0, 0.0]
                current_face_indices = [0, 0, 0, 0]
                
        elif current_entity == 'POLYLINE':
            if group_code == '70':
                polyline_flags = int(value)
                print(f"POLYLINE flags: {polyline_flags}")
                
        elif current_entity == 'VERTEX':
            if group_code == '70':
                current_vertex_type = int(value)
                print(f"Vertex type: {current_vertex_type}")
                current_vertex_coords = [0.0, 0.0, 0.0]
                current_face_indices = [0, 0, 0, 0]
                
            elif current_vertex_type == 192:  # Face vertex
                if group_code == '71':
                    current_face_indices[0] = int(value)
                elif group_code == '72':
                    current_face_indices[1] = int(value)
                elif group_code == '73':
                    current_face_indices[2] = int(value)
                elif group_code == '74':
                    current_face_indices[3] = int(value)
                    
            elif current_vertex_type != 192 and current_vertex_type != 128:  # Regular vertex
                if group_code == '10':
                    current_vertex_coords[0] = float(value)
                elif group_code == '20':
                    current_vertex_coords[1] = float(value)
                elif group_code == '30':
                    current_vertex_coords[2] = float(value)
        
        # Проверяем, нужно ли завершить обработку текущей VERTEX
        # Это происходит, когда мы встречаем новую сущность или конец файла
        next_group_code = lines[i+2].strip() if (i+2) < len(lines) else '0'
        if current_entity == 'VERTEX' and next_group_code == '0':
            # Завершаем обработку текущей VERTEX
            if current_vertex_type == 192:  # Face vertex
                # Добавляем грань, если есть хотя бы 3 индекса
                valid_indices = [idx for idx in current_face_indices if idx != 0]
                if len(valid_indices) >= 3:
                    faces.append(valid_indices)
                    print(f"Face: {valid_indices}")
            elif current_vertex_type != 192 and current_vertex_type != 128:  # Regular vertex
                vertices.append(current_vertex_coords)
                print(f"Vertex: {current_vertex_coords}")
        
        i += 2
    
    return vertices, faces

def analyze_mesh_closure(vertices, faces):
    """Анализирует замкнутость mesh"""
    
    print("\n=== Анализ замкнутости mesh ===")
    print(f"Вершин: {len(vertices)}")
    print(f"Граней: {len(faces)}")
    
    # Собираем все ребра
    edges = {}
    edge_count = {}
    
    for face_idx, face in enumerate(faces):
        print(f"\nГрань {face_idx + 1}: {face}")
        
        # Создаем ребра для треугольника или четырехугольника
        face_edges = []
        if len(face) >= 3:
            # Ребро (v1, v2)
            edge1 = tuple(sorted([face[0]-1, face[1]-1]))  # -1 т.к. индексы в DXF с 1
            face_edges.append(edge1)
            
            # Ребро (v2, v3)
            edge2 = tuple(sorted([face[1]-1, face[2]-1]))
            face_edges.append(edge2)
            
            # Ребро (v3, v1)
            edge3 = tuple(sorted([face[2]-1, face[0]-1]))
            face_edges.append(edge3)
            
            if len(face) >= 4:
                # Для четырехугольника добавляем еще два ребра
                edge4 = tuple(sorted([face[2]-1, face[3]-1]))
                face_edges.append(edge4)
                
                edge5 = tuple(sorted([face[3]-1, face[0]-1]))
                face_edges.append(edge5)
        
        print(f"  Ребра: {face_edges}")
        
        # Подсчитываем количество вхождений каждого ребра
        for edge in face_edges:
            if edge not in edge_count:
                edge_count[edge] = 0
            edge_count[edge] += 1
    
    print(f"\nСчетчик ребер: {edge_count}")
    
    # Анализируем ребра
    boundary_edges = []
    internal_edges = []
    
    for edge, count in edge_count.items():
        if count == 1:
            boundary_edges.append(edge)
        elif count == 2:
            internal_edges.append(edge)
        else:
            print(f"Предупреждение: ребро {edge} встречается {count} раз")
    
    print(f"\nГраничных ребер: {len(boundary_edges)}")
    print(f"Внутренних ребер: {len(internal_edges)}")
    
    if boundary_edges:
        print(f"Граничные ребра: {boundary_edges}")
    
    # Mesh считается замкнутым, если нет граничных ребер
    is_closed = len(boundary_edges) == 0
    print(f"\nMesh замкнут: {is_closed}")
    
    return is_closed, boundary_edges, internal_edges

def main():
    print("=== Тест polyfacemesh ===")
    
    vertices, faces = parse_polyfacemesh_dxf('examples/polyfacemesh_example.dxf')
    
    is_closed, boundary_edges, internal_edges = analyze_mesh_closure(vertices, faces)
    
    print(f"\n=== Результат ===")
    print(f"Загружено вершин: {len(vertices)}")
    print(f"Загружено граней: {len(faces)}")
    print(f"Mesh замкнут: {is_closed}")
    
    if not is_closed:
        print(f"Найдено {len(boundary_edges)} незакрытых ребер:")
        for edge in boundary_edges:
            print(f"  Ребро {edge}: vertices[{edge[0]}] - vertices[{edge[1]}]")
            if edge[0] < len(vertices) and edge[1] < len(vertices):
                v1 = vertices[edge[0]]
                v2 = vertices[edge[1]]
                print(f"    Координаты: {v1} -> {v2}")

if __name__ == "__main__":
    main()