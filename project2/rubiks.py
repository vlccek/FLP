import magiccube
import os
import argparse
from magiccube.cube_base import Face, Color
from magiccube.cube_move import CubeMove

SEP = os.getenv("SEP", "")

map_color_to_number = {
    Color.R: 5,
    Color.O: 1,
    Color.W: 2,
    Color.Y: 3,
    Color.B: 4,
    Color.G: 6,
}

map_number_to_color = {
    5: Color.R,
    1: Color.O,
    2: Color.W,
    3: Color.Y,
    4: Color.B,
    6: Color.G,
}

def get_cube_state(s):
    res = ''
    for i in range(len(s)):
        res += str(map_number_to_color[int(s[i])].name)

    return res

def get_face_str(face):
    result = ""
    for index, row in enumerate(face):
        for item in row:
            result += str(map_color_to_number[item]) + SEP

        if index != len(face) - 1:
            result += "\n"

    return result

def get_face_row_str(face, row_index):
    face_row = face[row_index]
    return SEP.join([str(map_color_to_number[item]) for item in face_row]) + SEP

def get_cube_str(cube):
    result = ""
    result += get_face_str(cube.get_face(Face.U)) + "\n"

    f_face = cube.get_face(Face.F)
    r_face = cube.get_face(Face.R)
    b_face = cube.get_face(Face.B)
    l_face = cube.get_face(Face.L)

    for i in range(3):
        f_face_row = get_face_row_str(f_face, i)
        r_face_row = get_face_row_str(r_face, i)
        b_face_row = get_face_row_str(b_face, i)
        l_face_row = get_face_row_str(l_face, i)
        result += " ".join([f_face_row, r_face_row, b_face_row, l_face_row]) + "\n"

    result += get_face_str(cube.get_face(Face.D))

    if SEP == ",":
        result = result[:-1]

    return result

def create_solved_cube():
    number_state = "555555555444444444111111111222222222333333333666666666"
    cube_state = get_cube_state(number_state)
    cube = magiccube.Cube(3, cube_state)
    return cube

def create_cube(rotations):
    cube = create_solved_cube()
    cube.rotate(rotations)
    return cube

def generate_random_rotations(n):
    cube = create_solved_cube()
    return cube, cube.scramble(n)

def all_rotations(cube):
    rotations = [
        "U", "U'",
        "D", "D'",
        "L", "L'",
        "R", "R'",
        "F", "F'",
        "B", "B'",
        "S", "S'",
        "M", "M'",
        "E", "E'",
    ]

    for rotation in rotations:
        cube.rotate(rotation)
        
        yield rotation

        if rotation.endswith("'"):
            cube.rotate(rotation[:-1])
        else:
            cube.rotate(rotation + "'")

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        '-g', '--generate',
        required=True,
        help="The input to generate (can be a number or string like 'U R L')"
    )

    parser.add_argument(
        '-s', '--separator',
        default='',
        help="Optional separator for the output"
    )

    parser.add_argument(
        '--moves',
        action='store_true',
        help="Optional flag to show all single moves"
    )

    parser.add_argument(
        '--history',
        action='store_true',
        help="History of moves"
    )

    args = parser.parse_args()

    global SEP
    SEP = args.separator

    if (args.generate.isnumeric()):
        n = int(args.generate)
        cube, rotations = generate_random_rotations(n)
    else:
        rotations = [CubeMove.create(move_str) for move_str in args.generate.split(" ") if move_str != ""]
        cube = create_cube(rotations)

    # print(" ".join([str(item) for item in rotations]))
    # print()
    print(get_cube_str(cube))

    if (args.history):
        print()
        print("History of moves:")
        print()
        initial_cube = create_solved_cube()
        
        print("Initial cube:")
        print()
        print(get_cube_str(initial_cube))
        print()

        for rotation in rotations:
            initial_cube.rotate([rotation])
            print(f"Rotation: {rotation}")
            print()
            print(get_cube_str(initial_cube))
            print()

    if args.moves:
        print()
        print("All single moves:")
        print()

        for rotation in all_rotations(cube):
            print(f"Rotation: {rotation}")
            print()
            print(get_cube_str(cube))
            print()

if __name__ == '__main__':
    main()