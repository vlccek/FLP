% Autor Jakub Vlk, 
% projekt do FLP - logická část

:- use_module(library(readutil)). % Pro read_line_to_string
:- use_module(library(lists)).    % Pro append, maplist, nth0 



% :- dynamic visited_state/1. % for dfs


%% read_cube(-Cube:list) is det.
%
%  Reads the Rubik's cube state from standard input.
%  Expects 9 lines: 3 for UP, 3 for LFRB faces, 3 for DOWN.
%  Parses the input strings into a list representation of the cube.
%
%  @param Cube The cube state represented as a list of 6 lists (faces),
%                where each face is a list of 9 integers (colors).
%                Order: [Up, Front, Right, Back, Left, Down].
%
read_cube(Cube) :-

    read_line_to_string(current_input, R1_str),
    read_line_to_string(current_input, R2_str),
    read_line_to_string(current_input, R3_str),
    parse_3_rows_face(R1_str, R2_str, R3_str, UpFaceList),


    read_line_to_string(current_input, MR1_str),
    read_line_to_string(current_input, MR2_str),
    read_line_to_string(current_input, MR3_str),

    split_string(MR1_str, " ", " ", ML1_strs),
    split_string(MR2_str, " ", " ", ML2_strs),
    split_string(MR3_str, " ", " ", ML3_strs),

    middle_face_builder([ML1_strs, ML2_strs, ML3_strs], LeftFaceList, FrontFaceList, RightFaceList, BackFaceList),

    read_line_to_string(current_input, R7_str),
    read_line_to_string(current_input, R8_str),
    read_line_to_string(current_input, R9_str),

    parse_3_rows_face(R7_str, R8_str, R9_str, DownFaceList),

    Cube = [UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList].


%% parse_3_rows_face(+String1:string, +String2:string, +String3:string, -FaceList:list) is det.
%
%  Parses three strings (each representing a row of 3 digits) into a single list
%  of 9 integers representing a cube face.
%
%  @param String1 First row string (e.g., "111").
%  @param String2 Second row string (e.g., "111").
%  @param String3 Third row string (e.g., "111").
%  @param FaceList Output list of 9 integers (e.g., [1,1,1,1,1,1,1,1,1]).
parse_3_rows_face(String1, String2, String3, FaceList) :-
    string3_to_numbers(String1, Nums1),
    string3_to_numbers(String2, Nums2),
    string3_to_numbers(String3, Nums3),
    append([Nums1, Nums2, Nums3], FaceList).

%% code_to_number(+AsciiCode:integer, -Digit:integer) is det.
%
%  Converts the ASCII code of a digit ('0'-'9') to its integer value.
%
%  @param AsciiCode ASCII value of a digit character.
%  @param Digit The corresponding integer value (0-9).
%
code_to_number(A, D) :-
    D is A - 48. % Předpokládá, že A je ASCII kód číslice '0'..'9'


%% string3_to_numbers(+String3:string, -Numbers:list) is det.
%
%  Converts a string of 3 digits (e.g., "225") into a list of 3 integers (e.g., [2,2,5]).
%
%  @param String3 Input string of 3 digits.
%  @param Numbers Output list of 3 integers.
%
string3_to_numbers(String3, Numbers) :-
    string_codes(String3, Codes),    % Převede string na seznam znakových kódů (např. "225" -> [50, 50, 53])
    maplist(code_to_number, Codes, Numbers). % Převede seznam kódů na seznam čísel ([50,50,53] -> [2,2,5])

%% middle_face_builder(+MiddleRowsStrings:list, -LeftFaceList:list, -FrontFaceList:list, -RightFaceList:list, -BackFaceList:list) is det.
%
%  Constructs the four middle faces (Left, Front, Right, Back) from the parsed input strings.
%  Input format assumes each inner list corresponds to a row, containing strings for L, F, R, B faces.
%
%  @param MiddleRowsStrings List of 3 lists, e.g., [[L1s,F1s,R1s,B1s],[L2s,F2s,R2s,B2s],[L3s,F3s,R3s,B3s]].
%  @param LeftFaceList Output list of 9 integers for the Left face.
%  @param FrontFaceList Output list of 9 integers for the Front face.
%  @param RightFaceList Output list of 9 integers for the Right face.
%  @param BackFaceList Output list of 9 integers for the Back face.
%
middle_face_builder([[F1_str, R1_str, B1_str, L1_str],
        [F2_str, R2_str, B2_str, L2_str],
        [F3_str, R3_str, B3_str, L3_str]],
        LeftFaceList, FrontFaceList, RightFaceList, BackFaceList) :-

    string3_to_numbers(L1_str, L1_num), string3_to_numbers(L2_str, L2_num), string3_to_numbers(L3_str, L3_num),
    string3_to_numbers(F1_str, F1_num), string3_to_numbers(F2_str, F2_num), string3_to_numbers(F3_str, F3_num),
    string3_to_numbers(R1_str, R1_num), string3_to_numbers(R2_str, R2_num), string3_to_numbers(R3_str, R3_num),
    string3_to_numbers(B1_str, B1_num), string3_to_numbers(B2_str, B2_num), string3_to_numbers(B3_str, B3_num),

    append([L1_num, L2_num, L3_num], LeftFaceList),
    append([F1_num, F2_num, F3_num], FrontFaceList),
    append([R1_num, R2_num, R3_num], RightFaceList),
    append([B1_num, B2_num, B3_num], BackFaceList).


%% all_same_color(+Face:list) is semidet.
%
%  Checks if a given face (list of 9 integers) consists of only one color.
%
%  @param Face List of 9 integers representing a face.
%  @true If all 9 integers in the list are identical.
%  @fail Otherwise, or if the list does not have length 9.
%
all_same_color(Face) :-
    length(Face, 9),                 % Zkontroluj delku
    Face = [HeadColor | Tail],       % Ziskej prvni barvu
    all_same_color_rest(Tail, HeadColor). % Zkontroluj zbytek proti prvni barve


%% all_same_color_rest(+List:list, +TargetColor:integer) is semidet.
%
%  Helper predicate for all_same_color/1. Checks if all elements in List
%  are equal to TargetColor.
%
%  @param List The remaining list of face colors.
%  @param TargetColor The color to compare against.
%  @true If List is empty or all elements match TargetColor.
%  @fail Otherwise.
%
all_same_color_rest([], _).
all_same_color_rest([H | T], TargetColor) :- % Vezmi hlavu zbytku (H) a cilovou barvu (TargetColor)
    H == TargetColor,              % Porovnej je
    all_same_color_rest(T, TargetColor). % Rekurzivne pokracuj

%% is_solved(+Cube:list) is semidet.
%
%  Checks if the entire cube is in a solved state.
%  A cube is solved if each of its 6 faces has only one color.
%
%  @param Cube The cube state ([Up, Front, Right, Back, Left, Down]).
%  @true If all 6 faces satisfy all_same_color/1.
%  @fail Otherwise.
%
is_solved([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList]):-
    all_same_color(UpFaceList),
    all_same_color(FrontFaceList),
    all_same_color(RightFaceList),
    all_same_color(BackFaceList),
    all_same_color(LeftFaceList),
    all_same_color(DownFaceList).
    
% --- Move Predicates ---
% Each move_X predicate takes the CurrentState of the cube and calculates
% the NewState after applying the corresponding 90-degree rotation.
% State representation: [Up, Front, Right, Back, Left, Down]
% Face representation: [1,2,3, 4,5,6, 7,8,9] (row by row)


move_U([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U7,U4,U1, U8,U5,U2, U9,U6,U3],
                [R1,R2,R3, F4,F5,F6, F7,F8,F9],
                [B1,B2,B3, R4,R5,R6, R7,R8,R9], 
                [L1,L2,L3, B4,B5,B6, B7,B8,B9],
                [F1,F2,F3, L4,L5,L6, L7,L8,L9], 
                [D1,D2,D3,D4,D5,D6,D7,D8,D9]    
                ].

move_U_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],


    NewState = [[U3,U6,U9, U2,U5,U8, U1,U4,U7], 
                [L1,L2,L3, F4,F5,F6, F7,F8,F9], 
                [F1,F2,F3, R4,R5,R6, R7,R8,R9], 
                [R1,R2,R3, B4,B5,B6, B7,B8,B9], 
                [B1,B2,B3, L4,L5,L6, L7,L8,L9], 
                [D1,D2,D3,D4,D5,D6,D7,D8,D9]    
                ].

move_D([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,U3,U4,U5,U6,U7,U8,U9],
                [F1,F2,F3,F4,F5,F6,L7,L8,L9],
                [R1,R2,R3,R4,R5,R6,F7,F8,F9],
                [B1,B2,B3,B4,B5,B6,R7,R8,R9],
                [L1,L2,L3,L4,L5,L6,B7,B8,B9],
                [D7,D4,D1,D8,D5,D2,D9,D6,D3] 
                ].

move_D_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,U3,U4,U5,U6,U7,U8,U9],
                [F1,F2,F3,F4,F5,F6,R7,R8,R9],
                [R1,R2,R3,R4,R5,R6,B7,B8,B9],
                [B1,B2,B3,B4,B5,B6,L7,L8,L9],
                [L1,L2,L3,L4,L5,L6,F7,F8,F9],
                [D3,D6,D9,D2,D5,D8,D1,D4,D7] 
                ].

move_R([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,B7,U4,U5,B4,U7,U8,B1],
                [F1,F2,U3,F4,F5,U6,F7,F8,U9],
                [R3,R6,R9,R2,R5,R8,R1,R4,R7],
                [D9,B2,B3,D6,B5,B6,D3,B8,B9],
                [L1,L2,L3,L4,L5,L6,L7,L8,L9],
                [D1,D2,F3,D4,D5,F6,D7,D8,F9] 
                ].

move_R_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,F3,U4,U5,F6,U7,U8,F9],
                [F1,F2,D3,F4,F5,D6,F7,F8,D9],
                [R7,R4,R1,R8,R5,R2,R9,R6,R3],
                [U9,B2,B3,U6,B5,B6,U3,B8,B9],
                [L1,L2,L3,L4,L5,L6,L7,L8,L9],
                [D1,D2,B7,D4,D5,B4,D7,D8,B1] 
                ].

move_L([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[F1,U2,U3,F4,U5,U6,F7,U8,U9],
                [D1,F2,F3,D4,F5,F6,D7,F8,F9],
                [R1,R2,R3,R4,R5,R6,R7,R8,R9],
                [B1,B2,U7,B4,B5,U4,B7,B8,U1],
                [L3,L6,L9,L2,L5,L8,L1,L4,L7],
                [B9,D2,D3,B6,D5,D6,B3,D8,D9] 
                ].

move_L_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],


    NewState = [[B9,U2,U3,B6,U5,U6,B3,U8,U9],
                [U1,F2,F3,U4,F5,F6,U7,F8,F9],
                [R1,R2,R3,R4,R5,R6,R7,R8,R9],
                [B1,B2,D7,B4,B5,D4,B7,B8,D1],
                [L7,L4,L1,L8,L5,L2,L9,L6,L3],
                [F1,D2,D3,F4,D5,D6,F7,D8,D9] 
                ].

move_F([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],


    NewState = [[U1,U2,U3,U4,U5,U6,L9,L6,L3],
                [F7,F4,F1,F8,F5,F2,F9,F6,F3],
                [U7,R2,R3,U8,R5,R6,U9,R8,R9],
                [B1,B2,B3,B4,B5,B6,B7,B8,B9],
                [L1,L2,D1,L4,L5,D2,L7,L8,D3],
                [R7,R4,R1,D4,D5,D6,D7,D8,D9] 
                ].

move_F_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,U3,U4,U5,U6,R1,R4,R7],
                [F3,F6,F9,F2,F5,F8,F1,F4,F7],
                [D3,R2,R3,D2,R5,R6,D1,R8,R9],
                [B1,B2,B3,B4,B5,B6,B7,B8,B9],
                [L1,L2,U9,L4,L5,U8,L7,L8,U7],
                [L3,L6,L9,D4,D5,D6,D7,D8,D9] 
                ].

move_B([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],


    NewState = [[R3,R6,R9,U4,U5,U6,U7,U8,U9],
                [F1,F2,F3,F4,F5,F6,F7,F8,F9],
                [R1,R2,D9,R4,R5,D8,R7,R8,D7],
                [B7,B4,B1,B8,B5,B2,B9,B6,B3],
                [U3,L2,L3,U2,L5,L6,U1,L8,L9],
                [D1,D2,D3,D4,D5,D6,L1,L4,L7] 
                ].

move_B_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[L7,L4,L1,U4,U5,U6,U7,U8,U9],
                [F1,F2,F3,F4,F5,F6,F7,F8,F9],
                [R1,R2,U1,R4,R5,U2,R7,R8,U3],
                [B3,B6,B9,B2,B5,B8,B1,B4,B7],
                [D7,L2,L3,D8,L5,L6,D9,L8,L9],
                [D1,D2,D3,D4,D5,D6,R9,R6,R3] 
                ].


move_M([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,B8,U3,U4,B5,U6,U7,B2,U9],
                [F1,U2,F3,F4,U5,F6,F7,U8,F9],
                [R1,R2,R3,R4,R5,R6,R7,R8,R9],
                [B1,D8,B3,B4,D5,B6,B7,D2,B9],
                [L1,L2,L3,L4,L5,L6,L7,L8,L9],
                [D1,F2,D3,D4,F5,D6,D7,F8,D9] 
                ].

move_M_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],


    NewState = [[U1,F2,U3,U4,F5,U6,U7,F8,U9],
                [F1,D2,F3,F4,D5,F6,F7,D8,F9],
                [R1,R2,R3,R4,R5,R6,R7,R8,R9],
                [B1,U8,B3,B4,U5,B6,B7,U2,B9],
                [L1,L2,L3,L4,L5,L6,L7,L8,L9],
                [D1,B8,D3,D4,B5,D6,D7,B2,D9] 
                ].

move_E([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,U3,U4,U5,U6,U7,U8,U9],
                [F1,F2,F3,L4,L5,L6,F7,F8,F9],
                [R1,R2,R3,F4,F5,F6,R7,R8,R9],
                [B1,B2,B3,R4,R5,R6,B7,B8,B9],
                [L1,L2,L3,B4,B5,B6,L7,L8,L9],
                [D1,D2,D3,D4,D5,D6,D7,D8,D9]
                ].

move_E_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,U3,U4,U5,U6,U7,U8,U9],
                [F1,F2,F3,R4,R5,R6,F7,F8,F9],
                [R1,R2,R3,B4,B5,B6,R7,R8,R9],
                [B1,B2,B3,L4,L5,L6,B7,B8,B9],
                [L1,L2,L3,F4,F5,F6,L7,L8,L9],
                [D1,D2,D3,D4,D5,D6,D7,D8,D9] 
                ].

move_S([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],


    NewState = [[U1,U2,U3,L8,L5,L2,U7,U8,U9],
                [F1,F2,F3,F4,F5,F6,F7,F8,F9],
                [R1,U4,R3,R4,U5,R6,R7,U6,R9],
                [B1,B2,B3,B4,B5,B6,B7,B8,B9],
                [L1,D4,L3,L4,D5,L6,L7,D6,L9],
                [D1,D2,D3,R8,R5,R2,D7,D8,D9] 
                ].

move_S_prime([UpFaceList, FrontFaceList, RightFaceList, BackFaceList, LeftFaceList, DownFaceList], NewState) :-
    UpFaceList = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFaceList = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFaceList = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFaceList = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFaceList = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFaceList = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    NewState = [[U1,U2,U3,R2,R5,R8,U7,U8,U9],
                [F1,F2,F3,F4,F5,F6,F7,F8,F9], 
                [R1,D6,R3,R4,D5,R6,R7,D4,R9],    
                [B1,B2,B3,B4,B5,B6,B7,B8,B9],    
                [L1,U6,L3,L4,U5,L6,L7,U4,L9],    
                [D1,D2,D3,L2,L5,L8,D7,D8,D9]     
        ].


%% all_moves(-Moves:list) is det.
%
%  Provides a list of all available move predicates (18 standard moves).
%
%  @param Moves List of predicate names, e.g., [move_U, move_U_prime, ...].
%
all_moves([move_U, move_U_prime, move_D, move_D_prime, move_R, move_R_prime,
    move_L, move_L_prime, move_F, move_F_prime, move_B, move_B_prime,
    move_M, move_M_prime, move_E, move_E_prime, move_S, move_S_prime]).


% --- Search Algorithms ---



%% solve_dls(+CurrentState:list, +PathSoFar:list, +DepthLimit:int, -Solution:list) is nondet.
%
%  Performs a Depth-Limited Search for a solution.
%
%  @param CurrentState The current state of the cube.
%  @param PathSoFar The list of moves made so far to reach CurrentState (reversed).
%  @param DepthLimit The maximum allowed depth (length of PathSoFar).
%  @param Solution The path found to the solved state (reversed).
%  @fail If no solution is found within the DepthLimit.
%
solve_dls(CurrentState, PathSoFar, _, PathSoFar) :-
    is_solved(CurrentState).

solve_dls(CurrentState, PathSoFar, DepthLimit, Solution) :-
    length(PathSoFar, CurrentDepth),
    CurrentDepth < DepthLimit,
    all_moves(Moves),
    member(Move, Moves),
    call(Move, CurrentState, NextState),
    solve_dls(NextState, [Move | PathSoFar], DepthLimit, Solution).


%% solve_ids(+StartState:list, -SolutionPath:list) is nondet.
%
%  Performs an Iterative Deepening Search for the shortest solution path.
%
%  @param StartState The initial state of the cube.
%  @param SolutionPath The shortest list of moves found to solve the cube (correct order).
%  @fail If no solution is found (should not happen for a solvable cube theoretically).
%
solve_ids(StartState, SolutionPath) :-
    between(0, infinite, DepthLimit), % Generuj limity 0, 1, 2...
    ( solve_dls(StartState, [], DepthLimit, SolutionPath) -> % Zkus DLS
        !                                       % Zastav hledání (Cut!)
    ;
        fail % DLS selhalo pro tento limit, zkus další (backtrack k between)
    ).

%% solve_dfs(+CurrentState:list, +PathSoFar:list, -Solution:list) is nondet.
%
%  Performs a Depth-First Search using a global visited state set.
%  Note: This is likely less efficient than IDS for finding shortest paths. Implemented just for testing
%  You need to activate the dynamic predicate
%
%  @param CurrentState The current state of the cube.
%  @param PathSoFar The list of moves made so far (reversed).
%  @param Solution The path found (reversed).
%  @fail If no solution is found or the entire state space reachable
%        without finding a solution has been visited.
%
solve_dfs(CurrentState, PathSoFar, PathSoFar) :-
    is_solved(CurrentState).

solve_dfs(CurrentState, _, _) :- 
    visited_state(CurrentState),
    !, fail.

solve_dfs(CurrentState, PathSoFar, Solution) :-
    assertz(visited_state(CurrentState)),
    all_moves(ListOfMoves),
    member(Move, ListOfMoves),
    call(Move, CurrentState, NewState),
    solve_dfs(NewState, [Move | PathSoFar],  Solution).

%% print_cube(+Cube:list) is det.
%
%  Prints the cube state to standard output according to the specified format.
%  Format: UP face (3 lines), LFRB faces (3 lines), DOWN face (3 lines).
%
%  @param Cube The cube state list [Up, Front, Right, Back, Left, Down].
%
print_cube([]).
print_cube([UpFace, FrontFace, RightFace, BackFace, LeftFace, DownFace]) :-
    UpFace    = [U1,U2,U3,U4,U5,U6,U7,U8,U9],
    FrontFace = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    RightFace = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    BackFace  = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    LeftFace  = [L1,L2,L3,L4,L5,L6,L7,L8,L9],
    DownFace  = [D1,D2,D3,D4,D5,D6,D7,D8,D9],

    format('~w~w~w~n', [U1,U2,U3]),
    format('~w~w~w~n', [U4,U5,U6]),
    format('~w~w~w~n', [U7,U8,U9]),
    format('~w~w~w ~w~w~w ~w~w~w ~w~w~w~n', [F1,F2,F3,R1,R2,R3,B1,B2,B3,L1,L2,L3]),
    format('~w~w~w ~w~w~w ~w~w~w ~w~w~w~n', [F4,F5,F6,R4,R5,R6,B4,B5,B6,L4,L5,L6]),
    format('~w~w~w ~w~w~w ~w~w~w ~w~w~w~n', [F7,F8,F9,R7,R8,R9,B7,B8,B9,L7,L8,L9]),
    format('~w~w~w~n', [D1,D2,D3]),
    format('~w~w~w~n', [D4,D5,D6]),
    format('~w~w~w~n~n', [D7,D8,D9]).


%% apply_moves(+Cube:list, +Moves:list) is det.
%
%  Applies a sequence of moves to a cube state and prints the state
%  after each move. Does not print the initial state.
%
%  @param Cube The starting cube state.
%  @param Moves A list of move predicate names (e.g., [move_U, move_R_prime]).
%
apply_moves(_, []).
apply_moves(Cube, [Move|Moves]):-
    call(Move, Cube, NewCube),
    print_cube(NewCube),
    apply_moves(NewCube, Moves).

%% start is det.
%
%  Main entry point of the program. Reads the cube, solves it using IDS,
%  and prints the initial state and the sequence of states during the solution.
%
start :-
    prompt(_, ''),
    ( read_cube(Cube) ->
        print_cube(Cube),
            % ( solve_dls(Cube, [], 4 , Solution) ->
            ( solve_ids(Cube, Solution) ->
                % writeln('Solution found (reversed path of moves):'),
                reverse(Solution, RSolution),
                apply_moves(Cube, RSolution)
                %, writeln(Solution) % used for checking
            ;
                write('Not sloved') % no chance of happening only for case of using diffren solve function
            )
        ;
        writeln('Cant parse input.')
    ),
    halt.