with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
With Ada.Text_IO;

with Parsing;

procedure Day06 is
    type Coordinates is record
        X : Integer;
        Y : Integer;
    end record;

    function "<"(A:Coordinates; B:Coordinates) return Boolean is
    begin
        if (A.X < B.X) then
            return True;
        elsif (A.X > B.X) then
            return False;
        elsif (A.Y < B.Y) then
            return True;
        else
            return False;
        end if;
    end "<";

    package Coordinates_Sets is new Ada.Containers.Ordered_Sets
                                 (Element_Type => Coordinates);
    use Coordinates_Sets;

    type Direction is record
        X : Integer range -1..1;
        Y : Integer range -1..1;
    end record;

    type Dir_Index is mod 4;

    Dirs : array (Dir_Index) of Direction := ((0,-1), (1,0), (0,1), (-1,0));

    Walls : Coordinates_Sets.Set;
    Initial_Position : Coordinates;
    Rows : Natural;
    Cols : Natural;

    procedure Load_Input(filename : String) is
        file : Ada.Text_IO.File_Type;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => Filename);
        Rows := 0;
        while not Ada.Text_IO.end_of_file(file) loop
            declare
                Line : String := Ada.Text_IO.get_line(file);
            begin
                Rows := Rows + 1;
                Cols := Line'Length;
                for C in 1 .. Cols loop
                    if Line(C) = '^' then
                        Initial_Position := (C, Rows);
                    elsif Line(C) = '#' then
                        Walls.Insert((C, Rows));
                    end if;
                end loop;
            end;
        end loop;
    end Load_Input;

    function Move_Position (Position: Coordinates; Dir: Direction) return Coordinates is
    begin
        return (Position.X + Dir.X, Position.Y + Dir.Y);
    end Move_Position;

    function Is_Outside_Area (Pos: Coordinates) return Boolean is
    begin
        if Pos.X < 1 or Pos.Y < 1 or Pos.X > Cols or Pos.Y > Rows then
            return True;
        end if;
        return False;
    end Is_Outside_Area;

    procedure Part1 is
        Visited : Coordinates_Sets.Set;
        Curr_Position : Coordinates := Initial_Position;
        Curr_Dir_Idx : Dir_Index := 0;
        Next_Position : Coordinates;
    begin
        while True loop
            Visited.Include(Curr_Position);
            Next_Position := Move_Position(Curr_Position, Dirs(Curr_Dir_Idx));
            if Walls.Contains(Next_Position) then
                Curr_Dir_Idx := Curr_Dir_Idx + 1;
            elsif Is_Outside_Area(Next_Position) then
                Ada.Text_IO.Put_Line(Count_Type'Image(Visited.Length));
                return;
            else
                Curr_Position := Next_Position;
            end if;
        end loop;
    end Part1;
        


begin
    Load_Input("inputs/test06.txt");
    Part1;
end Day06;
