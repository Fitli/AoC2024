with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day08 is
    type Coordinates is record
        X : Integer;
        Y : Integer;
    end record;

    function "+"(A: Coordinates; B:Coordinates) return Coordinates is
    begin
        return (A.X+B.X, A.Y+B.Y);
    end;

    function "-"(A: Coordinates; B:Coordinates) return Coordinates is
    begin
        return (A.X-B.X, A.Y-B.Y);
    end;

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
    
    package Coordinates_Vectors is new Ada.Containers.Vectors
                                           (Index_Type => Natural, Element_Type => Coordinates);
    
    use Coordinates_Vectors;

    package Coordinates_Sets is new Ada.Containers.Ordered_Sets
                                           (Element_Type => Coordinates);
    
    use Coordinates_Sets;

    package Anthena_Maps is new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => Character,
        Element_Type    => Coordinates_Vectors.Vector);

    use Anthena_Maps;

    Anthenas : Anthena_Maps.Map;
    Rows : Natural := 0;
    Cols : Natural := 0;

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
                    if Line(C) /= '.' then
                        if Anthenas.Contains(Line(C)) then
                            Anthenas(Line(C)).Append((C, Rows));
                        else
                            Anthenas.Include(Line(C), To_Vector((C, Rows), 1));
                        end if;
                    end if;
                end loop;
            end;
        end loop;
    end Load_Input;

    function In_Area(Point : Coordinates) return Boolean is
    begin
        return Point.X >= 1 and Point.X <= Cols and Point.Y >= 1 and Point.Y <= Rows;
    end In_Area;

    procedure Part1 is
        Antinodes : Coordinates_Sets.Set;
    begin
        for C in Anthenas.Iterate loop
            for A1 of Anthenas(C) loop
                for A2 of Anthenas(C) loop
                    if A1 /= A2 then
                        declare
                            Antinode : Coordinates := A1 + (A1 - A2);
                        begin
                            if In_Area(Antinode) then
                                Antinodes.Include(Antinode);
                            end if;
                        end;
                    end if;
                end loop;
            end loop;
        end loop;
        Ada.Text_IO.Put_Line(Count_Type'Image(Antinodes.Length));
    end Part1;

    procedure Part2 is
        Antinodes : Coordinates_Sets.Set;
    begin
        for C in Anthenas.Iterate loop
            for A1 of Anthenas(C) loop
                for A2 of Anthenas(C) loop
                    if A1 /= A2 then
                        declare
                            Antinode : Coordinates := A1;
                            Step : Coordinates := A1 - A2;
                        begin
                            while In_Area(Antinode) loop
                                Antinodes.Include(Antinode);
                                Antinode := Antinode + Step;
                            end loop;
                        end;
                    end if;
                end loop;
            end loop;
        end loop;
        Ada.Text_IO.Put_Line(Count_Type'Image(Antinodes.Length));
    end Part2;
        
begin
    Load_Input("inputs/input08.txt");
    Part1;
    Part2;
end Day08;
