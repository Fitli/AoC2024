with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat;   use GNAT.Regpat;

procedure Day03 is

    function Eval (Line : String; Matches : Match_Array) return Natural is
        First_Num : Natural := Natural'Value(Line(Matches(1).First .. Matches(1).Last));
        Second_Num : Natural := Natural'Value(Line(Matches(2).First .. Matches(2).Last));
    begin
        return First_Num * Second_Num;
    end Eval;
    
    procedure part1 is
        file : Ada.Text_IO.File_Type;
        Count : Natural := 0;
        Re : constant Pattern_Matcher := Compile ("mul\((\d+),(\d+)\)");
        Matches : Match_Array (0 .. 2);
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => "inputs/input03.txt");
        while not Ada.Text_IO.end_of_file(file) loop
            declare
                Line : String := Ada.Text_IO.Get_Line(file);
                I : Natural := Line'First;
            begin
                while I in Line'Range loop
                    Match(Re, Line(I .. Line'Last), Matches);
                    if Matches(0) = No_Match then
                        exit;
                    end if;
                    Count := Count +  Eval(Line, Matches);
                    I := Matches(0).Last;
                end loop;
            end;
        end loop;
        Ada.Text_IO.Close(file);
        Put_Line(Natural'Image(Count));
    end part1;

    procedure part2 is
        file : Ada.Text_IO.File_Type;
        Count : Natural := 0;
        Re : constant Pattern_Matcher := Compile ("mul\((\d+),(\d+)\)|do\(\)|don't\(\)");
        Matches : Match_Array (0 .. 2);
        Enabled : Boolean := True;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => "inputs/input03.txt");
        while not Ada.Text_IO.end_of_file(file) loop
            declare
                Line : String := Ada.Text_IO.Get_Line(file);
                I : Natural := Line'First;
            begin
                while I in Line'Range loop
                    Match(Re, Line(I .. Line'Last), Matches);
                    if Matches(0) = No_Match then
                        exit;
                    elsif Line(Matches(0).First .. Matches(0).Last) = "do()" then
                        Enabled := True;
                    elsif Line(Matches(0).First .. Matches(0).Last) = "don't()" then
                        Enabled := False;
                    elsif Enabled then
                        Count := Count +  Eval(Line, Matches);
                    end if;
                    I := Matches(0).Last;
                end loop;
            end;
        end loop;
        Put_Line(Natural'Image(Count));
    end part2;

begin
    part1;
    part2;
end Day03;
