with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Parsing;

procedure Day04 is
    --type Wordsearch is new Parsing.Unbounded_String_Vectors.Vector;
    My_WordSearch : Parsing.Unbounded_String_Vectors.Vector;

    procedure parse(Filename : String) is
        file : Ada.Text_IO.File_Type;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => Filename);
        while not Ada.Text_IO.end_of_file(file) loop
            My_WordSearch.Append(To_Unbounded_String(Ada.Text_IO.get_line(file)));
        end loop;
    end parse;

    function Count_Xmas(Str : String) return Natural is
    begin
        return Ada.Strings.Fixed.Count(Str, "XMAS") + Ada.Strings.Fixed.Count(Str, "SAMX");
    end Count_Xmas;

    function Count_Horizontal return Natural is
        Cnt : Natural := 0;
    begin
        for S of My_WordSearch loop
            Cnt := Cnt + Count_Xmas(To_String(S));
        end loop;
        return Cnt;
    end Count_Horizontal;

    

    function Count_Vertical return Natural is
        Cnt : Natural := 0;
        Rows : Natural := Natural(My_WordSearch.Length);
        Cols : Natural := Natural(Length(My_WordSearch(0)));
        WS : array (1 .. Rows) of String(1 .. Cols);
        S : String (1 .. Rows);
    begin
        for I in 1 .. Cols loop
            WS(I) := To_String(My_WordSearch.Element(I-1));
        end loop;
        for I in 1 .. Cols loop
            for J in 1 .. Rows loop
                S(J) := WS(J)(I);
            end loop;
            Cnt := Cnt + Count_Xmas(S);
        end loop;
        return Cnt;
    end Count_Vertical;

    function Count_Dioagonal_1 return Natural is
        Cnt : Natural := 0;
        Rows : Natural := Natural(My_WordSearch.Length);
        Cols : Natural := Natural(Length(My_WordSearch(0)));
        WS : array (1 .. Rows) of String(1 .. Cols);
        S : String (1 .. Natural'Max(Rows, Cols));
    begin
        for I in 1 .. Cols loop
            WS(I) := To_String(My_WordSearch.Element(I-1));
        end loop;
        for I in 1 .. Rows loop
            for J in 1 .. Natural'Max(Rows, Cols) loop
                declare
                    X : Natural := J;
                    Y : Natural := I+J-1;
                begin
                    if (X <= Cols and Y <= Rows) then 
                        S(J) := WS(Y)(X);
                    else
                        S(J) := ' ';
                    end if;
                end;
            end loop;
            Cnt := Cnt + Count_Xmas(S);
        end loop;

        
        for I in 2 .. Cols loop
            for J in 1 .. Natural'Max(Rows, Cols) loop
                declare
                    X : Natural := I+J-1;
                    Y : Natural := J;
                begin
                    if (X <= Cols and Y <= Rows) then 
                        S(J) := WS(Y)(X);
                    else
                        S(J) := ' ';
                    end if;
                end;
            end loop;
            Cnt := Cnt + Count_Xmas(S);
        end loop;
        return Cnt;
    end Count_Dioagonal_1;

    function Count_Dioagonal_2 return Natural is
        Cnt : Natural := 0;
        Rows : Natural := Natural(My_WordSearch.Length);
        Cols : Natural := Natural(Length(My_WordSearch(0)));
        WS : array (1 .. Rows) of String(1 .. Cols);
        S : String (1 .. Natural'Max(Rows, Cols));
    begin
        for I in 1 .. Cols loop
            WS(I) := To_String(My_WordSearch.Element(I-1));
        end loop;
        for I in 1 .. Rows loop
            for J in 1 .. Natural'Max(Rows, Cols) loop
                declare
                    X : Natural := J;
                    Y : Integer := Integer(I)-Integer(J)+1;
                begin
                    if (X <= Cols and Y > 0) then 
                        S(J) := WS(Y)(X);
                    else
                        S(J) := ' ';
                    end if;
                end;
            end loop;
            Cnt := Cnt + Count_Xmas(S);
        end loop;

        for I in 2 .. Cols loop
            for J in 1 .. Natural'Max(Rows, Cols) loop
                declare
                    X : Natural := I+J-1;
                    Y : Integer := Integer(Cols)-Integer(J)+1;
                begin
                    if (X <= Cols and Y > 0) then 
                        S(J) := WS(Y)(X);
                    else
                        S(J) := ' ';
                    end if;
                end;
            end loop;
            Cnt := Cnt + Count_Xmas(S);
        end loop;
        return Cnt;
    end Count_Dioagonal_2;

    procedure part1 is
        result : Natural := Count_Horizontal + Count_Vertical + Count_Dioagonal_1 + Count_Dioagonal_2;
    begin
        Ada.Text_IO.Put_Line(Natural'Image(result));
    end part1;

    function Is_Mas (first:Character; last:Character) return Boolean is
    begin
        return (first = 'M' and last = 'S') or (first = 'S' and last = 'M');
    end Is_Mas;
    
    function Is_X (tl:Character; tr:Character; bl:Character; br:Character) return Boolean is
    begin
        return Is_Mas(tl, br) and Is_Mas(tr, bl);
    end Is_X;

    function Count_X return Natural is
        Cnt : Natural := 0;
        Rows : Natural := Natural(My_WordSearch.Length);
        Cols : Natural := Natural(Length(My_WordSearch(0)));
        WS : array (1 .. Rows) of String(1 .. Cols);
    begin
        for I in 1 .. Cols loop
            WS(I) := To_String(My_WordSearch.Element(I-1));
        end loop;

        for X in 2 .. Cols-1 loop
            for Y in 2 .. Rows-1 loop
                if WS(Y)(X) = 'A' then
                    if Is_X(WS(Y-1)(X-1), WS(Y-1)(X+1), WS(Y+1)(X-1), WS(Y+1)(X+1)) then
                        Cnt := Cnt + 1;
                    end if;
                end if;
            end loop;
        end loop;
        return Cnt;
    end Count_X;

    procedure part2 is
        result : Natural := Count_X;
    begin
        Ada.Text_IO.Put_Line(Natural'Image(result));
    end part2;

begin
    parse("inputs/input04.txt");
    part1;
    part2;
end Day04;
        
