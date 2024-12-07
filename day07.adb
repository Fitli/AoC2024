with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

with Parsing;

procedure Day07 is
    package Integer_Vectors is new Ada.Containers.Vectors
                                 (Index_Type => Natural, Element_Type => Long_Integer);
    use Integer_Vectors;
    
    type Equation is record
        Result : Long_Integer;
        Members : Integer_Vectors.Vector;
    end record;

    package Equation_Vectors is new Ada.Containers.Vectors
                                 (Index_Type => Natural, Element_Type => Equation);
    use Equation_Vectors;

    function Parse_Line (Line : String) return Equation is
        Tokens : Parsing.Unbounded_String_Vectors.Vector := Parsing.Split(Line, " ");
        First_T : String := To_String(Tokens.First_Element);
        Result : Long_Integer := Long_Integer'Value(First_T(First_T'First .. First_T'Last-1));
        Members : Integer_Vectors.Vector;
    begin
        for I in Tokens.First_Index+1 .. Tokens.Last_Index loop
            Members.Append(Long_Integer'Value(To_String(Tokens.Element(I))));
        end loop;
        return (Result, Members);
    end Parse_Line;

    Equations : Equation_Vectors.Vector;

    procedure Load_Input(Filename : String) is
        file : Ada.Text_IO.File_Type;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => Filename);
        while not Ada.Text_IO.end_of_file(file) loop
            declare
                Line : String := Ada.Text_IO.get_line(file);
            begin
                Equations.Append(Parse_Line(Line));
            end;
        end loop;
    end Load_Input;

    function Bigger_Pow_Of_10(Num : Long_Integer) return Long_Integer is
        P : Long_Integer := 10;
    begin
        while P <= Num loop
            P := P*10;
        end loop;
        return P;
    end;

    function Can_Be_True(Eq : Equation; Is_Part_2:Boolean) return Boolean is
    begin
        if Eq.Members.Length = 1 then
            if Eq.Result = Eq.Members.First_Element then
                return True;
            else
                return False;
            end if;
        end if;
        declare 
            Prefix : Integer_Vectors.Vector := Eq.Members;
            Last : Long_Integer := Eq.Members.Last_Element;
        begin
            Prefix.Delete_Last;
            if Last < Eq.Result then
                if Can_Be_True((Eq.Result - Last, Prefix), Is_Part_2) then
                    return True;
                end if;
            end if;
            if Eq.Result rem Last = 0 then
                if Can_Be_True((Eq.Result / Last, Prefix), Is_Part_2) then
                    return True;
                end if;
            end if;
            if Is_Part_2 then
                if Eq.Result /= Last and Eq.Result rem Bigger_Pow_Of_10(Last) = Last then
                    if Can_Be_True((Eq.Result / Bigger_Pow_Of_10(Last), Prefix), Is_Part_2) then
                        return True;
                    end if;
                end if;
            end if;
        end;
        return False;
    end Can_Be_True;

    procedure Print_Sum(Is_Part_2 : Boolean) is
        Sum : Long_Integer := 0;
    begin
        for Eq of Equations loop
            if Can_Be_True(Eq, Is_Part_2) then
                Sum := Sum + Eq.Result;
            end if;
        end loop;
        Ada.Text_IO.Put_Line(Long_Integer'Image(Sum));
    end Print_Sum;

    procedure part1 is
    begin
        Print_Sum(False);
    end part1;

    procedure part2 is
    begin
        Print_Sum(True);
    end part2;

begin
    Load_Input("inputs/input07.txt");
    part1;
    part2;
end Day07;

    
        
