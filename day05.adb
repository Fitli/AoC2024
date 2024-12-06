with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Parsing;

procedure Day05 is
    type Manual_Page is new Natural;
    
    type Rule is record
        before : Manual_Page;
        after : Manual_Page;
    end record;

    function "<"(L, R:Rule) return Boolean is
    begin
        return L.before < R.before;
    end "<";

    package Page_Vectors is new Ada.Containers.Vectors
                                    (Index_Type => Natural,
                                     Element_Type => Manual_Page);
    use Page_Vectors;

    package Update_Vectors is new Ada.Containers.Vectors
                                    (Index_Type => Natural,
                                     Element_Type => Page_Vectors.Vector);
    use Update_Vectors;

    package Page_Sets is new Ada.Containers.Ordered_Sets
                                 (Element_Type => Manual_Page);
    use Page_Sets;

    package Natural_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => Manual_Page,
        Element_Type    => Natural);
     use Natural_Maps;

    package Page_Set_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => Manual_Page,
        Element_Type    => Page_Sets.Set);
    use Page_Set_Maps;

    package Rule_Vectors is new Ada.Containers.Vectors
                                    (Index_Type => Natural,
                                     Element_Type => Rule);
    use Rule_Vectors;

    package Rule_Vectors_Sorting is
        new Rule_Vectors.Generic_Sorting;

    use Rule_Vectors_Sorting;

    Rules : Rule_Vectors.Vector;
    Updates : Update_Vectors.Vector;

    function Parse_Rule(Line : String) return Rule is
        String_V : Parsing.Unbounded_String_Vectors.Vector := Parsing.Split(Line, "|");
    begin
        return (Manual_Page'Value(To_String(String_V(0))), Manual_Page'Value(To_String(String_V(1))));
    end Parse_Rule;

    function Parse_Update(Line : String) return Page_Vectors.Vector is
        String_V : Parsing.Unbounded_String_Vectors.Vector := Parsing.Split(Line, ",");
        Update : Page_Vectors.Vector;
    begin
        for String_Page of String_V loop
            Update.Append(Manual_Page'Value(To_String(String_Page)));
        end loop;
        return Update;
    end Parse_Update;
    
    procedure Load_Input(Filename : String) is
        file : Ada.Text_IO.File_Type;
        Parsing_Updates : Boolean := False;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => Filename);
        while not Ada.Text_IO.end_of_file(file) loop
            declare
                Line : String := Ada.Text_IO.get_line(file);
            begin
                if Line = "" then
                    Parsing_Updates := True;
                elsif Parsing_Updates then
                    Updates.Append(Parse_Update(Line));
                else
                    Rules.Append(Parse_Rule(Line));
                end if;
            end;
        end loop;
    end Load_Input;

    function Is_Valid_Update(Update : Page_Vectors.Vector) return Boolean is
        Read : Page_Sets.Set;
        Idx : Natural := 0;
    begin
        for Pg of Update loop
            Read.Insert(Pg);
            for Rl of Rules loop
                if Rl.before = Pg and Read.Contains(Rl.after) then
                    return False;
                end if;
            end loop;
        end loop;
        return True;
    end Is_Valid_Update;

    function Get_Middle_Page(Update : Page_Vectors.Vector) return Manual_Page is
        Idx : Natural := Natural(Update.Length / 2);
    begin
        return Update(Idx);
    end Get_Middle_Page;

    procedure Part1 is
        Result : Natural := 0;
    begin
        Sort(Rules);
        for Update of Updates loop
            if Is_Valid_Update(Update) then
                Result := Result + Natural(Get_Middle_Page(Update));
            end if;
        end loop;
        Ada.Text_IO.Put_Line(Natural'Image(Result));
    end Part1;

    function Reorder(Update : Page_Vectors.Vector) return Page_Vectors.Vector is
        Reordered : Page_Vectors.Vector;
        Num_Pages_Before : Natural_Maps.Map;
        Pages_After : Page_Set_Maps.Map;
        Pages_Used : Page_Sets.Set;
    begin
        for P of Update loop
            Num_Pages_Before.Include(P, 0);
            Pages_After.Include(P, Empty_Set);
            Pages_Used.Insert(P);
        end loop;
        for R of Rules loop
            if Pages_Used.Contains(R.before) and Pages_Used.Contains(R.after) then
                Num_Pages_Before(R.after) := Num_Pages_Before(R.after) + 1;
                Pages_After(R.before).Insert(R.after);
            end if;
        end loop;
        while Pages_Used.Length > 0 loop
            for I in Pages_After.Iterate loop
                declare
                    P : Manual_Page := Key(I);
                begin
                    if Pages_Used.Contains(P) and Num_Pages_Before(P) = 0 then
                        Reordered.Append(P);
                        for Af of Pages_After(P) loop
                            Num_Pages_Before(Af) := Num_Pages_Before(Af) - 1;
                        end loop;
                        Pages_Used.Delete(P);
                        exit;
                    end if;
                end;
            end loop;
        end loop;
        return Reordered;
    end Reorder;

    procedure Part2 is
        Result : Natural := 0;
    begin
        for Update of Updates loop
            if not Is_Valid_Update(Update) then
                Result := Result + Natural(Get_Middle_Page(Reorder(Update)));
            end if;
        end loop;
        Ada.Text_IO.Put_Line(Natural'Image(Result));
    end Part2;
        
begin
    Load_Input("inputs/input05.txt");
    Part1;
    Part2;
end Day05;
