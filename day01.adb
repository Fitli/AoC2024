with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day01 is
    type Location is new Natural;
    type Distance is new Natural;
    type Similarity_Score is new Natural;
    
    package Location_Vectors is new Ada.Containers.Vectors
                                        (Index_Type => Natural,
                                         Element_Type => Location);
    use Location_Vectors;

    package Locaion_Vectors_Sorting is
        new Location_Vectors.Generic_Sorting;

    use Locaion_Vectors_Sorting;

    List1 : Location_Vectors.Vector;
    List2 : Location_Vectors.Vector;

    procedure Parse_Line(Line : String;
                         Loc1 : out Location;
                         Loc2 : out Location) is
        space : constant Natural := index(Line, " ");
    begin
        Loc1 := Location'Value(Line(Line'First .. space-1));
        Loc2 := Location'Value(Line(space+1 .. Line'Last));
    end Parse_Line;
                        
        
    
    procedure Load_Input(Filename : String) is
        file : Ada.Text_IO.File_Type;
        loc1 : Location;
        loc2 : Location;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => Filename);
        while not Ada.Text_IO.end_of_file(file) loop
            Parse_Line(Ada.Text_IO.get_line(file), loc1, loc2);
            List1.append(loc1);
            List2.append(loc2);
        end loop;
    end Load_Input;

    function Get_Distance(Loc1 : Location;
                          Loc2 : Location) return Distance is
    begin
        return (if Loc1 > Loc2 then Distance(Loc1 - Loc2) else Distance(Loc2 - Loc1));
    end Get_Distance;

    function part1 return Distance is
        sorted_List1 : Location_Vectors.Vector := List1;
        sorted_List2 : Location_Vectors.Vector := List2;
        sum : Distance := 0;
    begin
        Sort(sorted_List1);
        Sort(sorted_List2);
        for I in sorted_List1.First_Index .. sorted_List1.Last_Index loop
            sum := sum + Get_Distance(sorted_List1(I), sorted_List2(I));
        end loop;
        return sum;
    end part1;

    function part2 return Similarity_Score is
        sorted_List1 : Location_Vectors.Vector := List1;
        sorted_List2 : Location_Vectors.Vector := List2;
        score : Similarity_Score := 0;
        I : Natural := sorted_List2.First_Index;
        prev : Location := 0;
        count_L : Natural := 0;
    begin
        Sort(sorted_List1);
        sorted_List1.Append(0); -- dirty trick to repeat the computation one more time at the end
        Sort(sorted_List2);
        for loc of sorted_List1 loop
            if loc /= prev then
                while sorted_List2(I) <= prev loop
                    if sorted_List2(I) = prev then
                        score := score + Similarity_Score(prev) * Similarity_Score(count_L);
                    end if;
                    I := I+1;
                    if I > sorted_List2.last_Index then
                        return score;
                    end if;
                end loop;
                count_L := 0;
            end if;
            prev := loc;
            count_L := count_L + 1;
        end loop;
        return score;
    end part2;

begin
    Load_Input("input01.txt");
    Ada.Text_IO.Put_Line(Distance'Image(part1));
    Ada.Text_IO.Put_Line(Similarity_Score'Image(part2));
end Day01;
        
        
