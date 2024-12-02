with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Parsing; use Parsing;

procedure Day02 is
    type Level is new Integer;

    package Reports is new Ada.Containers.Vectors
                                        (Index_Type => Natural,
                                         Element_Type => Level);
    
    package Report_Vectors is new Ada.Containers.Vectors
                                        (Index_Type => Natural,
                                         Element_Type => Reports.Vector,
                                         "=" => Reports."="
                                        );
    
    function Parse_Line(Line : String) return Reports.Vector is
        String_Vec : Parsing.Unbounded_String_Vectors.Vector;
        Report : Reports.Vector;
    begin
        String_Vec := Parsing.Split(Line, " ");
        for Num of String_Vec loop
            Report.Append(Level'Value(To_String(Num)));
        end loop;
        return Report;
    end Parse_Line;

    My_Reports : Report_Vectors.Vector;
    
    procedure Load_Input(Filename : String) is
        file : Ada.Text_IO.File_Type;
    begin
        Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Name => Filename);
        while not Ada.Text_IO.end_of_file(file) loop
            My_Reports.Append(Parse_Line(Ada.Text_IO.get_line(file)));
        end loop;
    end Load_Input;
        

    function Is_Safe(Report : Reports.Vector; Toleration : Natural := 0) return Boolean is
        type Direction is new Integer range -1..1;
        Dir : Direction := 0;
        Diff : Level;
        function Is_Unsafe_Diff return Boolean is
        begin
            if abs Diff > 3 then
                return True;
            elsif Diff = 0 then
                return True;
            elsif Direction(Diff / (abs Diff)) /= Dir then
                return True;
            end if;
            return False;
        end Is_Unsafe_Diff;
    begin
        for I in Report.First_Index+1 .. Report.Last_Index loop
            Diff := Report(I) - Report(I-1);
            if Dir = 0 and Diff /= 0 then
                Dir := Direction(Diff / (abs Diff));
            end if;
            if Is_Unsafe_Diff then
                if Toleration = 0 then
                    return False;
                end if;
                declare
                    Subreport1: Reports.Vector := Report;
                    Subreport2: Reports.Vector := Report;
                    Subreport3: Reports.Vector := Report;
                begin
                    Subreport1.Delete(I-1);
                    Subreport2.Delete(I);
                    Subreport3.Delete(Report.First_Index);
                    return Is_Safe(Subreport1, Toleration-1) or Is_Safe(Subreport2, Toleration-1)
                    or Is_Safe(Subreport3, Toleration-1);
                end;
            end if;
        end loop;
        return True;
    end Is_Safe;

    procedure execute(Toleration : Natural := 0) is
        Count : Natural := 0;
    begin
        for R of My_Reports loop            
            if Is_Safe(R, Toleration) then
                Count := Count + 1;
            end if;
        end loop;
        Put_Line(Natural'Image(Count));
    end execute;
    
    procedure part1 is
    begin
        execute;
    end part1;
    
    procedure part2 is
    begin
        execute(1);
    end part2;

begin
    Load_Input("inputs/input02.txt");
    part1;
    part2;
end Day02;
