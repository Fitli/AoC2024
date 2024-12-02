with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Parsing is
    function Split (str : String; sep : String) return Unbounded_String_Vectors.Vector is
        substrings : Unbounded_String_Vectors.Vector;
        I : Natural := str'First;
        Sep_Idx : Natural;
    begin
        while I in str'Range loop
            Sep_Idx := Index (Source => str, Pattern => sep, From => I);
            if Sep_Idx = 0 then
                substrings.Append(To_Unbounded_String(str(I..str'Last)));
                exit;
            end if;
            substrings.Append(To_Unbounded_String(str(I .. Sep_Idx-1)));
            I := Sep_Idx + sep'Length;
        end loop;
        
        return substrings;
    end Split;
end Parsing;
