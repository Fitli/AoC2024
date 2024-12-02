with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

package Parsing is
    package Unbounded_String_Vectors is new Ada.Containers.Vectors
                                      (Index_Type => Natural,
                                       Element_Type => Unbounded_String);
    use Unbounded_String_Vectors;
    
    function Split (str : String; sep : String) return Unbounded_String_Vectors.Vector;
end Parsing;
