--  Copyright (C) 2019 Roland Coghetto.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);
-- 
-- https://fr.wikipedia.org/wiki/Nombres_en_fran%C3%A7ais#cite_ref-1

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;
procedure Main is
   type t_orthographe is (nouvelle, ancienne);
   type t_region_linguistique is
     (france, canada, Pays_Africains_Francophones_Sauf_RDC,Belgique, RDC, suisse);

--   subtype t_nb is Natural;
   subtype t_digit is Natural range 0 .. 9;
   type t_digits is array (Natural range <>) of t_digit;

   function to_digit (c : Character) return t_digit with
      Pre => Ada.Characters.Handling.Is_Digit (c) is
   begin
      case c is
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when others =>
            return 0;
      end case;
   end to_digit;
   function to_character (d : t_digit) return Character is
   begin
      case d is
         when 0 =>
            return '0';
         when 1 =>
            return '1';
         when 2 =>
            return '2';
         when 3 =>
            return '3';
         when 4 =>
            return '4';
         when 5 =>
            return '5';
         when 6 =>
            return '6';
         when 7 =>
            return '7';
         when 8 =>
            return '8';
         when 9 =>
            return '9';
      end case;
   end to_character;

   function to_string (d : t_digits) return String is
   begin
      return tmp : String (d'Range) do
         for i in d'Range loop
            tmp (i) := to_character (d (i));
         end loop;
      end return;
   end to_string;

   function to_digits (i : Integer) return t_digits;
   function to_integer (d : t_digits) return Integer is
      --    i    : Integer := 1;
      flag : Boolean := False;
   begin
      --  ada.text_io.put_line("DEBUT"&to_string(d));
      return tmp : Integer do
         tmp := 0;
         for k in d'Range loop

            if flag then
               tmp := 10 * tmp + d (k);
            --    i   := i * 10;
            else
               if d (k) /= 0 then
                  flag := True;
                  tmp  := d (k);
                  -- i    := i * 10;
               end if;
            end if;
         end loop;
         --  ada.text_io.put_line("FIN"&tmp'img);
         pragma Assert (d = to_digits (tmp));
      end return;
   end to_integer;

   function to_digits (i : Integer) return t_digits is
      use Ada.Strings, Ada.Strings.Fixed;
      s : constant String := Trim (i'Img, Both);
   begin
      return tmp : t_digits (1 .. s'Length) do
         for l in s'Range loop
            tmp (l) := to_digit (s (l));
         end loop;
         pragma Assert (i = to_integer (tmp));
      end return;
   end to_digits;

   function to_nombre
     (i        : Integer;
      variante : t_region_linguistique) return String
   is

      function to_mot (d : t_digit) return String is
      begin
         case d is
            when 0 =>
               return "zero";
            when 1 =>
               return "un";
            when 2 =>
               return "deux";
            when 3 =>
               return "trois";
            when 4 =>
               return "quatre";
            when 5 =>
               return "cinq";
            when 6 =>
               return "six";
            when 7 =>
               return "sept";
            when 8 =>
               return "huit";
            when 9 =>
               return "neuf";
         end case;
      end to_mot;

      function to_mot_dizaine (d : t_digit) return String is
      begin
         case d is
            when 0 =>
               return "";
            when 1 =>
               return "dix";
            when 2 =>
               return "vingt";
            when 3 =>
               return "trente";
            when 4 =>
               return "quarante";
            when 5 =>
               return "cinquante";
            when 6 =>
               return "soixante";
            when 7 =>
--               case variante is
  --                when france | canada =>
    --                 return "soixante-dix";
      --            when belgique | RDC =>
                     return "septante";
        --          when others =>
          --           return "*";
         --      end case;
            when 8 =>
               return "quatre-vingt";
            when 9 =>
               return "nonante";
         end case;
      end to_mot_dizaine;

      -- function to_nombre (i : Integer) return String;

      function to_mot
        (ds          : t_digits;
         cents       : Boolean       := True;
         orthographe : t_orthographe := nouvelle) return String with
         Pre => ds'Length > 0 is
         function reduce (ds : t_digits) return t_digits with
            Pre => ds'Length > 0 is
         begin
            return to_digits (to_integer (ds));
         end reduce;
         tmp : t_digits := reduce (ds);
         n   : Integer  := to_integer (tmp);
      begin
         pragma Assert (to_integer (tmp) = to_integer (ds));
         if 1_000_000_000 <= n and n <= Integer'Last then
            declare
               n1 : Integer := n / 1_000_000_000;
               n2 : Integer := n - n1 * 1_000_000_000;
            --begin
         --   if tmp'length = 7 then declare n1:integer:=to_integer(tmp(1..1));
            --       n2:integer:=to_integer(tmp(2..7));
            begin
               if n1 = 1 then
                  if n2 = 0 then
                     return "un milliard";
                  else
                     return "un milliard " & to_nombre (n2, variante);--end if;
                  end if;
               else
                  if n2 = 0 then
                     return to_nombre (n1, variante) & " milliards";
                  else
                     return to_nombre (n1, variante) &
                       " milliards " &
                       to_nombre (n2, variante);
                  end if;
               end if;
            end;
         end if;
         if 1_000_000 <= n and n <= 999_999_999 then
            declare
               n1 : Integer := n / 1_000_000;
               n2 : Integer := n - n1 * 1_000_000;
            --begin
         --   if tmp'length = 7 then declare n1:integer:=to_integer(tmp(1..1));
            --       n2:integer:=to_integer(tmp(2..7));
            begin
               if n1 = 1 then
                  if n2 = 0 then
                     return "un million";
                  else
                     return "un million " & to_nombre (n2, variante);--end if;
                  end if;
               else
                  if n2 = 0 then
                     return to_nombre (n1, variante) & " millions";
                  else
                     return to_nombre (n1, variante) &
                       " millions " &
                       to_nombre (n2, variante);
                  end if;
               end if;
            end;
         end if;
         if tmp'Length = 6 then
            declare
               ds31 : t_digits (1 .. 3) :=
                 (1 => tmp (1), 2 => tmp (2), 3 => tmp (3));
               ds32 : t_digits (1 .. 3) :=
                 (1 => tmp (4), 2 => tmp (5), 3 => tmp (6));
            begin
               if to_integer (ds32) = 0 then
                  return to_mot (ds31, cents => False) & "-mille";
               else
                  return to_mot (ds31, cents => False) &
                    "-mille-" &
                    to_mot (ds32);
               end if;
            end;
         end if;
         if tmp'Length = 5 then
            declare
               ds2 : t_digits (1 .. 2) := (1 => tmp (1), 2 => tmp (2));
               ds3 : t_digits (1 .. 3) :=
                 (1 => tmp (3), 2 => tmp (4), 3 => tmp (5));
            begin
               if to_integer (ds3) = 0 then
                  return to_mot (ds2) & "-mille";
               else
                  return to_mot (ds2) & "-mille-" & to_mot (ds3);
               end if;
            end;
         end if;
         if tmp'Length = 4 then
            declare
               ds3 : t_digits (1 .. 3) :=
                 (1 => tmp (2), 2 => tmp (3), 3 => tmp (4));
            begin --null;
               --   Ada.Text_IO.Put_Line
               --   (to_string (ds3) & ":" & to_integer (ds3)'Img);
               if tmp (1) = 1 then
                  if to_integer (ds3) = 0 then
                     return "mille";
                  else

                     return "mille-" & to_mot (reduce (ds3));
                  end if;
               else
                  return to_mot (tmp (1)) &
                    "-mille-" &
                    to_mot (reduce (ds3), cents => True);
               end if;
            end;
         end if;
         if tmp'Length = 2 then
	    case To_Integer(Tmp) is when
	      11=> return "onze";
	      when 12=> return "douze";
	      when 13=>return "treize";
	      when 14=>return "quatorze";
	      when  15=>return "quinze";
	      when  16=>return "seize";
	   --   when  70=>
	 when 70 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix";
	       when Belgique|RDC|Suisse=>return "septante";
	    end case;
	 when 71 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-onze";
	       when Belgique|RDC|Suisse=>return "septante-et-un";
	    end case;
	 when 72 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-douze";
	       when Belgique|RDC|Suisse=>return "septante-deux";
	    end case;
	 when 73 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-treize";
	       when Belgique|RDC|Suisse=>return "septante-trois";
	    end case;
	 when 74 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-quatorze";
	       when Belgique|RDC|Suisse=>return "septante-quatre";
	    end case;
	 when 75 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-quinze";
	       when Belgique|RDC|Suisse=>return "septante-cinq";
	    end case;
	 when 76 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-seize";
	       when Belgique|RDC|Suisse=>return "septante-six";
	    end case;
	 when 77 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix-sept";
	       when Belgique|RDC|Suisse=>return "septante-sept";
	    end case;
	 when 78 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix-huit";
	       when Belgique|RDC|Suisse=>return "septante-huit";
	    end case;
	 when 79 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix-neuf";
	       when Belgique|RDC|Suisse=>return "septante-neuf";
 	    end case;
	 when 90 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix";
	       when Belgique|RDC|Suisse=>return "nonante";
	    end case;
	 when 91 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-onze";
	       when Belgique|RDC|Suisse=>return "nonante-et-un";
	    end case;
	 when 92 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-douze";
	       when Belgique|RDC|Suisse=>return "nonante-deux";
	    end case;
	 when 93 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-treize";
	       when Belgique|RDC|Suisse=>return "nonante-trois";
	    end case;
	 when 94 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-quatorze";
	       when Belgique|RDC|Suisse=>return "nonante-quatre";
	    end case;
	 when 95 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-quinze";
	       when Belgique|RDC|Suisse=>return "nonante-cinq";
	    end case;
	 when 96 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-seize";
	       when Belgique|RDC|Suisse=>return "nonante-six";
	    end case;
	 when 97 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix-sept";
	       when Belgique|RDC|Suisse=>return "nonante-sept";
	    end case;
	 when 98 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix-huit";
	       when Belgique|RDC|Suisse=>return "nonante-huit";
	    end case;
	 when 99 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix-neuf";
	       when Belgique|RDC|Suisse=>return "nonante-neuf";
	    end case;
		 
	       --end case;
--            if tmp (1) = 1 then
  --             case tmp (2) is
    --              when 1 =>
      --               return "onze";
        --          when 2 =>
          --           return "douze";
            -- -     when 3 =>
             --        return "treize";
               --   when 4 =>
                 --    return "quatorze";
                --  when 5 =>
                --     return "quinze";
                --  when 6 =>
                --     return "seize";
                 when others =>
                     null;
               end case;
--            end if;
            if tmp (2) = 0 then
               if tmp (1) = 8 then
                  return to_mot_dizaine (tmp (1)) & "s";
               else
                  return to_mot_dizaine (tmp (1));
               end if;
            elsif tmp (2) = 1 and Tmp(1) /= 8 then -- pas de 'et` avec 81
               case orthographe is
                  when ancienne =>
                     return to_mot_dizaine (tmp (1)) &
                       " et " &
                       to_mot (tmp (2));
                  when nouvelle =>
                     return to_mot_dizaine (tmp (1)) &
                       "-et-" &
                       to_mot (tmp (2));
               end case;
            else
               return to_mot_dizaine (tmp (1)) & "-" & to_mot (tmp (2));
            end if;
         --        return"*";
         elsif tmp'Length = 3 then
            declare
               ds2 : t_digits (1 .. 2) := (1 => tmp (2), 2 => tmp (3));
            begin

               if tmp (1) = 1 then
                  if tmp (2) = 0 then
                     return "cent" & to_mot (ds2);
                  else
                     return "cent-" & to_mot (ds2);
                  end if;
               else
                  if tmp (2) = 0 then
                     if to_integer (reduce (tmp (2 .. 3))) = 0 and cents then
                        return to_mot (tmp (1)) & "-cents";
                     else
                        return to_mot (tmp (1)) & "-cent-" & to_mot (ds2);
                     end if;
                  else
                     return to_mot (tmp (1)) & "-cent-" & to_mot (ds2);
                  end if;
               end if;
            end;
         elsif tmp (1) /= 0 then
--            return "-" & to_mot (tmp (1));
	                return to_mot (tmp (1));

         else
            return "";
         end if;
      end to_mot;

   begin
      case i is
         when 0 =>
            return "zero";
         when 1 =>
            return "un";
         when 2 =>
            return "deux";
         when 3 =>
            return "trois";
         when 4 =>
            return "quatre";
         when 5 =>
            return "cinq";
         when 6 =>
            return "six";
         when 7 =>
            return "sept";
         when 8 =>
            return "huit";
         when 9 =>
            return "neuf";
         when 10 =>
            return "dix";
         --  when 11=> return "onze";
         --when 12=> return "douze";
--         when 13=> return "treize";
         --       when 14 => return "quatorze";
         --     when 15=> return "quinze";
         --   when 16=> return "seize";

         -- when 17=> return "dix-sept";
	 when 70 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix";
	       when Belgique|RDC|Suisse=>return "septante";
	    end case;
	 when 71 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-onze";
	       when Belgique|RDC|Suisse=>return "septante-et-un";
	    end case;
	 when 72 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-douze";
	       when Belgique|RDC|Suisse=>return "septante-deux";
	    end case;
	 when 73 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-treize";
	       when Belgique|RDC|Suisse=>return "septante-trois";
	    end case;
	 when 74 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-quatorze";
	       when Belgique|RDC|Suisse=>return "septante-quatre";
	    end case;
	 when 75 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-quinze";
	       when Belgique|RDC|Suisse=>return "septante-cinq";
	    end case;
	 when 76 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-seize";
	       when Belgique|RDC|Suisse=>return "septante-six";
	    end case;
	 when 77 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix-sept";
	       when Belgique|RDC|Suisse=>return "septante-sept";
	    end case;
	 when 78 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix-huit";
	       when Belgique|RDC|Suisse=>return "septante-huit";
	    end case;
	 when 79 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "soixante-dix-neuf";
	       when Belgique|RDC|Suisse=>return "septante-neuf";
	    end case;
	 when 90 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix";
	       when Belgique|RDC|Suisse=>return "nonante";
	    end case;
	 when 91 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-onze";
	       when Belgique|RDC|Suisse=>return "nonante-et-un";
	    end case;
	 when 92 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-douze";
	       when Belgique|RDC|Suisse=>return "nonante-deux";
	    end case;
	 when 93 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-treize";
	       when Belgique|RDC|Suisse=>return "nonante-trois";
	    end case;
	 when 94 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-quatorze";
	       when Belgique|RDC|Suisse=>return "nonante-quatre";
	    end case;
	 when 95 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-quinze";
	       when Belgique|RDC|Suisse=>return "nonante-cinq";
	    end case;
	 when 96 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-seize";
	       when Belgique|RDC|Suisse=>return "nonante-six";
	    end case;
	 when 97 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix-sept";
	       when Belgique|RDC|Suisse=>return "nonante-sept";
	    end case;
	 when 98 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix-huit";
	       when Belgique|RDC|Suisse=>return "nonante-huit";
	    end case;
	 when 99 => 
	    case Variante is
	       when France| Canada| Pays_Africains_Francophones_Sauf_RDC => return "quatre-vingt-dix-neuf";
	       when Belgique|RDC|Suisse=>return "nonante-neuf";
	    end case;
	    
         when others =>

            return to_mot (to_digits (i));
--            return "*";
      end case;
   end to_nombre;

begin
  
   for i in 171_101..172_021 loop
      --1_500_000_390 .. 1_500_000_393 loop
      Ada.Text_IO.Put_Line (i'Img & ":" & to_nombre (i, variante => france));
   end loop;
end Main;
