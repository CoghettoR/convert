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

-- STATUT: expérimental

pragma License (Gpl);

with Ada.Strings;
with Ada.Strings.Fixed;

package body digit is
   function To_Natural (D : T_Digit) return Natural is
   begin
      return Natural (D);
   end To_Natural;

   function to_digit (c : Character) return T_Digit is
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
   function to_character (d : T_Digit) return Character is
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

   function to_natural (d : t_digits) return Natural is
      flag : Boolean := False;
   begin
      return tmp : Natural do
         tmp := 0;
         for k in d'Range loop
            if flag then
               tmp := 10 * tmp + Natural (d (k));
            else
               if d (k) /= 0 then
                  flag := True;
                  tmp  := Natural (d (k));
               end if;
            end if;
         end loop;
         pragma Assert (d = to_digits (tmp));
      end return;
   end to_natural;

   function to_digits (i : Natural) return t_digits is
      use Ada.Strings, Ada.Strings.Fixed;
      s : constant String := Trim (i'Img, Both);
   begin
      return tmp : t_digits (1 .. s'Length) do
         for l in s'Range loop
            tmp (l) := to_digit (s (l));
         end loop;
         pragma Assert (i = to_natural (tmp));
      end return;
   end to_digits;
end digit;
