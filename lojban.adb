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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with digit; use digit;

package body Lojban is

   function To_Nombre (N : Natural; Kiho : Boolean := False) return String is
   begin
      case N is
         when 0 =>
            return "no";
         when 1 =>
            return "pa";
         when 2 =>
            return "re";
         when 3 =>
            return "ci";
         when 4 =>
            return "vo";
         when 5 =>
            return "mu";
         when 6 =>
            return "xa";
         when 7 =>
            return "ze";
         when 8 =>
            return "bi";
         when 9 =>
            return "so";
         when others =>
            declare
               s   : constant String := Trim (N'Img, Both);
               T   : t_digits (s'Range);
               Tmp : Unbounded_String;
            begin
               T := to_digits (N);
               case Kiho is
                  when False =>
                     for I in T'Range loop
                        Ada.Strings.Unbounded.Append
                          (Tmp,
                           To_Nombre (To_Natural (T (I))));
                     end loop;
                  when True =>
                     for I in T'Range loop
                        if I >= 1
                          and then
                          ((T'Last - I + 1) mod 3 = 0 and
                           To_Natural (T (I)) = 0)
                        then
                           null;
                        elsif I >= 2
                          and then
                          ((T'Last - I + 2) mod 3 = 0 and
                           To_Natural (T (I - 1)) = 0 and
                           To_Natural (T (I)) = 0)
                        then
                           null;
                        elsif I >= 3
                          and then
                          ((T'Last - I + 3) mod 3 = 0 and
                           To_Natural (T (I - 2)) = 0 and
                           To_Natural (T (I - 1)) = 0 and
                           To_Natural (T (I)) = 0)
                        then
                           null;
                        else
                           Ada.Strings.Unbounded.Append
                             (Tmp,
                              To_Nombre (To_Natural (T (I))));
                        end if;
                        if (T'Last - I) mod 3 = 0 and I /= T'Last then
                           Tmp := Tmp & "ki'o";
                        end if;
                     end loop;
               end case;
               return To_String (Tmp);
            end;
      end case;
   end To_Nombre;

end Lojban;
