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

with Ada.Characters.Handling;

package digit is

   type T_Digit is private;
--   subtype T_Digit is Natural range 0 .. 9;--private;

   function To_Natural (D : T_Digit) return Natural;
   type t_digits is array (Natural range <>) of T_Digit;

   function to_digits (i : Natural) return t_digits;
   function to_natural (d : t_digits) return Natural;
   function to_string (d : t_digits) return String;
   function to_character (d : T_Digit) return Character;
private

   type T_Digit is new Natural range 0 .. 9;
   function to_digit (c : Character) return T_Digit with
      Pre => Ada.Characters.Handling.Is_Digit (c);

end digit;
