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

with Ada.Text_Io;
with Ada.Wide_Text_Io;--use Ada_Wide_Text_Io;

with Magyar;use Magyar;

procedure Test_Magyar is
   Nbs       : t_Nombres   := Init;
   use Variantes;
   Variantes : T_Variantes := Init_Variantes (Nbs);

begin
   for L in T_Lettres loop
      Ada.Wide_Text_IO.Put (To_Lower_Character (L) & " ");
   end loop;
   Ada.Text_IO.New_Line;
   for L in T_Lettres loop
      Ada.Wide_Text_IO.Put (To_Upper_Character (L) & " ");
   end loop;

   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("========");
   for I in 1E6+200000 .. 1E6+201000 loop
      Ada.Wide_Text_IO.Put_Line (To_Wide_String (Variantes, I));
   end loop;
end Test_Magyar;
